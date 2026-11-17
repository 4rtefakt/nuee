"""An example client of Nuée's engine protocol (docs/protocole.md), with nothing but Python's
standard library.

It starts the engine from the sources, as the desktop application does (Rscript moteur/serveur.R
<port>, the session's token in NUEE_JETON, a temporary folder of its own), asks for the protocol's
version, opens the repository's example project, fits one of its models and prints its R² and Q²,
then stops the engine.

    python outils/exemple_client.py [--rscript <Rscript>] [--modele <number>]

Rscript is looked for in RSCRIPT, then on Windows where R's installer puts it for one user
(%LOCALAPPDATA%\\Programs\\R\\R-4.6.2\\bin), then on the PATH.
"""

import argparse
import json
import os
import secrets
import shutil
import socket
import subprocess
import sys
import tempfile
import threading
import time
import urllib.error
import urllib.request
from pathlib import Path

RACINE = Path(__file__).resolve().parent.parent  # Nuée's sources: moteur/, exemple/...


class ErreurMoteur(Exception):
    """A request the engine refused: its HTTP status, its message, the whole answer."""

    def __init__(self, statut, message, reponse=None):
        super().__init__(f"{statut} {message}")
        self.statut, self.message, self.reponse = statut, message, reponse


def trouver_rscript(donne=None):
    candidats = [donne, os.environ.get("RSCRIPT")]
    if os.name == "nt":
        bin_r = Path(os.environ.get("LOCALAPPDATA", "")) / "Programs" / "R" / "R-4.6.2" / "bin"
        candidats += [str(bin_r / "x64" / "Rscript.exe"), str(bin_r / "Rscript.exe")]
    candidats.append(shutil.which("Rscript"))
    for c in candidats:
        if c and Path(c).is_file():
            # (on Windows, bin\Rscript.exe only starts bin\x64\Rscript.exe, which is R itself:
            # stopping the first would leave R running)
            x64 = Path(c).parent / "x64" / "Rscript.exe"
            return str(x64) if os.name == "nt" and x64.is_file() else c
    sys.exit("Rscript not found: give it with --rscript, or set RSCRIPT.")


def port_libre():
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


class Moteur:
    """Nuée's R engine, started for this client and stopped with it."""

    def __init__(self, rscript, langue="en"):
        self.rscript, self.langue = rscript, langue
        self.port = port_libre()
        self.jeton = secrets.token_hex(32)  # 32 random bytes, as the application draws them
        self.dossier = tempfile.mkdtemp(prefix="nuee-client-")  # R's temporary files go here
        self.sortie = []  # what the engine wrote, for when it stops
        # (127.0.0.1 is never reached through a proxy)
        self.http = urllib.request.build_opener(urllib.request.ProxyHandler({}))
        self.processus = None

    def demarrer(self, attente=180):
        env = dict(os.environ, NUEE_JETON=self.jeton, TMPDIR=self.dossier, TMP=self.dossier, TEMP=self.dossier)
        self.processus = subprocess.Popen(
            [self.rscript, str(RACINE / "moteur" / "serveur.R"), str(self.port)],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env=env,
            creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
        pret = threading.Event()

        def lire():  # (read to the end, so that the engine never waits on a full pipe)
            for ligne in self.processus.stdout:
                texte = ligne.decode("utf-8", "replace").rstrip()
                self.sortie.append(texte)
                if texte.startswith("PRET"):
                    pret.set()

        threading.Thread(target=lire, daemon=True).start()
        fin = time.monotonic() + attente
        while not pret.wait(0.2):
            if self.processus.poll() is not None or time.monotonic() > fin:
                self.arreter()
                raise RuntimeError("The engine didn't start:\n" + "\n".join(self.sortie[-20:]))
        return self

    def demander(self, nom, **parametres):
        """POST /api/<nom> with the parameters as JSON; the answer, or ErreurMoteur."""
        requete = urllib.request.Request(
            f"http://127.0.0.1:{self.port}/api/{nom}", method="POST",
            data=json.dumps(parametres).encode("utf-8"),
            headers={"Content-Type": "application/json", "X-Langue": self.langue,
                     "Cookie": f"nuee_{self.port}={self.jeton}"})
        try:
            with self.http.open(requete, timeout=900) as r:
                return json.loads(r.read().decode("utf-8"))
        except urllib.error.HTTPError as e:
            corps = e.read().decode("utf-8", "replace")
            try:
                reponse = json.loads(corps)
                message = reponse.get("erreur", corps)
            except ValueError:  # (a refused host or a path outside /api/ answers in plain text)
                reponse, message = None, corps
            raise ErreurMoteur(e.code, message, reponse) from None

    def arreter(self):
        if self.processus and self.processus.poll() is None:
            self.processus.terminate()
            try:
                self.processus.wait(10)
            except subprocess.TimeoutExpired:
                self.processus.kill()
                self.processus.wait()
        for _ in range(20):  # (Windows may hold R's last files a moment)
            shutil.rmtree(self.dossier, ignore_errors=True)
            if not os.path.exists(self.dossier):
                break
            time.sleep(0.25)

    def __enter__(self):
        return self.demarrer()

    def __exit__(self, *erreur):
        self.arreter()


def nombre(x):
    return "-" if x is None else f"{x:.3f}"


def principal():
    a = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    a.add_argument("--rscript", help="the Rscript to start the engine with")
    a.add_argument("--modele", type=int, help="the number of the example's model to fit (by default, its first PLS)")
    args = a.parse_args()

    with Moteur(trouver_rscript(args.rscript)) as moteur:
        try:
            p = moteur.demander("protocole")
        except ErreurMoteur as e:
            if e.statut != 400:
                raise
            p = {"version": "1.0", "nuee": None, "demandes": []}  # (Nuée 1.0.x: before this request)
        print(f"Engine on port {moteur.port}: protocol {p['version']}, Nuée {p['nuee']}, {len(p['demandes'])} requests")

        projet = moteur.demander("ouvrir", chemin=str(RACINE / "exemple" / "orchards.json"))
        uid = projet["observations"]["uid"]
        print(f"Project {projet['id']}: {projet['nom']}, {len(uid) if isinstance(uid, list) else 1} observations, "
              f"{len(projet['variables'])} variables")
        for d in projet["modeles"]:
            print(f"  model {d['numero']}: {d['nom']}, {d['type']}, {d['n_obs']} observations")

        choisi = next((d for d in projet["modeles"] if d["numero"] == args.modele), None) if args.modele else \
            next((d for d in projet["modeles"] if d["type"] == "PLS"), projet["modeles"][0])
        if choisi is None:
            sys.exit(f"The example project has no model {args.modele}.")
        m = moteur.demander("ajuster", projet=projet["id"], numero=choisi["numero"])
        y = ", ".join(m["noms_y"]) or "-"
        print(f"\nModel {m['nom']} ({m['type']}, Y: {y}) fitted as {m['id']}, {m['A']} components:")
        c = m["composantes"]  # (a table: an array per column)
        print("  component   R²X(cum)   R²Y(cum)   Q²(cum)")
        for i, k in enumerate(c["composante"]):
            r2y = c["R2Ycum"][i] if "R2Ycum" in c else None
            print(f"  {k:>9}   {nombre(c['R2Xcum'][i]):>8}   {nombre(r2y):>8}   {nombre(c['Q2cum'][i]):>7}")
        r2 = c["R2Ycum"][-1] if "R2Ycum" in c else c["R2Xcum"][-1]
        print(f"R² = {nombre(r2)}, Q² = {nombre(c['Q2cum'][-1])}")

        # (what an error looks like: a status, and {"erreur": message} in the request's language)
        try:
            moteur.demander("modele", modele="M999")
        except ErreurMoteur as e:
            print(f"\nAn error, as the engine sends it: HTTP {e.statut}, {e.message}")


if __name__ == "__main__":
    principal()
