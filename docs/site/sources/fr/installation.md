<!-- description: Installer Nuée sous Windows, Debian, Ubuntu et les autres distributions Linux. -->
# Installation

Nuée {{version}} fonctionne sous Windows 10 et 11 (64 bits) et sous Linux. Les fichiers sont sur la
[page de la version {{version}}]({{telechargement:publication}}) de GitHub ; les versions
précédentes sur la page des [publications]({{telechargement:toutes}}).

## Windows

1. Téléchargez [{{fichier:windows}}]({{telechargement:windows}}).
2. Lancez-le et choisissez de l'installer pour vous seul (sans droits d'administrateur) ou pour
   tous les utilisateurs de l'ordinateur.
3. Windows peut dire qu'il a protégé votre ordinateur, car l'installateur n'est pas signé :
   cliquez sur *Informations complémentaires*, puis *Exécuter quand même*.

R est inclus : il n'y a rien d'autre à installer, et un R déjà présent sur l'ordinateur n'est ni
utilisé ni modifié. Nuée apparaît dans le menu Démarrer ; un double-clic sur un projet `.nuee`
l'ouvre dans Nuée. On le désinstalle depuis *Paramètres › Applications* ; vos projets restent là
où vous les avez enregistrés, et vos réglages sont gardés.

### Mises à jour

Au démarrage, Nuée demande à GitHub s'il existe une nouvelle version (rien d'autre n'est envoyé).
S'il y en a une, la barre d'état le dit ; un clic télécharge son installateur, vérifie son
empreinte SHA-256 auprès de GitHub, propose d'enregistrer vos projets, l'installe et redémarre
Nuée. *Aide › Chercher une nouvelle version au démarrage* coupe cette recherche ; *Aide › Chercher
une mise à jour* la fait à tout moment.

### Dans un organisme

Pour les services informatiques : installation silencieuse pour un utilisateur ou pour tout
l'ordinateur, les réglages pour Intune, Configuration Manager, les stratégies de groupe et PDQ
Deploy (commandes, règle de détection), une stratégie qui coupe la recherche de mises à jour pour
tous, ce que Nuée écrit où et ce qu'il envoie sur le réseau : voir
[le guide de déploiement](https://github.com/4rtefakt/nuee/blob/main/docs/deploiement.md) (en
anglais).

## Linux

Sous Linux, Nuée utilise le R installé sur l'ordinateur (une version de R dépend des
bibliothèques de sa distribution : elle ne peut pas être emportée comme sous Windows).

### Debian, Ubuntu et dérivées

Téléchargez [{{fichier:deb}}]({{telechargement:deb}}), puis, dans un terminal, dans le dossier
du fichier :

```
sudo apt install ./{{fichier:deb}}
```

apt installe aussi R et les paquets R dont Nuée a besoin. Nuée apparaît dans le menu des
applications. Chaque changement de Nuée est vérifié avec ce paquet installé sous Ubuntu 22.04 et
24.04 et Debian 12, avec leur propre R.

### Autres distributions

Installez d'abord R depuis votre distribution, puis les paquets dont Nuée a besoin, dans R :

```
install.packages(c("httpuv", "jsonlite", "openxlsx", "haven",
  "readxl", "foreign", "nlme", "mgcv", "MASS", "rpart", "cluster",
  "RSQLite", "ranger"))
```

Puis l'un des deux :

- [{{fichier:appimage}}]({{telechargement:appimage}}) : rendez-le exécutable
  (`chmod +x {{fichier:appimage}}`), puis lancez-le. Il demande FUSE 2 (le paquet `libfuse2` dans
  la plupart des distributions). Sous Ubuntu 24.04 et après, préférez le `.deb` : il installe le
  profil AppArmor dont le bac à sable de Chromium a besoin, et une AppImage doit y être lancée avec
  `--no-sandbox`.
- [{{fichier:targz}}]({{telechargement:targz}}) : décompressez-le, puis lancez `./nuee`.

### Bon à savoir

- Nuée cherche `Rscript` dans le `PATH`, puis dans `/usr/local/bin`, `/usr/bin` et
  `/opt/R/<version>/bin` ; lancez-le avec `RSCRIPT=/chemin/vers/Rscript` pour un autre R.
- Avec un R plus ancien que 4.4 (celui d'Ubuntu 22.04 ou 24.04, ou de Debian 12), les fichiers R
  (`.rds`, `.RData`) ne s'ouvrent pas, car en lire un pourrait exécuter du code
  (CVE-2024-27322) : enregistrez le tableau en CSV ou en Excel.
- Les projets `.usp` demandent `python3`.
- Les réglages sont dans `~/.config/Nuee`. Au démarrage, Nuée demande à GitHub s'il existe une
  nouvelle version ; un clic dans la barre d'état ouvre sa page.

## Ensuite

Ouvrez un tableau (*Fichier › Ouvrir*, Ctrl+O), ou essayez *Aide › Projet d'exemple*, puis suivez
[un tutoriel](page:tutoriels) ou [le guide](page:guide.demarrer).
