// Personal access tokens, one per host (Zenodo's for now: zenodo.js), kept in the profile encrypted
// by Electron's safeStorage (DPAPI on Windows, the Keychain on macOS, the secret service or KWallet
// on Linux). Never logged, and never given back to the interface: main.js only tells it whether
// one is kept. Without encryption (Linux without a keyring, where safeStorage only has its
// basic_text backend, a key written in Chromium itself), a token is kept for the session only, in
// memory. Kept small, for any host that takes a token (a git host's too).
//   creerJetons({ fichier, coffre }): `fichier`, where the encrypted tokens go ({ "<host>": base64 });
//   `coffre`, safeStorage (or a stand-in, for the checks: tests/essai_zenodo.js).
const fs = require('fs');

function creerJetons({ fichier, coffre, plateforme = process.platform }) {
  const session = new Map();
  const chiffrable = () => {
    try {
      if (!coffre.isEncryptionAvailable()) return false;
      return !(plateforme === 'linux' && typeof coffre.getSelectedStorageBackend === 'function' && coffre.getSelectedStorageBackend() === 'basic_text');
    } catch { return false; }
  };
  const lus = () => {
    try {
      const o = JSON.parse(fs.readFileSync(fichier, 'utf8'));
      return o && typeof o === 'object' && !Array.isArray(o) ? o : {};
    } catch { return {}; }
  };
  const ecrire = (o) => fs.writeFileSync(fichier, JSON.stringify(o, null, 1), { mode: 0o600 });
  const a = (o, hote) => Object.prototype.hasOwnProperty.call(o, hote);
  return {
    // whether a token kept now outlives the session
    persistant: chiffrable,
    // the token of `hote` kept: true when encrypted in the profile, false when for this session only
    garder(hote, jeton) {
      session.delete(hote);
      if (chiffrable()) {
        try {
          const o = lus();
          o[hote] = coffre.encryptString(jeton).toString('base64');
          ecrire(o);
          return true;
        } catch { /* a read-only profile: for this session */ }
      }
      session.set(hote, jeton);
      return false;
    },
    // the token of `hote`, or null
    lire(hote) {
      if (session.has(hote)) return session.get(hote);
      const o = lus();
      if (!a(o, hote) || typeof o[hote] !== 'string' || !chiffrable()) return null;
      try { return coffre.decryptString(Buffer.from(o[hote], 'base64')); } catch { return null; }
    },
    oublier(hote) {
      session.delete(hote);
      const o = lus();
      if (!a(o, hote)) return;
      delete o[hote];
      try { ecrire(o); } catch { /* a read-only profile */ }
    },
  };
}

module.exports = { creerJetons };
