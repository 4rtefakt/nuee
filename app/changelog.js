// CHANGELOG.md from the list the application shows in Help › What's new (interface/nouveautes.js),
// in English. construire.js runs it before each build; `node changelog.js` runs it alone.
const fs = require('fs');
const os = require('os');
const path = require('path');
const { pathToFileURL } = require('url');

async function ecrireChangelog() {
  // read as an ES module (a .mjs copy: the package itself is CommonJS)
  const copie = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-changelog-')), 'nouveautes.mjs');
  fs.copyFileSync(path.join(__dirname, 'interface', 'nouveautes.js'), copie);
  const { NOUVEAUTES } = await import(pathToFileURL(copie).href);
  fs.rmSync(path.dirname(copie), { recursive: true, force: true });
  const texte = ['# Changes', '',
    'What changed in each version: the same list as *Help › What\'s new* in the application',
    '(in French: *Aide › Quoi de neuf*). Downloads: [Releases](https://github.com/4rtefakt/nuee/releases).', '',
    ...NOUVEAUTES.flatMap((v) => [`## ${v.version} — ${v.date}`, '', ...v.en.map((l) => `- ${l}`), ''])].join('\n');
  fs.writeFileSync(path.join(__dirname, '..', 'CHANGELOG.md'), texte);
  return NOUVEAUTES.length;
}

if (require.main === module) ecrireChangelog().then((n) => console.log(`CHANGELOG.md : ${n} versions`));
module.exports = { ecrireChangelog };
