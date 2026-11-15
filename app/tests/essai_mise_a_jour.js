// The update from 0.9.9 keeps Nuée's Start menu and desktop shortcuts, and with them its taskbar
// pin (installateur.nsh). Windows, in CI only: it installs Nuée for the user running it.
// 0.9.9's installer, from its GitHub release, installs it; its two shortcuts are marked (their
// description) and their file index noted. This version's installer, built around a stand-in
// application (construire.js --installateur), then runs as 0.9.9's updater runs it (/S
// /currentuser /D=<folder>, without --updated: main.js of 0.9.9), and once more as this version's
// does (with --updated). Each time: Settings › Apps gives the new version, the folder holds the new
// application, and both shortcuts are the same files (same index, still marked), not new ones.
// Electron-builder's uninstaller unpins Nuée (WinShell::UninstAppUserModelId, UninstShortcut) only
// where it deletes them, so kept shortcuts mean a kept pin, which a runner can't show.
//   node tests/essai_mise_a_jour.js        (CI=true: GitHub Actions)
const { execFileSync, spawnSync } = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');

if (process.platform !== 'win32' || process.env.CI !== 'true') {
  console.error('Seulement en CI, sous Windows : cet essai installe Nuée.');
  process.exit(1);
}
const VERSION = JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'package.json'), 'utf8')).version;
const CLE = 'HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\3dabb207-3e0d-5aa0-bdfc-da17bd7f0503';
const DOSSIER = path.join(process.env.LOCALAPPDATA, 'Programs', 'Nuee');
const MARQUE = 'essai-mise-a-jour';
const tmp = fs.mkdtempSync(path.join(os.tmpdir(), 'essai-maj-'));
const pause = (ms) => new Promise((r) => setTimeout(r, ms));
let echecs = 0;
function verifier(ok, nom) {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}`);
  if (!ok) echecs += 1;
}
const powershell = (code) => execFileSync('powershell', ['-NoProfile', '-NonInteractive', '-Command', code], { encoding: 'utf8' }).trim();
const version = () => {
  try { return (execFileSync('reg', ['query', CLE, '/v', 'DisplayVersion'], { encoding: 'utf8' }).match(/DisplayVersion\s+REG_SZ\s+(\S+)/) || [])[1] || null; } catch { return null; }
};
const raccourcis = () => [path.join(process.env.APPDATA, 'Microsoft', 'Windows', 'Start Menu', 'Programs', 'Nuée.lnk'),
  path.join(powershell("[Environment]::GetFolderPath('Desktop')"), 'Nuée.lnk')];
// each shortcut: its file index (a new file gets another, even where NTFS tunnelling gives it the
// old one's creation time) and its description
const etat = () => raccourcis().map((f) => (fs.existsSync(f) ? {
  f, index: fs.statSync(f, { bigint: true }).ino, description: powershell(`(New-Object -ComObject WScript.Shell).CreateShortcut('${f.replace(/'/g, "''")}').Description`),
} : { f, index: null, description: null }));
async function installer(fichier, args, attendue) {
  const r = spawnSync(fichier, args, { stdio: 'inherit', windowsHide: true });
  if (r.status !== 0) throw new Error(`${path.basename(fichier)} ${args.join(' ')} : code ${r.status}`);
  for (let i = 0; i < 120 && version() !== attendue; i += 1) await pause(500);  // (an installer may finish in a child process)
  return version();
}

(async () => {
  if (fs.existsSync(DOSSIER) || version()) throw new Error(`Nuée est déjà installée (${DOSSIER}) : l'essai ne la remplace pas.`);
  // 0.9.9, as published
  const ancien = path.join(tmp, 'Nuee-Setup-0.9.9.exe');
  const r = await fetch('https://github.com/4rtefakt/nuee/releases/download/v0.9.9/Nuee-Setup-0.9.9.exe');
  if (!r.ok) throw new Error(`téléchargement de la 0.9.9 : ${r.status}`);
  fs.writeFileSync(ancien, Buffer.from(await r.arrayBuffer()));
  verifier(await installer(ancien, ['/S', '/currentuser'], '0.9.9') === '0.9.9' && fs.existsSync(path.join(DOSSIER, 'Nuee.exe')), '0.9.9 installée pour cet utilisateur');
  for (const { f } of etat()) powershell(`$s = (New-Object -ComObject WScript.Shell).CreateShortcut('${f.replace(/'/g, "''")}'); $s.Description = '${MARQUE}'; $s.Save()`);
  const avant = etat();
  verifier(avant.every((x) => x.index !== null && x.description === MARQUE), `ses deux raccourcis, marqués : ${avant.map((x) => x.f).join(' ; ')}`);

  // this version's installer, around a stand-in application (a small program named Nuee.exe)
  const application = path.join(tmp, 'application');
  fs.mkdirSync(path.join(application, 'resources'), { recursive: true });
  const programme = path.join(process.env.SystemRoot, 'System32', 'whoami.exe');
  fs.copyFileSync(programme, path.join(application, 'Nuee.exe'));
  execFileSync(process.execPath, [path.join(__dirname, '..', 'construire.js'), '--installateur', application, path.join(tmp, 'sortie')], { stdio: 'inherit' });
  const nouveau = path.join(tmp, 'sortie', `Nuee-Setup-${VERSION}.exe`);

  const memes = (apres) => apres.every((x, i) => x.index !== null && x.index === avant[i].index && x.description === MARQUE);
  for (const [nom, args] of [['comme la mise à jour de la 0.9.9 la lance (sans --updated)', ['/S', '/currentuser', `/D=${DOSSIER}`]],
    ['comme celle de cette version (avec --updated)', ['/S', '--updated', '/currentuser', `/D=${DOSSIER}`]]]) {
    const v = await installer(nouveau, args, VERSION);
    const apres = etat();
    verifier(v === VERSION && fs.statSync(path.join(DOSSIER, 'Nuee.exe')).size === fs.statSync(programme).size, `${VERSION} installée ${nom}`);
    verifier(memes(apres), `les mêmes raccourcis, gardés ${nom} : ${apres.map((x) => `${path.basename(path.dirname(x.f))} index ${x.index === avant[apres.indexOf(x)].index ? 'le même' : 'autre'}, ${x.description === MARQUE ? 'marqué' : `« ${x.description} »`}`).join(' ; ')}`);
  }
  const desinstallateur = path.join(DOSSIER, 'Uninstall Nuee.exe');
  if (fs.existsSync(desinstallateur)) spawnSync(desinstallateur, ['/currentuser', '/S'], { stdio: 'inherit' });
  console.log(echecs ? `\n${echecs} vérification(s) en échec.` : '\nTout est bon.');
  process.exit(echecs ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(1); });
