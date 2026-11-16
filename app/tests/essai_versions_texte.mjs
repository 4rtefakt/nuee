// Checks interface/versions_texte.js (the words and numbers of File › Versions and of What
// changed), without the application: dates in each language, a version's name and its copy's file
// name, the status bar's indicator, the summary of a divergence, the help's links, the values and
// changes of key results, the models of two sides matched and ordered, and the strings of both
// languages with the same placeholders.
//   node tests/essai_versions_texte.mjs
import {
  dateLongue, dateVersion, sansExtension, titreVersion, nomFichier, nomDeChemin, dossierDe, memeFichier, nomHote, hoteAdresse, liensConnexion,
  indicateurVersions, resumeDivergence, phraseDivergence, descriptionVersion, nombreVersions, valeurResultat, texteP, ecartResultat, drapeauxEntree,
  apparierModeles, ordonnerComparaisons, entreesChangees, TEXTES_VERSIONS,
} from '../interface/versions_texte.js';
import { changerLangue } from '../interface/i18n.js';

// (i18n.js puts the language on the page: none here)
globalThis.document ??= { documentElement: {} };

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${detail}`}`);
  if (!ok) echecs += 1;
};
const egal = (nom, obtenu, attendu) => verifier(nom, JSON.stringify(obtenu) === JSON.stringify(attendu), `${JSON.stringify(obtenu)} au lieu de ${JSON.stringify(attendu)}`);
// (Intl writes a narrow no-break space in some times and dates, and a no-break space in others)
const espaces = (x) => (typeof x === 'string' ? x.replace(/[\u202f\u00a0]/g, ' ') : x);
const egalTexte = (nom, obtenu, attendu) => egal(nom, espaces(obtenu), attendu);
const fuseau = 'UTC';

console.log('Les dates');
changerLangue('en');
const iso = '2026-11-12T14:32:00.000Z';
egalTexte('en anglais, en entier', dateLongue(iso, { fuseau }), '12 Nov 2026, 14:32');
egalTexte('en anglais, comme la description d’une restauration', dateLongue(iso, { fuseau, style: 'long' }), '12 November 2026 at 14:32');
const maintenant = new Date('2026-11-13T09:00:00Z');
egalTexte('aujourd’hui, hier, avant', [dateVersion('2026-11-13T08:15:00Z', { fuseau, maintenant }), dateVersion('2026-11-12T23:59:00Z', { fuseau, maintenant }),
  dateVersion('2026-11-11T10:00:00Z', { fuseau, maintenant })].join(' | '), 'today, 08:15 | yesterday, 23:59 | 11 Nov 2026, 10:00');
egal('une date illisible reste telle quelle', [dateLongue('pas une date'), dateVersion('')], ['pas une date', '']);
changerLangue('fr');
egalTexte('en français', dateLongue(iso, { fuseau }), '12 nov. 2026, 14:32');
egalTexte('en français, aujourd’hui et hier', [dateVersion('2026-11-13T08:15:00Z', { fuseau, maintenant }), dateVersion('2026-11-12T23:59:00Z', { fuseau, maintenant })].join(' | '),
  'aujourd’hui, 08:15 | hier, 23:59');
egalTexte('le jour dans le fuseau donné : 23 h 30 à New York est la veille', dateVersion('2026-11-13T04:30:00Z', { fuseau: 'America/New_York', maintenant: new Date('2026-11-13T15:00:00Z') }),
  'hier, 23:30');

console.log('Le nom d’une version ouverte, et celui de sa copie');
egal('sans l’extension du fichier', [sansExtension('vergers.nuee'), sansExtension('relevés 2026.xlsx'), sansExtension('a.b.nuee'), sansExtension('notes')], ['vergers', 'relevés 2026', 'a.b', 'notes']);
egalTexte('en français', titreVersion('vergers.nuee', iso, { fuseau }), 'vergers — version du 12 nov. 2026, 14:32');
changerLangue('en');
const titre = titreVersion('orchards.nuee', iso, { fuseau });
egalTexte('en anglais', titre, 'orchards — version of 12 Nov 2026, 14:32');
egalTexte('un nom de fichier sans deux-points ni point final', nomFichier(`${titre}.`), 'orchards — version of 12 Nov 2026, 14-32');
egal('ni barres, guillemets, étoiles ; jamais vide', [nomFichier('a/b\\c "d" *e?'), nomFichier('...'), nomFichier('')], ['a-b-c -d- -e-', 'projet', 'projet']);
egal('le nom et le dossier d’un chemin', [nomDeChemin('C:\\Données\\vergers.nuee'), nomDeChemin('/home/m/vergers.nuee'), dossierDe('C:\\Données\\vergers.nuee'), dossierDe('/home/m/v.nuee')],
  ['vergers.nuee', 'vergers.nuee', 'C:\\Données', '/home/m']);
verifier('le même fichier sous Windows, quels que soient les séparateurs et les majuscules', memeFichier('C:\\Données\\Vergers.nuee', 'c:/données/vergers.nuee', true));
verifier('pas ailleurs, ni sans chemin', !memeFichier('/home/m/Vergers.nuee', '/home/m/vergers.nuee', false) && !memeFichier(null, 'a', true));

console.log('GitHub et GitLab');
egal('le nom du service', [nomHote('github.com'), nomHote('gitlab.com'), nomHote('gitlab.inrae.fr'), nomHote('git.example.org'), nomHote(null)], ['GitHub', 'GitLab', 'GitLab', 'git.example.org', '']);
egal('le serveur d’une adresse en https seulement', [hoteAdresse(' https://GitHub.com/moi/vergers '), hoteAdresse('http://github.com/a/b'), hoteAdresse('git@github.com:a/b.git'), hoteAdresse('')],
  ['github.com', '', '', '']);
const sans = liensConnexion('');
verifier('sans adresse, les pages des deux', sans.github.depot === 'https://github.com/new' && /^https:\/\/gitlab\.com\/-\/user_settings\/personal_access_tokens\?/.test(sans.gitlab.jeton)
  && /scopes=write_repository/.test(sans.gitlab.jeton), JSON.stringify(sans));
const gl = liensConnexion('https://gitlab.inrae.fr/equipe/suivi');
verifier('le GitLab de l’adresse, celui d’un institut', !gl.github && gl.gitlab.depot === 'https://gitlab.inrae.fr/projects/new' && gl.gitlab.jeton.startsWith('https://gitlab.inrae.fr/-/user_settings/'),
  JSON.stringify(gl));
const gh = liensConnexion('https://github.com/moi/vergers');
verifier('GitHub seul pour une adresse de GitHub', gh.github && !gh.gitlab && gh.github.jeton === 'https://github.com/settings/personal-access-tokens/new', JSON.stringify(gh));
const autre = liensConnexion('https://git.example.org/equipe/suivi.git');
verifier('un autre serveur : les pages des deux', autre.github && autre.gitlab && autre.gitlab.depot === 'https://gitlab.com/projects/new', JSON.stringify(autre));

console.log('L’indicateur de la barre d’état');
egal('rien sans versions', [indicateurVersions(null), indicateurVersions({ suivi: false, depot: true })], [null, null]);
egal('gardées sur l’ordinateur seulement', indicateurVersions({ suivi: true, versions: 12, distant: null }), { texte: '', titre: '12 versions kept on this computer' });
egal('une seule', indicateurVersions({ suivi: true, versions: 1, distant: null }).titre, '1 version kept on this computer');
const distant = { url: 'https://github.com/moi/vergers.git', hote: 'github.com' };
egal('à envoyer et à recevoir', indicateurVersions({ suivi: true, versions: 5, distant, avance: 2, retard: 1 }),
  { texte: '↑2 ↓1', titre: '2 to send, 1 to get — GitHub, since the last send or get' });
egal('à envoyer seulement', indicateurVersions({ suivi: true, versions: 5, distant, avance: 3, retard: 0 }).texte, '↑3');
egal('à jour', indicateurVersions({ suivi: true, versions: 5, distant, avance: 0, retard: 0 }), { texte: '', titre: 'Versions up to date with GitHub, as of the last send or get' });
egal('un dépôt que Nuée ne joint pas (SSH) : son adresse', indicateurVersions({ suivi: true, versions: 5, distant: { url: 'git@example.org:a/b.git', hote: null }, avance: 0, retard: 0 }).titre,
  'Versions up to date with git@example.org:a/b.git, as of the last send or get');
changerLangue('fr');
egal('en français', indicateurVersions({ suivi: true, versions: 5, distant: { url: 'https://gitlab.com/a/b.git', hote: 'gitlab.com' }, avance: 0, retard: 4 }),
  { texte: '↓4', titre: '4 à recevoir — GitLab, depuis le dernier envoi ou la dernière réception' });

console.log('Des versions des deux côtés');
const v = (oid, date, description = oid) => ({ oid, description, auteur: { nom: 'M', email: 'm@example.org' }, date, noms: [] });
const liste = [v('c3', '2026-11-12T10:00:00Z'), v('c2', '2026-11-10T10:00:00Z'), v('c1', '2026-11-01T10:00:00Z')];
const r = { resultat: 'diverge', mien: [v('c3', '2026-11-12T10:00:00Z')], leur: [v('e2', '2026-11-11T10:00:00Z'), v('e1', '2026-11-10T12:00:00Z')] };
const d = resumeDivergence(r, liste);
egal('depuis la dernière version commune', [d.depuis, d.mien.length, d.leur.length], ['2026-11-10T10:00:00Z', 1, 2]);
egalTexte('la phrase', phraseDivergence(d, 'github.com', { fuseau }),
  'Depuis la version du 10 nov. 2026, 10:00, des versions ont été enregistrées ici (1) et sur GitHub (2). Gardez les vôtres ou prenez les leurs : les deux historiques restent, seul l’état du projet est choisi.');
const sansCommune = resumeDivergence({ resultat: 'diverge', mien: liste, leur: [v('e1', '2026-11-10T12:00:00Z')] }, liste);
verifier('deux historiques sans rien en commun', sansCommune.depuis === null && /sans version commune/.test(phraseDivergence(sansCommune, 'gitlab.com')), JSON.stringify(sansCommune.depuis));
egal('une liste vide ou absente', resumeDivergence({ resultat: 'diverge' }), { depuis: null, mien: [], leur: [] });
egal('la description, ou ce qui la remplace', [descriptionVersion({ description: ' Comptages 2026 ' }), descriptionVersion({ description: '' })], ['Comptages 2026', '(sans description)']);
egal('le nombre de versions', [nombreVersions(1), nombreVersions(12)], ['1 version', '12 versions']);

console.log('Les valeurs et les écarts des résultats clés');
changerLangue('en');
egal('quatre chiffres significatifs, sans zéros au bout', [valeurResultat(0.80812), valeurResultat(12.5), valeurResultat(-0.0212345), valeurResultat(123456.7), valeurResultat(2)],
  ['0.8081', '12.5', '-0.02123', '123500', '2']);
egal('entier, pourcentage, très petit ou grand', [valeurResultat(59.6, 'entier'), valeurResultat(0.4567, 'pourcentage'), valeurResultat(2.9e-14), valeurResultat(1.5e9)],
  ['60', '45.7%', '2.9e-14', '1.5e+9']);
egal('manquante', [valeurResultat(null), valeurResultat(undefined), valeurResultat('x')], ['', '', '']);
egal('un écart et sa part', ecartResultat({ ecart: 0.012, ecart_relatif: 0.15 }), '+0.012 (+15%)');
egal('négatif ; d’un entier ; sans part', [ecartResultat({ ecart: -5, ecart_relatif: -0.0833, forme: 'entier' }), ecartResultat({ ecart: 0.02, ecart_relatif: null }),
  ecartResultat({ ecart: 0.031, ecart_relatif: 0.2, forme: 'pourcentage' }), ecartResultat({ ecart: null })], ['−5 (−8.3%)', '+0.02', '+3.1 points', '']);
changerLangue('fr');
egal('la virgule en français', [valeurResultat(0.80812), valeurResultat(0.4567, 'pourcentage'), ecartResultat({ ecart: -0.25, ecart_relatif: -0.5 })], ['0,8081', '45,7 %', '−0,25 (−50 %)']);
egal('une valeur p', [texteP(2.9e-14), texteP(0.0342), texteP(0.5), texteP(null)], ['p < 0,001', 'p = 0,034', 'p = 0,500', '']);
egal('les drapeaux, les plus parlants d’abord', drapeauxEntree({ ic: true, signe: true, nouveau: false, seuil: true, change: true }), ['signe', 'seuil', 'ic']);

console.log('Les modèles des deux côtés');
const m = (nom, numero) => ({ nom, numero, entrees: [] });
const a = apparierModeles({ modeles: [m('PLS', 1), m('ACP', 2), m('Ancien', 3)] }, { modeles: [m('ACP', 2), m('PLS', 1), m('Nouveau', 4)] });
egal('par leur nom, dans l’ordre d’après', [a.deux.map(([x, y]) => `${x.nom}:${y.nom}`), a.avantSeul.map((x) => x.nom), a.apresSeul.map((x) => x.nom)], [['ACP:ACP', 'PLS:PLS'], ['Ancien'], ['Nouveau']]);
const b = apparierModeles({ modeles: [m('M', 1), m('M', 2)] }, { modeles: [m('M', 5)] });
egal('un nom en double : dans l’ordre', [b.deux.map(([x, y]) => `${x.numero}:${y.numero}`), b.avantSeul.map((x) => x.numero)], [['1:5'], [2]]);
egal('rien d’un côté', apparierModeles(null, { modeles: [m('A', 1)] }).apresSeul.length, 1);
const c = (nom, phrases, changees) => ({ nom, comparaison: { phrases: phrases.map((importance) => ({ texte: nom, importance })), resume: { changees } } });
egal('les changements notables d’abord, les plus importants devant', ordonnerComparaisons([c('rien', [], 0), c('petit', [], 3), c('moyen', [2], 4), c('fort', [1, 6], 5), c('erreur', [], 0)].concat([{ nom: 'sans' }]))
  .map((x) => x.nom), ['fort', 'moyen', 'petit', 'rien', 'erreur', 'sans']);
egal('les entrées changées, les plus importantes d’abord', entreesChangees({ entrees: [{ cle: 'a', change: true, importance: 1 }, { cle: 'b', change: false, importance: 0 },
  { cle: 'c', change: true, importance: 3 }, { cle: 'd', change: true, importance: 1 }] }).map((e) => e.cle), ['c', 'a', 'd']);

console.log('Les textes des deux langues');
const { fr, en } = TEXTES_VERSIONS;
const manquent = [...Object.keys(fr).filter((k) => !(k in en)), ...Object.keys(en).filter((k) => !(k in fr))];
verifier('les mêmes clés', !manquent.length, manquent.join(', '));
const marques = (s) => [...String(s).matchAll(/\{(\w+)\}/g)].map((x) => x[1]).sort().join();
const differents = Object.keys(fr).filter((k) => k in en && marques(fr[k]) !== marques(en[k]));
verifier('les mêmes {…} dans chaque texte', !differents.length, differents.join(', '));
verifier('aucun mot de git dans ce qu’on lit (commit, branch, push, pull, merge, clone)',
  Object.values({ ...fr, ...en }).every((s) => !/\b(commit|branch|push|pull|merge|clone|checkout|rebase)\b/i.test(s)),
  Object.entries({ ...fr, ...en }).filter(([, s]) => /\b(commit|branch|push|pull|merge|clone|checkout|rebase)\b/i.test(s)).map(([k]) => k).join(', '));

console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
process.exit(echecs ? 1 : 0);
