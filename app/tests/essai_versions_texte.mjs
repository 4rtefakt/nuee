// Checks interface/versions_texte.js (the words and numbers of File › History and of What changed),
// without the application: dates and days in each language, the list grouped by day, a size on
// disk, an old version's name and its copy's file name, a shared project's state for the status
// bar, the sentence of a version received, the help's links, the values and changes of key
// results, the models of two sides matched and ordered, and the strings of both languages with
// the same placeholders and no word of the tool underneath.
//   node tests/essai_versions_texte.mjs
import {
  dateLongue, heureVersion, dateVersion, jourVersion, grouperParJour, tailleLisible, sansExtension, titreVersion, nomFichier, nomDeChemin, dossierDe,
  memeFichier, nomHote, hoteAdresse, liensConnexion, placeDansDepot, etatPartage, phraseRecu, autreAuteur, titreDeVersion, descriptionVersion,
  nombreVersions, demandeAbsente, valeurResultat, texteP, ecartResultat, drapeauxEntree, apparierModeles, ordonnerComparaisons, entreesChangees, TEXTES_VERSIONS,
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

console.log('Les dates et les jours');
changerLangue('en');
const iso = '2026-11-12T14:32:00.000Z';
egalTexte('en anglais, en entier', dateLongue(iso, { fuseau }), '12 Nov 2026, 14:32');
egalTexte('en anglais, comme la description d’une restauration', dateLongue(iso, { fuseau, style: 'long' }), '12 November 2026 at 14:32');
egalTexte('l’heure', heureVersion(iso, { fuseau }), '14:32');
const maintenant = new Date('2026-11-13T09:00:00Z');
egalTexte('aujourd’hui, hier, avant', [dateVersion('2026-11-13T08:15:00Z', { fuseau, maintenant }), dateVersion('2026-11-12T23:59:00Z', { fuseau, maintenant }),
  dateVersion('2026-11-11T10:00:00Z', { fuseau, maintenant })].join(' | '), 'today at 08:15 | yesterday at 23:59 | 11 Nov 2026, 10:00');
egalTexte('le titre d’un jour', [jourVersion('2026-11-13T08:15:00Z', { fuseau, maintenant }), jourVersion('2026-11-12T08:15:00Z', { fuseau, maintenant }),
  jourVersion('2026-11-05T08:15:00Z', { fuseau, maintenant })].join(' | '), 'Today | Yesterday | Thursday, 5 November 2026');
egal('une date illisible reste telle quelle', [dateLongue('pas une date'), dateVersion(''), heureVersion('x'), jourVersion('x')], ['pas une date', '', '', '']);
const v = (oid, date, extra = {}) => ({ oid, description: oid, auteur: { nom: 'Alice', email: 'a@example.org' }, date, auto: true, noms: [], envoyee: false, ...extra });
const groupes = grouperParJour([v('c5', '2026-11-13T08:30:00Z'), v('c4', '2026-11-13T08:00:00Z'), v('c3', '2026-11-12T20:00:00Z'), v('c2', '2026-11-05T10:00:00Z'), v('c1', '2026-11-05T09:00:00Z')], { fuseau, maintenant });
egal('groupées par jour, dans leur ordre', groupes.map((g) => `${g.titre}: ${g.versions.map((x) => x.oid).join(',')}`), ['Today: c5,c4', 'Yesterday: c3', 'Thursday, 5 November 2026: c2,c1']);
egal('rien à grouper', grouperParJour([]), []);
changerLangue('fr');
egalTexte('en français', [dateLongue(iso, { fuseau }), dateVersion('2026-11-13T08:15:00Z', { fuseau, maintenant }), dateVersion('2026-11-12T23:59:00Z', { fuseau, maintenant })].join(' | '),
  '12 nov. 2026, 14:32 | aujourd’hui à 08:15 | hier à 23:59');
egalTexte('un jour en français, sa majuscule', jourVersion('2026-11-05T08:15:00Z', { fuseau, maintenant }), 'Jeudi 5 novembre 2026');
egalTexte('le jour dans le fuseau donné : 23 h 30 à New York est la veille', dateVersion('2026-11-13T04:30:00Z', { fuseau: 'America/New_York', maintenant: new Date('2026-11-13T15:00:00Z') }),
  'hier à 23:30');

console.log('La taille, le nom d’une version ouverte, et celui de sa copie');
egal('une taille sur le disque', [tailleLisible(0, 'en'), tailleLisible(850, 'en'), tailleLisible(12345, 'en'), tailleLisible(12.6e6, 'en'), tailleLisible(3.21e9, 'en'), tailleLisible(1.25e6, 'fr')],
  ['0 B', '850 B', '12 kB', '13 MB', '3.2 GB', '1,3 Mo']);
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
egal('ce qu’une version montre d’abord : ses noms, sinon sa description', [titreDeVersion(v('c1', iso, { noms: ['soumis', 'v1'] })), titreDeVersion(v('c2', iso, { description: ' Data: 120 rows ' })),
  titreDeVersion(v('c3', iso, { description: '' }))], ['soumis, v1', 'Data: 120 rows', '(no description)']);
egal('le nombre de versions', [nombreVersions(1), nombreVersions(12)], ['1 version', '12 versions']);

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
egal('où va le projet dans le dépôt', [placeDansDepot('vergers.nuee', 'vergers.nuee'), placeDansDepot('vergers.nuee', 'vergers/vergers.nuee')],
  ['', 'In this repository, the project is at vergers/vergers.nuee: another file was already at vergers.nuee.']);

console.log('L’état d’un projet partagé');
const partage = (x = {}) => ({ actif: true, versions: 5, partage: { url: 'https://github.com/moi/vergers.git', hote: 'github.com', aEnvoyer: 0, aRecevoir: 0, erreur: null, ...x }, conflit: null });
egal('pas partagé : rien', [etatPartage(null), etatPartage({ actif: true, partage: null })], [null, null]);
egal('à jour', etatPartage(partage()), { cle: 'a_jour', court: 'up to date', titre: 'Up to date — shared with GitHub' });
egal('à envoyer', etatPartage(partage({ aEnvoyer: 2 })), { cle: 'a_envoyer', court: '↑2', titre: '2 to send — shared with GitHub' });
egal('plus récente à recevoir, avant ce qui est à envoyer', etatPartage(partage({ aEnvoyer: 2, aRecevoir: 1 })).cle, 'a_recevoir');
egal('en cours d’envoi', etatPartage(partage({ aEnvoyer: 2 }), { type: 'envoi' }).court, 'sending…');
egal('une erreur, son message', etatPartage(partage({ erreur: 'Couldn’t reach github.com.' })), { cle: 'erreur', court: 'error', titre: 'Couldn’t reach github.com. — shared with GitHub' });
egal('un conflit avant tout', etatPartage({ ...partage({ erreur: 'x' }), conflit: { mien: {}, leur: {} } }).cle, 'conflit');
egal('en attente des changements non enregistrés', etatPartage(partage(), { type: 'attente' }).cle, 'attente');
egal('un dépôt que Nuée ne joint pas (SSH) : son adresse', etatPartage(partage({ hote: null, url: 'git@example.org:a/b.git' })).titre, 'Up to date — shared with git@example.org:a/b.git');
egalTexte('une version reçue aujourd’hui', phraseRecu(v('c9', '2026-11-13T08:32:00Z'), { fuseau, maintenant }), 'Updated with the version Alice sent at 08:32.');
egalTexte('reçue un autre jour, sans auteur', phraseRecu({ date: '2026-11-10T08:32:00Z' }, { fuseau, maintenant }), 'Updated with the version sent on 10 Nov 2026, 08:32.');
egal('l’autre côté d’un conflit', [autreAuteur(v('c1', iso)), autreAuteur(null)], ['Alice', 'A colleague']);
changerLangue('fr');
egal('en français', etatPartage(partage({ aRecevoir: 3 })), { cle: 'a_recevoir', court: 'plus récente', titre: '3 plus récente(s) à recevoir — partagé avec GitHub' });
egalTexte('reçue, en français', phraseRecu(v('c9', '2026-11-13T08:32:00Z'), { fuseau, maintenant }), 'Mis à jour avec la version que Alice a envoyée à 08:32.');
egal('la description, ou ce qui la remplace', [descriptionVersion({ description: ' Comptages 2026 ' }), descriptionVersion({ description: '' })], ['Comptages 2026', '(sans description)']);
verifier('une demande que le moteur n’a pas encore', demandeAbsente(new Error('Unknown request: fusionner')) && demandeAbsente(new Error('Demande inconnue : fichier_change'))
  && !demandeAbsente(new Error('Projet inconnu : P3')) && !demandeAbsente(null));

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
const motsGit = /\b(commit|branch|push|pull|clone|checkout|rebase|fetch|tag)s?\b/i;
verifier('aucun mot de git dans ce qu’on lit (commit, branch, push, pull, clone, fetch, tag…)', Object.values({ ...fr, ...en }).every((s) => !motsGit.test(s)),
  Object.entries({ ...fr, ...en }).filter(([, s]) => motsGit.test(s)).map(([k]) => k).join(', '));

console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
process.exit(echecs ? 1 : 0);
