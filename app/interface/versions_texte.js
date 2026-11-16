// The words and numbers of File › History (versions.js) and of What changed (changements.js), pure:
// no page, no engine, no main process, so that tests/essai_versions_texte.mjs checks them alone. The
// strings of both are here too. People never read a word of the tool underneath (app/versions.js):
// history, versions, named versions, send and get, shared with GitHub or GitLab.
import { t, ajouterTextes, langueActuelle } from './i18n.js';
import { decimal } from './nombres.js';

const locale = (langue) => (langue === 'fr' ? 'fr-FR' : 'en-GB');
// (fuseau: a time zone for the checks; otherwise the computer's)
const zone = (fuseau) => (fuseau ? { timeZone: fuseau } : {});
const jour = (d, fuseau) => new Intl.DateTimeFormat('en-CA', { year: 'numeric', month: '2-digit', day: '2-digit', ...zone(fuseau) }).format(d);
const valide = (iso) => { const d = new Date(iso); return Number.isNaN(d.getTime()) ? null : d; };

// A version's date, in full: "12 Nov 2026, 14:32", "12 nov. 2026, 14:32" (a title, a sentence);
// style 'long', as the main process writes it in a restored version's description (app/versions.js:
// "12 November 2026 at 14:32").
export function dateLongue(iso, { langue = langueActuelle(), fuseau, style = 'medium' } = {}) {
  const d = valide(iso);
  return d ? new Intl.DateTimeFormat(locale(langue), { dateStyle: style, timeStyle: 'short', ...zone(fuseau) }).format(d) : String(iso || '');
}

// A version's hour: "14:32".
export function heureVersion(iso, { langue = langueActuelle(), fuseau } = {}) {
  const d = valide(iso);
  return d ? new Intl.DateTimeFormat(locale(langue), { timeStyle: 'short', ...zone(fuseau) }).format(d) : '';
}

// A version's date in a sentence or a short list: today's and yesterday's by their hour, the
// others in full.
export function dateVersion(iso, { langue = langueActuelle(), maintenant = new Date(), fuseau } = {}) {
  const d = valide(iso);
  if (!d) return String(iso || '');
  const heure = heureVersion(iso, { langue, fuseau });
  if (jour(d, fuseau) === jour(maintenant, fuseau)) return t('versions_aujourdhui', { heure });
  if (jour(d, fuseau) === jour(new Date(maintenant.getTime() - 86400000), fuseau)) return t('versions_hier', { heure });
  return dateLongue(iso, { langue, fuseau });
}

// The heading of a day in History: "Today", "Yesterday", "Thursday, 12 November 2026".
export function jourVersion(iso, { langue = langueActuelle(), maintenant = new Date(), fuseau } = {}) {
  const d = valide(iso);
  if (!d) return '';
  if (jour(d, fuseau) === jour(maintenant, fuseau)) return t('versions_jour_aujourdhui');
  if (jour(d, fuseau) === jour(new Date(maintenant.getTime() - 86400000), fuseau)) return t('versions_jour_hier');
  const s = new Intl.DateTimeFormat(locale(langue), { weekday: 'long', day: 'numeric', month: 'long', year: 'numeric', ...zone(fuseau) }).format(d);
  return s.charAt(0).toUpperCase() + s.slice(1);
}

// The versions (newest first) grouped by day, in their order: [{ cle, titre, versions }].
export function grouperParJour(liste, options = {}) {
  const groupes = [];
  for (const v of [].concat(liste || [])) {
    const d = valide(v.date);
    const cle = d ? jour(d, options.fuseau) : '';
    if (!groupes.length || groupes[groupes.length - 1].cle !== cle) groupes.push({ cle, titre: jourVersion(v.date, options), versions: [] });
    groupes[groupes.length - 1].versions.push(v);
  }
  return groupes;
}

// A size on disk: "12 MB", "850 kB", "12 Mo" in French.
export function tailleLisible(octets, langue = langueActuelle()) {
  const n = Math.max(0, Number(octets) || 0);
  const unites = langue === 'fr' ? ['o', 'ko', 'Mo', 'Go'] : ['B', 'kB', 'MB', 'GB'];
  let k = 0;
  let x = n;
  while (x >= 1000 && k < unites.length - 1) { x /= 1000; k += 1; }
  const texte = k === 0 || x >= 10 ? String(Math.round(x)) : x.toFixed(1);
  return `${langue === 'fr' ? texte.replace('.', ',') : texte} ${unites[k]}`;
}

// A project's name without its file's extension: "orchards.nuee" → "orchards".
export const sansExtension = (nom) => String(nom || '').replace(/\.(nuee|json|usp|csv|tsv|txt|xlsx?|ods|rds|rdata|sav|dta|sas7bdat|xpt|omv|dbf|arff|gpkg|zip)$/i, '');

// The name of an old version opened beside the project: "orchards — version of 12 Nov 2026, 14:32".
export const titreVersion = (nomProjet, iso, options = {}) => t('versions_titre_version', { nom: sansExtension(nomProjet), date: dateLongue(iso, options) });

// A file name from a project's name, for Save as: without the characters a file name can't hold
// (a version's name has the colon of its time), nor dots or spaces at its end (Windows drops them).
export const nomFichier = (nom) => String(nom || 'projet').replace(/[<>:"/\\|?*\u0000-\u001f]+/g, '-').replace(/[. ]+$/, '').trim() || 'projet';

// The file name of a path, whatever its separators.
export const nomDeChemin = (chemin) => String(chemin || '').split(/[\\/]/).pop();

// The folder of a path.
export const dossierDe = (chemin) => String(chemin || '').replace(/[\\/][^\\/]*$/, '');

// Two paths to the same file: on Windows, whatever the separators and capitals.
export function memeFichier(a, b, windows = typeof navigator !== 'undefined' && /^Win/.test(navigator.platform)) {
  if (!a || !b) return false;
  const cle = (c) => (windows ? String(c).replace(/\\/g, '/').toLowerCase() : String(c));
  return cle(a) === cle(b);
}

// The service of an address or host, as people call it.
export function nomHote(hote) {
  const h = String(hote || '').toLowerCase();
  if (h === 'github.com' || h.endsWith('.github.com')) return 'GitHub';
  if (h === 'gitlab.com' || /(^|\.)gitlab\./.test(h)) return 'GitLab';
  return hote || '';
}

// The server of an address being typed, when it reads as an https address ('' otherwise). The main
// process checks the address itself (app/versions.js: adresseDistante) and says why it refuses one:
// this only chooses the help's links.
export function hoteAdresse(texte) {
  try {
    const u = new URL(String(texte || '').trim());
    return u.protocol === 'https:' && u.hostname ? u.host.toLowerCase() : '';
  } catch { return ''; }
}

// Where to create the repository and the token: GitHub's pages, or those of the GitLab of the
// address (gitlab.com, or a GitLab of one's own institution), the token's name and scope filled in;
// both, until the address says which.
export function liensConnexion(texte) {
  const hote = hoteAdresse(texte);
  const github = { depot: 'https://github.com/new', jeton: 'https://github.com/settings/personal-access-tokens/new' };
  const gitlab = (h) => ({ depot: `https://${h}/projects/new`, jeton: `https://${h}/-/user_settings/personal_access_tokens?name=Nu%C3%A9e&scopes=write_repository` });
  if (nomHote(hote) === 'GitHub') return { github };
  if (nomHote(hote) === 'GitLab') return { gitlab: gitlab(hote) };
  return { github, gitlab: gitlab('gitlab.com') };
}

// Where a shared project went in its repository, when not at the root under its own name (another
// file was there): the sentence that says so, else ''.
export const placeDansDepot = (nom, chemin) => (chemin && chemin !== nom ? t('versions_place', { chemin, nom }) : '');

// A server whose accounts Nuée signs in to (app/comptes.js): GitHub, gitlab.com, or a GitLab by its
// name (a self-hosted GitLab under another name is known once its application ID is given)
export const serveurDeComptes = (hote) => {
  const h = String(hote || '').toLowerCase();
  return h === 'github.com' || nomHote(h) === 'GitLab';
};

// A repository's owner and name (on GitLab, its groups and name), from its address
export function cheminDepot(url) {
  try { return new URL(String(url || '')).pathname.replace(/\/-\/.*$/, '').replace(/^\/+|\/+$/g, '').replace(/\.git$/i, ''); } catch { return String(url || ''); }
}
// A repository's page, from its address
export function pageDepot(url) {
  try { return `${new URL(String(url || '')).origin}/${cheminDepot(url)}`; } catch { return String(url || ''); }
}

// Another name for a repository whose name is taken: "orchards" → "orchards-2" → "orchards-3"
// (a year stays: "survey-2026" → "survey-2026-2")
export function nomSuivant(nom) {
  const n = String(nom || '').trim() || 'nuee';
  const m = /^(.*\D)-(\d{1,2})$/.exec(n);
  return m ? `${m[1]}-${Number(m[2]) + 1}` : `${n}-2`;
}

// The state of a shared project, from etat() and what the background last told (`vu`: { type:
// 'envoi' | 'attente' | 'conflit' | 'erreur', message }), for the status bar and History:
// → { cle, court, titre } (cle: a_jour, envoi, a_envoyer, a_recevoir, attente, conflit, erreur), or
// null when the project isn't shared.
export function etatPartage(e, vu = null) {
  const p = e && e.partage;
  if (!p) return null;
  const hote = nomHote(p.hote) || p.url || '';
  const aEnvoyer = Number(p.aEnvoyer) || 0;
  const aRecevoir = Number(p.aRecevoir) || 0;
  const avec = (cle, court, titre) => ({ cle, court, titre: `${titre} — ${t('versions_partage_hote', { hote })}` });
  if (e.conflit || (vu && vu.type === 'conflit')) return avec('conflit', t('versions_court_conflit'), t('versions_etat_conflit'));
  if (vu && vu.type === 'erreur') return avec('erreur', t('versions_court_erreur'), vu.message || t('versions_etat_erreur'));
  if (p.erreur) return avec('erreur', t('versions_court_erreur'), p.erreur);
  if (vu && vu.type === 'envoi') return avec('envoi', t('versions_court_envoi'), t('versions_etat_envoi'));
  if (vu && vu.type === 'attente') return avec('attente', t('versions_court_attente'), t('versions_etat_attente'));
  if (aRecevoir) return avec('a_recevoir', t('versions_court_a_recevoir'), t('versions_etat_a_recevoir', { n: aRecevoir }));
  if (aEnvoyer) return avec('a_envoyer', `↑${aEnvoyer}`, t('versions_etat_a_envoyer', { n: aEnvoyer }));
  return avec('a_jour', t('versions_court_a_jour'), t('versions_etat_a_jour'));
}

// "Updated with the version Alice sent at 14:32" (a version received in the background).
export function phraseRecu(v, options = {}) {
  const maintenant = options.maintenant || new Date();
  const d = valide(v && v.date);
  const nom = (v && v.auteur && v.auteur.nom) || '';
  const quand = d && jour(d, options.fuseau) === jour(maintenant, options.fuseau)
    ? t('versions_quand_heure', { heure: heureVersion(v.date, options) }) : t('versions_quand_date', { date: dateLongue(v && v.date, options) });
  return t(nom ? 'versions_recu' : 'versions_recu_anonyme', { nom, quand });
}

// The colleague a conflict is with: who made their last version.
export const autreAuteur = (leur) => (leur && leur.auteur && leur.auteur.nom) || t('versions_collegue');

// What a version shows first: its names, else its description; an automatic one says what changed.
export const titreDeVersion = (v) => ([].concat((v && v.noms) || []).length ? [].concat(v.noms).join(', ') : descriptionVersion(v));

// A version's description, or what stands for it.
export const descriptionVersion = (v) => (v && String(v.description || '').trim()) || t('versions_sans_description');

// "12 versions"
export const nombreVersions = (n) => t(Number(n) === 1 ? 'versions_une' : 'versions_n', { n: Number(n) || 0 });

// An engine request that doesn't exist (yet): the merge by meaning, the shared folder's check
// (serveur.R answers an unknown request "Unknown request: <name>")
export const demandeAbsente = (e) => /^(Demande inconnue|Unknown request)\s*:/.test(String((e && e.message) || ''));

// ------------------------------------------------------------ what changed

// A result's value as it reads: a whole number, a percentage, or four significant digits (the
// reader's decimal mark), a category in words; '' when missing.
export function valeurResultat(x, forme) {
  if (x === null || x === undefined || Number.isNaN(Number(x))) return '';
  const v = Number(x);
  if (forme === 'entier') return String(Math.round(v));
  if (forme === 'pourcentage') return `${decimal(chiffres(100 * v, 3))}${langueActuelle() === 'fr' ? ' %' : '%'}`;
  return decimal(chiffres(v, 4));
}

// v with k significant digits, without the zeros after them; very large or small, in powers of ten.
function chiffres(v, k) {
  if (v === 0) return '0';
  const a = Math.abs(v);
  if (Number.isInteger(v) && a < 1e7) return String(v);
  if (a >= 1e7 || a < 1e-4) return v.toExponential(k - 2).replace(/\.?0+e/, 'e');
  // (toPrecision writes 123456.7 "1.235e+5": a whole number rounded to k digits instead)
  if (a >= 10 ** k) {
    const pas = 10 ** (Math.floor(Math.log10(a)) - k + 1);
    return String(Math.round(v / pas) * pas);
  }
  const s = v.toPrecision(k);
  return s.includes('.') ? s.replace(/\.?0+$/, '') : s;
}

// A p-value beside a value: "p < 0.001", "p = 0.034".
export const texteP = (p) => (p === null || p === undefined || Number.isNaN(Number(p)) ? '' : Number(p) < 0.001 ? `p < ${decimal('0.001')}` : `p = ${decimal(Number(p).toFixed(3))}`);

// A compared entry's change: "+0.012 (+15%)"; a percentage's in points ("−15.9 points", not to be
// read as a share of it); the relative change only when it means something (not from 0).
export function ecartResultat(e) {
  if (e.ecart === null || e.ecart === undefined) return '';
  const signe = (x) => (x > 0 ? '+' : x < 0 ? '−' : '±');
  if (e.forme === 'pourcentage') return `${signe(e.ecart)}${decimal(chiffres(Math.abs(100 * e.ecart), 3))} ${t('changements_points')}`;
  const ecart = `${signe(e.ecart)}${valeurResultat(Math.abs(e.ecart), e.forme === 'entier' ? 'entier' : null)}`;
  if (e.ecart_relatif === null || e.ecart_relatif === undefined || !Number.isFinite(e.ecart_relatif)) return ecart;
  return `${ecart} (${signe(e.ecart_relatif)}${decimal(chiffres(Math.abs(100 * e.ecart_relatif), 2))}${langueActuelle() === 'fr' ? ' %' : '%'})`;
}

// What happened to an entry, as the table's marks say it: its flags, most telling first.
export const DRAPEAUX = ['nouveau', 'disparu', 'signe', 'seuil', 'ic', 'categorie', 'retourne'];
export const drapeauxEntree = (e) => DRAPEAUX.filter((k) => e[k] === true);

// The models of the two sides, matched by their name (a name is how documents cite a model):
// those on both sides, in the order of the side after, then those of one side only.
export function apparierModeles(avant, apres) {
  const a = [].concat((avant && avant.modeles) || []);
  const b = [].concat((apres && apres.modeles) || []);
  const restants = new Map();
  for (const m of a) restants.set(m.nom, [...(restants.get(m.nom) || []), m]);
  const deux = [];
  const apresSeul = [];
  for (const m of b) {
    const l = restants.get(m.nom);
    if (l && l.length) deux.push([l.shift(), m]);
    else apresSeul.push(m);
  }
  const avantSeul = a.filter((m) => deux.every(([x]) => x !== m));
  return { deux, avantSeul, apresSeul };
}

// The compared models, those with notable changes first (their most important change first),
// then those that changed a little, then the others, each group in its order.
export function ordonnerComparaisons(liste) {
  const rang = (c) => (c.comparaison && [].concat(c.comparaison.phrases || []).length ? 0 : c.comparaison && (c.comparaison.resume || {}).changees > 0 ? 1 : 2);
  const importance = (c) => Math.max(0, ...[].concat((c.comparaison && c.comparaison.phrases) || []).map((f) => Number(f.importance) || 0));
  return liste.map((c, i) => ({ c, i })).sort((x, y) => rang(x.c) - rang(y.c) || (rang(x.c) === 0 ? importance(y.c) - importance(x.c) : 0) || x.i - y.i).map((x) => x.c);
}

// The entries that changed, the most important first (in their order when equal).
export const entreesChangees = (comparaison) => [].concat((comparaison && comparaison.entrees) || []).map((e, i) => ({ e, i }))
  .filter((x) => x.e.change).sort((x, y) => (Number(y.e.importance) || 0) - (Number(x.e.importance) || 0) || x.i - y.i).map((x) => x.e);

// (exported for the check: the same keys and placeholders in both languages)
export const TEXTES_VERSIONS = { fr: {
  versions: 'Historique…', versions_vue: 'Historique', versions_ouvrir_distant: 'Ouvrir depuis GitHub ou GitLab…', versions_partager: 'Partager sur GitHub ou GitLab…',
  versions_navigateur: 'L’historique d’un projet est gardé par l’application de bureau : il n’est pas disponible dans le navigateur.',
  versions_jamais_enregistre: 'Les versions se gardent pour un projet enregistré dans un fichier, dans un dossier caché à côté de lui.',
  versions_pas_nuee: 'Les versions se gardent pour un projet enregistré au format de Nuée (.nuee) : enregistrez-le sous ce format.',
  versions_enregistrer_projet: 'Enregistrer le projet…',
  versions_tete: 'Les versions sont gardées d’elles-mêmes, dans un dossier caché à côté du projet ({taille}).',
  versions_tete_vide: 'Les versions sont gardées d’elles-mêmes, dans un dossier caché à côté du projet.',
  versions_garder_projet: 'Garder des versions de ce projet',
  versions_desactivees: 'Ce projet ne garde pas de versions ; celles déjà gardées restent lisibles.',
  versions_aucune_encore: 'Aucune version encore : la première est gardée quand le projet est enregistré.',
  versions_maintenant: 'Enregistrer une version…', versions_reglages: 'Réglages…', versions_actualiser: 'Actualiser',
  versions_jour_aujourdhui: 'Aujourd’hui', versions_jour_hier: 'Hier', versions_aujourdhui: 'aujourd’hui à {heure}', versions_hier: 'hier à {heure}',
  versions_auto: 'automatique', versions_auto_aide: 'Une version gardée d’elle-même : sa description dit ce qui a changé depuis la précédente.',
  versions_envoyee: 'envoyée', versions_envoyee_aide: 'Envoyée au dépôt partagé', versions_actuelle: 'état actuel',
  versions_ouvrir: 'Ouvrir', versions_ouvrir_aide: 'L’ouvrir à côté du projet, en lecture seule',
  versions_comparer: 'Comparer', versions_comparer_aide: 'Comparer avec le projet actuel : ce qui a changé dans les résultats des modèles',
  versions_nommer: 'Nommer…', versions_restaurer: 'Restaurer…', versions_sans_description: '(sans description)',
  versions_n: '{n} versions', versions_une: '1 version', versions_titre_version: '{nom} — version du {date}',
  versions_description_titre: 'Enregistrer une version — {nom}',
  versions_description_aide: 'Quelques mots sur cet état, pour le retrouver plus tard :', versions_description_exemple: 'Comptages de 2026 ajoutés, modèles réajustés',
  versions_description_vide: 'Écrivez une courte description.', versions_enregistre_avant: 'Le projet a des changements non enregistrés : il est d’abord enregistré.',
  versions_bouton_enregistrer: 'Enregistrer la version', versions_inchange: 'Le projet n’a pas changé depuis sa dernière version : rien à enregistrer.',
  versions_enregistree: 'Version enregistrée : {description}',
  versions_nommer_titre: 'Nommer la version du {date}',
  versions_nommer_aide: 'Un nom marque une version pour la retrouver d’un coup d’œil (« soumis à la revue », « chapitre 2 de la thèse ») ; une version nommée n’est jamais effacée.',
  versions_nom: 'Nom', versions_nom_vide: 'Écrivez un nom.', versions_noms: 'Ses noms :', versions_retirer_nom: 'Retirer le nom « {nom} »',
  versions_bouton_nommer: 'Nommer', versions_nommee: 'Version nommée « {nom} ».', versions_nom_retire: 'Nom « {nom} » retiré.',
  versions_ouverte: 'Version du {date} ouverte en lecture seule.', versions_deja_ouverte: 'Cette version est déjà ouverte.',
  lecture_seule: 'lecture seule',
  lecture_seule_aide: 'Une ancienne version, ouverte en lecture seule : Enregistrer sous en garde une copie ailleurs, Fichier › Historique la restaure.',
  versions_lecture_titre: 'Version en lecture seule',
  versions_lecture_texte: '« {nom} » est une ancienne version de {source}, ouverte en lecture seule : l’enregistrer ne changerait pas le projet. Enregistrer sous… en garde une copie ailleurs ; Restaurer… redonne cet état au projet (son état actuel reste dans l’historique).',
  versions_enregistrer_sous: 'Enregistrer sous…', versions_lecture_panneau: 'Ce projet est la version du {date} de {source}, ouverte en lecture seule.',
  versions_voir_source: 'L’historique de {source}',
  versions_restaurer_titre: 'Restaurer la version du {date}',
  versions_restaurer_texte: 'Le projet redevient cette version : ses données, ses modèles, ses documents et ses figures. Son état actuel reste dans l’historique : ce qui n’est dans aucune version y est gardé d’abord, puis une version « Version du {longue} restaurée » est enregistrée.',
  versions_bouton_restaurer: 'Restaurer', versions_avant_titre: 'Des changements non enregistrés',
  versions_avant_texte: 'Le projet a des changements non enregistrés : restaurer les perdrait. Les enregistrer d’abord ? Ils restent dans l’historique.',
  versions_sans_garder: 'Ne pas les garder', versions_enregistrer_restaurer: 'Enregistrer et restaurer', versions_restauree: 'Version du {date} restaurée.',
  versions_comparaison_titre: 'version du {date} → projet actuel', versions_progression_avant: 'Modèles de la version du {date} : {nom} ({k} sur {n})…',
  versions_progression_apres: 'Modèles du projet actuel : {nom} ({k} sur {n})…', versions_comparaison_faite: 'Comparaison faite.',
  versions_progression_autre: 'Modèles de l’autre version : {nom} ({k} sur {n})…',
  versions_reglages_titre: 'Réglages des versions', versions_reglage_auto: 'Garder des versions de mes projets',
  versions_reglage_auto_aide: 'Les versions d’un projet sont gardées dans un dossier caché à côté de lui : quand il est enregistré (une par heure au plus), quand il est fermé, avant que ses données soient mises à jour. Sans ce réglage, aucun nouvel historique ne commence ; ceux qui existent restent.',
  versions_auteur_titre: 'Qui enregistre les versions',
  versions_auteur_texte: 'Votre nom est écrit avec chaque version ; votre adresse électronique aussi, dès qu’un projet est partagé : GitHub et GitLab montrent par elle qui a fait quoi.',
  versions_auteur_nom: 'Nom', versions_auteur_email: 'Adresse électronique', versions_auteur_manque: 'Donnez un nom et une adresse électronique.',
  versions_auteur: 'Versions signées {nom}', versions_auteur_email_court: '{nom} <{email}>', versions_reglages_gardes: 'Réglages des versions gardés.',
  versions_partager_titre: 'Partager sur GitHub ou GitLab',
  versions_partager_texte: 'Les versions du projet sont envoyées dans un dépôt sur GitHub ou GitLab, et celles de vos collègues en reviennent : chacun travaille sur sa copie, Nuée les garde au même point.',
  versions_etape_depot: 'Un dépôt sur GitHub ou GitLab : un nouveau, vide ou non, ou un qui a déjà d’autres fichiers ; privé si les données le sont.',
  versions_etape_jeton: 'Un jeton qui peut y écrire : sur GitHub, un jeton à accès précis (« fine-grained ») avec Contents en lecture et écriture sur ce dépôt ; sur GitLab, un jeton avec write_repository.',
  versions_etape_coller: 'L’adresse du dépôt (https://…) et le jeton, ici.',
  versions_lien_github_depot: 'nouveau dépôt GitHub', versions_lien_gitlab_depot: 'nouveau projet GitLab', versions_lien_github_jeton: 'jeton GitHub', versions_lien_gitlab_jeton: 'jeton GitLab',
  versions_adresse: 'Adresse du dépôt', versions_adresse_exemple: 'https://github.com/vous/projet', versions_jeton: 'Jeton',
  versions_jeton_aide: 'Gardé dans le trousseau de l’ordinateur, il n’est plus jamais montré.', versions_jeton_garder: 'Un jeton est gardé pour {hote} : laissez vide pour le garder.',
  versions_verifier: 'Vérifier', versions_verification: 'Vérification…', versions_partage_ok: 'Partagé avec {hote}.',
  versions_place: 'Dans ce dépôt, le projet est à {chemin} : un autre fichier était déjà à {nom}.',
  versions_compte: 'Se connecter à {service}…', versions_inviter: 'Inviter un collègue…',
  versions_partager_compte: 'Connectez-vous, et Nuée lui crée un dépôt privé sur votre compte :',
  versions_depot_existant: 'Utiliser un dépôt existant, ou un autre serveur',
  versions_depot_existant_texte: 'Son adresse, telle que la donne son bouton Code sur GitHub ou GitLab. Connecté à GitHub ou GitLab, il ne faut rien d’autre ; pour un autre serveur, un jeton qui peut y écrire.',
  versions_plutot_existant: 'Utiliser plutôt un dépôt existant',
  versions_nouveau_depot: 'Un nouveau dépôt sur {service}', versions_nom_depot: 'Son nom',
  versions_depot_prive: 'Privé : vous seul et les collègues que vous invitez le voyez',
  versions_creer_partager: 'Créer et partager', versions_creation: 'Création du dépôt sur {service}…', versions_partage_en_cours: 'Partage du projet…',
  versions_partager_bouton: 'Partager',
  versions_partage_fait: '{nom} est partagé sur {service}, dans le dépôt {depot}.', versions_partage_fait_prive: '{nom} est partagé sur {service}, dans le dépôt privé {depot}.',
  versions_partage_suite: 'Ses versions y sont envoyées d’elles-mêmes, et celles de vos collègues en reviennent. Invitez-les : ils l’ouvrent ensuite avec Fichier › Ouvrir depuis GitHub ou GitLab.',
  versions_non_connecte: 'Non connecté à {service} : envoyer et recevoir les versions demandent une connexion.',
  versions_connexion_refusee: '{service} n’accepte plus la connexion : reconnectez-vous pour envoyer et recevoir les versions.',
  versions_deconnecter_titre: 'Se déconnecter de {service}',
  versions_deconnecter_texte: 'Nuée oublie sa connexion à {service} sur cet ordinateur : les projets partagés là-bas n’envoient ni ne reçoivent plus leurs versions jusqu’à ce que vous vous reconnectiez. Votre compte et ses dépôts restent tels qu’ils sont.',
  versions_connexion_gardee: 'Connexion gardée dans le trousseau de cet ordinateur.',
  versions_distant_connexion: 'Ce dépôt est privé : connectez-vous à {service} avec un compte qui y a accès, ou donnez un jeton.',
  versions_partage_hote: 'partagé avec {hote}', versions_partage_avec: 'Partagé avec {hote} :', versions_dernier_echange: 'dernier échange {date}',
  versions_court_a_jour: 'à jour', versions_court_envoi: 'envoi…', versions_court_a_recevoir: 'plus récente', versions_court_attente: 'en attente',
  versions_court_conflit: 'à choisir', versions_court_erreur: 'erreur',
  versions_etat_a_jour: 'À jour', versions_etat_envoi: 'Envoi des versions…', versions_etat_a_envoyer: '{n} à envoyer',
  versions_etat_a_recevoir: '{n} plus récente(s) à recevoir', versions_etat_attente: 'Une version plus récente attend que vos changements soient enregistrés',
  versions_etat_conflit: 'Le projet a changé des deux côtés : choisissez', versions_etat_erreur: 'Le dernier échange a échoué',
  versions_envoyer_maintenant: 'Envoyer maintenant', versions_recevoir_maintenant: 'Recevoir maintenant', versions_reessayer: 'Réessayer',
  versions_changer_jeton: 'Changer l’adresse ou le jeton…', versions_arreter_partage: 'Arrêter le partage',
  versions_arreter_texte: 'Arrêter de partager {nom} avec {hote} ? Les versions restent ici, et dans le dépôt.', versions_partage_arrete: 'Le projet n’est plus partagé.',
  versions_jeton_garde: 'Jeton gardé dans le trousseau de cet ordinateur.',
  versions_jeton_session: 'Jeton gardé jusqu’à la fermeture de Nuée : cet ordinateur n’a pas de trousseau que Nuée puisse utiliser. Il sera à coller de nouveau la prochaine fois.',
  versions_jeton_aucun: 'Aucun jeton gardé : envoyer et recevoir en demandent un.',
  versions_envoyees: 'Versions envoyées à {hote}.', versions_recu_a_jour: 'Rien de nouveau sur {hote} : le projet est à jour.',
  versions_recu: 'Mis à jour avec la version que {nom} a envoyée {quand}.', versions_recu_anonyme: 'Mis à jour avec la version envoyée {quand}.',
  versions_quand_heure: 'à {heure}', versions_quand_date: 'le {date}', versions_voir_changements: 'Ce qui a changé',
  versions_attente: 'Une version plus récente de {nom} attend sur {hote} : enregistrez vos changements, puis recevez-la.',
  versions_recevoir_avant: 'Le projet a des changements non enregistrés. Les enregistrer d’abord, puis recevoir la version plus récente ?',
  versions_enregistrer_recevoir: 'Enregistrer et recevoir', versions_recu_non_recharge: 'Une version plus récente de {nom} est dans son fichier ; vos changements non enregistrés sont gardés ici. Les enregistrer remplacerait cette version.',
  versions_recharger: 'Recharger le projet',
  versions_conflit_titre: 'Le projet a changé des deux côtés', versions_conflit_texte: '{nom} a envoyé une version de ce projet pendant que vous le changiez aussi.',
  versions_conflit_vous: 'La vôtre', versions_conflit_leur: 'Celle de {nom}', versions_collegue: 'Un collègue',
  versions_garder_miennes: 'Garder la mienne', versions_prendre_leurs: 'Prendre la leur', versions_comparer_conflit: 'Comparer',
  versions_fusionner: 'Réunir les deux', versions_fusion_calcul: 'Nuée regarde ce que chacun a changé…',
  versions_fusion_auto: 'Rien ne s’oppose : Nuée peut réunir les deux versions.', versions_fusion_apports_mien: 'Vous :', versions_fusion_apports_leur: '{nom} :',
  versions_fusion_conflits: 'Certains changements s’opposent : choisissez, pour chacun, le côté à garder.',
  versions_fusion_choix_mien: 'le mien', versions_fusion_choix_leur: 'le leur', versions_fusion_appliquer: 'Réunir avec ces choix',
  versions_fusion_description: 'Réunie avec la version de {nom}', versions_fusion_faite: 'Les deux versions sont réunies.',
  versions_conflit_enregistrer: 'Le projet a des changements non enregistrés : enregistrez-les d’abord, ils seraient perdus.',
  versions_conflit_avis: '{nom} a changé des deux côtés.', versions_choisir: 'Choisir…',
  versions_gardees_miennes: 'Votre version est gardée, et envoyée à {hote}.', versions_prises_leurs: 'Le projet est maintenant celui de {hote} ; votre version reste dans l’historique.',
  versions_comparaison_leur: 'votre projet → la version de {nom} du {date}', versions_comparaison_recu: 'la version d’avant → celle de {nom}',
  versions_distant_titre: 'Ouvrir depuis GitHub ou GitLab',
  versions_distant_texte: 'Un projet partagé par un collègue : l’adresse du dépôt, puis le projet, puis le dossier où le mettre. Ses versions viennent avec lui, et restent au même point que le dépôt.',
  versions_distant_chercher: 'Chercher les projets', versions_distant_jeton: 'Ce dépôt est privé : donnez un jeton qui peut le lire (et y écrire, pour envoyer vos versions).',
  versions_distant_aucun: 'Aucun projet (.nuee) dans ce dépôt.', versions_distant_projets: 'Projet', versions_distant_dossier: 'Dossier',
  versions_distant_choisir_dossier: 'Choisir…', versions_distant_ouvrir: 'Ouvrir', versions_distant_pas_dossier: 'Choisissez le dossier où mettre le projet.',
  versions_distant_pas_projet: 'Choisissez un projet du dépôt.', versions_distant_ouvert: '{nom} ouvert depuis {hote}, et partagé avec lui.',
  versions_dossier_titre: '{nom} a changé sur le disque',
  versions_dossier_texte: '{qui} a enregistré ce projet pendant que vous l’aviez ouvert : ses changements peuvent rejoindre les vôtres.',
  versions_dossier_texte_enregistrer: '{qui} a enregistré ce projet pendant que vous le changiez aussi : réunir ses changements avec les vôtres avant d’enregistrer ?',
  versions_dossier_sans_base: '{nom} a été changé sur le disque par {qui}. Nuée a redémarré depuis son ouverture et ne peut plus le réunir : l’enregistrer demandera s’il faut écraser ses changements.',
  versions_dossier_fusionne: 'Ses changements sont réunis avec le projet : enregistrez-le pour les garder.', versions_pas_maintenant: 'Pas maintenant',
  versions_fusion_sans_base: 'Pas de version commune d’où réunir les deux : gardez l’un des côtés.', versions_fusion_sans_moteur: 'Nuée ne peut pas réunir ces deux versions : gardez l’un des côtés.',
  versions_fusion_donnees_etapes: 'Les données : les étapes des deux côtés, rejouées depuis la source.', versions_fusion_donnees_cellules: 'Les données : réunies cellule par cellule.',
  versions_fusion_donnees_raison: 'Les données : réunies cellule par cellule ({raison}).',
  versions_fusion_passages: '{n} passage(s) écrit(s) autrement', versions_fusion_ligne: 'Paragraphe {ligne}', versions_fusion_cellules: '{n} cellule(s) changée(s) autrement',
  versions_fusion_cellules_plus: 'et {n} de plus', versions_fusion_observation: 'Observation',
  versions_indicateur_local: 'Historique : {n} versions gardées à côté du projet', versions_indicateur_local_une: 'Historique : 1 version gardée à côté du projet',
  versions_indicateur_aucune: 'Historique : aucune version encore', versions_indicateur_desactive: 'Historique : ce projet ne garde pas de versions',
  changements: 'Ce qui a changé', changements_titre: 'Ce qui a changé : {titre}',
  changements_resume: '{n} modèle(s) comparé(s), {notables} avec des changements notables.', changements_aucun_modele: 'Aucun modèle à comparer : ni l’un ni l’autre n’a de modèle enregistré.',
  changements_inchange: 'Aucun changement dans ses résultats clés.', changements_peu: 'De petits changements, aucun notable.',
  changements_entrees: '{n} résultat(s) changé(s) sur {total}', changements_tout: 'Montrer les {n} résultats changés', changements_avant_seul: 'Seulement avant :',
  changements_apres_seul: 'Seulement après :', changements_erreur: '{nom} n’a pas pu être ajusté : {message}', changements_non_compare: '{nom} n’a pas pu être comparé : {message}',
  changements_pas_la: 'Cette comparaison n’est plus là : comparez de nouveau depuis Fichier › Historique.', changements_calcul: 'Comparaison des résultats…',
  changements_resultat: 'Résultat', changements_avant: 'Avant', changements_apres: 'Après', changements_ecart: 'Écart', changements_points: 'points',
  changements_g_ajustement: 'Ajustement', changements_g_estimations: 'Estimations', changements_g_tests: 'Tests', changements_g_aleatoires: 'Effets aléatoires',
  changements_g_composantes: 'Composantes', changements_g_variables: 'Variables', changements_g_especes: 'Espèces', changements_g_moyennes: 'Moyennes ajustées',
  changements_g_puissance: 'Puissance', changements_g_jeu_test: 'Jeu de test', changements_g_stocke: 'Résultats du fichier',
  changements_d_nouveau: 'nouveau', changements_d_disparu: 'disparu', changements_d_signe: 'signe', changements_d_seuil: 'seuil de 5 %', changements_d_ic: 'hors de l’ancien IC',
  changements_d_categorie: 'catégorie', changements_d_retourne: 'composante retournée',
  changements_d_aide: 'nouveau, disparu ; signe : passé de l’autre côté de 0 (ou de 1) ; seuil de 5 % : significatif d’un côté seulement ; hors de l’ancien IC : la nouvelle valeur sort de l’ancien intervalle à 95 % ; catégorie : sa catégorie a changé ; composante retournée : son signe arbitraire a changé, remis dans le même sens avant de comparer',
}, en: {
  versions: 'History…', versions_vue: 'History', versions_ouvrir_distant: 'Open from GitHub or GitLab…', versions_partager: 'Share on GitHub or GitLab…',
  versions_navigateur: 'A project’s history is kept by the desktop app: it isn’t available in a browser.',
  versions_jamais_enregistre: 'Versions are kept for a project saved to a file, in a hidden folder next to it.',
  versions_pas_nuee: 'Versions are kept for a project saved in Nuée’s format (.nuee): save it in that format.',
  versions_enregistrer_projet: 'Save the project…',
  versions_tete: 'Versions are kept automatically, in a hidden folder next to the project ({taille}).',
  versions_tete_vide: 'Versions are kept automatically, in a hidden folder next to the project.',
  versions_garder_projet: 'Keep versions of this project',
  versions_desactivees: 'This project keeps no versions; those already kept stay readable.',
  versions_aucune_encore: 'No version yet: the first is kept when the project is saved.',
  versions_maintenant: 'Save a version…', versions_reglages: 'Settings…', versions_actualiser: 'Refresh',
  versions_jour_aujourdhui: 'Today', versions_jour_hier: 'Yesterday', versions_aujourdhui: 'today at {heure}', versions_hier: 'yesterday at {heure}',
  versions_auto: 'automatic', versions_auto_aide: 'A version kept automatically: its description says what changed since the one before.',
  versions_envoyee: 'sent', versions_envoyee_aide: 'Sent to the shared repository', versions_actuelle: 'current state',
  versions_ouvrir: 'Open', versions_ouvrir_aide: 'Open it beside the project, read-only',
  versions_comparer: 'Compare', versions_comparer_aide: 'Compare with the current project: what changed in the models’ results',
  versions_nommer: 'Name…', versions_restaurer: 'Restore…', versions_sans_description: '(no description)',
  versions_n: '{n} versions', versions_une: '1 version', versions_titre_version: '{nom} — version of {date}',
  versions_description_titre: 'Save a version — {nom}',
  versions_description_aide: 'A few words on this state, to find it again later:', versions_description_exemple: '2026 counts added, models refitted',
  versions_description_vide: 'Write a short description.', versions_enregistre_avant: 'The project has unsaved changes: it is saved first.',
  versions_bouton_enregistrer: 'Save the version', versions_inchange: 'The project hasn’t changed since its last version: nothing to save.',
  versions_enregistree: 'Version saved: {description}',
  versions_nommer_titre: 'Name the version of {date}',
  versions_nommer_aide: 'A name marks a version so it can be found at a glance (“submitted to the journal”, “thesis chapter 2”); a named version is never thinned out.',
  versions_nom: 'Name', versions_nom_vide: 'Write a name.', versions_noms: 'Its names:', versions_retirer_nom: 'Remove the name “{nom}”',
  versions_bouton_nommer: 'Name it', versions_nommee: 'Version named “{nom}”.', versions_nom_retire: 'Name “{nom}” removed.',
  versions_ouverte: 'Version of {date} opened read-only.', versions_deja_ouverte: 'This version is already open.',
  lecture_seule: 'read-only',
  lecture_seule_aide: 'An old version, opened read-only: Save as keeps a copy of it elsewhere, File › History restores it.',
  versions_lecture_titre: 'Read-only version',
  versions_lecture_texte: '“{nom}” is an old version of {source}, opened read-only: saving it wouldn’t change the project. Save as… keeps a copy of it elsewhere; Restore… makes it the project’s state again (its current state stays in the history).',
  versions_enregistrer_sous: 'Save as…', versions_lecture_panneau: 'This project is the version of {date} of {source}, opened read-only.',
  versions_voir_source: 'The history of {source}',
  versions_restaurer_titre: 'Restore the version of {date}',
  versions_restaurer_texte: 'The project becomes this version again: its data, models, documents and figures. Its current state stays in the history: what is in no version is kept in one first, then a version “Restored the version of {longue}” is saved.',
  versions_bouton_restaurer: 'Restore', versions_avant_titre: 'Unsaved changes',
  versions_avant_texte: 'The project has unsaved changes: restoring would lose them. Save them first? They stay in the history.',
  versions_sans_garder: 'Don’t keep them', versions_enregistrer_restaurer: 'Save and restore', versions_restauree: 'Version of {date} restored.',
  versions_comparaison_titre: 'version of {date} → current project', versions_progression_avant: 'Models of the version of {date}: {nom} ({k} of {n})…',
  versions_progression_apres: 'Current project’s models: {nom} ({k} of {n})…', versions_comparaison_faite: 'Comparison done.',
  versions_progression_autre: 'Models of the other version: {nom} ({k} of {n})…',
  versions_reglages_titre: 'Version settings', versions_reglage_auto: 'Keep versions of my projects',
  versions_reglage_auto_aide: 'A project’s versions are kept in a hidden folder next to it: when it is saved (one an hour at most), when it is closed, before its data are updated. Without this setting, no new history starts; existing ones stay.',
  versions_auteur_titre: 'Who saves the versions',
  versions_auteur_texte: 'Your name is written with each version; your email address too, once a project is shared: GitHub and GitLab show who did what by it.',
  versions_auteur_nom: 'Name', versions_auteur_email: 'Email address', versions_auteur_manque: 'Give a name and an email address.',
  versions_auteur: 'Versions signed {nom}', versions_auteur_email_court: '{nom} <{email}>', versions_reglages_gardes: 'Version settings kept.',
  versions_partager_titre: 'Share on GitHub or GitLab',
  versions_partager_texte: 'The project’s versions are sent to a repository on GitHub or GitLab, and your colleagues’ come back from it: everyone works on their own copy, and Nuée keeps them in step.',
  versions_etape_depot: 'A repository on GitHub or GitLab: a new one, empty or not, or one that already holds other files; private if the data are.',
  versions_etape_jeton: 'A token that can write to it: on GitHub, a fine-grained token with Contents read and write on that repository; on GitLab, a token with write_repository.',
  versions_etape_coller: 'The repository’s address (https://…) and the token, here.',
  versions_lien_github_depot: 'new GitHub repository', versions_lien_gitlab_depot: 'new GitLab project', versions_lien_github_jeton: 'GitHub token', versions_lien_gitlab_jeton: 'GitLab token',
  versions_adresse: 'Repository’s address', versions_adresse_exemple: 'https://github.com/you/project', versions_jeton: 'Token',
  versions_jeton_aide: 'Kept in the computer’s keychain, it is never shown again.', versions_jeton_garder: 'A token is kept for {hote}: leave this empty to keep it.',
  versions_verifier: 'Check', versions_verification: 'Checking…', versions_partage_ok: 'Shared with {hote}.',
  versions_place: 'In this repository, the project is at {chemin}: another file was already at {nom}.',
  versions_compte: 'Sign in to {service}…', versions_inviter: 'Invite a colleague…',
  versions_partager_compte: 'Sign in, and Nuée makes it a private repository on your account:',
  versions_depot_existant: 'Use an existing repository, or another server',
  versions_depot_existant_texte: 'Its address, as its Code button on GitHub or GitLab gives it. Signed in to GitHub or GitLab, nothing else is needed; for another server, a token that can write to it.',
  versions_plutot_existant: 'Use an existing repository instead',
  versions_nouveau_depot: 'A new repository on {service}', versions_nom_depot: 'Its name',
  versions_depot_prive: 'Private: only you and the colleagues you invite can see it',
  versions_creer_partager: 'Create and share', versions_creation: 'Creating the repository on {service}…', versions_partage_en_cours: 'Sharing the project…',
  versions_partager_bouton: 'Share',
  versions_partage_fait: '{nom} is shared on {service}, in the repository {depot}.', versions_partage_fait_prive: '{nom} is shared on {service}, in the private repository {depot}.',
  versions_partage_suite: 'Its versions are sent there by themselves, and your colleagues’ come back from it. Invite them: they then open it with File › Open from GitHub or GitLab.',
  versions_non_connecte: 'Not signed in to {service}: sending and getting the versions need a sign-in.',
  versions_connexion_refusee: '{service} no longer accepts the sign-in: sign in again to send and get the versions.',
  versions_deconnecter_titre: 'Sign out of {service}',
  versions_deconnecter_texte: 'Nuée forgets its sign-in to {service} on this computer: the projects shared there stop sending and getting their versions until you sign in again. Your account and its repositories stay as they are.',
  versions_connexion_gardee: 'Sign-in kept in this computer’s keychain.',
  versions_distant_connexion: 'This repository is private: sign in to {service} with an account that can see it, or give a token.',
  versions_partage_hote: 'shared with {hote}', versions_partage_avec: 'Shared with {hote}:', versions_dernier_echange: 'last exchange {date}',
  versions_court_a_jour: 'up to date', versions_court_envoi: 'sending…', versions_court_a_recevoir: 'newer version', versions_court_attente: 'waiting',
  versions_court_conflit: 'to choose', versions_court_erreur: 'error',
  versions_etat_a_jour: 'Up to date', versions_etat_envoi: 'Sending the versions…', versions_etat_a_envoyer: '{n} to send',
  versions_etat_a_recevoir: '{n} newer to get', versions_etat_attente: 'A newer version waits for your changes to be saved',
  versions_etat_conflit: 'The project changed on both sides: choose', versions_etat_erreur: 'The last exchange failed',
  versions_envoyer_maintenant: 'Send now', versions_recevoir_maintenant: 'Get now', versions_reessayer: 'Try again',
  versions_changer_jeton: 'Change the address or token…', versions_arreter_partage: 'Stop sharing',
  versions_arreter_texte: 'Stop sharing {nom} with {hote}? The versions stay here, and in the repository.', versions_partage_arrete: 'The project is no longer shared.',
  versions_jeton_garde: 'Token kept in this computer’s keychain.',
  versions_jeton_session: 'Token kept until Nuée closes: this computer has no keychain Nuée can use. It will need pasting again next time.',
  versions_jeton_aucun: 'No token kept: sending and getting need one.',
  versions_envoyees: 'Versions sent to {hote}.', versions_recu_a_jour: 'Nothing new on {hote}: the project is up to date.',
  versions_recu: 'Updated with the version {nom} sent {quand}.', versions_recu_anonyme: 'Updated with the version sent {quand}.',
  versions_quand_heure: 'at {heure}', versions_quand_date: 'on {date}', versions_voir_changements: 'What changed',
  versions_attente: 'A newer version of {nom} waits on {hote}: save your changes, then get it.',
  versions_recevoir_avant: 'The project has unsaved changes. Save them first, then get the newer version?',
  versions_enregistrer_recevoir: 'Save and get', versions_recu_non_recharge: 'A newer version of {nom} is in its file; your unsaved changes are kept here. Saving them would replace that version.',
  versions_recharger: 'Reload the project',
  versions_conflit_titre: 'The project changed on both sides', versions_conflit_texte: '{nom} sent a version of this project while you changed it too.',
  versions_conflit_vous: 'Yours', versions_conflit_leur: '{nom}’s', versions_collegue: 'A colleague',
  versions_garder_miennes: 'Keep mine', versions_prendre_leurs: 'Take theirs', versions_comparer_conflit: 'Compare',
  versions_fusionner: 'Merge both', versions_fusion_calcul: 'Nuée looks at what each side changed…',
  versions_fusion_auto: 'Nothing clashes: Nuée can merge both versions.', versions_fusion_apports_mien: 'You:', versions_fusion_apports_leur: '{nom}:',
  versions_fusion_conflits: 'Some changes clash: choose, for each, the side to keep.',
  versions_fusion_choix_mien: 'mine', versions_fusion_choix_leur: 'theirs', versions_fusion_appliquer: 'Merge with these choices',
  versions_fusion_description: 'Merged with the version of {nom}', versions_fusion_faite: 'Both versions are merged.',
  versions_conflit_enregistrer: 'The project has unsaved changes: save them first, they would be lost.',
  versions_conflit_avis: '{nom} changed on both sides.', versions_choisir: 'Choose…',
  versions_gardees_miennes: 'Your version is kept, and sent to {hote}.', versions_prises_leurs: 'The project is now the one on {hote}; your version stays in the history.',
  versions_comparaison_leur: 'your project → {nom}’s version of {date}', versions_comparaison_recu: 'the version before → {nom}’s',
  versions_distant_titre: 'Open from GitHub or GitLab',
  versions_distant_texte: 'A project a colleague shared: the repository’s address, then the project, then the folder to put it in. Its versions come with it, and stay in step with the repository.',
  versions_distant_chercher: 'Find the projects', versions_distant_jeton: 'This repository is private: give a token that can read it (and write to it, to send your versions).',
  versions_distant_aucun: 'No project (.nuee) in this repository.', versions_distant_projets: 'Project', versions_distant_dossier: 'Folder',
  versions_distant_choisir_dossier: 'Choose…', versions_distant_ouvrir: 'Open', versions_distant_pas_dossier: 'Choose the folder to put the project in.',
  versions_distant_pas_projet: 'Choose one of the repository’s projects.', versions_distant_ouvert: '{nom} opened from {hote}, and shared with it.',
  versions_dossier_titre: '{nom} changed on disk',
  versions_dossier_texte: '{qui} saved this project while you had it open: their changes can join yours.',
  versions_dossier_texte_enregistrer: '{qui} saved this project while you changed it too: merge their changes with yours before saving?',
  versions_dossier_sans_base: '{nom} was changed on disk by {qui}. Nuée has restarted since it was opened and can’t merge it any more: saving will ask whether to overwrite their changes.',
  versions_dossier_fusionne: 'Their changes are merged into the project: save it to keep them.', versions_pas_maintenant: 'Not now',
  versions_fusion_sans_base: 'There is no version in common to merge from: keep one side.', versions_fusion_sans_moteur: 'Nuée can’t merge these two versions: keep one side.',
  versions_fusion_donnees_etapes: 'The data: both sides’ steps, replayed from the source.', versions_fusion_donnees_cellules: 'The data: merged cell by cell.',
  versions_fusion_donnees_raison: 'The data: merged cell by cell ({raison}).',
  versions_fusion_passages: '{n} passage(s) written differently', versions_fusion_ligne: 'Paragraph {ligne}', versions_fusion_cellules: '{n} cell(s) changed differently',
  versions_fusion_cellules_plus: 'and {n} more', versions_fusion_observation: 'Observation',
  versions_indicateur_local: 'History: {n} versions kept next to the project', versions_indicateur_local_une: 'History: 1 version kept next to the project',
  versions_indicateur_aucune: 'History: no version yet', versions_indicateur_desactive: 'History: this project keeps no versions',
  changements: 'What changed', changements_titre: 'What changed: {titre}',
  changements_resume: '{n} model(s) compared, {notables} with notable changes.', changements_aucun_modele: 'No model to compare: neither side has a saved model.',
  changements_inchange: 'No change in its key results.', changements_peu: 'Small changes, none notable.',
  changements_entrees: '{n} result(s) changed out of {total}', changements_tout: 'Show the {n} changed results', changements_avant_seul: 'Only before:',
  changements_apres_seul: 'Only after:', changements_erreur: '{nom} couldn’t be fitted: {message}', changements_non_compare: '{nom} couldn’t be compared: {message}',
  changements_pas_la: 'This comparison is no longer here: compare again from File › History.', changements_calcul: 'Comparing the results…',
  changements_resultat: 'Result', changements_avant: 'Before', changements_apres: 'After', changements_ecart: 'Change', changements_points: 'points',
  changements_g_ajustement: 'Fit', changements_g_estimations: 'Estimates', changements_g_tests: 'Tests', changements_g_aleatoires: 'Random effects',
  changements_g_composantes: 'Components', changements_g_variables: 'Variables', changements_g_especes: 'Species', changements_g_moyennes: 'Adjusted means',
  changements_g_puissance: 'Trend power', changements_g_jeu_test: 'Test set', changements_g_stocke: 'Stored results',
  changements_d_nouveau: 'new', changements_d_disparu: 'gone', changements_d_signe: 'sign', changements_d_seuil: '5% threshold', changements_d_ic: 'outside the former CI',
  changements_d_categorie: 'category', changements_d_retourne: 'reflected component',
  changements_d_aide: 'new, gone; sign: crossed to the other side of 0 (or 1); 5% threshold: significant on one side only; outside the former CI: the new value lies outside the former 95% interval; category: its category changed; reflected component: its arbitrary sign flipped, turned back before comparing',
} };
ajouterTextes(TEXTES_VERSIONS.fr, TEXTES_VERSIONS.en);
