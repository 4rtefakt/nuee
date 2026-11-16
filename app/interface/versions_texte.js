// The words and numbers of File › Versions (versions.js) and of What changed (changements.js), pure:
// no page, no engine, no main process, so that tests/essai_versions.mjs checks them alone. The
// strings of both are here too. People never read a word of the tool underneath (app/versions.js):
// versions, names, send and get.
import { t, ajouterTextes, langueActuelle } from './i18n.js';
import { decimal } from './nombres.js';

const locale = (langue) => (langue === 'fr' ? 'fr-FR' : 'en-GB');
// (fuseau: a time zone for the checks; otherwise the computer's)
const jour = (d, fuseau) => new Intl.DateTimeFormat('en-CA', { year: 'numeric', month: '2-digit', day: '2-digit', ...(fuseau ? { timeZone: fuseau } : {}) }).format(d);

// A version's date, in full: "12 Nov 2026, 14:32", "12 nov. 2026, 14:32" (a title, a sentence);
// style 'long', as the main process writes it in a restored version's description (app/versions.js:
// "12 November 2026 at 14:32").
export function dateLongue(iso, { langue = langueActuelle(), fuseau, style = 'medium' } = {}) {
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return String(iso || '');
  return new Intl.DateTimeFormat(locale(langue), { dateStyle: style, timeStyle: 'short', ...(fuseau ? { timeZone: fuseau } : {}) }).format(d);
}

// A version's date in the list: today's and yesterday's by their hour, the others in full.
export function dateVersion(iso, { langue = langueActuelle(), maintenant = new Date(), fuseau } = {}) {
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return String(iso || '');
  const heure = new Intl.DateTimeFormat(locale(langue), { timeStyle: 'short', ...(fuseau ? { timeZone: fuseau } : {}) }).format(d);
  if (jour(d, fuseau) === jour(maintenant, fuseau)) return t('versions_aujourdhui', { heure });
  if (jour(d, fuseau) === jour(new Date(maintenant.getTime() - 86400000), fuseau)) return t('versions_hier', { heure });
  return dateLongue(iso, { langue, fuseau });
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

// The server of an address being typed in Connect, when it reads as an https address ('' otherwise).
// The main process checks the address itself (app/versions.js: adresseDistante) and says why it
// refuses one: this only chooses the help's links.
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

// The status bar's indicator, from etat(): nothing when the project keeps no versions; the
// versions to send (↑) and to get (↓), as of the last send or get, when it is connected.
// → null or { texte, titre }
export function indicateurVersions(e) {
  if (!e || !e.suivi) return null;
  const n = Number(e.versions) || 0;
  if (!e.distant) return { texte: '', titre: t(n === 1 ? 'versions_indicateur_local_une' : 'versions_indicateur_local', { n }) };
  // (a remote Nuée can't reach, an SSH address set with git: its address)
  const hote = nomHote(e.distant.hote) || e.distant.url || '';
  const avance = Number(e.avance) || 0;
  const retard = Number(e.retard) || 0;
  const texte = [avance ? `↑${avance}` : '', retard ? `↓${retard}` : ''].filter(Boolean).join(' ');
  const titre = avance || retard
    ? t('versions_indicateur', { quoi: [avance ? t('versions_a_envoyer', { n: avance }) : '', retard ? t('versions_a_recevoir', { n: retard }) : ''].filter(Boolean).join(', '), hote })
    : t('versions_indicateur_a_jour', { hote });
  return { texte, titre };
}

// Get versions found both sides saved versions since a common one (recevoir → { resultat:
// 'diverge', mien, leur }, each shaped like liste(), newest first): the date of that version, from
// the list of this side's versions: the first that isn't one of ours since; none when the two
// histories have nothing in common. → { depuis (ISO or null), mien, leur }
export function resumeDivergence(r, liste = []) {
  const mien = [].concat((r && r.mien) || []);
  const leur = [].concat((r && r.leur) || []);
  const nouvelles = new Set(mien.map((v) => v.oid));
  const commune = [].concat(liste || []).find((v) => !nouvelles.has(v.oid));
  return { depuis: commune ? commune.date : null, mien, leur };
}

// The sentence of the divergence dialog.
export function phraseDivergence(d, hote, options = {}) {
  const lieu = nomHote(hote);
  return d.depuis ? t('versions_divergence_texte', { date: dateLongue(d.depuis, options), hote: lieu, ici: d.mien.length, la: d.leur.length })
    : t('versions_divergence_sans_date', { hote: lieu, ici: d.mien.length, la: d.leur.length });
}

// A version's description, or what stands for it.
export const descriptionVersion = (v) => (v && String(v.description || '').trim()) || t('versions_sans_description');

// "12 versions"
export const nombreVersions = (n) => t(Number(n) === 1 ? 'versions_une' : 'versions_n', { n: Number(n) || 0 });

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
  versions: 'Versions…', versions_vue: 'Versions',
  versions_navigateur: 'Les versions d’un projet sont gardées par l’application de bureau : elles ne sont pas disponibles dans le navigateur.',
  versions_jamais_enregistre: 'Pour garder des versions, le projet doit d’abord être enregistré dans un fichier : ses versions sont gardées dans son dossier.',
  versions_enregistrer_projet: 'Enregistrer le projet…',
  versions_explication: 'Une version est un état enregistré du projet, que vous pouvez retrouver plus tard : ses données, ses modèles, ses documents et ses figures tels qu’ils étaient. Les versions restent dans le dossier du projet ; rien ne quitte l’ordinateur tant que vous ne connectez pas GitHub ou GitLab.',
  versions_dossier_partage: 'D’autres fichiers de ce dossier gardent déjà des versions : celles de ce projet seront gardées avec elles.',
  versions_activer: 'Garder des versions de ce projet', versions_activees: 'Versions gardées : une première version est enregistrée.',
  versions_auteur_titre: 'Qui enregistre les versions',
  versions_auteur_texte: 'Chaque version garde le nom et l’adresse électronique de qui l’a enregistrée, pour qu’on sache qui a fait quoi. Ils sont gardés sur cet ordinateur, pour toutes les versions.',
  versions_auteur_nom: 'Nom', versions_auteur_email: 'Adresse électronique', versions_auteur_manque: 'Donnez un nom et une adresse électronique.',
  versions_auteur_bouton: 'Continuer',
  versions_hors_https: 'Ce dépôt est à une adresse que Nuée ne joint pas (https:// seulement) : changez l’adresse pour envoyer et recevoir.',
  versions_auteur: 'Versions signées {nom} <{email}>', versions_changer: 'Changer…', versions_actualiser: 'Actualiser',
  versions_enregistrer: 'Enregistrer une version…', versions_description_titre: 'Enregistrer une version — {nom}',
  versions_description_aide: 'Quelques mots sur cet état, pour le retrouver plus tard :', versions_description_exemple: 'Comptages de 2026 ajoutés, modèles réajustés',
  versions_description_vide: 'Écrivez une courte description.', versions_enregistre_avant: 'Le projet a des changements non enregistrés : il est d’abord enregistré.',
  versions_bouton_enregistrer: 'Enregistrer la version', versions_inchange: 'Le projet n’a pas changé depuis sa dernière version : rien à enregistrer.',
  versions_enregistree: 'Version enregistrée : {description}', versions_n: '{n} versions', versions_une: '1 version', versions_dans: 'gardées dans {dossier}',
  versions_non_versionne: 'Le projet a des changements qui ne sont pas encore dans une version.', versions_actuelle: 'état actuel',
  versions_ouvrir: 'Ouvrir', versions_ouvrir_aide: 'L’ouvrir à côté du projet, en lecture seule',
  versions_comparer: 'Comparer', versions_comparer_aide: 'Comparer avec le projet actuel : ce qui a changé dans les résultats des modèles',
  versions_nommer: 'Nommer…', versions_restaurer: 'Restaurer…', versions_sans_description: '(sans description)', versions_aucune: 'Aucune version.',
  versions_aujourdhui: 'aujourd’hui, {heure}', versions_hier: 'hier, {heure}', versions_titre_version: '{nom} — version du {date}',
  versions_ouverte: 'Version du {date} ouverte en lecture seule.', versions_deja_ouverte: 'Cette version est déjà ouverte.',
  lecture_seule: 'lecture seule',
  lecture_seule_aide: 'Une ancienne version, ouverte en lecture seule : Enregistrer sous en garde une copie ailleurs, Fichier › Versions la restaure.',
  versions_lecture_titre: 'Version en lecture seule',
  versions_lecture_texte: '« {nom} » est une ancienne version de {source}, ouverte en lecture seule : l’enregistrer ne changerait pas le projet. Enregistrer sous… en garde une copie ailleurs ; Restaurer… redonne cet état au projet (son état actuel reste dans l’historique).',
  versions_enregistrer_sous: 'Enregistrer sous…', versions_lecture_panneau: 'Ce projet est la version du {date} de {source}, ouverte en lecture seule.',
  versions_voir_source: 'Les versions de {source}',
  versions_nommer_titre: 'Nommer la version du {date}',
  versions_nommer_aide: 'Un nom marque une version pour la retrouver d’un coup d’œil : « soumis à la revue », « chapitre 2 de la thèse ».',
  versions_nom: 'Nom', versions_nom_vide: 'Écrivez un nom.', versions_noms: 'Ses noms :', versions_retirer_nom: 'Retirer le nom « {nom} »',
  versions_bouton_nommer: 'Nommer', versions_nommee: 'Version nommée « {nom} ».', versions_nom_retire: 'Nom « {nom} » retiré.',
  versions_restaurer_titre: 'Restaurer la version du {date}',
  versions_restaurer_texte: 'Le projet redevient cette version : ses données, ses modèles, ses documents et ses figures. Son état actuel reste dans l’historique : une version « Version du {longue} restaurée » est enregistrée, et vous pourrez revenir à l’état d’aujourd’hui.',
  versions_bouton_restaurer: 'Restaurer', versions_avant_titre: 'Des changements ne sont dans aucune version',
  versions_avant_texte: 'Le projet a des changements non enregistrés : restaurer les perdrait. Les enregistrer d’abord dans une version ?',
  versions_avant_fichier: 'Le projet a des changements qui ne sont dans aucune version : ils sont d’abord enregistrés dans une version, pour rester dans l’historique, puis la version du {date} est restaurée.',
  versions_enregistrer_restaurer: 'Enregistrer une version et restaurer',
  versions_pas_nuee: 'Les versions se gardent pour un projet enregistré au format de Nuée (.nuee) : enregistrez-le sous ce format.',
  versions_avant_description: 'Avant de restaurer la version du {date}', versions_sans_garder: 'Ne pas les garder', versions_enregistrer_dabord: 'Enregistrer une version d’abord',
  versions_restauree: 'Version du {date} restaurée.',
  versions_comparaison_titre: 'version du {date} → projet actuel', versions_progression_avant: 'Modèles de la version du {date} : {nom} ({k} sur {n})…',
  versions_progression_apres: 'Modèles du projet actuel : {nom} ({k} sur {n})…', versions_comparaison_faite: 'Comparaison faite.',
  versions_envoi: 'Envoyer et recevoir',
  versions_envoi_aide: 'Gardez une copie des versions sur GitHub ou GitLab : pour retrouver le projet sur un autre ordinateur, ou y travailler à plusieurs.',
  versions_connecter: 'Connecter GitHub ou GitLab…', versions_connecte: 'Connecté à {hote} :', versions_envoyer: 'Envoyer les versions', versions_recevoir: 'Recevoir les versions',
  versions_a_envoyer: '{n} à envoyer', versions_a_recevoir: '{n} à recevoir', versions_a_jour_distant: 'à jour, au dernier envoi ou à la dernière réception',
  versions_verifier: 'Vérifier', versions_modifier_connexion: 'Changer l’adresse ou le jeton…',
  versions_jeton_garde: 'Jeton gardé dans le trousseau de cet ordinateur.',
  versions_jeton_session: 'Jeton gardé jusqu’à la fermeture de Nuée : cet ordinateur n’a pas de trousseau que Nuée puisse utiliser. Il sera à coller de nouveau la prochaine fois.',
  versions_jeton_aucun: 'Aucun jeton gardé : envoyer et recevoir en demandent un.', versions_oublier_jeton: 'Oublier le jeton', versions_jeton_oublie: 'Jeton oublié.',
  versions_envoyees: 'Versions envoyées à {hote}.',
  versions_recevoir_refuse: 'Le projet a des changements qui ne sont dans aucune version : recevoir les versions de {hote} pourrait les écraser. Les enregistrer d’abord dans une version ?',
  versions_recu_a_jour: 'Rien de nouveau sur {hote} : le projet est à jour.', versions_recu_avance: 'Versions reçues de {hote} : le projet est à jour.',
  versions_divergence_titre: 'De nouvelles versions des deux côtés',
  versions_divergence_texte: 'Depuis la version du {date}, des versions ont été enregistrées ici ({ici}) et sur {hote} ({la}). Gardez les vôtres ou prenez les leurs : les deux historiques restent, seul l’état du projet est choisi.',
  versions_divergence_sans_date: 'Des versions ont été enregistrées ici ({ici}) et sur {hote} ({la}), sans version commune. Gardez les vôtres ou prenez les leurs : les deux historiques restent, seul l’état du projet est choisi.',
  versions_ici: 'Ici', versions_la_bas: 'Sur {hote}', versions_garder_miennes: 'Garder les miennes', versions_prendre_leurs: 'Prendre les leurs',
  versions_comparer_dabord: 'Comparer d’abord', versions_gardees_miennes: 'Vos versions sont gardées et envoyées à {hote}.',
  versions_prises_leurs: 'Le projet est maintenant celui de {hote} ; vos versions restent dans l’historique.', versions_comparaison_leur: 'votre projet → la version de {hote} du {date}',
  versions_divergence_banniere: 'Des versions ont été enregistrées ici et sur {hote} depuis la dernière réception.', versions_choisir: 'Choisir…',
  versions_connexion_titre: 'Connecter GitHub ou GitLab',
  versions_connexion_texte: 'Les versions sont envoyées dans un dépôt à vous, sur GitHub ou GitLab : une copie en ligne, que vous retrouvez sur un autre ordinateur ou partagez avec vos collègues.',
  versions_etape_depot: 'Créez un dépôt privé et vide, sans README, .gitignore ni licence (sur GitLab, décochez « Initialize repository with a README ») : un dépôt qui a déjà un fichier est refusé.',
  versions_etape_jeton: 'Créez un jeton qui peut y écrire : sur GitHub, un jeton à accès précis (« fine-grained ») avec Contents en lecture et écriture sur ce dépôt ; sur GitLab, un jeton avec write_repository :',
  versions_etape_coller: 'Collez ici l’adresse du dépôt (https://…) et le jeton.',
  versions_lien_github_depot: 'sur GitHub', versions_lien_gitlab_depot: 'sur GitLab', versions_lien_github_jeton: 'jeton GitHub', versions_lien_gitlab_jeton: 'jeton GitLab',
  versions_adresse: 'Adresse du dépôt', versions_adresse_exemple: 'https://github.com/vous/projet', versions_jeton: 'Jeton',
  versions_jeton_aide: 'Gardé dans le trousseau de l’ordinateur, il n’est plus jamais montré.', versions_jeton_garder: 'Un jeton est gardé pour {hote} : laissez vide pour le garder.',
  versions_verification: 'Vérification…', versions_connexion_ok: 'Connecté à {hote}.',
  versions_indicateur_local: '{n} versions gardées sur cet ordinateur', versions_indicateur_local_une: '1 version gardée sur cet ordinateur',
  versions_indicateur: '{quoi} — {hote}, depuis le dernier envoi ou la dernière réception', versions_indicateur_a_jour: 'Versions à jour avec {hote}, au dernier envoi ou à la dernière réception',
  changements: 'Ce qui a changé', changements_titre: 'Ce qui a changé : {titre}',
  changements_resume: '{n} modèle(s) comparé(s), {notables} avec des changements notables.', changements_aucun_modele: 'Aucun modèle à comparer : ni l’un ni l’autre n’a de modèle enregistré.',
  changements_inchange: 'Aucun changement dans ses résultats clés.', changements_peu: 'De petits changements, aucun notable.',
  changements_entrees: '{n} résultat(s) changé(s) sur {total}', changements_tout: 'Montrer les {n} résultats changés', changements_avant_seul: 'Seulement avant :',
  changements_apres_seul: 'Seulement après :', changements_erreur: '{nom} n’a pas pu être ajusté : {message}', changements_non_compare: '{nom} n’a pas pu être comparé : {message}',
  changements_pas_la: 'Cette comparaison n’est plus là : comparez de nouveau depuis Fichier › Versions.', changements_calcul: 'Comparaison des résultats…',
  changements_resultat: 'Résultat', changements_avant: 'Avant', changements_apres: 'Après', changements_ecart: 'Écart', changements_points: 'points',
  changements_g_ajustement: 'Ajustement', changements_g_estimations: 'Estimations', changements_g_tests: 'Tests', changements_g_aleatoires: 'Effets aléatoires',
  changements_g_composantes: 'Composantes', changements_g_variables: 'Variables', changements_g_especes: 'Espèces', changements_g_moyennes: 'Moyennes ajustées',
  changements_g_puissance: 'Puissance', changements_g_jeu_test: 'Jeu de test', changements_g_stocke: 'Résultats du fichier',
  changements_d_nouveau: 'nouveau', changements_d_disparu: 'disparu', changements_d_signe: 'signe', changements_d_seuil: 'seuil de 5 %', changements_d_ic: 'hors de l’ancien IC',
  changements_d_categorie: 'catégorie', changements_d_retourne: 'composante retournée',
  changements_d_aide: 'nouveau, disparu ; signe : passé de l’autre côté de 0 (ou de 1) ; seuil de 5 % : significatif d’un côté seulement ; hors de l’ancien IC : la nouvelle valeur sort de l’ancien intervalle à 95 % ; catégorie : sa catégorie a changé ; composante retournée : son signe arbitraire a changé, remis dans le même sens avant de comparer',
}, en: {
  versions: 'Versions…', versions_vue: 'Versions',
  versions_navigateur: 'Versions of a project are kept by the desktop app: they aren’t available in a browser.',
  versions_jamais_enregistre: 'To keep versions, the project must first be saved to a file: its versions are kept in its folder.',
  versions_enregistrer_projet: 'Save the project…',
  versions_explication: 'A version is a saved state of the project that you can go back to: its data, models, documents and figures as they were. Versions stay in the project’s folder; nothing leaves the computer unless you connect GitHub or GitLab.',
  versions_dossier_partage: 'Other files in this folder already keep versions: this project’s will be kept with them.',
  versions_activer: 'Keep versions of this project', versions_activees: 'Versions kept: a first version is saved.',
  versions_auteur_titre: 'Who saves the versions',
  versions_auteur_texte: 'Each version keeps the name and email address of whoever saved it, so that everyone knows who did what. They are kept on this computer, for every version.',
  versions_auteur_nom: 'Name', versions_auteur_email: 'Email address', versions_auteur_manque: 'Give a name and an email address.',
  versions_auteur_bouton: 'Continue',
  versions_hors_https: 'This repository is at an address Nuée can’t reach (https:// only): change the address to send and get.',
  versions_auteur: 'Versions signed {nom} <{email}>', versions_changer: 'Change…', versions_actualiser: 'Refresh',
  versions_enregistrer: 'Save a version…', versions_description_titre: 'Save a version — {nom}',
  versions_description_aide: 'A few words on this state, to find it again later:', versions_description_exemple: '2026 counts added, models refitted',
  versions_description_vide: 'Write a short description.', versions_enregistre_avant: 'The project has unsaved changes: it is saved first.',
  versions_bouton_enregistrer: 'Save the version', versions_inchange: 'The project hasn’t changed since its last version: nothing to save.',
  versions_enregistree: 'Version saved: {description}', versions_n: '{n} versions', versions_une: '1 version', versions_dans: 'kept in {dossier}',
  versions_non_versionne: 'The project has changes that aren’t in a version yet.', versions_actuelle: 'current state',
  versions_ouvrir: 'Open', versions_ouvrir_aide: 'Open it beside the project, read-only',
  versions_comparer: 'Compare', versions_comparer_aide: 'Compare with the current project: what changed in the models’ results',
  versions_nommer: 'Name…', versions_restaurer: 'Restore…', versions_sans_description: '(no description)', versions_aucune: 'No versions.',
  versions_aujourdhui: 'today, {heure}', versions_hier: 'yesterday, {heure}', versions_titre_version: '{nom} — version of {date}',
  versions_ouverte: 'Version of {date} opened read-only.', versions_deja_ouverte: 'This version is already open.',
  lecture_seule: 'read-only',
  lecture_seule_aide: 'An old version, opened read-only: Save as keeps a copy of it elsewhere, File › Versions restores it.',
  versions_lecture_titre: 'Read-only version',
  versions_lecture_texte: '“{nom}” is an old version of {source}, opened read-only: saving it wouldn’t change the project. Save as… keeps a copy of it elsewhere; Restore… makes it the project’s state again (its current state stays in the history).',
  versions_enregistrer_sous: 'Save as…', versions_lecture_panneau: 'This project is the version of {date} of {source}, opened read-only.',
  versions_voir_source: 'The versions of {source}',
  versions_nommer_titre: 'Name the version of {date}',
  versions_nommer_aide: 'A name marks a version so it can be found at a glance: “submitted to the journal”, “thesis chapter 2”.',
  versions_nom: 'Name', versions_nom_vide: 'Write a name.', versions_noms: 'Its names:', versions_retirer_nom: 'Remove the name “{nom}”',
  versions_bouton_nommer: 'Name it', versions_nommee: 'Version named “{nom}”.', versions_nom_retire: 'Name “{nom}” removed.',
  versions_restaurer_titre: 'Restore the version of {date}',
  versions_restaurer_texte: 'The project becomes this version again: its data, models, documents and figures. Its current state stays in the history: a version “Restored the version of {longue}” is saved, and you can come back to today’s state.',
  versions_bouton_restaurer: 'Restore', versions_avant_titre: 'Changes in no version',
  versions_avant_texte: 'The project has unsaved changes: restoring would lose them. Save them in a version first?',
  versions_avant_fichier: 'The project has changes that aren’t in any version: they are saved in a version first, to stay in the history, then the version of {date} is restored.',
  versions_enregistrer_restaurer: 'Save a version and restore',
  versions_pas_nuee: 'Versions are kept for a project saved in Nuée’s format (.nuee): save it in that format.',
  versions_avant_description: 'Before restoring the version of {date}', versions_sans_garder: 'Don’t keep them', versions_enregistrer_dabord: 'Save a version first',
  versions_restauree: 'Version of {date} restored.',
  versions_comparaison_titre: 'version of {date} → current project', versions_progression_avant: 'Models of the version of {date}: {nom} ({k} of {n})…',
  versions_progression_apres: 'Current project’s models: {nom} ({k} of {n})…', versions_comparaison_faite: 'Comparison done.',
  versions_envoi: 'Send and receive',
  versions_envoi_aide: 'Keep a copy of the versions on GitHub or GitLab: to find the project on another computer, or to work on it with others.',
  versions_connecter: 'Connect GitHub or GitLab…', versions_connecte: 'Connected to {hote}:', versions_envoyer: 'Send versions', versions_recevoir: 'Get versions',
  versions_a_envoyer: '{n} to send', versions_a_recevoir: '{n} to get', versions_a_jour_distant: 'up to date, as of the last send or get',
  versions_verifier: 'Check', versions_modifier_connexion: 'Change the address or token…',
  versions_jeton_garde: 'Token kept in this computer’s keychain.',
  versions_jeton_session: 'Token kept until Nuée closes: this computer has no keychain Nuée can use. It will need pasting again next time.',
  versions_jeton_aucun: 'No token kept: sending and getting need one.', versions_oublier_jeton: 'Forget the token', versions_jeton_oublie: 'Token forgotten.',
  versions_envoyees: 'Versions sent to {hote}.',
  versions_recevoir_refuse: 'The project has changes that aren’t in any version: getting the versions from {hote} could overwrite them. Save them in a version first?',
  versions_recu_a_jour: 'Nothing new on {hote}: the project is up to date.', versions_recu_avance: 'Versions received from {hote}: the project is up to date.',
  versions_divergence_titre: 'New versions on both sides',
  versions_divergence_texte: 'Since the version of {date}, versions were saved both here ({ici}) and on {hote} ({la}). Keep yours or take theirs: both histories are kept; only the project’s state is chosen.',
  versions_divergence_sans_date: 'Versions were saved both here ({ici}) and on {hote} ({la}), with no version in common. Keep yours or take theirs: both histories are kept; only the project’s state is chosen.',
  versions_ici: 'Here', versions_la_bas: 'On {hote}', versions_garder_miennes: 'Keep mine', versions_prendre_leurs: 'Take theirs',
  versions_comparer_dabord: 'Compare first', versions_gardees_miennes: 'Your versions are kept, and sent to {hote}.',
  versions_prises_leurs: 'The project is now the one on {hote}; your versions stay in the history.', versions_comparaison_leur: 'your project → the version on {hote} of {date}',
  versions_divergence_banniere: 'Versions were saved both here and on {hote} since the last get.', versions_choisir: 'Choose…',
  versions_connexion_titre: 'Connect GitHub or GitLab',
  versions_connexion_texte: 'Versions are sent to a repository of yours on GitHub or GitLab: an online copy that you get back on another computer, or share with colleagues.',
  versions_etape_depot: 'Create an empty private repository, without a README, .gitignore or licence (on GitLab, untick “Initialize repository with a README”): a repository that already holds a file is refused.',
  versions_etape_jeton: 'Create a token that can write to it: on GitHub, a fine-grained token with Contents read and write on that repository; on GitLab, a token with write_repository:',
  versions_etape_coller: 'Paste the repository’s address (https://…) and the token here.',
  versions_lien_github_depot: 'on GitHub', versions_lien_gitlab_depot: 'on GitLab', versions_lien_github_jeton: 'GitHub token', versions_lien_gitlab_jeton: 'GitLab token',
  versions_adresse: 'Repository’s address', versions_adresse_exemple: 'https://github.com/you/project', versions_jeton: 'Token',
  versions_jeton_aide: 'Kept in the computer’s keychain, it is never shown again.', versions_jeton_garder: 'A token is kept for {hote}: leave this empty to keep it.',
  versions_verification: 'Checking…', versions_connexion_ok: 'Connected to {hote}.',
  versions_indicateur_local: '{n} versions kept on this computer', versions_indicateur_local_une: '1 version kept on this computer',
  versions_indicateur: '{quoi} — {hote}, since the last send or get', versions_indicateur_a_jour: 'Versions up to date with {hote}, as of the last send or get',
  changements: 'What changed', changements_titre: 'What changed: {titre}',
  changements_resume: '{n} model(s) compared, {notables} with notable changes.', changements_aucun_modele: 'No model to compare: neither side has a saved model.',
  changements_inchange: 'No change in its key results.', changements_peu: 'Small changes, none notable.',
  changements_entrees: '{n} result(s) changed out of {total}', changements_tout: 'Show the {n} changed results', changements_avant_seul: 'Only before:',
  changements_apres_seul: 'Only after:', changements_erreur: '{nom} couldn’t be fitted: {message}', changements_non_compare: '{nom} couldn’t be compared: {message}',
  changements_pas_la: 'This comparison is no longer here: compare again from File › Versions.', changements_calcul: 'Comparing the results…',
  changements_resultat: 'Result', changements_avant: 'Before', changements_apres: 'After', changements_ecart: 'Change', changements_points: 'points',
  changements_g_ajustement: 'Fit', changements_g_estimations: 'Estimates', changements_g_tests: 'Tests', changements_g_aleatoires: 'Random effects',
  changements_g_composantes: 'Components', changements_g_variables: 'Variables', changements_g_especes: 'Species', changements_g_moyennes: 'Adjusted means',
  changements_g_puissance: 'Trend power', changements_g_jeu_test: 'Test set', changements_g_stocke: 'Stored results',
  changements_d_nouveau: 'new', changements_d_disparu: 'gone', changements_d_signe: 'sign', changements_d_seuil: '5% threshold', changements_d_ic: 'outside the former CI',
  changements_d_categorie: 'category', changements_d_retourne: 'reflected component',
  changements_d_aide: 'new, gone; sign: crossed to the other side of 0 (or 1); 5% threshold: significant on one side only; outside the former CI: the new value lies outside the former 95% interval; category: its category changed; reflected component: its arbitrary sign flipped, turned back before comparing',
} };
ajouterTextes(TEXTES_VERSIONS.fr, TEXTES_VERSIONS.en);
