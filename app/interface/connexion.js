// Signing in to GitHub or GitLab and inviting a colleague, in dialogs over window.bureau.comptes (the
// main process's app/comptes.js, through preload.js). The History and sharing screens call
//   ouvrirConnexion(app, { hote }) → Promise<{ ok, compte }>   (hote: 'github.com', 'gitlab.com', a
//     self-hosted GitLab's name; without it, GitHub, GitLab or another GitLab are offered first)
//   ouvrirInvitation(app, { hote, url }) → Promise<{ ok, invites }>   (url: the repository's address)
// - Sign in: the server's own sign-in, its short code shown big and copyable, its page opened in the
//   browser, the wait for the approval there, then the account (picture, name); this computer's git
//   sign-in when git is installed; a token, the advanced way. A self-hosted GitLab where Nuée is
//   registered asks once for the application ID its administrator gives.
// - Invite a colleague: their user name; GitHub sends them an invitation, GitLab makes them a member
//   of the project as Developer.
// - dialogueComptesEssai: Help's temporary entry to try them, until those screens exist.
import { t, ajouterTextes, langueActuelle } from './i18n.js';
import { el, icone, dialogue } from './ui.js';
import { bureau } from './api.js';

const comptes = () => (bureau && bureau.comptes) || null;
const nomService = (hote) => (hote === 'github.com' ? 'GitHub' : hote === 'gitlab.com' ? 'GitLab' : hote);
// An element's content replaced, what is absent left out (the DOM would write "null")
const remplir = (parent, ...enfants) => parent.replaceChildren(...enfants.flat().filter((x) => x !== null && x !== undefined && x !== false));
// A repository's owner and name (or groups and name), from its address
const nomDuDepot = (url) => {
  try { return new URL(url).pathname.replace(/\/-\/.*$/, '').replace(/^\/+|\/+$/g, '').replace(/\.git$/i, ''); } catch { return String(url || ''); }
};
// Where to make a token, its name and scopes filled in
const pageJeton = (hote) => (hote === 'github.com' ? 'https://github.com/settings/tokens/new?scopes=repo&description=Nu%C3%A9e'
  : `https://${hote}/-/user_settings/personal_access_tokens?name=Nu%C3%A9e&scopes=api,read_user,write_repository`);

// An account: its picture (or a silhouette), its name, user name and how it signed in
function carteCompte(hote, k) {
  const service = nomService(hote);
  const details = [k.login && k.nom ? `@${k.login}` : null, k.source ? t(`connexion_source_${k.source}`, { service }) : null,
    k.horsLigne ? t('comptes_hors_ligne', { service }) : null].filter(Boolean).join(' · ');
  return el('div', { class: 'carte-compte' },
    k.avatar ? el('img', { class: 'avatar-compte', src: k.avatar, alt: '' }) : el('span', { class: 'avatar-compte vide' }, icone('circle-user-round')),
    el('div', { class: 'texte-compte' }, el('b', {}, k.nom || (k.login ? `@${k.login}` : service)), details ? el('div', { class: 'doux' }, details) : null));
}

// ------------------------------------------------------------ signing in

export function ouvrirConnexion(app, { hote = null } = {}) {
  const c = comptes();
  if (!c) { app.statut(t('connexion_navigateur'), true); return Promise.resolve({ ok: false }); }
  return new Promise((resolve) => {
    let h = null;       // the server
    let s = null;       // what it offers (serveur)
    let fini = null;    // the account, once signed in
    let numero = 0;     // the step drawn: what an older one gets back comes too late
    const zone = el('div', { class: 'etape-connexion' });
    const message = el('p', { class: 'message-compte', 'aria-live': 'polite' });
    const autres = el('div', { class: 'autres-connexions' });
    const d = dialogue({ titre: t('connexion_titre'), contenu: el('div', { class: 'dialogue-compte' }, zone, message, autres),
      boutons: [{ libelle: t('annuler'), action: () => true }],
      surFermeture: () => {
        numero += 1;
        if (h && !fini) c.annulerConnexion(h).catch(() => {});
        resolve(fini ? { ok: true, compte: fini } : { ok: false });
      } });
    const titre = (x) => { d.boite.querySelector('.tete-dialogue h2').textContent = x; };
    const pied = (boutons) => d.boite.querySelector('.pied-dialogue').replaceChildren(...boutons.map((b) => el('button', {
      class: b.principal ? 'principal' : '', onclick: () => { if (b.action() !== false) d.fermer(); } }, b.libelle)));
    const dire = (texte, genre = '') => { message.className = `message-compte ${genre}`; message.textContent = texte || ''; };
    // a new step: its content, and the steps before it stop
    const etape = (...enfants) => { numero += 1; remplir(zone, ...enfants); return numero; };
    const attente = (texte) => el('p', { class: 'attente-connexion' }, el('span', { class: 'point' }), texte);

    // GitHub, GitLab, or the institution's GitLab by its name
    function choisir() {
      const serveurAutre = el('input', { placeholder: 'gitlab.example.org', spellcheck: 'false', autocomplete: 'off' });
      const continuer = () => {
        const v = serveurAutre.value.trim().replace(/^https?:\/\//i, '').replace(/\/.*$/, '').toLowerCase();
        if (!v) { dire(t('connexion_autre_vide'), 'erreur'); serveurAutre.focus(); return; }
        serveurChoisi(v);
      };
      serveurAutre.addEventListener('keydown', (ev) => { if (ev.key === 'Enter') continuer(); });
      const github = el('button', { onclick: () => serveurChoisi('github.com') }, 'GitHub');
      etape(el('p', {}, t('connexion_choix')),
        el('div', { class: 'services-connexion' }, github, el('button', { onclick: () => serveurChoisi('gitlab.com') }, 'GitLab')),
        el('div', { class: 'ligne autre-gitlab' }, el('label', {}, t('connexion_autre_gitlab')), serveurAutre, el('button', { onclick: continuer }, t('connexion_continuer'))));
      github.focus();
    }

    // A server: its own sign-in when Nuée is set up there, else why not; and the other ways
    async function serveurChoisi(x) {
      dire('');
      const n = etape(attente(t('connexion_preparation')));
      try { s = await c.serveur(x); } catch (e) { if (n === numero) { etape(); dire(e.message, 'erreur'); } return; }
      if (n !== numero) return;
      h = s.hote;
      titre(t('connexion_titre_service', { service: nomService(h) }));
      dessinerAutres();
      if (s.connexion) { commencer(); return; }
      if (s.reglable) { demanderId(); return; }
      etape(el('p', {}, t('connexion_pas_prete', { service: nomService(h) })));
      if (!s.git) basculerJeton(true);
    }

    // A self-hosted GitLab: the application ID its administrator gives, kept for next time
    function demanderId() {
      const champ = el('input', { spellcheck: 'false', autocomplete: 'off', style: 'flex:1' });
      const valider = async () => {
        dire('');
        try { s = await c.reglerServeur(h, champ.value); } catch (e) { dire(e.message, 'erreur'); return; }
        if (s.connexion) commencer();
      };
      champ.addEventListener('keydown', (ev) => { if (ev.key === 'Enter') valider(); });
      etape(el('p', {}, t('connexion_pas_enregistre', { hote: h })),
        el('div', { class: 'ligne' }, el('label', {}, t('connexion_id')), champ, el('button', { class: 'principal', onclick: valider }, t('connexion_id_bouton'))));
      champ.focus();
    }

    // The server's sign-in: a code, its page, the wait for the approval
    async function commencer() {
      const service = nomService(h);
      dire('');
      const n = etape(attente(t('connexion_demande', { service })));
      pied([{ libelle: t('annuler'), action: () => true }]);
      let r;
      try { r = await c.commencerConnexion(h); } catch (e) {
        if (n !== numero) return;
        etape(el('p', { class: 'erreur' }, e.message));
        if (!s.git) basculerJeton(true);
        return;
      }
      if (n !== numero) { c.annulerConnexion(h).catch(() => {}); return; }
      montrerCode(r);
      let fin;
      try { fin = await c.attendreConnexion(h); } catch (e) {
        if (n === numero) etape(el('p', { class: 'erreur' }, e.message), boutonCode());
        return;
      }
      if (n !== numero || fin.raison === 'annulee') return;
      if (fin.ok) { reussi(fin.compte); return; }
      etape(el('p', {}, t(fin.raison === 'refusee' ? 'connexion_refusee' : 'connexion_expiree', { service })), boutonCode());
    }
    const boutonCode = () => el('div', { class: 'ligne' }, el('button', { class: 'principal', onclick: commencer }, icone('refresh-cw'), t('connexion_nouveau_code')));

    function montrerCode(r) {
      const service = nomService(h);
      const heure = new Date(r.expire).toLocaleTimeString(langueActuelle() === 'fr' ? 'fr-FR' : 'en-GB', { hour: '2-digit', minute: '2-digit' });
      const copier = () => navigator.clipboard.writeText(r.code).then(() => true, () => false);
      const ouvrir = el('button', { class: 'principal', onclick: async () => {
        const copie = await copier();
        try { await c.ouvrirPage(h); dire(t(copie ? 'connexion_ouverte' : 'connexion_ouverte_seule', { service })); } catch (e) { dire(e.message, 'erreur'); }
      } }, icone('external-link'), t('connexion_ouvrir', { service }));
      zone.replaceChildren(
        el('p', {}, t('connexion_code_texte', { service })),
        el('div', { class: 'code-appareil' }, el('code', {}, r.code),
          el('button', { class: 'icone-seule', title: t('connexion_copier'), 'aria-label': t('connexion_copier'),
            onclick: async () => dire(t((await copier()) ? 'connexion_copie' : 'connexion_copie_impossible')) }, icone('copy'))),
        el('div', { class: 'ouvrir-connexion' }, ouvrir, el('span', { class: 'adresse-connexion' }, r.adresse.replace(/^https:\/\//, ''))),
        attente([t('connexion_attente', { service }), ' ', el('span', { class: 'doux' }, t('connexion_jusqua', { heure }))]));
      ouvrir.focus();
    }

    function reussi(compte) {
      const service = nomService(h);
      fini = compte;
      numero += 1;
      dire('');
      autres.replaceChildren();
      remplir(zone, el('p', { class: 'ok-compte' }, icone('circle-check'), t('connexion_reussie', { service })), carteCompte(h, compte),
        compte.limite ? el('p', { class: 'note' }, t('connexion_limitee', { service })) : null,
        compte.session ? el('p', { class: 'note' }, t('connexion_session')) : null);
      pied([{ libelle: t('connexion_terminer'), principal: true, action: () => true }]);
      d.boite.querySelector('.pied-dialogue .principal').focus();
      app.statut(compte.login ? t('connexion_faite', { service, login: `@${compte.login}` }) : t('connexion_reussie', { service }));
    }

    // Other ways: this computer's git sign-in, a token
    const jeton = el('div', { class: 'jeton-connexion' });
    function dessinerAutres() {
      jeton.replaceChildren();
      remplir(autres, el('h3', {}, t('connexion_autres')),
        s.git ? el('div', { class: 'ligne' }, el('button', { onclick: parGit }, icone('monitor'), t('connexion_git')), el('span', { class: 'note' }, t('connexion_git_aide'))) : null,
        el('div', {}, el('a', { href: '#', onclick: (ev) => { ev.preventDefault(); basculerJeton(); } }, icone('key-round'), t('connexion_jeton_lien'))), jeton);
    }

    async function parGit() {
      const service = nomService(h);
      await c.annulerConnexion(h).catch(() => {});  // (a code under way is no longer needed)
      dire('');
      const n = etape(attente(t('connexion_git_attente')));
      pied([{ libelle: t('annuler'), action: () => true }]);
      let r;
      try { r = await c.compteGit(h); } catch (e) { if (n === numero) etape(el('p', { class: 'erreur' }, e.message), retour()); return; }
      if (n !== numero || r.raison === 'annulee') return;
      if (r.ok) { reussi(r.compte); return; }
      etape(el('p', {}, t(`connexion_git_${r.raison}`, { service })), retour());
    }
    // (back to the server's own sign-in, when it has one)
    const retour = () => (s.connexion ? el('div', { class: 'ligne' }, el('button', { class: 'principal', onclick: commencer }, t('connexion_avec', { service: nomService(h) }))) : null);

    function basculerJeton(ouvrir = !jeton.childElementCount) {
      if (!ouvrir) { jeton.replaceChildren(); return; }
      const service = nomService(h);
      const champ = el('input', { type: 'password', autocomplete: 'off', spellcheck: 'false', style: 'flex:1' });
      const verifier = async () => {
        if (!champ.value.trim()) { champ.focus(); return; }
        dire(t('connexion_jeton_verification'), 'doux');
        try {
          const k = await c.utiliserJeton(h, champ.value);
          champ.value = '';
          c.annulerConnexion(h).catch(() => {});
          reussi(k);
        } catch (e) { dire(e.message, 'erreur'); }
      };
      champ.addEventListener('keydown', (ev) => { if (ev.key === 'Enter') verifier(); });
      jeton.replaceChildren(
        el('p', {}, t('connexion_jeton_texte', { service }), ' ', el('a', { href: pageJeton(h), target: '_blank', rel: 'noopener' }, icone('external-link'),
          t('connexion_jeton_creer', { service })), ' ', t(h === 'github.com' ? 'connexion_jeton_droits_github' : 'connexion_jeton_droits_gitlab')),
        el('div', { class: 'ligne' }, el('label', {}, t('connexion_jeton')), champ, el('button', { onclick: verifier }, t('connexion_jeton_bouton'))),
        el('p', { class: 'note' }, t('connexion_jeton_garde')));
      champ.focus();
    }

    if (hote) serveurChoisi(hote); else choisir();
  });
}

// ------------------------------------------------------------ inviting a colleague

export async function ouvrirInvitation(app, { hote, url } = {}) {
  const c = comptes();
  if (!c) { app.statut(t('connexion_navigateur'), true); return { ok: false }; }
  let k = null;
  try { k = await c.compte(hote); } catch { /* the server's refusal comes with the invitation */ }
  if (!k || (!k.connecte && !k.horsLigne)) {
    if (!(await ouvrirConnexion(app, { hote })).ok) return { ok: false };
  }
  const service = nomService(hote);
  return new Promise((resolve) => {
    const invites = [];
    const champ = el('input', { spellcheck: 'false', autocomplete: 'off', placeholder: t('invitation_exemple'), style: 'width:100%' });
    const message = el('p', { class: 'message-compte', 'aria-live': 'polite' });
    const liste = el('ul', { class: 'invites-compte' });
    const dire = (texte, genre = '') => { message.className = `message-compte ${genre}`; message.textContent = texte; };
    let enCours = false;
    const inviter = async () => {
      if (enCours) return;
      if (!champ.value.trim()) { dire(t('invitation_vide'), 'erreur'); champ.focus(); return; }
      enCours = true;
      dire(t('invitation_en_cours'), 'doux');
      try {
        const r = await c.inviter(hote, url, champ.value);
        const qui = r.nom ? `${r.nom} (@${r.login})` : `@${r.login}`;
        dire(t(`invitation_${r.resultat}`, { qui }), 'ok-compte');
        if (r.resultat !== 'deja') { invites.push(r); liste.append(el('li', {}, icone('check'), qui)); }
        champ.value = '';
        champ.focus();
      } catch (e) { dire(e.message, 'erreur'); } finally { enCours = false; }
    };
    champ.addEventListener('keydown', (ev) => { if (ev.key === 'Enter') inviter(); });
    dialogue({ titre: t('invitation_titre'),
      contenu: el('div', { class: 'dialogue-compte' },
        el('p', {}, t(hote === 'github.com' ? 'invitation_texte_github' : 'invitation_texte_gitlab', { depot: nomDuDepot(url), service })),
        el('label', { class: 'etiquette-compte' }, t('invitation_utilisateur', { service })), champ,
        el('p', { class: 'note' }, t('invitation_aide', { exemple: `https://${hote}/${t('invitation_exemple')}` })),
        message, liste),
      boutons: [{ libelle: t('fermer'), action: () => true }, { libelle: t('invitation_bouton'), principal: true, action: () => { inviter(); return false; } }],
      surFermeture: () => resolve({ ok: invites.length > 0, invites }) });
  });
}

// ------------------------------------------------------------ Help's temporary entry

// The accounts on GitHub, GitLab and another GitLab, with Sign in, Sign out, a new repository named
// after the project in front and Invite a colleague to it: to try the dialogs until the History and
// sharing screens call them (then this goes, with its menu entry)
export function dialogueComptesEssai(app) {
  const c = comptes();
  if (!c) { app.statut(t('connexion_navigateur'), true); return; }
  const lignes = el('div', { class: 'comptes-essai' });
  const autre = el('input', { placeholder: 'gitlab.example.org', spellcheck: 'false', autocomplete: 'off', style: 'width:14em' });
  const depot = el('input', { type: 'url', placeholder: 'https://github.com/…', spellcheck: 'false', autocomplete: 'off', style: 'flex:1' });
  const message = el('p', { class: 'message-compte', 'aria-live': 'polite' });
  const dire = (texte, genre = '') => { message.className = `message-compte ${genre}`; message.textContent = texte; };
  const nomProjet = () => { const p = app.projet(app.projetActif()); return (p && p.nom) || 'Nuée'; };
  async function dessiner() {
    const hotes = ['github.com', 'gitlab.com', autre.value.trim().toLowerCase()].filter(Boolean);
    const rangs = [];
    for (const h of hotes) {
      let k;
      try { k = await c.compte(h); } catch (e) { k = { connecte: false, erreur: e.message }; }
      rangs.push(ligne(h, k));
    }
    lignes.replaceChildren(...rangs);
  }
  function ligne(h, k) {
    const service = nomService(h);
    const actions = k.connecte ? [
      el('button', { onclick: async () => {
        try { const r = await c.creerDepot(h, nomProjet(), { prive: true }); depot.value = r.url; dire(t('comptes_depot_cree', { url: r.url }), 'ok-compte'); } catch (e) { dire(e.message, 'erreur'); }
      } }, icone('plus'), t('comptes_creer_depot', { nom: nomProjet() })),
      el('button', { onclick: () => {
        if (!depot.value.trim()) { dire(t('comptes_depot_manque'), 'erreur'); depot.focus(); return; }
        ouvrirInvitation(app, { hote: h, url: depot.value.trim() });
      } }, icone('user-plus'), t('comptes_inviter')),
      el('button', { onclick: async () => { await c.deconnecter(h).catch((e) => dire(e.message, 'erreur')); dire(t('comptes_deconnecte', { service })); dessiner(); } },
        icone('log-out'), t('comptes_deconnecter'))]
      : [el('button', { class: 'principal', onclick: async () => { if ((await ouvrirConnexion(app, { hote: h })).ok) dessiner(); } }, icone('log-in'), t('comptes_connecter'))];
    return el('div', { class: 'ligne-compte' },
      k.connecte ? carteCompte(h, k) : el('div', { class: 'carte-compte' }, el('span', { class: 'avatar-compte vide' }, icone('circle-user-round')),
        el('div', { class: 'texte-compte' }, el('b', {}, service), el('div', { class: 'doux' }, k.erreur || t(k.refuse ? 'comptes_refuse' : 'comptes_non_connecte')))),
      el('div', { class: 'actions-compte' }, actions));
  }
  autre.addEventListener('keydown', (ev) => { if (ev.key === 'Enter') dessiner(); });
  dialogue({ titre: t('comptes_essai').replace('…', ''),
    contenu: el('div', { class: 'dialogue-compte' }, el('p', { class: 'note' }, t('comptes_essai_note')),
      el('div', {}, el('button', { onclick: async () => { if ((await ouvrirConnexion(app)).ok) dessiner(); } }, icone('log-in'), t('comptes_choisir'))), lignes,
      el('div', { class: 'ligne' }, el('label', {}, t('comptes_autre')), autre, el('button', { onclick: dessiner }, t('comptes_afficher'))),
      el('div', { class: 'ligne' }, el('label', {}, t('comptes_depot')), depot), message),
    boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
  dessiner();
}

// ------------------------------------------------------------ strings

ajouterTextes({
  connexion_titre: 'Se connecter', connexion_titre_service: 'Se connecter à {service}',
  connexion_navigateur: 'La connexion à GitHub ou GitLab se fait dans l’application de bureau.',
  connexion_choix: 'Connectez-vous pour garder les versions de vos projets en ligne, les partager et inviter des collègues.',
  connexion_autre_gitlab: 'Le GitLab de votre institution', connexion_autre_vide: 'Tapez le nom de son serveur, comme gitlab.example.org.', connexion_continuer: 'Continuer',
  connexion_preparation: 'Un instant…', connexion_demande: 'Demande d’un code à {service}…',
  connexion_code_texte: 'Tapez ce code sur la page de {service}, puis autorisez Nuée :',
  connexion_copier: 'Copier le code', connexion_copie: 'Code copié.', connexion_copie_impossible: 'Le code n’a pas pu être copié : tapez-le.',
  connexion_ouvrir: 'Ouvrir la page de {service}', connexion_ouverte: 'La page est ouverte dans votre navigateur, et le code copié : collez-le là-bas.',
  connexion_ouverte_seule: 'La page est ouverte dans votre navigateur : tapez-y le code.',
  connexion_attente: 'En attente de votre accord sur {service}…', connexion_jusqua: 'Le code reste valable jusqu’à {heure}.',
  connexion_refusee: 'Nuée n’a pas été autorisé sur la page de {service}.', connexion_expiree: 'Le code a expiré avant d’être tapé.',
  connexion_nouveau_code: 'Obtenir un nouveau code',
  connexion_reussie: 'Connecté à {service}.', connexion_faite: 'Connecté à {service} : {login}.',
  connexion_source_connexion: 'connexion {service}', connexion_source_git: 'connexion git de cet ordinateur', connexion_source_jeton: 'avec un jeton',
  connexion_session: 'Cet ordinateur ne peut pas garder la connexion : vous vous reconnecterez au prochain lancement de Nuée.',
  connexion_limitee: 'Cette connexion permet d’envoyer et de recevoir des versions, mais pas de créer un dépôt ni d’inviter des collègues : connectez-vous avec {service} pour cela.',
  connexion_terminer: 'Terminé',
  connexion_pas_prete: 'La connexion à {service} n’est pas encore prête dans cette copie de Nuée.',
  connexion_pas_enregistre: 'Nuée se connecte à {hote} une fois que son administrateur l’y a enregistré : il vous donne alors son identifiant d’application.',
  connexion_id: 'Identifiant d’application', connexion_id_bouton: 'Se connecter',
  connexion_autres: 'Autres façons de se connecter', connexion_avec: 'Se connecter avec {service}',
  connexion_git: 'Utiliser la connexion git de cet ordinateur', connexion_git_aide: 'git est installé ici : Nuée peut se servir de la connexion qu’il garde.',
  connexion_git_attente: 'Demande à git, sur cet ordinateur… Si une fenêtre de Git Credential Manager s’ouvre, connectez-vous dans celle-ci.',
  connexion_git_aucun: 'Le git de cet ordinateur n’a pas de connexion pour {service}, ou elle a été annulée.',
  connexion_git_refuse: '{service} refuse la connexion git de cet ordinateur : elle est peut-être périmée.',
  connexion_git_delai: 'Le git de cet ordinateur n’a pas répondu à temps.', connexion_git_absent: 'git n’est pas installé sur cet ordinateur.',
  connexion_jeton_lien: 'Utiliser plutôt un jeton', connexion_jeton_texte: 'Un jeton, créé sur le site de {service}, permet à Nuée d’agir en votre nom.',
  connexion_jeton_creer: 'En créer un sur {service}', connexion_jeton_droits_github: '(portée « repo »)', connexion_jeton_droits_gitlab: '(portées api, read_user et write_repository)',
  connexion_jeton: 'Jeton', connexion_jeton_bouton: 'Vérifier et garder', connexion_jeton_verification: 'Vérification du jeton…',
  connexion_jeton_garde: 'Gardé dans le trousseau de l’ordinateur, il n’est plus jamais montré.',
  invitation_titre: 'Inviter un collègue',
  invitation_texte_github: 'GitHub envoie à votre collègue une invitation à {depot}. Une fois l’invitation acceptée, votre collègue peut envoyer et recevoir les versions du projet.',
  invitation_texte_gitlab: 'Votre collègue devient membre de {depot} sur {service} (rôle Developer), et peut alors envoyer et recevoir les versions du projet.',
  invitation_utilisateur: 'Son nom d’utilisateur sur {service}', invitation_exemple: 'alice-martin', invitation_aide: 'Tel qu’il est dans l’adresse de son profil : {exemple}.',
  invitation_bouton: 'Inviter', invitation_en_cours: 'Invitation…', invitation_vide: 'Tapez le nom d’utilisateur de votre collègue.',
  invitation_invite: 'Invitation envoyée à {qui}, à accepter depuis le courriel reçu ou les notifications de GitHub.',
  invitation_ajoute: '{qui} est maintenant membre du projet (Developer).', invitation_deja: '{qui} a déjà accès à ce dépôt.',
  comptes_essai: 'Comptes GitHub et GitLab (essai)…',
  comptes_essai_note: 'Provisoire : pour essayer la connexion, la création d’un dépôt et l’invitation, en attendant les écrans Historique et Partage.',
  comptes_non_connecte: 'Non connecté', comptes_refuse: 'la connexion ne marche plus : reconnectez-vous', comptes_hors_ligne: '{service} injoignable',
  comptes_connecter: 'Se connecter…', comptes_deconnecter: 'Se déconnecter', comptes_deconnecte: 'Déconnecté de {service}.',
  comptes_creer_depot: 'Nouveau dépôt « {nom} »', comptes_depot_cree: 'Dépôt créé : {url}', comptes_depot_manque: 'Donnez d’abord l’adresse du dépôt, ou créez-en un.',
  comptes_inviter: 'Inviter un collègue…', comptes_autre: 'Un autre GitLab', comptes_afficher: 'Afficher', comptes_depot: 'Dépôt',
  comptes_choisir: 'Se connecter, en choisissant le service…',
}, {
  connexion_titre: 'Sign in', connexion_titre_service: 'Sign in to {service}',
  connexion_navigateur: 'Signing in to GitHub or GitLab works in the desktop app.',
  connexion_choix: 'Sign in to keep your projects’ versions online, share them and invite colleagues.',
  connexion_autre_gitlab: 'Your institution’s GitLab', connexion_autre_vide: 'Type its server’s name, such as gitlab.example.org.', connexion_continuer: 'Continue',
  connexion_preparation: 'One moment…', connexion_demande: 'Asking {service} for a code…',
  connexion_code_texte: 'Type this code on {service}’s page, then approve Nuée:',
  connexion_copier: 'Copy the code', connexion_copie: 'Code copied.', connexion_copie_impossible: 'The code couldn’t be copied: type it.',
  connexion_ouvrir: 'Open {service}’s page', connexion_ouverte: 'The page is open in your browser, and the code copied: paste it there.',
  connexion_ouverte_seule: 'The page is open in your browser: type the code there.',
  connexion_attente: 'Waiting for your approval on {service}…', connexion_jusqua: 'The code works until {heure}.',
  connexion_refusee: 'Nuée wasn’t approved on {service}’s page.', connexion_expiree: 'The code expired before it was typed.',
  connexion_nouveau_code: 'Get a new code',
  connexion_reussie: 'Signed in to {service}.', connexion_faite: 'Signed in to {service} as {login}.',
  connexion_source_connexion: '{service} sign-in', connexion_source_git: 'this computer’s git sign-in', connexion_source_jeton: 'with a token',
  connexion_session: 'This computer can’t keep the sign-in: you will sign in again the next time Nuée starts.',
  connexion_limitee: 'This sign-in may send and receive versions, but not create repositories nor invite colleagues: sign in with {service} for that.',
  connexion_terminer: 'Done',
  connexion_pas_prete: 'Signing in with {service} isn’t set up in this copy of Nuée yet.',
  connexion_pas_enregistre: 'Nuée signs in to {hote} once its administrator has registered it there: they then give you its application ID.',
  connexion_id: 'Application ID', connexion_id_bouton: 'Sign in',
  connexion_autres: 'Other ways to sign in', connexion_avec: 'Sign in with {service}',
  connexion_git: 'Use this computer’s git sign-in', connexion_git_aide: 'git is installed here: Nuée can use the sign-in it keeps.',
  connexion_git_attente: 'Asking git, on this computer… If a Git Credential Manager window opens, sign in there.',
  connexion_git_aucun: 'This computer’s git has no sign-in for {service}, or it was cancelled.',
  connexion_git_refuse: '{service} refuses this computer’s git sign-in: it may be out of date.',
  connexion_git_delai: 'This computer’s git didn’t answer in time.', connexion_git_absent: 'git isn’t installed on this computer.',
  connexion_jeton_lien: 'Use a token instead', connexion_jeton_texte: 'A token, made on {service}’s site, lets Nuée act on your behalf.',
  connexion_jeton_creer: 'Make one on {service}', connexion_jeton_droits_github: '(scope “repo”)', connexion_jeton_droits_gitlab: '(scopes api, read_user and write_repository)',
  connexion_jeton: 'Token', connexion_jeton_bouton: 'Check and keep', connexion_jeton_verification: 'Checking the token…',
  connexion_jeton_garde: 'Kept in the computer’s keychain, it is never shown again.',
  invitation_titre: 'Invite a colleague',
  invitation_texte_github: 'GitHub sends your colleague an invitation to {depot}. Once they accept it, they can send and receive the project’s versions.',
  invitation_texte_gitlab: 'Your colleague becomes a member of {depot} on {service} (as a Developer), and can then send and receive the project’s versions.',
  invitation_utilisateur: 'Their user name on {service}', invitation_exemple: 'alice-martin', invitation_aide: 'As it is in the address of their profile: {exemple}.',
  invitation_bouton: 'Invite', invitation_en_cours: 'Inviting…', invitation_vide: 'Type your colleague’s user name.',
  invitation_invite: 'Invitation sent to {qui}: they accept it from the email they get, or their GitHub notifications.',
  invitation_ajoute: '{qui} is now a member of the project (Developer).', invitation_deja: '{qui} already has access to this repository.',
  comptes_essai: 'GitHub and GitLab accounts (test)…',
  comptes_essai_note: 'Temporary: to try signing in, creating a repository and inviting, until the History and sharing screens offer them.',
  comptes_non_connecte: 'Not signed in', comptes_refuse: 'the sign-in no longer works: sign in again', comptes_hors_ligne: '{service} not reached',
  comptes_connecter: 'Sign in…', comptes_deconnecter: 'Sign out', comptes_deconnecte: 'Signed out of {service}.',
  comptes_creer_depot: 'New repository “{nom}”', comptes_depot_cree: 'Repository created: {url}', comptes_depot_manque: 'Give the repository’s address first, or create one.',
  comptes_inviter: 'Invite a colleague…', comptes_autre: 'Another GitLab', comptes_afficher: 'Show', comptes_depot: 'Repository',
  comptes_choisir: 'Sign in, choosing the service…',
});
