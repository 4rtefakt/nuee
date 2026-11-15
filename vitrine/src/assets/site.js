// nuee.io: the swarm, the figures it lands in, and the page's small machines (Which analysis?,
// the R console, the Methods sheet, the downloads). No library; the page works without it, still.
// The data come with the page (construire.mjs): the swarm's shapes are computed by Nuée's engine
// (donnees.R), and Which analysis? runs the application's own table of rules.

const D = JSON.parse(document.getElementById('donnees').textContent);
const T = D.t;
const calme = matchMedia('(prefers-reduced-motion: reduce)').matches;
const $ = (s, racine = document) => racine.querySelector(s);
const $$ = (s, racine = document) => [...racine.querySelectorAll(s)];
const el = (nom, attributs = {}, ...enfants) => {
  const e = document.createElement(nom);
  for (const [k, v] of Object.entries(attributs)) {
    if (v === null || v === undefined || v === false) continue;
    if (k === 'class') e.className = v; else if (k.startsWith('on')) e.addEventListener(k.slice(2), v); else e.setAttribute(k, v === true ? '' : v);
  }
  e.append(...enfants.flat().filter((x) => x !== null && x !== undefined && x !== false));
  return e;
};
document.documentElement.classList.add('js');

// ================================================================ the shapes

// The application's plot colours (site.css: --c0 to --c6), then the ink of a bird in flight.
const COULEURS = ['#2f6fb5', '#d1495b', '#3b9a5b', '#e09f3e', '#7b5ea7', '#2a9d8f', '#9aa0ae', '#16233f'];
const ENCRE = 7;
// The width of a bird's stroke: on a line, in flight, as a point of a plot.
const TRAITS = [2.9, 3.2, 5.4];

// Nuée's logo as a shape: its seventeen points and its ellipse (app/interface/nuee.svg).
const LOGO = (() => {
  const unite = (x, y) => [(x - 40) / 944, 1 - (y - 40) / 944];
  const points = [[368, 634], [621, 416], [616, 569], [488, 509], [366, 493], [419, 591], [570, 368], [500, 590], [700, 480], [630, 503], [711, 381],
    [572, 616], [436, 418], [441, 673], [331, 555], [506, 659], [533, 451]].map(([x, y]) => [...unite(x, y), 0]);
  const a = -28 * Math.PI / 180;
  const ellipse = Array.from({ length: 97 }, (_, k) => {
    const u = k / 96 * 2 * Math.PI;
    const x = 340 * Math.cos(u);
    const y = 186 * Math.sin(u);
    return unite(512 + x * Math.cos(a) - y * Math.sin(a), 522 + x * Math.sin(a) + y * Math.cos(a));
  });
  return { points, lignes: [{ g: 0, pts: ellipse }], part_lignes: 0.5 };
})();
const FORMES = { ...D.formes, logo: LOGO };

// Where n birds go to draw a shape: on its points in turn (several birds on a point when there are
// more birds than points), and evenly along its lines. Sorted from left to right, so that two
// shapes exchange birds without the whole swarm crossing itself.
function cibles(forme, n) {
  const P = forme.points || [];
  const L = forme.lignes || [];
  const nL = !L.length ? 0 : !P.length ? n : Math.round(n * (forme.part_lignes ?? 0.5));
  const nP = n - nL;
  const c = [];
  for (let i = 0; i < nP; i++) { const p = P[i % P.length]; c.push([p[0], p[1], p[2], 2, Math.random()]); }
  if (nL) {
    const segments = [];
    let total = 0;
    for (const l of L) {
      for (let j = 1; j < l.pts.length; j++) {
        const a = l.pts[j - 1];
        const b = l.pts[j];
        const d = Math.hypot(b[0] - a[0], b[1] - a[1]);
        if (d > 0) { segments.push([a, b, d, l.g]); total += d; }
      }
    }
    let k = 0;
    let avant = 0;
    for (let i = 0; i < nL; i++) {
      const s = (i + 0.5) * total / nL;
      while (k < segments.length - 1 && avant + segments[k][2] < s) { avant += segments[k][2]; k++; }
      const [a, b, d, g] = segments[k];
      const u = Math.min(1, (s - avant) / d);
      c.push([a[0] + (b[0] - a[0]) * u, a[1] + (b[1] - a[1]) * u, g, 0, Math.random()]);
    }
  }
  c.sort((a, b) => (a[0] + 0.12 * a[4]) - (b[0] + 0.12 * b[4]));
  return c;
}

// ================================================================ the swarm

// Birds on a canvas that covers the window. They fly within an element (voler), boids that flee
// the pointer, or land on a shape drawn in an element (former), each pulled to its place by a
// spring. The element is read at every frame: the swarm follows it as the page scrolls.
class Nuee {
  constructor(toile) {
    this.toile = toile;
    this.g = toile.getContext('2d');
    const n = this.n = innerWidth < 720 ? 420 : 540;
    for (const k of ['x', 'y', 'vx', 'vy', 'tx', 'ty', 'attente', 'phase']) this[k] = new Float32Array(n);
    for (const k of ['etat', 'coul', 'trait', 'cc', 'ct']) this[k] = new Uint8Array(n);
    this.coul.fill(ENCRE);
    this.trait.fill(1);
    for (let i = 0; i < n; i++) this.phase[i] = Math.random() * 6.283;
    this.mode = null;
    this.zone = null;
    this.forme = null;
    this.temps = 0;
    this.pointeur = { x: -9999, y: -9999, vu: -1e9 };
    this.image = 0;
    this.lent = 0;
    this.gardees = new Map();
    this.tailler();
    addEventListener('resize', () => { this.tailler(); this.reveiller(); });
    addEventListener('pointermove', (e) => { this.pointeur = { x: e.clientX, y: e.clientY, vu: performance.now() }; }, { passive: true });
    addEventListener('pointerleave', () => { this.pointeur.vu = -1e9; });
    document.addEventListener('visibilitychange', () => this.reveiller());
  }

  tailler() {
    this.L = innerWidth;
    this.H = innerHeight;
    this.densite = Math.min(devicePixelRatio || 1, 2);
    this.toile.width = Math.round(this.L * this.densite);
    this.toile.height = Math.round(this.H * this.densite);
    this.echelle = this.L < 720 ? 0.8 : 1;
  }

  // The birds for the first time: scattered in the element, flying.
  semer(zone) {
    const r = zone.getBoundingClientRect();
    for (let i = 0; i < this.n; i++) {
      this.x[i] = r.left + Math.random() * r.width;
      this.y[i] = r.top + Math.random() * r.height;
      const a = Math.random() * 6.283;
      this.vx[i] = 3 * Math.cos(a);
      this.vy[i] = 3 * Math.sin(a);
    }
  }

  // Birds far out of the window come back to its edge: they are in the picture at once.
  rapprocher() {
    for (let i = 0; i < this.n; i++) {
      if (this.y[i] < -80) this.y[i] = -30 - Math.random() * 50;
      else if (this.y[i] > this.H + 80) this.y[i] = this.H + 30 + Math.random() * 50;
      if (this.x[i] < -80) this.x[i] = -30 - Math.random() * 50;
      else if (this.x[i] > this.L + 80) this.x[i] = this.L + 30 + Math.random() * 50;
    }
  }

  voler(zone) {
    if (this.mode === 'vol' && this.zone === zone) return;
    const dejaLa = this.zone === zone;
    this.mode = 'vol';
    this.zone = zone;
    this.forme = null;
    if (!dejaLa) this.rapprocher();
    // they take off one after the other, from the left
    for (let i = 0; i < this.n; i++) this.attente[i] = this.etat[i] === 1 ? 0.05 + 0.5 * this.tx[i] + 0.25 * Math.random() : 0;
    this.reveiller();
  }

  former(id, zone) {
    if (this.mode === 'forme' && this.forme === id && this.zone === zone) return;
    const dejaLa = this.zone === zone;
    this.mode = 'forme';
    this.zone = zone;
    this.forme = id;
    if (!dejaLa) this.rapprocher();
    if (!this.gardees.has(id)) this.gardees.set(id, cibles(FORMES[id], this.n));
    const c = this.gardees.get(id);
    for (let i = 0; i < this.n; i++) {
      this.tx[i] = c[i][0];
      this.ty[i] = c[i][1];
      this.cc[i] = c[i][2];
      this.ct[i] = c[i][3];
      this.attente[i] = 0.02 + 0.4 * c[i][0] + 0.22 * Math.random();
    }
    this.reveiller();
  }

  // A bird's wait is over: it takes its new part.
  changer(i) {
    if (this.mode === 'forme') {
      this.etat[i] = 1;
      this.coul[i] = this.cc[i];
      this.trait[i] = this.ct[i];
    } else {
      if (this.etat[i] === 1) {  // take-off: upwards, mostly
        const a = -Math.PI / 2 + (Math.random() - 0.5) * 2.4;
        this.vx[i] = 3.4 * Math.cos(a);
        this.vy[i] = 3.4 * Math.sin(a);
      }
      this.etat[i] = 0;
      this.coul[i] = ENCRE;
      this.trait[i] = 1;
    }
  }

  // Without motion: every bird at its place, once.
  poser() {
    const r = this.zone.getBoundingClientRect();
    for (let i = 0; i < this.n; i++) {
      this.attente[i] = 0;
      if (this.mode === 'forme') {
        this.changer(i);
        this.x[i] = r.left + this.tx[i] * r.width;
        this.y[i] = r.top + (1 - this.ty[i]) * r.height;
        this.vx[i] = this.vy[i] = 0;
      }
    }
    this.dessiner();
  }

  visible() {
    if (!this.zone || document.hidden) return false;
    const r = this.zone.getBoundingClientRect();
    return r.bottom > -160 && r.top < this.H + 160;
  }

  reveiller() {
    if (calme) { if (this.zone) this.poser(); return; }
    if (this.image || !this.visible()) return;
    this.dernier = performance.now();
    this.image = requestAnimationFrame((t) => this.tourner(t));
  }

  tourner(t) {
    const dt = Math.min(t - this.dernier, 50);
    this.dernier = t;
    this.avancer(dt);
    this.dessiner();
    // a computer that can't keep up gets half the birds
    this.lent = dt > 30 ? this.lent + 1 : Math.max(0, this.lent - 1);
    if (this.lent > 45 && this.n > 140) { this.n = Math.floor(this.n / 2); this.lent = 0; this.gardees.clear(); if (this.forme) { const f = this.forme; this.forme = null; this.former(f, this.zone); } }
    if (this.visible()) this.image = requestAnimationFrame((u) => this.tourner(u));
    else { this.image = 0; this.g.clearRect(0, 0, this.toile.width, this.toile.height); }
  }

  avancer(dt) {
    const f = dt / 16.667;
    const s = dt / 1000;
    this.temps += s;
    const { n, x, y, vx, vy, etat, attente } = this;
    const r = this.zone.getBoundingClientRect();
    const chasse = performance.now() - this.pointeur.vu < 2500;
    const px = this.pointeur.x;
    const py = this.pointeur.y;

    // ---- the neighbours of the birds in flight, by a grid
    const R = 58 * this.echelle;
    const colonnes = Math.ceil(this.L / R) + 2;
    const lignes = Math.ceil(this.H / R) + 2;
    if (!this.tete || this.tete.length !== colonnes * lignes) this.tete = new Int32Array(colonnes * lignes);
    if (!this.suivant || this.suivant.length !== n) { this.suivant = new Int32Array(n); this.caseDe = new Int32Array(n); }
    const { tete, suivant, caseDe } = this;
    tete.fill(-1);
    for (let i = 0; i < n; i++) {
      if (etat[i] !== 0) { caseDe[i] = -1; continue; }
      const cx = Math.min(colonnes - 1, Math.max(0, Math.floor(x[i] / R) + 1));
      const cy = Math.min(lignes - 1, Math.max(0, Math.floor(y[i] / R) + 1));
      const k = caseDe[i] = cy * colonnes + cx;
      suivant[i] = tete[k];
      tete[k] = i;
    }

    // the point the swarm drifts towards: it wanders over the element, so the swarm sweeps it
    const but = { x: r.left + r.width * (0.5 + 0.36 * Math.sin(this.temps * 0.31) + 0.08 * Math.sin(this.temps * 0.83 + 1)),
      y: r.top + r.height * (0.5 + 0.3 * Math.sin(this.temps * 0.43 + 2) + 0.07 * Math.cos(this.temps * 0.97)) };
    const marge = Math.min(70, r.width * 0.12);
    const vmin = 2.3 * this.echelle;
    const vmax = 4.5 * this.echelle;
    const S2 = (17 * this.echelle) ** 2;
    const R2 = R * R;

    for (let i = 0; i < n; i++) {
      if (attente[i] > 0) {
        attente[i] -= s;
        if (attente[i] <= 0) this.changer(i);
        else { if (etat[i] === 0) { x[i] += vx[i] * f; y[i] += vy[i] * f; } continue; }
      }
      if (etat[i] === 1) {
        // ---- landed: a spring to its place, a little breath, and a step aside from the pointer
        const cx = r.left + this.tx[i] * r.width + 0.7 * Math.sin(this.temps * 1.3 + this.phase[i]);
        const cy = r.top + (1 - this.ty[i]) * r.height + 0.7 * Math.cos(this.temps * 1.1 + this.phase[i]);
        let ax = (cx - x[i]) * 0.085;
        let ay = (cy - y[i]) * 0.085;
        const a = Math.hypot(ax, ay);
        if (a > 4.2) { ax *= 4.2 / a; ay *= 4.2 / a; }
        if (chasse) {
          const dx = x[i] - px;
          const dy = y[i] - py;
          const d2 = dx * dx + dy * dy;
          if (d2 < 6400 && d2 > 0.01) { const d = Math.sqrt(d2); const p = (1 - d / 80) * 3.2; ax += dx / d * p; ay += dy / d * p; }
        }
        const amorti = Math.pow(0.8, f);
        vx[i] = (vx[i] + ax * f) * amorti;
        vy[i] = (vy[i] + ay * f) * amorti;
      } else {
        // ---- in flight: stay apart, turn and stay with the neighbours, follow the wandering point
        let nb = 0, mx = 0, my = 0, mvx = 0, mvy = 0, ex = 0, ey = 0;
        const k = caseDe[i];
        if (k >= 0) {
          const cx = k % colonnes;
          const cy = (k - cx) / colonnes;
          for (let jy = Math.max(0, cy - 1); jy <= Math.min(lignes - 1, cy + 1); jy++) {
            for (let jx = Math.max(0, cx - 1); jx <= Math.min(colonnes - 1, cx + 1); jx++) {
              for (let j = tete[jy * colonnes + jx]; j !== -1; j = suivant[j]) {
                if (j === i) continue;
                const dx = x[j] - x[i];
                const dy = y[j] - y[i];
                const d2 = dx * dx + dy * dy;
                if (d2 > R2) continue;
                nb++; mx += x[j]; my += y[j]; mvx += vx[j]; mvy += vy[j];
                if (d2 < S2 && d2 > 0.01) { ex -= dx / d2; ey -= dy / d2; }
              }
            }
          }
        }
        let ax = (but.x - x[i]) * 0.00042 + (Math.random() - 0.5) * 0.08;
        let ay = (but.y - y[i]) * 0.00042 + (Math.random() - 0.5) * 0.08;
        if (nb) {
          ax += (mx / nb - x[i]) * 0.0016 + (mvx / nb - vx[i]) * 0.07 + ex * 1.5;
          ay += (my / nb - y[i]) * 0.0016 + (mvy / nb - vy[i]) * 0.07 + ey * 1.5;
        }
        // the element's edges turn them back, the harder the further out
        if (x[i] < r.left + marge) ax += 0.2 + (r.left + marge - x[i]) * 0.004;
        else if (x[i] > r.right - marge) ax -= 0.2 + (x[i] - r.right + marge) * 0.004;
        if (y[i] < r.top + marge) ay += 0.2 + (r.top + marge - y[i]) * 0.004;
        else if (y[i] > r.bottom - marge) ay -= 0.2 + (y[i] - r.bottom + marge) * 0.004;
        let vite = vmax;
        if (chasse) {  // the falcon
          const dx = x[i] - px;
          const dy = y[i] - py;
          const d2 = dx * dx + dy * dy;
          if (d2 < 25600 && d2 > 0.01) { const d = Math.sqrt(d2); const p = (1 - d / 160) ** 2 * 2.6; ax += dx / d * p; ay += dy / d * p; vite = vmax * (1 + 0.9 * (1 - d / 160)); }
        }
        vx[i] += ax * f;
        vy[i] += ay * f;
        const v = Math.hypot(vx[i], vy[i]);
        if (v > vite) { vx[i] *= vite / v; vy[i] *= vite / v; } else if (v < vmin && v > 0.001) { vx[i] *= vmin / v; vy[i] *= vmin / v; }
      }
      x[i] += vx[i] * f;
      y[i] += vy[i] * f;
    }
  }

  // Each bird a stroke along its speed, a dot when it is still; one path per colour and width.
  dessiner() {
    const g = this.g;
    const k = this.densite;
    g.setTransform(k, 0, 0, k, 0, 0);
    g.clearRect(0, 0, this.L, this.H);
    g.lineCap = 'round';
    const chemins = new Array(COULEURS.length * TRAITS.length);
    const { n, x, y, vx, vy } = this;
    for (let i = 0; i < n; i++) {
      if (x[i] < -20 || x[i] > this.L + 20 || y[i] < -20 || y[i] > this.H + 20) continue;
      const c = this.coul[i] * TRAITS.length + this.trait[i];
      const p = chemins[c] || (chemins[c] = new Path2D());
      const v = Math.hypot(vx[i], vy[i]);
      const e = v > 5.5 ? 9.5 / v : 1.7;  // (no longer than a bird, however fast)
      p.moveTo(x[i], y[i]);
      p.lineTo(x[i] - vx[i] * e - 0.01, y[i] - vy[i] * e);
    }
    for (let c = 0; c < chemins.length; c++) {
      if (!chemins[c]) continue;
      g.strokeStyle = COULEURS[Math.floor(c / TRAITS.length)];
      g.lineWidth = TRAITS[c % TRAITS.length] * this.echelle;
      g.stroke(chemins[c]);
    }
  }
}

// ================================================================ what is drawn crisp over a shape

const SVG = 'http://www.w3.org/2000/svg';
const svg = (nom, attributs) => { const e = document.createElementNS(SVG, nom); for (const [k, v] of Object.entries(attributs)) e.setAttribute(k, v); return e; };
const X = (x) => 100 * x;
const Y = (y) => 100 * (1 - y);

// The axes, ticks, ellipse or hulls of a shape, over its zone (hidden until montrer).
function decorer(zone, id) {
  const d = (FORMES[id] || {}).deco || {};
  const boite = el('div', { class: 'deco' });
  const dessin = svg('svg', { viewBox: '0 0 100 100', preserveAspectRatio: 'none', 'aria-hidden': 'true' });
  boite.append(dessin);
  const etiquette = (x, y, texte, classe) => {
    const e = el('span', { class: `grad ${classe}` }, texte);
    e.style.left = `${X(x)}%`;
    e.style.top = `${Y(y)}%`;
    boite.append(e);
  };
  const trace = (pts) => pts.map(([x, y]) => `${X(x)},${Y(y)}`).join(' ');
  for (const [a, b] of d.bandes || []) dessin.append(svg('rect', { x: X(a), y: 0, width: X(b) - X(a), height: 100, class: 'bande' }));
  for (const [x1, y1, x2, y2] of d.axes || []) dessin.append(svg('line', { x1: X(x1), y1: Y(y1), x2: X(x2), y2: Y(y2) }));
  if (d.ellipse) dessin.append(svg('polygon', { points: trace(d.ellipse), class: 'tirets' }));
  for (const e of d.enveloppes || []) dessin.append(svg('polygon', { points: trace(e.pts), class: `enveloppe s${e.g}` }));
  for (const m of d.moyennes || []) dessin.append(svg('polyline', { points: trace(m.pts), class: `moyenne s${m.g}` }));
  if (d.evenement !== undefined) {
    dessin.append(svg('line', { x1: X(d.evenement), y1: 0, x2: X(d.evenement), y2: 100, class: 'tirets' }));
    etiquette(d.evenement, 1, T.evenement, 'titre note-haut');
  }
  // an axis that crosses the other in the middle (the scores) carries its ticks there
  const milieu = (d.axes || []).length === 2 && d.axes[0][1] === 0.5;
  for (const t of d.gx || []) etiquette(t.p, milieu ? 0.5 : 0, String(t.v), 'x');
  for (const t of d.gy || []) etiquette(milieu ? 0.5 : 0, t.p, String(t.v), 'y');
  if (id === 'acp') {
    etiquette(1, 0.5, 't[1]', 'titre fin-x');
    etiquette(0.5, 1, 't[2]', 'titre haut-y');
    // each variety's name by its apples: under them in the lower half, over them in the upper
    (FORMES.acp.groupes || []).forEach((nom, g) => {
      const pts = FORMES.acp.points.filter((p) => p[2] === g);
      const ys = pts.map((p) => p[1]);
      const cx = pts.reduce((s, p) => s + p[0], 0) / pts.length;
      const bas = ys.reduce((s, y) => s + y, 0) / ys.length < 0.5;
      etiquette(cx, bas ? Math.min(...ys) - 0.035 : Math.max(...ys) + 0.085, nom, `groupe g${g}`);
    });
  }
  if (id === 'spectres') etiquette(0.5, 0, T.axe_spectres, 'titre centre');
  if (id === 'distance') etiquette(0.5, 0, T.axe_distance, 'titre centre');
  if (id === 'carte') etiquette(1, 1, '↑ N', 'titre fin-x');
  zone.append(boite);
  return boite;
}

function figure(conteneur, ids) {
  const zone = $('.zone', conteneur);
  const decos = Object.fromEntries(ids.map((id) => [id, decorer(zone, id)]));
  let montree = null;
  return { zone, montrer(id) {
    if (id === montree) return;
    if (montree) decos[montree].classList.remove('montre');
    if (id) decos[id].classList.add('montre');
    montree = id;
  } };
}

// ================================================================ the swarm on the page

// The swarm belongs to the hero's sky, to the tour's plate or to the logo near the downloads:
// whichever shows most in the window. In the sky it flies, or lands as the example's PCA; on the
// plate it takes the shape of the step being read.
function scene() {
  const toile = $('#nuee');
  const ciel = $('#ciel');
  const planche = $('#planche');
  const cielFin = $('#ciel-fin');
  if (!toile || !ciel) return;
  const nuee = new Nuee(toile);
  const hero = figure(ciel, ['acp']);
  const tour = figure(planche, $$('.etape').map((e) => e.dataset.forme));
  const fin = figure(cielFin, []);
  const etapes = $$('.etape');
  const nom = $('#planche-nom');
  const bascule = $('#poser');
  let pose = calme;       // the hero's swarm has landed
  let touche = false;     // the reader chose: no more turns on its own
  let lieu = null;
  let etape = null;
  nuee.semer(ciel);

  const part = (e) => {
    const r = e.getBoundingClientRect();
    const vu = Math.min(r.bottom, innerHeight) - Math.max(r.top, 0);
    return vu <= 0 ? 0 : vu / Math.min(r.height, innerHeight);
  };
  function regler() {
    const parts = { hero: part(ciel), tour: part(planche), fin: part(cielFin) };
    const mieux = Object.keys(parts).reduce((a, b) => (parts[b] > parts[a] ? b : a));
    if (parts[mieux] >= 0.35 || !lieu) lieu = mieux;
    // the step being read: the one that holds the reading line (lower on a phone, under the plate)
    const ligne = innerHeight * (matchMedia('(max-width: 54rem)').matches ? 0.74 : 0.5);
    const lue = etapes.find((e) => { const r = e.getBoundingClientRect(); return r.top <= ligne && r.bottom > ligne; }) || null;
    if (lue !== etape) {
      if (etape) etape.classList.remove('active');
      if (lue) lue.classList.add('active');
      etape = lue;
      nom.textContent = lue ? $('.etape-nom', lue).textContent.replace(/^(\d+)/, '$1 · ') : '';
    }
    const forme = etape ? etape.dataset.forme : calme ? etapes[0].dataset.forme : null;
    if (lieu === 'hero') { if (pose) nuee.former('acp', hero.zone); else nuee.voler(ciel); }
    else if (lieu === 'tour') { if (forme) nuee.former(forme, tour.zone); else nuee.voler(planche); }
    else nuee.former('logo', fin.zone);
    hero.montrer(lieu === 'hero' && pose ? 'acp' : null);
    tour.montrer(lieu === 'tour' ? forme : null);
    bascule.textContent = pose ? T.envoler : T.poser;
    nuee.reveiller();
  }

  let prevu = false;
  const bientot = () => { if (!prevu) { prevu = true; requestAnimationFrame(() => { prevu = false; regler(); }); } };
  addEventListener('scroll', bientot, { passive: true });
  addEventListener('resize', bientot);

  const basculer = () => { pose = !pose; regler(); };
  ciel.addEventListener('click', () => { touche = true; basculer(); });
  bascule.addEventListener('click', () => { touche = true; basculer(); });
  bascule.hidden = false;
  // on its own, until the reader touches it: a few seconds of flight, then the plot, then again
  if (!calme) {
    const tourSeul = () => { if (!touche) { if (lieu === 'hero' && !document.hidden) basculer(); setTimeout(tourSeul, pose ? 6500 : 7500); } };
    setTimeout(tourSeul, 4200);
  }
  regler();
}

// ================================================================ Which analysis?

// The application's rules (choix_analyse.js: REGLES, CATALOGUE, NOTES) on the answers given here.
// What the application reads in the data (overdispersion, skewness, sites met once...) is left out.
function quelleAnalyse() {
  const boite = $('#quelle-analyse');
  if (!boite) return;
  const C = D.choix;
  const t = C.t;
  const R = { question: 'tendance', but: 'composition', reponse: 'comptage', zeros: false, repetes: true, annees: false, baci: false, coordonnees: false, x: null, beaucoup: false };
  const fiche = () => (R.question === 'especes' ? C.formulaire[`especes/${R.but}`] : C.formulaire[R.question]) || { reponses: [], champs: [] };
  const vaut = (attendu, v) => (Array.isArray(attendu) ? attendu.includes(v) : attendu === v);
  const convient = (si, c) => Object.entries(si || {}).every(([k, v]) => vaut(v, c[k] === undefined ? false : c[k]));
  const contexte = () => {
    const champs = fiche().champs;
    const coche = (k) => champs.includes(`${k}!`) || (champs.includes(k) && R[k]);
    return { question: R.question, but: R.question === 'especes' ? R.but : null, reponse: R.reponse, zeros: R.reponse === 'comptage' && R.zeros,
      repetes: coche('repetes'), annees: coche('annees'), baci: coche('baci'), coordonnees: coche('coordonnees'), unParSiteTemps: null, especesEntieres: null,
      xQuantitatives: champs.includes('x') && R.x === 'quanti', xQualitatives: champs.includes('x') && R.x === 'quali', beaucoupDeX: champs.includes('x') && R.beaucoup };
  };
  const recommander = (c) => {
    const r = C.regles.find((x) => vaut(x.question, c.question) && (x.but === undefined || vaut(x.but, c.but)) && (x.reponse === undefined || vaut(x.reponse, c.reponse)) && convient(x.si, c));
    return r ? { choix: r.choix, autres: r.autres.filter((id) => id !== r.choix && convient(C.si[id], c)) } : null;
  };
  // (a linear regression with the site's random effect is the linear mixed model)
  const nom = (id, c) => {
    const n = c.repetes && id === 'normal' ? C.noms.lmm : c.repetes && id === 'lm_log' ? C.noms.lmm_log : C.noms[id];
    return c.repetes && C.regressions.includes(id) ? t.avec_site.replace('{nom}', n).replace('{site}', T.site) : n;
  };
  const notes = (id, c) => C.notes.filter((x) => x.pour.includes(id) && convient(x.si, c)).map((x) => x.t);

  // the kind of response the question leaves: its only one, the one chosen if it still fits, or none
  const ajuster = () => {
    const f = fiche();
    if (f.reponses.length === 1) R.reponse = f.reponses[0];
    else if (!f.reponses.includes(R.reponse)) R.reponse = null;
  };
  const puce = (type, groupe, coche, texte, aide, changer) => el('label', {},
    el('input', { type, name: groupe, checked: coche, onchange: (e) => { changer(e.target.checked); rendre(); } }),
    el('span', {}, texte, aide ? el('small', {}, aide) : null));

  const resultat = $('#qa-resultat');
  function rendre(anime = true) {
    const f = fiche();
    $('#qa-questions').replaceChildren(...C.questions.map(([id, texte, exemple]) => puce('radio', 'qa-q', R.question === id, texte, exemple, () => { R.question = id; ajuster(); })));
    const reponses = R.question === 'especes'
      ? C.buts.map(([id, texte]) => puce('radio', 'qa-b', R.but === id, texte, null, () => { R.but = id; ajuster(); }))
      : f.reponses.map((id) => puce('radio', 'qa-r', R.reponse === id, C.reponses[id][0], C.reponses[id][1], () => { R.reponse = id; }));
    if (f.facultative) reponses.push(puce('radio', 'qa-r', R.reponse === null, t.aucune.replace(/—/g, '').trim(), null, () => { R.reponse = null; }));
    $('#qa-reponses').replaceChildren(...reponses);
    const plan = ['repetes', 'annees', 'baci', 'coordonnees'].filter((k) => f.champs.includes(k)).map((k) => puce('checkbox', `qa-${k}`, R[k], t[k], null, (v) => { R[k] = v; }));
    if (R.reponse === 'comptage') plan.push(puce('checkbox', 'qa-zeros', R.zeros, C.reponses.comptage[0], t.zeros, (v) => { R.zeros = v; }));
    if (R.question === 'especes' && f.champs.includes('x')) {
      plan.unshift(puce('checkbox', 'qa-xn', R.x === 'quanti', T.x_quanti, T.x_quanti_ex, (v) => { R.x = v ? 'quanti' : null; }),
        puce('checkbox', 'qa-xq', R.x === 'quali', T.x_quali, T.x_quali_ex, (v) => { R.x = v ? 'quali' : null; }));
    }
    if (R.question === 'predire') plan.unshift(puce('checkbox', 'qa-xb', R.beaucoup, T.x_beaucoup, T.x_beaucoup_ex, (v) => { R.beaucoup = v; }));
    $('#qa-plan').replaceChildren(...(plan.length ? plan : [el('p', { class: 'vide' }, T.plan_aucun)]));

    const c = contexte();
    const r = recommander(c);
    const detail = (id) => [
      el('p', {}, el('b', {}, t.pourquoi), C.pourquoi[id]),
      el('p', {}, el('b', {}, t.conditions), C.conditions[id]),
      ...notes(id, c).map((x) => el('p', { class: 'note-regle' }, x))];
    resultat.replaceChildren(...(!r ? [el('p', { class: 'attente' }, R.question ? T.choisir_attente_reponse : T.choisir_attente_question)] : [
      el('p', { class: 'tampon' }, t.recommandee),
      el('h3', {}, nom(r.choix, c)),
      ...detail(r.choix),
      r.autres.length ? el('h4', {}, t.autres) : null,
      ...r.autres.map((id) => el('details', {}, el('summary', {}, nom(id, c)), ...detail(id))),
      el('p', { class: 'conseil' }, t.conseil)]));
    if (anime && !calme) { resultat.classList.remove('change'); void resultat.offsetWidth; resultat.classList.add('change'); }
  }
  rendre(false);
  boite.hidden = false;
}

// ================================================================ the page's other machines

// The R console types its command when it comes into view, then gives its output.
function consoleR() {
  const boite = $('#console');
  const commande = $('#commande');
  if (!boite || !commande) return;
  if (calme || !('IntersectionObserver' in window)) { boite.classList.add('tape'); return; }
  // each character of the command in a span of its own, hidden: typing shows them one by one,
  // and the line wraps as it grows
  const lettres = [];
  const decouper = (noeud) => {
    for (const n of [...noeud.childNodes]) {
      if (n.nodeType !== Node.TEXT_NODE) { decouper(n); continue; }
      n.replaceWith(...[...n.textContent].map((c) => { const s = el('span', { hidden: true }, c); lettres.push(s); return s; }));
    }
  };
  decouper(commande);
  commande.after(el('span', { class: 'curseur', 'aria-hidden': 'true' }));
  new IntersectionObserver((vues, o) => {
    if (!vues.some((v) => v.isIntersecting)) return;
    o.disconnect();
    let i = 0;
    const taper = () => {
      lettres[i++].hidden = false;
      if (i < lettres.length) setTimeout(taper, 16 + Math.random() * 42);
      else setTimeout(() => boite.classList.add('tape'), 380);
    };
    setTimeout(taper, 500);
  }, { threshold: 0.6 }).observe(boite);
}

// The Methods sheet in English or in French, as the application writes it in either.
function feuille() {
  const boutons = $$('.langues button');
  const textes = $$('.feuille-texte');
  const montrer = (langue) => {
    for (const b of boutons) b.setAttribute('aria-pressed', String(b.dataset.langue === langue));
    for (const x of textes) x.hidden = x.dataset.langue !== langue;
  };
  for (const b of boutons) b.addEventListener('click', () => montrer(b.dataset.langue));
  montrer(D.langue);
  // the numbers get their line one after the other
  $$('.valeur').forEach((v, i) => { v.style.transitionDelay = `${Math.min(i, 40) * 45}ms`; });
}

// The window's switch: the application's dark theme wipes over the light one. And the window
// leans a little towards the pointer.
function fenetre() {
  const boite = $('#fenetre');
  const bouton = $('#theme');
  if (!boite) return;
  bouton.addEventListener('click', () => bouton.setAttribute('aria-pressed', String(boite.classList.toggle('nuit'))));
  const cadre = $('.fenetre-cadre', boite);
  if (calme || !matchMedia('(hover: hover) and (pointer: fine)').matches) return;
  boite.addEventListener('pointermove', (e) => {
    const r = cadre.getBoundingClientRect();
    const u = (e.clientX - r.left) / r.width - 0.5;
    const v = (e.clientY - r.top) / r.height - 0.5;
    cadre.style.transitionDuration = '.12s';
    cadre.style.transform = `rotateY(${(u * 3.2).toFixed(2)}deg) rotateX(${(-v * 2.6).toFixed(2)}deg)`;
  });
  boite.addEventListener('pointerleave', () => { cadre.style.transitionDuration = ''; cadre.style.transform = ''; });
}

// The download buttons for the reader's system, the version GitHub has and which of its files
// (worker.js).
function telechargements() {
  const systeme = (navigator.userAgentData && navigator.userAgentData.platform) || navigator.platform || '';
  const ua = navigator.userAgent;
  // (an iPad says it is a Mac: it has several touch points, a Mac none)
  const os = /android|iphone|ipad/i.test(ua) ? null : /win/i.test(systeme) ? 'windows'
    : /mac/i.test(systeme) ? (navigator.maxTouchPoints > 1 ? null : 'mac') : /linux/i.test(systeme) ? 'linux' : null;
  const principal = $('[data-telecharger]');
  if (os === 'linux') { principal.href = '/download/deb'; principal.textContent = T.telecharger_linux; }
  // (a Mac doesn't say which processor it has: the choice is on its card)
  else if (os === 'mac') { principal.href = '#telecharger'; principal.textContent = T.telecharger_mac; }
  else if (!os) { principal.href = '#telecharger'; principal.textContent = T.telecharger_ici; }
  const carte = os ? $(`.dl-carte[data-systeme="${os}"]`) : null;
  if (carte) carte.classList.add('ici');

  fetch('/version.json').then((r) => (r.ok ? r.json() : null)).then((x) => {
    if (!x || !/^\d+\.\d+\.\d+$/.test(x.version || '')) return;
    for (const e of $$('[data-version]')) e.textContent = x.version;
    for (const e of $$('[data-version-phrase]')) e.textContent = `Nuée ${x.version}`;
    $('#dl-version').hidden = false;
    // a card whose file the release doesn't have says so, in place of its buttons
    for (const c of $$('.dl-carte')) {
      const absent = (x.fichiers || {})[c.dataset.fichier] === false;
      c.classList.toggle('absent', absent);
      $('.dl-absent', c).hidden = !absent;
    }
    if (carte && carte.classList.contains('absent')) principal.href = '#telecharger';
  }).catch(() => {});
}

// The header takes its paper once the page has scrolled; a section's axis draws itself when it comes.
function page() {
  const entete = $('#entete');
  const poser = () => entete.classList.toggle('pose', scrollY > 12);
  addEventListener('scroll', poser, { passive: true });
  poser();
  const aVoir = [...$$('.bloc'), ...$$('.feuille')];
  if (!('IntersectionObserver' in window)) { for (const e of aVoir) e.classList.add('vu'); return; }
  const o = new IntersectionObserver((vues) => { for (const v of vues) if (v.isIntersecting) { v.target.classList.add('vu'); o.unobserve(v.target); } }, { rootMargin: '0px 0px -12% 0px' });
  for (const e of aVoir) o.observe(e);
}

for (const f of [page, scene, quelleAnalyse, consoleR, feuille, fenetre, telechargements]) {
  try { f(); } catch (e) { console.error(e); }
}
