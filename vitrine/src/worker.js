// What no file of the site answers (the static assets are served before this runs):
//   /download/<file>   → the latest release's file on GitHub, so the page's buttons and the links
//                        given elsewhere never go stale: windows, mac-arm64, mac-x64, deb,
//                        appimage, tar; the release's page when it doesn't have that file
//   /version.json      → {"version": "1.0.0", "fichiers": {"windows": true, …}}: the latest
//                        release and which of these files it has, for the page to say
//   /relais/…          → the relay that lets the browser version share on GitHub and GitLab
//                        (src/relais.js)
//   anything else      → the 404 page
// The latest release is read from the redirect of github.com/…/releases/latest, and each file
// asked for by its name (no API, no rate limit to share with the other Workers); the answer is
// kept ten minutes in the cache.

import { relayer } from './relais.js';

const DEPOT = 'https://github.com/4rtefakt/nuee';
const FICHIERS = {
  windows: (v) => `Nuee-Setup-${v}.exe`,
  'mac-arm64': (v) => `Nuee-${v}-arm64.dmg`,
  'mac-x64': (v) => `Nuee-${v}-x64.dmg`,
  deb: (v) => `Nuee-${v}-amd64.deb`,
  appimage: (v) => `Nuee-${v}-x86_64.AppImage`,
  tar: (v) => `Nuee-${v}-x64.tar.gz`,
};
const DUREE = 600;  // seconds
const adresse = (version, fichier) => `${DEPOT}/releases/download/v${version}/${FICHIERS[fichier](version)}`;

// The latest release: { version: "1.0.0", fichiers: { windows: true, … } }, each file there
// (true), missing (false) or unknown (null); version null when GitHub can't say.
async function derniere(ctx) {
  const cle = new Request('https://nuee.io/__derniere.json');
  const garde = await caches.default.match(cle);
  if (garde) return garde.json();
  const demander = (url, options) => fetch(url, { redirect: 'manual', headers: { 'User-Agent': 'nuee.io' }, ...options }).catch(() => null);
  const r = await demander(`${DEPOT}/releases/latest`);
  const m = r && /\/releases\/tag\/v(\d+\.\d+\.\d+)$/.exec(r.headers.get('Location') || '');
  if (!m) return { version: null, fichiers: {} };  // GitHub out of reach: the page and the downloads fall back
  const version = m[1];
  // (a file that is there redirects to where it is stored; one that isn't gives 404)
  const etats = await Promise.all(Object.keys(FICHIERS).map(async (f) => {
    const x = await demander(adresse(version, f), { method: 'HEAD' });
    return [f, !x ? null : x.status === 404 ? false : x.status < 400 ? true : null];
  }));
  const reponse = { version, fichiers: Object.fromEntries(etats) };
  ctx.waitUntil(caches.default.put(cle, Response.json(reponse, { headers: { 'Cache-Control': `max-age=${DUREE}` } })));
  return reponse;
}

export default {
  async fetch(request, env, ctx) {
    const url = new URL(request.url);
    if (url.pathname === '/relais' || url.pathname.startsWith('/relais/')) return relayer(request, env, url);
    if (request.method !== 'GET' && request.method !== 'HEAD') return new Response('Method not allowed', { status: 405, headers: { Allow: 'GET, HEAD' } });

    const telechargement = /^\/download\/([a-z0-9-]+)\/?$/.exec(url.pathname);
    if (telechargement && FICHIERS[telechargement[1]]) {
      const { version, fichiers } = await derniere(ctx);
      const cible = version && fichiers[telechargement[1]] !== false ? adresse(version, telechargement[1]) : `${DEPOT}/releases/latest`;
      return new Response(null, { status: 302, headers: { Location: cible, 'Cache-Control': 'no-store' } });
    }

    if (url.pathname === '/version.json') {
      const reponse = await derniere(ctx);
      return Response.json(reponse, { headers: { 'Cache-Control': reponse.version ? `public, max-age=${DUREE}` : 'no-store' } });
    }

    const page = await env.ASSETS.fetch(new URL(url.pathname.startsWith('/fr/') ? '/fr/404.html' : '/404.html', url));
    return new Response(request.method === 'HEAD' ? null : page.body, { status: 404, headers: page.headers });
  },
};
