// What no file of the site answers (the static assets are served before this runs):
//   /download/<file>   → the latest release's installer on GitHub, so the page's buttons and the
//                        links given elsewhere never go stale: windows, deb, appimage, tar
//   /version.json      → {"version": "1.0.0"}, the latest release, for the page to show
//   anything else      → the 404 page
// The latest release is read from the redirect of github.com/…/releases/latest (no API, no rate
// limit to share with the other Workers), and kept ten minutes in the cache.

const DEPOT = 'https://github.com/4rtefakt/nuee';
const FICHIERS = {
  windows: (v) => `Nuee-Setup-${v}.exe`,
  deb: (v) => `Nuee-${v}-amd64.deb`,
  appimage: (v) => `Nuee-${v}-x86_64.AppImage`,
  tar: (v) => `Nuee-${v}-x64.tar.gz`,
};
const DUREE = 600;  // seconds

// The version of the latest release ("1.0.0"), or null when GitHub can't say.
async function derniereVersion(ctx) {
  const cle = new Request('https://nuee.io/__derniere-version');
  const garde = await caches.default.match(cle);
  if (garde) return garde.text();
  let version = null;
  try {
    const r = await fetch(`${DEPOT}/releases/latest`, { redirect: 'manual', headers: { 'User-Agent': 'nuee.io' } });
    const m = /\/releases\/tag\/v(\d+\.\d+\.\d+)$/.exec(r.headers.get('Location') || '');
    if (m) version = m[1];
  } catch { /* GitHub out of reach: no version, the page and the downloads fall back */ }
  if (version) ctx.waitUntil(caches.default.put(cle, new Response(version, { headers: { 'Cache-Control': `max-age=${DUREE}` } })));
  return version;
}

export default {
  async fetch(request, env, ctx) {
    const url = new URL(request.url);
    if (request.method !== 'GET' && request.method !== 'HEAD') return new Response('Method not allowed', { status: 405, headers: { Allow: 'GET, HEAD' } });

    const telechargement = /^\/download\/([a-z]+)\/?$/.exec(url.pathname);
    if (telechargement && FICHIERS[telechargement[1]]) {
      const version = await derniereVersion(ctx);
      const cible = version ? `${DEPOT}/releases/download/v${version}/${FICHIERS[telechargement[1]](version)}` : `${DEPOT}/releases/latest`;
      return new Response(null, { status: 302, headers: { Location: cible, 'Cache-Control': 'no-store' } });
    }

    if (url.pathname === '/version.json') {
      const version = await derniereVersion(ctx);
      return Response.json({ version }, { headers: { 'Cache-Control': version ? `public, max-age=${DUREE}` : 'no-store' } });
    }

    const page = await env.ASSETS.fetch(new URL(url.pathname.startsWith('/fr/') ? '/fr/404.html' : '/404.html', url));
    return new Response(request.method === 'HEAD' ? null : page.body, { status: 404, headers: page.headers });
  },
};
