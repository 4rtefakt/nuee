# nuee.io

Nuée's landing page, in English (`/`) and in French (`/fr/`), on Cloudflare Workers. Plain HTML,
CSS and JavaScript, no framework; two typefaces served from the site; no cookie, nothing loaded
from another site. The documentation stays where it is (`docs/site`, on GitHub Pages): this page
links to it.

What it shows is taken from the application, not written beside it:

- the figure the swarm lands as on the first screen (the monitoring tutorial's skylarks: each
  count over its site's level, under TRIM's indices), the six shapes of the tour, the R console's
  output, the Methods text, the Export to R script and the values the document cites are computed
  by the engine (`donnees.R`), all from one study: the tutorial's skylarks, and surveys of the
  same birds made up in that script (their songs, a trial of skylark plots, the other birds of
  the squares);
- the map under the tour's last figure is IGN's Plan IGN, one of the two backgrounds the
  application's Map view offers, fetched once and served from here (`carte.py`);
- *Which analysis?* runs the application's own rules and texts (`app/interface/choix_analyse.js`),
  and a document's cited values are written by its own functions (`app/interface/references.js`);
- the screenshots are the documentation site's (`docs/site/images`: the window with the skylarks'
  model, `suivi-fenetre.png`, is taken for this page by `node docs/site/captures.mjs suivi`), the
  logo and the icons the application's (`app/interface/nuee.svg`, `app/icone`);
- the download buttons go to the latest release on GitHub, whatever its number (`src/worker.js`).

| | |
|---|---|
| `src/gabarit.html` | the page: `{{key}}` is a text of `textes.mjs` or a block made by `construire.mjs` |
| `src/textes.mjs` | the texts, English and French, the same keys in both |
| `src/assets/` | `site.css`, `site.js` (the swarm, *Which analysis?*, the page's small machines), the fonts, `og.png`, `carte.webp` |
| `src/donnees.json` | the figures and the quoted texts, written by `donnees.R`: don't edit it by hand |
| `src/carte.json` | where the map's tiles are, written by `carte.py` for `donnees.R` |
| `src/worker.js` | what no file answers: `/download/<windows, mac-arm64, mac-x64, deb, appimage, tar>`, `/version.json`, the 404 pages |
| `construire.mjs` | builds `dist/` (not committed) |
| `donnees.R` | computes `src/donnees.json` with the engine |
| `image.mjs` | makes `src/assets/og.png`, the picture of a shared link |
| `carte.py` | makes `src/assets/carte.webp`, the map, and `src/carte.json` |
| `wrangler.jsonc` | the Worker: its assets, its build, `nuee.io` and `www.nuee.io` |

## Looking at it

```
cd vitrine
npm install          # once: Wrangler
npx wrangler dev     # http://localhost:8787, rebuilt when src/ changes
```

`node construire.mjs` alone builds `dist/`; it needs Node 22 or later and nothing else. The build
stops when a text is missing in one language, or when the page names a text that doesn't exist.

## Publishing

```
cd vitrine
npx wrangler deploy
```

It builds, then publishes to `nuee.io` and `www.nuee.io` (Custom Domains of the `nuee-vitrine`
Worker, in the Cloudflare account that holds the `nuee.io` zone; `npx wrangler login` first on a
new computer). Assets are stamped (`?v=…`) and kept a year by browsers; the pages are always
checked again.

**After a release, nothing to do:** the buttons and the version shown follow the latest release
within ten minutes. A system whose file the latest release doesn't have (macOS before 1.0.0, or
a release whose packages are still being added) says so on its card, in place of its buttons,
and its `/download/…` address goes to the release's page. Publish again when the page's texts
change; a new kind of file is one more line in `FICHIERS` (`src/worker.js`) and its button in
the page.

## After a change to the engine, the example or the tutorials' data

```
Rscript vitrine/donnees.R      # from the repository's folder: writes vitrine/src/donnees.json
node vitrine/image.mjs         # if the skylarks' figure changed: the shared link's picture (needs Chrome or Edge)
```

`donnees.R` needs R with the engine's packages. It checks that the Export to R script gives the
engine's results before quoting it. The songs' spectra, the skylark plots and the squares' other
birds are simulated in it, with fixed seeds, and the page calls them made up; the rest is the
monitoring tutorial's data (`docs/site/donnees/`). One number of the tour's captions comes from
that tutorial's text (the skylarks' density): update `src/textes.mjs` when the tutorial changes.

## The map

The tutorial's 40 sites have no coordinates: `donnees.R` gives each a place, made up like its
counts (the cropland squares on the Beauce plain, the grassland ones along the Loire and its
tributaries), and the page says so. Each site is drawn as a flock: the swarm's birds shared
between the sites by their mean counts, so the flocks must not overlap (`donnees.R` stops if two
do: move one). The background is IGN's Plan IGN (© IGN, Licence Ouverte 2.0), twelve tiles at
zoom 11 stitched by `carte.py`, and shown with less colour than IGN gives it:

```
python vitrine/carte.py        # needs Pillow; then Rscript vitrine/donnees.R, and build again
```

To show another place, change the tiles in `carte.py` and the sites' longitudes and latitudes in
`donnees.R`. The page credits "© IGN – Plan IGN" on the map and in its colophon.

## The texts

Each family of analyses, each claim and each menu name follows the README and the documentation
site: correct them there first. Adding a language: a third dictionary in `textes.mjs`, its page in
`construire.mjs` (the texts of *Which analysis?* exist in French and English only).

## Fonts

[Fraunces](https://github.com/undercasetype/Fraunces) and
[IBM Plex Mono](https://github.com/IBM/plex), both under the SIL Open Font License 1.1
(`src/assets/polices/OFL-*.txt`), their Latin subsets as [Fontsource](https://fontsource.org)
packages them (`@fontsource-variable/fraunces`, `@fontsource/ibm-plex-mono`, versions 5.3.0).
