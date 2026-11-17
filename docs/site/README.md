# Nuée's documentation website

A static site, in French and in English: the home page, the installation, the user guide in
pages, six tutorials and What's new. Plain HTML and CSS, system fonts, a light and a dark theme,
nothing loaded from another site. Published on GitHub Pages, https://4rtefakt.github.io/nuee/, by
`.github/workflows/site.yml` at each push that changes it (from `main`; from `nuee-1.0` too until 1.0
is merged).

| | |
|---|---|
| `index.html` | the root: sends the reader to `fr/` (or to `en/` when the browser prefers English) |
| `fr/`, `en/` | the pages, built by `construire.js`: don't edit them by hand |
| `assets/` | `site.css`, `site.js` (the theme the reader chose, the guide's menu folded on a phone), the logo |
| `images/fr/`, `images/en/` | the tutorials' screenshots, made by `captures.mjs`; their dark versions in `sombre/` |
| `donnees/` | the tutorials' data (simulated) and the R scripts that made them |
| `greffons/` | the example plugins of `exemple/plugins` as `.nueeplugin` files, for download, built by `construire.js` (don't edit them by hand) |
| `sources/fr/`, `sources/en/` | the home page, the installation and the tutorials, in Markdown |
| `construire.js` | builds the pages |
| `captures.mjs` | takes the screenshots, the tutorials' and the README's (`docs/captures/`) |
| `verifier_liens.js` | checks the links |

## Rebuilding the pages

```
node docs/site/construire.js
```

It needs markdown-it, from `app/node_modules` (`npm ci` in `app/` first). It reads:

- `USER-GUIDE.txt` and `LISEZ-MOI-APPLICATION.txt`: the user guide, cut into the pages listed in
  `PAGES_GUIDE` (`construire.js`). The guides stay the reference: correct them, then rebuild. A
  section added to the guides must be given a page there; the build stops and names it otherwise,
  as it does when the two guides no longer have the same sections;
- `CHANGELOG.md` (English) and `app/interface/nouveautes.js` (French), for What's new;
- `sources/<langue>/*.md`. In them, `page:<id>` links to another page of the same language
  (`page:guide.regressions`, `page:tutoriels.baci#…`; the ids are in `PAGES`) and `@/` starts a
  path from the site's root (`@/images/fr/baci-choix.png`): both become relative links, checked at
  build time. A lone image with a title becomes a figure with its caption;
- `app/package.json`: the version, in the texts and the download links.

The download links point to the files of the release of that version
(`releases/download/v<version>/…`): they work once that release is published on GitHub. After a
version change, rebuild.

Commit the pages with their sources, so that the site works without building it.

## Checking

```
node docs/site/verifier_liens.js              # every link and #anchor of every page, every page reachable
node docs/site/verifier_liens.js --externes   # the links to GitHub too
```

To look at the site, serve the folder, for instance with `python3 -m http.server 8792 --directory
docs/site`, then open http://127.0.0.1:8792/. The pages also open from the disk (file://).

## Screenshots and data

```
node docs/site/captures.mjs [baci] [pls] [suivi] [communautes] [cartes] [rapport] [readme] [--langue fr|en] [--theme clair|sombre]
```

Starts the R engine (`moteur/serveur.R`, port 8793) and a headless Chrome or Edge (port 9333),
then goes through each tutorial in the browser version of Nuée, as a person would, in French with
the French data and in English with the English data, each in Nuée's light theme and its dark one,
and writes `images/<langue>/` and `images/<langue>/sombre/`: the site's dark theme shows the dark
screenshots (`construire.js` puts both in the page when both exist). It needs
Node 22 or later, R with the engine's packages (`RSCRIPT=…`) and Chrome or Edge (`CHROME=…`); with
Python and Pillow, the images are then reduced to 256 colours. With `NUEE_TEXTES=<folder>`, it also
writes each model's Methods and Results texts there (the tutorials quote them: when the numbers
change, update the Markdown sources). A change in the interface can break a step: the script says
which one. `suivi` also takes `suivi-fenetre.png`, the whole window with the skylarks' TRIM model:
no page of this site shows it, nuee.io does (`vitrine/`).

The data are simulated, with fixed seeds: `Rscript docs/site/donnees/baci.R` (and `spectres.R`,
`suivi.R`, `communautes.R`, `cartes.R`) writes the same files again; `cartes.R` also writes the
elevation model, a GeoTIFF it encodes itself. `references.bib`, for the report tutorial, lists
four published methods papers and books. The browser has no file chooser that gives a path: for
the rasters and the `.bib` the tutorials link, the script answers the prompt the interface opens
instead.

`readme` takes the README's screenshots, `docs/captures/*.png` (`README.md` and `README.fr.md`
show them), only when it is named:

```
node docs/site/captures.mjs readme
```

Each is the whole window, 1440 × 900, in English whatever `--langue` says, and in Nuée's light
theme whatever `--theme` says, but `console.png` in the dark one, as its caption says. Six come
from the example project (*Help › Example project*): the PCA, the PLS with its VIP, the OPLS-DA
with its S-plot, the R console under the PCA, the tests between two varieties and the class
models; `spectres.png` and `zones.png` from spectra the script makes up in a temporary folder (a
table of 60 spectra; 24 JCAMP-DX files and a table of their sugar contents, imported by *File ›
Import spectra*). Retake them after a change to what they show; they are reduced to 256 colours
as the site's are.

## Publishing

The site is this folder as it is: any web server that serves files will do. `sources/` and the
scripts are harmless if they go along.

**On GitHub Pages**, as it is: `.github/workflows/site.yml` publishes the folder at each push to
`main` that changes it (*Settings › Pages › Source: GitHub Actions*), at
`https://4rtefakt.github.io/nuee/`. The `github-pages` environment allows `main`, and `nuee-1.0`
until 1.0 is merged: remove that branch from its deployment branches then. `.nojekyll` keeps
GitHub from processing the pages.

**On a server of your own**, copy the folder, for instance
`rsync -a --delete --exclude sources docs/site/ user@server:/var/www/nuee/`, and serve it as
static files. All links are relative: the site works at a domain's root or in a sub-folder.
