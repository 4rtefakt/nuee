# Nuée's documentation website

A static site, in French and in English: the home page, the installation, the user guide in
pages, three tutorials and What's new. Plain HTML and CSS, system fonts, a light and a dark theme,
nothing loaded from another site. Published on GitHub Pages, https://4rtefakt.github.io/nuee/, by
`.github/workflows/site.yml` at each push that changes it (from `main`; from `nuee-1.0` too until 1.0
is merged).

| | |
|---|---|
| `index.html` | the root: sends the reader to `fr/` (or to `en/` when the browser prefers English) |
| `fr/`, `en/` | the pages, built by `construire.js`: don't edit them by hand |
| `assets/` | `site.css`, `site.js` (the theme the reader chose, the guide's menu folded on a phone), the logo |
| `images/fr/`, `images/en/` | the tutorials' screenshots, made by `captures.mjs` |
| `donnees/` | the tutorials' data (simulated) and the R scripts that made them |
| `sources/fr/`, `sources/en/` | the home page, the installation and the tutorials, in Markdown |
| `construire.js` | builds the pages |
| `captures.mjs` | takes the screenshots |
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
node docs/site/captures.mjs [baci] [pls] [suivi] [--langue fr|en]
```

Starts the R engine (`moteur/serveur.R`, port 8793) and a headless Chrome or Edge (port 9333),
then goes through each tutorial in the browser version of Nuée, as a person would, in French with
the French data and in English with the English data, and writes `images/<langue>/`. It needs
Node 22 or later, R with the engine's packages (`RSCRIPT=…`) and Chrome or Edge (`CHROME=…`); with
Python and Pillow, the images are then reduced to 256 colours. With `NUEE_TEXTES=<folder>`, it also
writes each model's Methods and Results texts there (the tutorials quote them: when the numbers
change, update the Markdown sources). A change in the interface can break a step: the script says
which one.

The data are simulated, with fixed seeds: `Rscript docs/site/donnees/baci.R` (and `spectres.R`,
`suivi.R`) writes the same files again.

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
