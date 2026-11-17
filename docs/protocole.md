# Nuée's engine protocol

<!-- Written by docs/ecrire_protocole.R from moteur/protocole.json: change the description there, then run Rscript docs/ecrire_protocole.R (moteur/tests/verifier_protocole.R checks that this page is up to date). -->

Version **2.2** of the protocol by which the interface, and any other client (a plugin, a Python package, QGIS), talks to Nuée's R engine: requests in JSON over HTTP on 127.0.0.1, each carrying the session's token. This page says how to start the engine and reach it, then describes each of its 129 requests: its purpose, its parameters, its result, its errors, what it changes and the version it exists since.

It is written from `moteur/protocole.json`, the description that the request [`protocole`](#protocole) also gives (with `complet`); `moteur/tests/verifier_protocole.R` checks that every request the engine answers is described there, and nothing else. `outils/exemple_client.py` is a client in Python, with nothing but its standard library: it starts the engine from the sources, opens the example project and fits one of its models.

- [Versions](#versions)
- [Starting the engine](#starting-the-engine)
- [The port](#the-port)
- [The session's token](#the-sessions-token)
- [Language](#language)
- [Sending a request](#sending-a-request)
- [Answers](#answers)
- [Status codes](#status-codes)
- [Paths](#paths)
- [What a client does](#what-a-client-does)
- [Reading the description](#reading-the-description)
- [Types](#types), [Parameter sets](#parameter-sets), [Errors](#errors)
- [The requests](#the-requests): [Session and projects](#session-and-projects) · [Opening and importing](#opening-and-importing) · [Editing the data](#editing-the-data) · [Data steps and updates](#data-steps-and-updates) · [Versions and merges](#versions-and-merges) · [Defining and fitting models](#defining-and-fitting-models) · [Plugins](#plugins) · [A model's results and diagnostics](#a-models-results-and-diagnostics) · [Statistics and plots of the data](#statistics-and-plots-of-the-data) · [Exports](#exports) · [Documents and figures](#documents-and-figures) · [The R console](#the-r-console)

## Versions

The protocol's version is MAJOR.MINOR. MINOR grows when a request, a parameter or a field of a result is added: a client written for 2.0 works with every later 2.x. MAJOR grows when a request goes or changes its name, or a parameter or field changes its meaning or goes. Each request says the version it exists since (`depuis`), and so may a parameter or a field added after its request.

| Version | Changes |
|---|---|
| 1.0 | The requests of Nuée 1.0.0, before the protocol had a version: an engine that answers protocole with "Unknown request: protocole" speaks 1.0. |
| 2.0 | Nuée 1.1.0. New requests: protocole; retablir (Redo); the data steps, etapes, mettre_a_jour and retirer_etape; renommer_variable, supprimer_variable and type_variable; the versions of a project, fichier_change, fusionner and fusionner_finir; R-code models (the model type CODE) with empreinte_code, code_approuve and code_modeles; plugins (the model type PLUGIN) with greffons, greffon_lire, greffon_installer, greffon_retirer, greffon_projet and greffon_mettre_a_jour; resultats_cles and comparer_resultats; guide. New parameters: exemple's nom, ouvrir's lecture_seule and cache, enregistrer's auteur, reajuster's centrer, effets' echelle, ajouter_colonnes' colle, a model definition's meme_numero. New fields: Projet's retablissable, a_source and lecture_seule; ResumeModele's code_r and greffon; Modele's cles and an R-code or plugin model's own; annuler's documents, reajuster and a_reajuster, and retablir's; projets_modifies' ids; fusionner_finir's documents; RapportMiseAJour's changements_avant_etapes and its observations' distinguees; etat's code_interdit and greffons; error answers with fields of their own (arrete for a computation stopped, source_modifiee, code_non_approuve and empreinte, ligne, a plugin's refusal, a 409's fusion). Changed in a way a client of 1.0 notices: annuler keeps the model definitions added since the change (fitted models), which 1.0 took back with it; enregistrer without chemin no longer marks the project saved (it is a download); document_enregistrer and figure_enregistrer refuse an id the project doesn't have instead of making a new document or figure, and document_supprimer and figure_supprimer refuse it too; fermer_projet, rapports_vraisemblance, console and completer refuse an unknown project or model, which 1.0 ignored; exporter_prediction names its columns in the request's language; moyennes and puissance_tendance take only their own settings; an unknown model type or cross-validation is refused, not fitted as a PLS or validated by centres; an id is read as text (1 no longer names the first project); a missing projet or modele, an unknown observation and the choices the requests check get the engine's own messages instead of R's; most undo labels are in the request's language. |
| 2.1 | New request: paquet, the reproducibility package (a .zip of the project, its data as CSV with a data dictionary, one R script that redoes every model, its documents as reproducible Quarto files, a README, the licence of the data, CITATION.cff, codemeta.json and the SHA-256 of every file), with the DOI Zenodo reserved for the draft it goes in. New field: Projet's paquet, the answers kept from the last one. |
| 2.2 | New request: paquet_ouvrir, a reproducibility package opened again: unzipped in the engine's temporary folder, its files checked against its manifest, its project opened as a new project, with the results it deposited (ResultatsDeposes) to compare with its models fitted again. New parameter: paquet's qr, the QR code of its link. New fields: ouvrir's paquet_ouvrir (a reproducibility package, to open with paquet_ouvrir), Projet's depuis_paquet, the error answer's pas_un_paquet; paquet's .zip holds results/key_results.json (ResultatsDeposes), and with doi and qr, qr-code.svg. |

## Starting the engine

```
Rscript moteur/serveur.R <port> <interface folder>
```

The engine is the R script moteur/serveur.R of Nuée's sources (in an installed copy, the folder moteur of its resources). It finds Nuée's root from its own path, so it can be started from any working folder; it loads the engine (moteur/charger.R, every file of moteur/R) and the R console (moteur/console.R), then listens.

Both arguments are optional: the port, 8765 by default; the folder of the interface it serves at /, app/interface of the same tree by default.

It needs R 4.6 (the checks also run on R 4.1 to 4.3) with the packages jsonlite, httpuv, openxlsx, haven, readxl, RSQLite and ranger, and R's recommended packages foreign, nlme, mgcv, MASS, rpart and cluster.

Once it listens, it writes the line PRET &lt;port&gt; on its standard output: a client waits for it before its first request.

It listens on 127.0.0.1 only, and answers one request at a time (R has one thread): a long computation delays the requests that follow it.

Nothing stops it but its process: a client kills it when done. On Windows, bin\\Rscript.exe only starts bin\\x64\\Rscript.exe, which is R itself: start the latter, or stop the whole process tree (taskkill /T).

Its temporary files (pasted tables, converted .usp projects, files exported without a path, the console's plots) go in R's temporary folder, under TMPDIR, TMP or TEMP: the application gives it a folder of its own for each run and deletes it when Nuée closes.

Its environment:

| Variable | Meaning |
|---|---|
| `NUEE_JETON` | The session's token (see jeton). The engine removes it from its environment once read, so that neither the console nor the programs it starts inherit it. |
| `TMPDIR, TMP, TEMP` | Where R puts its temporary folder. |
| `CONVERTISSEUR_USP` | The compiled .usp converter; without it, outils/export_project.exe, else convertisseur/export_project.py run by Python. |
| `PYTHON` | The Python that runs the .usp converter when it isn't compiled (python, or python3 outside Windows). |
| `R_LIBS` | An installed copy points it at the packages it brings along (Windows, macOS); the application also starts R with --no-init-file there, so that the researcher's .Rprofile stays out. |
| `NUEE_CODE_SECRET` | A secret the application draws at each start, which it alone sends with code_approuve, in the header X-Nuee-Code: only it approves R code (R-code and plugin models); without it, code_approuve adds the fingerprints it is given. The engine removes it from its environment once read. |
| `NUEE_ARRET` | The file whose appearance stops a long computation (the application's Stop): see Answers. Without it, nothing stops. |
| `NUEE_SANS_CODE` | 1 when the organisation's policy DisableCustomCode turns R-code models off, and lets only its own plugins run (etat's code_interdit). |
| `NUEE_GREFFONS` | The user's plugins folder, where greffon_installer writes; without it, no plugin is installed. |
| `NUEE_GREFFONS_ORGANISATION` | The organisation's plugins folder (the policy PluginsFolder), read only. |
| `NUEE_VERSION` | Nuée's version, which a plugin's manifest may require (nuee); without it, nothing is checked. |

## The port

The first argument. The desktop application asks for 47863 when it is free (the interface keeps its settings per address), else any free port; a client chooses a free port of 127.0.0.1 itself and gives it.

Argument 1 of the command; 8765 by default; the application's: 47863.

## The session's token

The engine answers only the requests that carry the session's token: other web pages and other users of the computer reach 127.0.0.1 too, and the R console runs what it is sent.

The token is drawn by whoever starts the engine and given to it in NUEE_JETON: the application draws 32 random bytes, written as 64 hexadecimal characters. Started without NUEE_JETON, the engine draws its own and prints the address to open in a browser, http://127.0.0.1:&lt;port&gt;/api/connexion?jeton=&lt;token&gt;.

Each request carries it in the cookie nuee_&lt;port&gt; (cookies don't tell ports apart: one name per engine), in its Cookie header: Cookie: nuee_&lt;port&gt;=&lt;token&gt;. Without it, or with another value, the answer is 403 {"erreur": "Request refused: it doesn't come from Nuée's window."}.

The Host header must be 127.0.0.1:&lt;port&gt; or localhost:&lt;port&gt;; any other name gets 403 with the plain text interdit, even with the cookie (a site whose name points to 127.0.0.1, DNS rebinding).

Environment variable: `NUEE_JETON`. Cookie: `nuee_<port>`.

## Language

The header X-Langue: en gives the engine's messages and texts in English; without it, or with any other value, they are in French. A few requests take the language of the text they write as a parameter instead (methodes, resultats_texte, script_r, document_reproductible; guide and comparer_resultats take one too, the request's language by default).

Header: `X-Langue`, `en` (English) or `fr` (French); without it: `fr`.

## Sending a request

`POST` `/api/<demande>`

A request is POST /api/&lt;name&gt; with a JSON object of its parameters as body, in UTF-8; an empty body is a request without parameters. Neither the method nor the Content-Type is checked.

The engine reads the body with jsonlite::fromJSON(simplifyVector = TRUE): an array of numbers or strings becomes a vector, so a one-element array and its element are the same parameter; an array of objects becomes a table (a data.frame), which the requests that take a list of objects accept; an array of arrays becomes a matrix; an array mixing numbers and strings becomes strings; null is the same as leaving the parameter out.

Parameters the description doesn't list are ignored, though a request that changes the data keeps all its parameters but projet and apercu in its data step (etapes), saved with the project.

The engine reads most parameters with R's $, which also takes a parameter whose name starts with the one read when that one is left out (a few are read by their exact name). Send only the parameters a request takes.

## Answers

Content type: `application/json; charset=utf-8`.

The answer is written by jsonlite::toJSON(auto_unbox = TRUE, dataframe = "columns", na = "null", null = "null", digits = NA).

A vector of one element is written as that element: a field described as an array can arrive as a single value when it has one element, unless the engine keeps it an array (I() in its code). A table (data.frame) becomes an object of columns, each always an array; a matrix, an array of its rows; a missing value, NaN or an infinity, null; numbers keep all their digits.

Errors carry no code, only their message, said for a user in the request's language: a client tells them apart by their status and the fields some carry besides erreur, not by their words: conflit, date and fusion (409, enregistrer), arrete (a computation stopped), source_modifiee (retirer_etape), code_non_approuve and empreinte (an R-code model not approved here), ligne (an error in an R-code model's or a plugin's code), a plugin's refusal: greffon_non_approuve, empreinte, id and version; greffon_interdit; paquets_manquants and id; and pas_un_paquet (paquet_ouvrir: a .zip that isn't a reproducibility package). A malformed parameter often gets R's own message, untranslated. A few requests answer 200 with an error inside: series' tendances_erreur, raster_infos' erreur for a file it can't read, test_deux_variables' erreur for a pair it can't test.

Stop: the engine reads no request while it computes, so a client stops a long computation by writing the file NUEE_ARRET names (any content). The loops that look at it (permutations, cross-validation rounds, the AIC selection, boosting, simulations…: arret.R), five times a second at most, end the request at once: it answers 400 {"erreur": "Computation stopped.", "arrete": true}, the session put back as it was before the request (its projects, models, undo steps), the R console's log aside. The engine deletes the file when the next request comes, so that a Stop that came after the computation ended stops nothing.

## Status codes

| Status | Meaning |
|---|---|
| 200 | The request's result, in JSON. |
| 302 | /api/connexion with the right token: the cookie is set and the browser sent to the interface. |
| 400 | The request failed: {"erreur": message}, the message in the request's language (an R error, often one of the engine's own, said for the user). |
| 403 | Without the session's token: {"erreur": message}; code_approuve without the secret NUEE_CODE_SECRET in X-Nuee-Code, when the engine was started with one, likewise. With another host name: the plain text interdit. /api/connexion with a wrong token: the plain text jeton refusé. |
| 404 | A path outside /api/ that isn't a file of the interface: the plain text introuvable. |
| 409 | enregistrer only: the file changed elsewhere since it was read or written, {"erreur": message, "conflit": true, "date": its modification time, seconds since 1970, and "fusion": true when its changes can be merged instead (fusionner)}. |

## Paths

| Path | What |
|---|---|
| `/api/<demande>` | One of the requests below. |
| `/api/connexion?jeton=<token>[&...]` | For a browser (development): with the right token, sets the cookie (HttpOnly, SameSite=Strict, Path=/) and redirects (302) to /, keeping the address's other parameters (ouvrir=&lt;path&gt; opens a file). No cookie is needed. |
| `/api/fichier?chemin=<path>` | The bytes of a file the engine wrote in its temporary folder (an export asked without chemin), as an attachment; any other path is refused (400, File not allowed). The cookie is needed. |
| `/api/televerser?nom=<file name>` | The body is a file's bytes: kept in the engine's temporary folder under that name, then opened as ouvrir opens a file (its answer is ouvrir's). The cookie is needed. |
| `/api/deposer?nom=<file name>` | The body is a file's bytes: kept in the engine's temporary folder under that name; answers {"chemin": its path}, for a request that takes a path (predire_fichier, the formulaire of kobo_apercu…). The cookie is needed. |
| `/` | The files of the interface folder (the second argument), without the token: index.html, its scripts. Served with Cache-Control: no-cache. |

## What a client does

1. Find R and its Rscript: on Windows, Nuée's own (resources\\R\\bin\\x64\\Rscript.exe in an installed copy) or R's (%LOCALAPPDATA%\\Programs\\R\\R-4.6.2\\bin\\x64\\Rscript.exe for a user installation), elsewhere the one on the PATH.
2. Choose a free port of 127.0.0.1 and draw a token (32 random bytes in hexadecimal).
3. Start Rscript &lt;Nuée&gt;/moteur/serveur.R &lt;port&gt; with NUEE_JETON=&lt;token&gt; in its environment, and preferably TMPDIR, TMP and TEMP set to a new folder of the client's (and NUEE_ARRET, a file of that folder, to be able to stop a computation).
4. Read its standard output until the line PRET &lt;port&gt;; if the process stops first, what it wrote says why (a missing package, often).
5. Send each request as POST http://127.0.0.1:&lt;port&gt;/api/&lt;name&gt;, with the headers Cookie: nuee_&lt;port&gt;=&lt;token&gt;, Content-Type: application/json and X-Langue: en (or fr), and the parameters as a JSON object.
6. Read the answer: 200, the result; 400 or 409, {"erreur": …} said for a user (arrete: true for a computation stopped); 403, a wrong token or host.
7. Ask protocole first: its version tells which requests and parameters exist (a 400 Unknown request: the engine speaks 1.0).
8. Projects (P1, P2…) and fitted models (M1, M2…) live in the engine's session, under the ids its answers give: they last as long as the engine, unless saved with enregistrer.
9. Stop the engine's process when done, and delete the temporary folder.

## Reading the description

`moteur/protocole.json` holds `version`; `versions`, the changes of each; `groupes`, the sections of this page; `transport`, what the sections above say; `types`, the shapes several requests share; `ensembles`, the parameters several requests share; `erreurs`, the errors several requests raise; and `demandes`, each request by its name:

| Key | What |
|---|---|
| `groupe` | Its section of this page. |
| `but` | Its purpose, in one sentence. |
| `parametres` | Its parameters, by name: `type`, `requis` (whether it is required), `description`, and when they apply `valeurs` (each value allowed, with its meaning), `defaut` (the value taken when it is left out), `min`, `max` and `depuis`. |
| `ensembles` | The parameter sets it also takes; a parameter of its own of the same name takes the set's place. |
| `resultat` | Its result: `type`, and `champs`, its fields (`type`, `description`). With a named type, `champs` are the fields the request adds to it; with an array of objects, the fields of each. |
| `erreurs` | The errors it raises: the name of one of `erreurs`, or `{ message, quand }`, the English message (`<…>` stands for what the engine writes in) and when. |
| `effet` | What it changes: `aucun`, nothing; `seance`, the session but no project's content (a project opened or closed, a fitted model added, a result kept with a model, a cache); `projet`, a project's content, what saving writes, without an undo step; `annulable`, a project's content, with an undo step that `annuler` takes back. `effet_detail` says how, and on what condition. |
| `depuis` | The protocol version it exists since. |

A type is written as `string`, `number`, `integer`, `boolean`, `object`, `null`, `any` or the name of one of `types`, followed by `[]` for an array of it; `a|b` is either. An array of one element may arrive as that element (see [Answers](#answers)), unless its description says it is always an array.

## Types

### Type `Colonne`

A column of the project: a variable's number (its uid), or "id:k", the observations' k-th identifier ("id:1" their names; k as the rang of Projet's identifiants).

Written as `number` \| `string`.

### Type `Groupement`

What makes groups of observations, for a random effect, a cross-validation by groups or the time of an autocorrelation (regression.R: specs_groupement): an identifier of the observations by its name, or a variable by its number. Several arrive as an array of objects, a table for jsonlite.

| Field | Type | Meaning |
|---|---|---|
| `by` | `string` | "id" (an identifier) or "variable"; some requests also take "effets", the regression's random effects. |
| `id` | `string` | With by = "id": the identifier's name, as in Projet's identifiants (not "id:k"). |
| `variable` | `number` | With by = "variable": the variable's number. |
| `pentes` | `number[]` | A random effect's random slopes: the numbers of their variables. |

### Type `Fichiers`

The files an export wrote.

| Field | Type | Meaning |
|---|---|---|
| `chemin` | `string` | The file written, or the first of them. |
| `chemins` | `string[]` | Every file written (always an array): a format without sheets writes one file per table. |

### Type `Projet`

A project's summary (serveur.R: resume_projet): what the interface shows of it, with the ids requests take.

| Field | Type | Meaning |
|---|---|---|
| `id` | `string` | The project's id in the session (P1, P2…): the parameter projet of other requests. |
| `nom` | `string` | Its name: its file's name, a sheet's name in brackets, or the name it was given. |
| `source` | `string` | The name of the file or table its data came from ("" when unknown). |
| `modifie` | `boolean` | Whether it has changes not saved yet. |
| `chemin` | `string` \| `null` | The file it was opened from; null for a pasted table, an example, imported spectra or a summary by group. |
| `table` | `string` \| `null` | The sheet or table of that file it was read from. |
| `brut` | `boolean` | A KoboToolbox or ODK export opened as it is, not imported. |
| `annulable` | `string` \| `null` | The label of the change annuler would undo; null when there is nothing to undo. |
| `retablissable` | `string` \| `null` | The label of the change retablir would redo; null when there is nothing to redo. Since 2.0. |
| `revision` | `number` | Counts the changes of its data and their undos: a view that keeps a computation on the data computes it again when it changes. |
| `chemin_enregistrement` | `string` \| `null` | Where saving writes without asking: the .nuee it was saved to, or opened from. |
| `observations` | `object` | The observations, as columns: uid, their numbers, and nom, their names (a project of one observation gives single values). |
| `identifiants` | `object[]` | The observations' other identifiers (site, year…), each {rang, nom}: rang is the k of "id:k"; without the .usp files' internal ones ($ClassID…) and empty ones. |
| `variables` | `object[]` | The variables with data, each {uid, nom, qualitative, categories (always an array), manquantes (missing values), temps (a qualitative variable whose categories are all dates or numbers, usable as a time), unite, description}. |
| `jeux` | `object[]` | The datasets of a .usp project, each {nom, n_obs, n_var}. |
| `modeles` | [`ResumeModele`](#type-resumemodele)`[]` | The model definitions kept in the project. |
| `session` | `boolean` | Whether the project holds a session of the interface (its views and fitted models), saved with it. |
| `coordonnees` | `object` \| `null` | Its coordinates (spatial.R: coordonnees_projet): {x, y, the variables' numbers; systeme, an EPSG code or "plan"; nom_x, nom_y}; null when none are set or they no longer hold. |
| `documents` | `object[]` | Its documents, each {id, titre, langue}. |
| `figures` | `object[]` | Its figures, each {id, titre}. |
| `paquet` | `object` \| `null` | The answers of its last reproducibility package (paquet's meta), kept in the project and saved with it, as paquet cleans them: {titre, auteurs (each {prenom, nom, orcid}), description, mots_cles (always an array), licence}; null when it has none. Since 2.1. |
| `depuis_paquet` | `object` \| `null` | A project opened from a reproducibility package (paquet_ouvrir): {titre, doi (the package's own, null without), fichier (its .zip's name), manifeste (paquet_ouvrir's)}, kept for the session only, never saved; null otherwise. Since 2.2. |
| `kobo` | `object` \| `null` | The settings of the KoboToolbox or ODK import that made it, to make it again (kobo.R); null otherwise. |
| `a_source` | `boolean` | Whether its data came from a file it keeps as their source (a table opened, an export imported, an ODK archive's table opened as it is, the file of an update), which etapes describes and mettre_a_jour reads again. Since 2.0. |
| `lecture_seule` | `object` \| `null` | An old version opened read-only (ouvrir's lecture_seule): {source, oid, date}, as ouvrir was given them; null otherwise. Since 2.0. |
| `historique` | `number` | The number of events in the audit trail a .usp file kept (the request historique lists them). |
| `predictions` | `object[]` | The prediction sets of a .usp project, each {nom, n_obs}. |

### Type `Modele`

A fitted model's results (serveur.R: resultat_modele for a PCA, PLS, OPLS, PLS-DA or OPLS-DA; resultat_regression for a regression; resultat_&lt;type&gt; of another analysis, analyses.R). The common fields first; each family adds its own, as noted.

| Field | Type | Meaning |
|---|---|---|
| `id` | `string` | The fitted model's id in the session (M1, M2…): the parameter modele of other requests. |
| `projet` | `string` | Its project's id. |
| `nom` | `string` | Its name (M3, or the name it was given; a refit is "M3 (1)"…). |
| `titre` | `string` | Its title. |
| `type` | `string` | PCA, PLS, OPLS, PLS-DA, OPLS-DA, a regression (LM, LMM, GLM, GAM), another analysis (RDA, CA, CCA, LDA, TREE, KMEANS, INDVAL, TRIM, PCOA, NMDS, PERMANOVA, MANTEL, FOREST, BOOST, DISTANCE), an R-code model (CODE) or a plugin's model (PLUGIN). |
| `opls` | `boolean` | Whether it is an OPLS or OPLS-DA. |
| `A` | `integer` | PCA and PLS: its number of components (an OPLS: predictive and orthogonal); 0 for a regression or another analysis. |
| `Ap` | `number` | An OPLS's number of predictive components; 1 for a PCA or a PLS, 0 for a regression or another analysis. |
| `N` | `integer` | Its number of observations. |
| `K` | `integer` | Its number of X terms (a regression: the terms it kept; an analysis: its X columns). |
| `noms_y` | `string[]` | The names of its Y terms (always an array; empty for a PCA). |
| `observations` | `object` | A table, one row per observation of the model. PCA and PLS: t1…tA, u1…uA (PLS), T2, DModX, DModX_absolu, DModX_pondere, "&lt;y&gt; observé", "&lt;y&gt; ajusté", "&lt;y&gt; résidu" (PLS), classe, classe_predite, classe_predite_vc (PLS-DA), uid, nom. A regression: uid, nom, "&lt;y&gt; observé", "&lt;y&gt; ajusté", "&lt;y&gt; ajusté (effets fixes)" (a mixed model), "&lt;y&gt; prédit (VC)", residu, quantile_normal, levier and cook (LM), a column per random effect's level, groupe_vc. Another analysis: its own columns, with uid and nom. An R-code or plugin model: {uid, nom}, the observations given to its code. |
| `reglages` | `object` | Its settings in the form of the definition parameters, to modify it (nouveau.R: reglages_modele): type, x, y, classe, exclues, condition, echelles, blocs, modificateurs, transformations, A, G, vc, validation, aleatoires, correlation, famille, lisses, decalage, k_courbes, par_courbes, lisse_spatial, croises, croises_reglages, options, spectres, test, nom, titre. |
| `numero` | `number` \| `null` | Its definition's number in the project (ajuster's numero), when the project keeps it. |
| `app` | `boolean` | Whether its definition was made by Nuée (and joins the project), not read from a .usp file. |
| `note` | `string` | Its free note. |
| `test` | `object` \| `null` | With a test set (the definition's test): its predictions and statistics, {n, hors_domaine, observations, and for a PLS statistiques (y, n, RMSEP, biais, SEP, Q2ext) and classement (PLS-DA)} (jeu_test.R: evaluer_test; a regression's and an analysis's own). An RDA's or a CCA's test is its permutation test instead, {F, p, df, permutations}. |
| `demande` | `object` | The parameters it was fitted with, as the engine received them; {type, numero} for a model read from a file. |
| `G` | `integer` | PCA, PLS, regressions and the analyses validated by groups: the number of cross-validation groups. |
| `vc_par` | `string` | What made the cross-validation groups (an identifier's or a variable's name); "" when the observations were dealt in turn. |
| `composantes` | `object` \| `null` | PCA and PLS: a table, one row per component, composante, R2X, R2Xcum, R2Y and R2Ycum (PLS), Q2, Q2cum, valeur_propre; null for a regression. |
| `variables` | `object` | PCA and PLS: a table, one row per X term, terme, bloc, p1…pA, R2V_cum, Q2V_cum and p1_IC… (PCA), centre, poids, echelle, VIP, VIP_IC, w1, w\*1… and their \_IC (PLS); an RDA or a CA: its own. |
| `variables_y` | `object` \| `null` | PLS: a table, one row per Y term, terme, bloc, c1…cA and their \_IC, R2V_cum, Q2V_cum, centre, poids, RMSEE, RMSEcv. |
| `coefficients` | `object` \| `null` | PLS: a table, one row per X term then Constante: terme, and for each Y "CoeffCS &lt;y&gt;" (centred and scaled), "Coeff &lt;y&gt;" (in the data's units) and each one's "… IC" (jack-knife half-width). An R-code or plugin model: a table, one row per coefficient its fit gives, partie, terme, estimation, erreur_type, statistique, ddl, p, bas, haut (the 95 % interval); null without coefficients. |
| `limites` | `object` | PCA and PLS: the limits T2Crit(95%), T2Crit(99%) and, with more terms than components, DCrit(5%). |
| `RMSEE` | `object` | PLS, regressions and some analyses: by Y name, the root mean square error of estimation. |
| `RMSEcv` | `object` | By Y name, the root mean square error of cross-validation. |
| `seuil_vip` | `object` \| `null` | PLS: the VIP threshold found, {seuil, chute, marche, retenues}: the threshold, the drop at it, whether that drop is a clear step, the terms above it (statistiques.R: seuil_vip). |
| `variance_t` | `number[]` | PCA and PLS: each component's score variance. |
| `ellipse` | `object` | PCA and PLS: the squared radii of Hotelling's T² ellipse of a score plot, {95, 99}. |
| `validation` | `string` | PCA and PLS: how it was cross-validated, centres, sans_fuite or complet. |
| `gardes_vc` | `object` \| `null` | PCA and PLS validated sans_fuite: the terms that kept the full model's weight in a cross-validation sub-model, with the group and the share of their standard deviation they kept (validation_croisee.R: gardes_vc); null when none did. |
| `q2_vide` | `number` \| `null` | PLS and OPLS validated with recomputed centres: the Q2 of the empty model (each group predicted by the mean of the others). |
| `gain` | `number` \| `null` | The model's gain over the empty model, (Q2cum − q2_vide) / (1 − q2_vide). |
| `hierarchique` | `object` \| `null` | A PLS whose X are scores of other models: its honest Q2, each base model refitted without the hidden group (hierarchique.R: q2_hierarchique); null otherwise. |
| `stocke` | `object` \| `null` | The components a .usp file stored with the model, as arrays R2X, R2Xcum, R2Y, R2Ycum, Q2, Q2cum. |
| `version_origine` | `string` \| `null` | The version of the program that made a model read from a file. |
| `groupe` | `string` \| `null` | A class model (modeles_classe): its group's name. |
| `classe_modele` | `string` \| `null` | A class model: its class (the category it was fitted on). |
| `cv_anova` | `object` \| `null` | PLS: by Y name, its CV-ANOVA table {source, SS, DF, MS, F, p, SD}. |
| `classification` | `object` \| `null` | PLS-DA: its classification, fitted and cross-validated, {ajuste, vc}. |
| `regression` | `boolean` | true for a regression, false for another analysis; absent for a PCA or a PLS. |
| `transformation_y` | `string` | Regressions (and trees): the response's transformation, "" for none. |
| `coefficients_regression` | `object` | Regressions: a table, one row per coefficient, terme, estimation, erreur_type, ddl, t, p, bas, haut (the 95 % interval). |
| `termes_retires` | `string[]` | Regressions: the terms left out as redundant (a qualitative variable's reference category…). |
| `termes_absents` | `string[]` | Regressions: the combinations of categories never observed. |
| `qualite` | `object` | Regressions: the fit's quality, r2 and r2_ajuste (LM), marginal and conditionnel (mixed), or deviance_expliquee (GLM, GAM), then AIC, AICc, BIC, logLik, parametres, Q2, RMSEcv, echecs_vc, sans_categorie_vc, facteurs_vc; trees and some analyses: their own. |
| `famille` | `string` | Regressions: the response's distribution (gaussienne, poisson…). |
| `lien` | `string` | Regressions: its link (identity, log, logit…). |
| `parametre_loi` | `object` \| `null` | Regressions with a distribution that has one: its estimated parameter (the negative binomial's θ, Tweedie's p, the zero-inflated Poisson's θ1 and θ2, the beta's φ). |
| `courbes` | `object` \| `null` | A GAM: a table of its curves (edf, test). |
| `decalage` | `string` \| `null` | GLM and GAM: the effort variable's name, with an offset. |
| `dispersion` | `number` \| `null` | A Poisson model: its Pearson dispersion (χ² over the residual degrees of freedom). |
| `gamm` | `boolean` | Regressions: fitted by mgcv::gamm (a normal GLM or GAM with random effects and CAR1). |
| `dans` | `string` \| `null` | With CAR1 in a finer level than the random effects: that level's name. |
| `variances` | `object` \| `null` | Mixed models: a table of the random effects' variances, composante, terme, variance, correlation. |
| `phi` | `number` \| `null` | With CAR1: its estimated correlation φ. |
| `niveaux` | `string[]` | Regressions: the random effects' levels, the widest first. |
| `temps` | `string` \| `null` | With CAR1: the time's name. |
| `exclues_manquantes` | `integer` | Regressions and analyses: the observations left out for a missing value. |
| `pentes` | `object[]` | Regressions: the terms with a random slope, an array per random effect's level. |
| `parts_variance` | `object` \| `null` | Mixed models: each level's share of the variance. |
| `singularites` | `object` \| `null` | Mixed models with random slopes: the signs of a singular fit. |
| `qualitatives` | `string[]` | Regressions: the qualitative variables, those with adjusted means (moyennes). |
| `signature` | `string` | Regressions: what the AIC can compare (the response, its transformation, the observations, the kind of distribution and likelihood): two models of the same signature have comparable AICs. |
| `analyse` | `boolean` | Another analysis: true. |
| `prediction` | `boolean` | Another analysis: whether it predicts new observations (predire). |
| `chiffres` | `string` | Another analysis: its line of figures for the explorer ("R2 0.70 · Q2 0.52 · 3 leaves"). |
| `vues_defaut` | `string[]` | Another analysis: the views the interface opens first. |
| `cles` | [`ResultatCle`](#type-resultatcle)`[]` | Another analysis, an R-code or a plugin model: its key results as resultats_cles gives them, which a document cites ({{M1.&lt;key&gt;}}); absent when they can't be computed. Since 2.0. |
| `code_r` | `boolean` | An R-code or plugin model: true. Since 2.0. |
| `n_donnees` | `integer` | An R-code or plugin model: the number of observations given to its code (N is its fit's nobs() when it has one). Since 2.0. |
| `code` | `string` | An R-code model: its code, as it ran; a plugin's: the call, &lt;function&gt;(donnees, entrees, options). Since 2.0. |
| `classe` | `string[]` | An R-code or plugin model: the R class of its fit (always an array, empty when its code returned no model). Since 2.0. |
| `reponse` | `string` \| `null` | An R-code or plugin model: its fit's response, when R gives it. Since 2.0. |
| `resume` | `string[]` | An R-code or plugin model: what summary() of its fit prints, line by line (always an array). Since 2.0. |
| `sortie` | `string[]` | An R-code or plugin model: what its code printed, 3,000 lines at most (always an array). Since 2.0. |
| `messages` | `string[]` | An R-code or plugin model: its code's messages (always an array). Since 2.0. |
| `avertissements` | `string[]` | An R-code or plugin model: its code's warnings (always an array). Since 2.0. |
| `autres` | `string[]` | An R-code or plugin model: the other names of the list its code returned, besides modele, valeurs and tableaux (always an array). Since 2.0. |
| `images` | `string[]` | An R-code or plugin model: its code's plots, 20 at most, each a data:image/png;base64 address (always an array). Since 2.0. |
| `statistiques` | `object` | An R-code or plugin model: those of nobs, logLik, ddl, AIC and BIC its fit gives ([] when its code returned no model). Since 2.0. |
| `ajustees` | `object` \| `null` | An R-code or plugin model: {uid, nom, ajuste, residu}, its fitted values and residuals by observation; null when R gives none. Since 2.0. |
| `qq` | `object` \| `null` | An R-code or plugin model: {theorique, residu, pente, ordonnee}, the normal Q–Q plot of its residuals and its line; null without residuals. Since 2.0. |
| `valeurs` | `object` \| `null` | An R-code or plugin model: {nom, valeur}, the named numbers its code returned (list(modele, valeurs, tableaux)); null without. Since 2.0. |
| `tableaux` | `object[]` | An R-code or plugin model: the tables its code returned, each {nom, n (its rows), colonnes, valeurs (one array per column, its first 1,000 rows)} (always an array). Since 2.0. |
| `paquets` | `object` | An R-code or plugin model: the packages its code uses, as columns nom and version (null for one not installed). Since 2.0. |
| `R` | `string` | An R-code or plugin model: the version of R it ran on. Since 2.0. |
| `comparaison` | `object` | An R-code or plugin model: {AIC, signature} when its AIC can be compared with another fit's (the same signature), else {AIC, raison} or {raison}. Since 2.0. |
| `greffon` | `object` | A plugin's model: its plugin, {id, version, empreinte, nom, description, licence, auteurs, citations, fonction, nuee, fichiers (their paths), manifeste ({entrees, options}, as Greffon), reglages ({entrees, options}: what was chosen), installe ({version, empreinte, source}, null without one)}. Since 2.0. |

### Type `ResumeModele`

A model definition kept in a project (serveur.R: resume_modele).

| Field | Type | Meaning |
|---|---|---|
| `numero` | `number` \| `null` | Its number in the project (3 for M3): the parameter numero of ajuster. |
| `nom` | `string` | Its name (M3, or the name it was given). |
| `titre` | `string` | Its title. |
| `type` | `string` | PCA, PLS, OPLS, PLS-DA, OPLS-DA, a regression (LM, LMM, GLM, GAM), another analysis (RDA, CA, CCA, LDA, TREE, KMEANS, INDVAL, TRIM, PCOA, NMDS, PERMANOVA, MANTEL, FOREST, BOOST, DISTANCE), an R-code model (CODE) or a plugin's model (PLUGIN). |
| `A` | `number` | Its number of components (0: chosen when fitted, or none). |
| `ajuste` | `boolean` | Whether it was fitted (in the file it came from, or by Nuée). |
| `erreur` | `string` \| `null` | Why it couldn't be fitted, when it couldn't. |
| `n_obs` | `integer` | Its number of observations. |
| `x` | `string[]` | The names of its X terms (without a regression's factors that have no main effect). |
| `y` | `string[]` | The names of its Y terms. |
| `stocke` | `object` \| `null` | The components a .usp file stored with it, as arrays: R2X, R2Xcum, R2Y, R2Ycum, Q2, Q2cum; null without them. |
| `note` | `string` | Its free note. |
| `version_origine` | `string` \| `null` | The version of the program that made it, for a model from a file. |
| `code_r` | `object` \| `null` | An R-code model or a plugin's: {empreinte, approuve}, the fingerprint of its code (the plugin's) and whether it is approved on this computer, so that it runs (code_approuve); null for another model. Since 2.0. |
| `greffon` | `object` \| `null` | A plugin's model: its plugin (greffons.R: resume_greffon_modele), {id, version, empreinte, valide, approuve, nom, auteurs (their names), interdit (the DisableCustomCode policy keeps it from running here), installe ({version, empreinte, source}: the installed plugin of that id; null without one)}; {valide: false, approuve: false, empreinte: null} when the definition has no copy of it; null for another model. Since 2.0. |

### Type `EtatFichier`

What the engine knows of a project's file when it read or wrote it, to see before writing over it whether it changed since (serveur.R: etat_fichier). What the engine gives may carry two fields more, which ouvrir doesn't read back.

| Field | Type | Meaning |
|---|---|---|
| `chemin` | `string` | The file's path, made absolute, with / as separator. |
| `taille` | `number` | Its size in bytes. |
| `date` | `number` | Its modification time, in seconds since 1970. |
| `copie` | `string` | The engine's copy of the file as the project read or wrote it, in its temporary folder: the base of a merge (fusionner); absent without one. Since 2.0. |
| `sha256` | `string` | That copy's SHA-256, kept once fichier_change compared contents; absent before. Since 2.0. |

### Type `EntreeJournal`

An entry of the R console's log (console.R: noter); the engine keeps the last 1,500 to 2,000.

| Field | Type | Meaning |
|---|---|---|
| `n` | `integer` | Its number: entries are numbered from 1 in the order they are written. |
| `type` | `string` | appel (an engine call the interface made, written in R), resultat (what R prints of a model just fitted), commande (a console command), sortie (its output), avertissement (a warning), erreur (an error, a console command's or a request's), image (a plot). |
| `heure` | `string` | The time it was written, hh:mm:ss. |
| `texte` | `string` | Its text, lines joined by \\n; for an image, a data:image/png;base64 address. |

### Type `ApercuKobo`

The preview of the import of a KoboToolbox or ODK Central export with given settings (kobo.R: apercu_collecte), what the import dialog shows.

| Field | Type | Meaning |
|---|---|---|
| `outil` | `string` | "kobo" or "odk". |
| `nom_outil` | `string` | "KoboToolbox" or "ODK Central". |
| `format` | `string` | The file's extension (xlsx, csv, zip…). |
| `fichier` | `string` | The file's name. |
| `tables` | `object[]` | The export's tables, the submissions table first, each {nom, lignes (its rows), parent (its parent table's name, null for the submissions table), principale (whether it is the submissions table), niveau (how many parent tables it has above it)}. |
| `table` | `string` | The table this preview imports. |
| `lignes` | `integer` | Its number of rows. |
| `colonnes` | `integer` | The number of columns the import gives. |
| `principale` | `boolean` | Whether that table is the submissions table. |
| `metadonnees` | `object[]` | The metadata that can be kept, one per role present in the table or its parent tables, each {role, colonnes (always an array), garde (whether it is kept), nom (the role in words, in the request's language)}. |
| `garder` | `string[]` | The roles kept (always an array). |
| `retirees` | `string[]` | The metadata columns dropped (always an array). |
| `parents` | `object[]` | One per parent table of the imported table, nearest first, each {table, colonnes (the columns its import gives, which can be joined; always an array), choisies (those joined; always an array)}. |
| `formulaire` | `object` \| `null` | The XLSForm form given: {fichier, titre, langues (its label languages, "" for the label column; always an array), langue (the one used)}; null without a form. |
| `coordonnees` | `boolean` | Whether the import gives the project coordinates (a GPS point, in WGS 84). |
| `phrases` | `string[]` | What the import does, one sentence per change, in the request's language (always an array). |

### Type `Coordonnees`

A project's coordinates as they are set and still hold (spatial.R: coordonnees_projet): two different quantitative variables and their system.

| Field | Type | Meaning |
|---|---|---|
| `x` | `number` | The number of the variable that holds x (the east, or the longitude). |
| `y` | `number` | The number of the variable that holds y (the north, or the latitude). |
| `systeme` | `integer` \| `string` | Their system: an EPSG code Nuée knows (4326, 2154…), or "plan" for plane coordinates in metres of an unstated system. |
| `nom_x` | `string` | The name of the x variable. |
| `nom_y` | `string` | The name of the y variable. |

### Type `FichierLu`

A file a project read, as it was read and where it stands now (etapes.R: etat_fiche): the source of its data, a step's table or raster, an import's XLSForm form.

| Field | Type | Meaning |
|---|---|---|
| `chemin` | `string` | Its absolute path when it was read, with / as separator. |
| `relatif` | `string` \| `null` | Its path from the folder of the project's file ("donnees/releves.csv", "../releves.csv"), written when the project is saved; null before, or on another drive or server. |
| `trouve` | `string` \| `null` | The file that would be read now: by relatif from the folder of the project's file first (a folder moved with its data), then by chemin; null when it is at neither. |
| `existe` | `boolean` | Whether it is found (trouve isn't null). |
| `taille` | `number` \| `null` | Its size in bytes when it was read. |
| `date` | `string` \| `null` | Its modification time when it was read, the computer's local time, YYYY-MM-DDThh:mm:ss. |
| `sha256` | `string` | Its SHA-256 when it was read, in hexadecimal ("md5:" and its MD5 when an R older than 4.5 read it); "" when unknown. |
| `lu` | `string` | When it was read, local time, YYYY-MM-DDThh:mm:ss; "" when unknown. |
| `modifie` | `boolean` \| `null` | Whether it changed since it was read: another size, or another modification time and a content whose fingerprint differs or can't be compared (a date alone changed, a copy, doesn't count); null when it isn't found. |
| `actuel` | `object` | Only when it is found: {taille, date}, its size and modification time now, written as taille and date. |

### Type `RapportMiseAJour`

What an update of a project's data changes, or would change (etapes.R: mise_a_jour, aligner_projet): the data the file and the steps give, compared to the project's by the names of the variables and the identifiers of the observations, what becomes of the models, the steps replayed and the file read.

| Field | Type | Meaning |
|---|---|---|
| `observations` | `object` | {ajoutees: the number of observations new in the file; retirees: the number of the project's that it no longer has; premieres_ajoutees, premieres_retirees: the names of the first ten of each (always arrays); cellules: the cells that changed in the observations and variables both have; cellules_par_variable: one per variable with changed cells, each {variable: its name, cellules} (always an array); par_position: whether the observations are matched by their place, no column naming them; repetees: how many observations of a same name nothing tells apart, matched by their rank among those of their name (on the side that has more of them); distinguees: the columns that tell apart the observations of a same name, by which they are matched (a year, a date, a survey number: identifiers and qualitative or whole-number variables both sides have, chosen one by one while one tells more apart; always an array, empty when names don't come back)}. |
| `variables` | `object` | {nouvelles: the names of the file's variables the project hasn't; absentes: the project's that the data no longer have, removed; recalculees: the computed variables missing from the file, computed again from their formula (each always an array); categories: one per qualitative variable with new categories, each {variable, nouvelles (always an array)}, placed just before its last; types: one per variable whose type changed in the file and was given back, each {variable, type: the type kept, qualitative (the file's numbers read as categories) or quantitative (its text read as numbers), perdues: the values that weren't numbers, now missing}; both always arrays}. |
| `identifiants` | `object` | {nouveaux: the observations' identifiers (secondary ID columns) new in the file; absents: the project's that the file doesn't have, without the .usp files' internal ones and the empty ones}, always arrays. |
| `modeles` | `object` | {variables_absentes: the models that use a variable or an identifier the data no longer have, which can't be fitted again, each {nom, numero, variables: those missing (always an array)}; observations_choisies: the models that left out rows they could fit and to which the file brings rows they could fit, to ask whether they take them (mettre_a_jour's sans_nouvelles), each {nom, numero, observations: the rows they fit now, admises: those they could fit outside their test set, nouvelles: the file's new rows they would fit}; sans_nouvelles: the names of the models that don't take the new observations, those of sans_nouvelles and those whose cross-validation groups came from a .usp file (always an array); nouvelles_categories: the models that take a term for a new category, each {nom, numero, variable, categories (always an array)}; references_changees: the regressions whose reference category changes anyway, their last one having no rows left, each {nom, numero, variable, avant, apres: the categories, null for none}}; every list an array, numero null for a model without a number. |
| `etapes` | `object[]` | One per step replayed, in order (always an array): {n: its number among those replayed; demande; etiquette, in the request's language; statut: ok, sautee (skipped: a variable, an observation or a category it names is missing, or nothing is left to do) or erreur; raison: why, "" for ok; notes: what a step done in part left out, the observations or categories it names that the data don't have (always an array)}. A skipped step doesn't stop the others. |
| `fichier` | `object` | The file read: {chemin: its absolute path, with /; table: its sheet or table, null for none; lecteur: how it was read, tableau, archive or collecte, as etapes' source says}. |
| `notes` | `string[]` | What to know of the file's reading (values read as missing…), always an array. |
| `changements_avant_etapes` | `boolean` | Whether the project is one from before 1.1 whose changes then aren't steps (etapes' conformes null): the update replaces its data with what the file and the steps give, those changes included, and the project is no longer such a project after it. Since 2.0. |

### Type `Conflit`

A clash of a merge (fusion.R: conflit_fusion): something both sides changed differently since the base, or that one side removed and the other changed. fusionner_finir's choix settles it: what clashes is then taken from the side chosen (a model's definition or a figure whole, a document's clashing passages and fields, a variable's clashing cells, and its fields for a variable new on both sides; a thing removed on one side kept or removed).

| Field | Type | Meaning |
|---|---|---|
| `id` | `string` | Its id, the key of choix, the same when the same merge is prepared again: modele:&lt;model&gt;, document:&lt;id&gt;, figure:&lt;id&gt;, variable:&lt;variable's key&gt;, source; or champ:categories:&lt;variable's key&gt; (the order of its categories), champ:observations:leur and champ:observations:mien (observations removed on that side and changed on the other), champ:variable:&lt;variable's key&gt;:&lt;field&gt; (a variable's unite, description, formula…), champ:coordonnees, champ:prediction:&lt;set&gt;, champ:&lt;field&gt; (another field of the project). |
| `genre` | `string` | modele, document, figure, variable (a variable's or an identifier's values, or a variable removed on one side and changed on the other), source (both sides updated the data from different files) or champ (anything else, as its id says). |
| `cle` | `string` | What it names: the model's name, the document's or figure's id, a variable's key (its name, followed by U+001F and its rank when several variables have it; for an identifier of the observations, U+001D, id: and its name), &lt;variable's key&gt;.&lt;field&gt;, observations, coordonnees, source, the prediction set's name or the project field's. |
| `titre` | `string` | Its title, in the request's language: "Model M3", "Document “Report”", "Figure “Map”", "Variable “x1”", "Identifier “site”", "Order of the categories of “zone”", "Unit of “x1”", "Observations removed on their side, changed on mine", "Coordinates", "Data source", "Prediction set “2024”", "Project field “format”"… |
| `mien` | `string` | What my side did, in a few words, in the request's language: "M3: 2 component(s) → 4 component(s); X: + x5", "paragraph 3 rewritten", "unit: kg; 2 cell(s): S1: 4 → 5, S2: 3 → empty", "data updated from releves.csv", "changed", "removed"… |
| `leur` | `string` | What their side did, likewise. |
| `details` | `object` \| `null` | What else to show, by genre; null when there is nothing more (a document or figure removed on one side and changed on the other, a variable's field, the coordinates, a prediction set, a project field). modele: {champs}, the definition's fields changed both ways (x, y, A, condition…), or ["retire"] when one side removed the model. document: {passages, each {ligne (the number in my text of its first paragraph, each line that isn't empty being one, as the editor shows them; where it would be, when I removed it), base, mien, leur (its lines in each version, joined by \\n)}; champs, its titre, langue, bib or style changed both ways}. figure: {champs}, titre or contenu. variable: {n, the number of cells changed both ways; cellules, the first 100, each {observation (its name; "S1 (2)" for the second of that name), base ("" when the base didn't have that observation), mien, leur: values as text, numbers to 6 significant digits with the request's decimal mark, "empty" for a missing value}}, with champs (the fields that differ) for a variable new on both sides and made differently, or {champs} alone when only its fields differ; {retiree: "mien" \| "leur"}, the side that removed the variable the other changed. source: {mien, leur}, the names of the files each side updated the data from. champ, the order of a variable's categories: {base, mien, leur}, the categories common to the three in each one's order; observations removed and changed: {observations}, their names. Every array in details stays an array. |

### Type `Greffon`

A plugin as the engine shows it (greffons.R: greffon_interface): who it is, where it comes from, whether it is well formed and may run here, its packages, and, from a manifest without errors, its card and the inputs and options its models' dialog asks (vues_greffons.js); those fields from the manifest are absent when it has errors.

| Field | Type | Meaning |
|---|---|---|
| `id` | `string` \| `null` | Its id, as its manifest writes it, even a manifest with errors (a folder that can't be read: the folder's name); null when the manifest has no id in text. |
| `version` | `string` \| `null` | Its version as its manifest writes it (X.Y.Z when valid); null when it has none in text. |
| `source` | `string` | utilisateur (the user's plugins folder), organisation (the organisation's), fichier (a file greffon_lire read), projet (the copy a project's model keeps). |
| `dossier` | `string` \| `null` | Its folder, for the user's and the organisation's; null otherwise. |
| `etat` | `string` | ok; invalide (it has errors: erreurs; it doesn't run); double (a folder of the same source listed before it has its id: this one isn't used); remplace (the user's, of an id the organisation also gives: the organisation's is used). A file's or a project's copy: ok or invalide. |
| `empreinte` | `string` \| `null` | Its fingerprint, the SHA-256 of its files in a fixed order (64 hexadecimal characters; greffons.R: empreinte_greffon), what code_approuve approves; null for a folder that couldn't be read. |
| `permis` | `boolean` | Whether it may run on this computer: the organisation's (its fingerprint), or approved here (code_approuve) while the DisableCustomCode policy is off. |
| `trop_recent` | `boolean` | Whether its manifest asks for a newer Nuée than this one (nuee). |
| `erreurs` | `string[]` | What is wrong with it, each saying where and why ("inputs[2].accepts: …"), in the request's language (always an array, empty when it is well formed). |
| `avertissements` | `string[]` | What is worth saying without being wrong (texts without French) (always an array). |
| `paquets` | `object` | The R packages its manifest asks for, as columns, each always an array: nom; present, whether the R Nuée uses has it; version, its version (null when absent). |
| `nom` | `object` | Its name, {en, fr} (fr: the English when the manifest has no French). |
| `description` | `object` | Its description, {en, fr}. |
| `licence` | `string` | Its licence. |
| `nuee` | `string` | The version of Nuée it needs at least, X.Y.Z. |
| `fonction` | `string` | The name of its R function, called &lt;fonction&gt;(donnees, entrees, options). |
| `auteurs` | `object[]` | Its authors, each {nom, orcid (without https://orcid.org/; null without)}. |
| `citations` | `object[]` | How to cite its method, each {texte, bibtex (null without)} (always an array, empty without). |
| `entrees` | `object[]` | The project's variables its dialog asks for, each {id, type (variable: one; variables: several), accepte (quantitative, qualitative or any), min, max (null: no maximum), facultatif, libelle ({en, fr}), aide ({en, fr}; null without)} (always an array). |
| `options` | `object[]` | Its settings, each {id, type (number, choice, checkbox or text), libelle, aide, defaut, and for a number min, max (null without) and entier, for a choice valeurs ([{valeur, libelle}])} (always an array). |
| `fichiers` | `object[]` | greffon_lire and greffon_projet only: its files, each {chemin, texte}, plugin.json then the R files its manifest lists. |

### Type `ResultatCle`

A key result of a fitted model (resultats_cles.R: entree_resultat): a number, or a category, under the key a document cites and What changed compares; a field it doesn't have is absent.

| Field | Type | Meaning |
|---|---|---|
| `cle` | `string` | Its key, the same from one fit to another and in both languages: the reference a document cites ({{M1.Q2}}: Q2; {{M1.coef.annee}}: coef.annee); unique in a summary. |
| `libelle` | `string` | What it is, in the request's language, without an article ("Cumulative Q²", "Coefficient of “annee”"). |
| `groupe` | `string` | ajustement (the fit's statistics), estimations, tests, aleatoires (random effects), composantes (per component), variables (per variable: VIP, coefficients, loadings), especes (indicator species), moyennes (adjusted means), puissance (trend power), jeu_test (the test set), stocke (results a .usp file stored). |
| `valeur` | `number` | The number; absent for an entry that is a category only. |
| `ic` | `number[]` | Its 95 % interval, [low, high]. |
| `p` | `number` | Its p-value. |
| `n` | `number` | Its count (the observations, presences or test set observations it rests on). |
| `limite` | `number` | An estimate's null value, 0 (1 for a multiplicative slope, an index, a ratio): crossing it is changing sign, and without p an interval that excludes it makes it significant. |
| `repere` | `number` | A threshold of the value itself (1 for a VIP). |
| `forme` | `string` | pourcentage (a share, written in %) or entier. |
| `genre` | `string` | f when its label is feminine in French, for the French sentences. |
| `alias` | `string[]` | The other references a document can cite for it (est.annee, ic.annee and p.annee for coef.annee; &lt;key&gt;.ic, .bas, .haut, .p, .pourcent… for another analysis's entry) (always an array). |
| `axe` | `string` | The component of a value whose sign is arbitrary (a PCA loading: "1", "2"…), which comparer_resultats turns the other fit's way. |
| `classe` | `string` | A category that goes with the value or makes the entry: TRIM's trend class, an indicator species' group, the detection function chosen, the most important variable, the first split's. |
| `classe_texte` | `string` | That category in words, when its code doesn't read (TRIM's class). |

### Type `ResultatsDeposes`

A reproducibility package's key results, its file results/key_results.json (resultats/resultats_cles.json in a French package; paquet.R: json_resultats_paquet): written by paquet from the latest fit of each model in the session, read back by paquet_ouvrir, its numbers to 15 significant digits.

| Field | Type | Meaning |
|---|---|---|
| `format` | `number` | The file's format: 1. |
| `nuee` | `string` | The version of Nuée that wrote it (paquet's version). |
| `R` | `string` | R's version, X.Y.Z. |
| `plateforme` | `string` | R's platform (x86_64-w64-mingw32, aarch64-apple-darwin20, wasm32-unknown-emscripten…). |
| `paquets` | `object` | Every R package loaded then but R's own (base, stats, utils…), its version under its name. |
| `date` | `string` | When it was written, YYYY-MM-DD. |
| `langue` | `string` | The package's language, that of the labels: fr or en. |
| `doi` | `string` \| `null` | The package's own DOI (paquet's doi), null without. |
| `modeles` | `object[]` | Each model, in the order paquet got them: {nom, numero, titre, type (ResumeModele's: PCA, OPLS-DA, LM…), A (its fit's number of components, which a fit again takes; 0 for a regression or another analysis), entrees (ResultatCle[], as resultats_cles gives them, in the package's language)}, or, for a model the package doesn't redo (its categories changed since its fit, not fitted: paquet's non_ajustes), {nom, numero, titre, type, erreur} (always an array). |

## Parameter sets

### Set `definition`

Taken by [`verifier`](#verifier), [`formule`](#formule), [`ajuster`](#ajuster), [`modeles_classe`](#modeles_classe).

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `type` | `string` | yes | The model's type; another value is refused (Unknown model type). verifier also takes CLASSES (the class models' PCA, without the class). Values: `PCA` (principal component analysis), `PLS` (partial least squares regression), `PLS-DA` (PLS discriminant analysis, its Y the categories of classe), `OPLS` (orthogonal PLS, as many predictive components as Y), `OPLS-DA` (orthogonal PLS-DA, as many predictive components as classes less one), `LM` (linear model (lm)), `LMM` (linear mixed model (nlme::lme)), `GLM` (generalized linear model (mgcv)), `GAM` (generalized additive model (mgcv)), `RDA` (redundancy analysis), `CA` (correspondence analysis), `CCA` (canonical correspondence analysis), `LDA` (linear discriminant analysis), `TREE` (decision tree (rpart)), `KMEANS` (k-means), `INDVAL` (indicator species), `TRIM` (trends and indices of counts), `PCOA` (principal coordinates analysis), `NMDS` (non-metric multidimensional scaling), `PERMANOVA` (permutational analysis of variance), `MANTEL` (Mantel test), `FOREST` (random forest (ranger)), `BOOST` (boosted regression trees), `DISTANCE` (distance sampling), `CODE` (a model written in R by the user (code.R): its options' code, run when approved on this computer), `PLUGIN` (a model of a plugin (greffons.R): its options' greffon, entrees and options). |
| `x` | `number[]` | no | The X variables' numbers (a qualitative variable enters by its categories). A PCA, PLS or OPLS needs at least one; a regression without X is the intercept-only model; another analysis reads the roles it needs. With spectres, they are taken in the table's order. |
| `y` | `number[]` | no | The Y variables' numbers: PLS and OPLS (one or more), a regression (exactly one, quantitative), an analysis with responses (RDA, CCA…). A PLS-DA takes classe instead. |
| `classe` | `number` | no | PLS-DA and OPLS-DA: the qualitative variable whose categories are the classes (only the observations of a known class enter); LDA and a classification tree: their class variable. |
| `exclues` | `number[]` | no | The observations left out of the model (unticked), by their numbers (Projet's observations.uid). |
| `echelle` | `string` | no | The scaling of the terms that have none in echelles. Values: `UV` (centred and divided by the standard deviation), `Par` (Pareto: centred and divided by the square root of the standard deviation), `Ctr` (centred only), `none` (neither centred nor scaled). Default: `"UV"`. |
| `echelles` | `object` | no | By variable, its scaling: {"&lt;variable number&gt;": "UV" \| "Par" \| "Ctr" \| "none"}. |
| `A` | `integer` | no | PCA, PLS and OPLS: the number of components; 0 chooses it by the automatic fit (nouveau.R: autoajuster, cross-validation and the published rules). An OPLS has at least its predictive ones. Ignored by regressions and the other analyses. At least 0. Default: `0`. |
| `nom` | `string` | no | The model's name; by default M&lt;n + 1&gt;, n the number of definitions in the project. A definition of the project with that name is replaced by this one (Model › Modify model). |
| `titre` | `string` | no | Its title. Default: `""`. |
| `G` | `integer` | no | Without vc: the number of cross-validation groups, the observations dealt to them in turn. At least 2. Default: `7`. |
| `transformations` | `object` | no | By variable, its transformation: {"&lt;variable number&gt;": "racine2" \| "racine4" \| "log10" \| "log10:C2" for log10(x + C2) \| "ln:C1:C2", "neglog:C1:C2" (−log10), "exp:C1:C2", "lineaire:C1:C2" or "log10:C1:C2" of C1·x + C2 \| "logit:C1:C2" between the bounds C1 &lt; C2 \| "puissance:C1:C2:C3" for (C1·x + C2)^C3, "puissance:C3"}. A qualitative variable isn't transformed. |
| `croises` | `number[][]` | no | Cross terms and powers, each the numbers of two or more X variables: [[3, 5], [4, 4]] for v3·v5 and v4². A qualitative variable enters once at most, by each of its categories. In a regression, a factor that isn't in x enters without a main effect (y \~ a + a:b). |
| `croises_reglages` | `object` | no | By cross term, named by its variables' numbers in order ("3\*5"): {echelle, bloc, modificateur}; without it, the default scaling, neither block nor modifier. |
| `spectres` | `object[]` | no | Spectral preprocessing, applied in order to each observation's quantitative X: {type: "snv"}, {type: "msc"}, {type: "sg", derivee (0), fenetre (11: odd, 3 or more), degre (2)}, {type: "detrend", degre (2)}, {type: "ligne_base"} (spectres.R). Not for a regression. |
| `test` | `number[]` | no | The observations of a test set: left out of the model, then predicted by it (Modele's test). The analyses that don't predict refuse one. |
| `vc` | [`Groupement`](#type-groupement) | no | What makes the cross-validation groups: an identifier ({by: "id", id: its name}) or a variable ({by: "variable", variable: its number}), one group per value; absent, or {by: "order"}: G groups in turn. A mixed model without it is validated by its first random effect. |
| `blocs` | `object` | no | By variable, the name of its block: block weighting, each term's weight divided by the square root of its block's number of terms; in a regression, only the groups of the contributions table (apports). |
| `modificateurs` | `object` | no | By variable, a factor multiplying its weight (1: none). |
| `aleatoires` | [`Groupement`](#type-groupement)`[]` | no | Regressions: the random effects, nested from the widest to the finest, three at most, each an identifier or a qualitative variable; its pentes: the quantitative X variables (terms on their own) with a random slope at that level. An LMM needs one, an LM takes none. |
| `correlation` | `object` | no | Regressions with random effects: {type: "CAR1", temps: a Groupement, the time (an identifier or a quantitative variable), dans: a Groupement, a finer level than the last random effect (optional)}: a CAR1 autocorrelation within those groups, a single time per group. A GLM or GAM takes it only with the normal distribution (mgcv::gamm). |
| `famille` | `string` | no | GLM and GAM: the response's distribution. Values: `gaussienne` (normal), `poisson` (Poisson (counts)), `binomiale_negative` (negative binomial (counts)), `binomiale` (binomial (0 or 1)), `gamma` (gamma (positive values)), `tweedie` (Tweedie (zero or more)), `poisson_zi` (zero-inflated Poisson), `beta` (beta (proportions strictly between 0 and 1)). Default: `"gaussienne"`. |
| `lisses` | `number[]` | no | GAM: the quantitative X variables fitted as smooth curves, s(x). |
| `decalage` | `number` | no | GLM and GAM with a log link (poisson, binomiale_negative, gamma, tweedie, poisson_zi): the effort variable, offset(log(effort)). |
| `k_courbes` | `object` | no | GAM: by curve's variable, its basis dimension k, a whole number from 3 to 100 (10 at most without it). |
| `par_courbes` | `object` | no | GAM: by curve's variable, the number of a qualitative X variable that is also a term of the model: a curve per category, s(x, by = f). |
| `lisse_spatial` | `object` | no | GAM: a spatial smooth of two quantitative coordinates, {variables: [x, y], base: "tp" (thin plate, the default) or "sos" (on the sphere, longitudes and latitudes in degrees), k: a whole number from 5 to 300}. |
| `options` | `object` | no | Another analysis's own settings, which its ajuster_&lt;type&gt; reads (analyses.R). An R-code model (CODE): {code}, its R code, a text or an array of lines, whose last value is the fitted model (or list(modele, valeurs, tableaux)); it sees the model's observations as donnees, one column per variable. A plugin's model (PLUGIN): {greffon: {id, empreinte (a version by its fingerprint; by default the installed one)}, entrees: {&lt;input id&gt;: variable numbers}, options: {&lt;option id&gt;: value}}, checked against its manifest (Greffon's entrees and options). Both ignore x, y, the scalings, A and the cross-validation, and take no test set. |
| `condition` | `string` | no | Keep only the observations where this R expression is true (condition.R: the project's variables and identifiers, numbers, text and a fixed list of functions), evaluated again at each fit; a missing value counts as false. |
| `validation` | `string` | no | PCA, PLS and OPLS: how cross-validation is done; another value is refused (Unknown cross-validation). Values: `centres` (only the centres recomputed without the hidden group), `sans_fuite` (centres and weights recomputed; a term nearly constant without the group keeps the full model's weight), `complet` (the full model's centres and weights, as the .usp files validate). Default: `"centres"`. |
| `meme_numero` | `number` | no | The number of a model of the project that this definition replaces, under its name or another (an R-code model whose code was changed, Model › Change the model's code): the new definition keeps that number, and so its figures, the references of documents and its place in the session follow it; the model of that number, or of the definition's name, is replaced. Not kept in the definition. Since 2.0. |

### Set `import_kobo`

Taken by [`kobo_apercu`](#kobo_apercu), [`kobo_importer`](#kobo_importer).

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `table` | `string` | no | The export's table to import, as ApercuKobo's tables name it; the submissions table by default. |
| `garder` | `string[]` | no | The roles of the metadata to keep; the other metadata columns are dropped. Values: `envoi` (the submission date and time), `auteur` (the submitter), `identifiant` (the submission ID), `releve` (the start, end and day of the survey), `validation` (the validation status). Default: `[]`. |
| `parents` | `object` | no | For each parent table of the imported table, by its name, the columns of its import to join to each row (as ApercuKobo's parents list them); a parent table left out joins all its columns, and without parents every one does. |
| `formulaire` | `string` | no | The path of the XLSForm form (.xlsx, .xlsm, .xls) the export was made with, which gives the labels, the categories' order and the multiple choices' 0/1 columns; none when absent or "". |
| `langue` | `string` | no | With formulaire, the language of its labels, one of the form's ("" for its label column); by default the request's language if the form has it, else its default_language, else its label column, else its first language. |

## Errors

Errors several requests raise. Each comes as status 400 with `{"erreur": message}`, unless another status is given; the message is in the request's language, and its words between `<…>` are filled in by the engine.

| Name | Message (in English) | When |
|---|---|---|
| `projet_inconnu` | Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. |
| `modele_inconnu` | Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. |
| `modele_perime` | The categories of &lt;variable&gt; changed since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a category the model saw was renamed, merged or emptied since, or a variable it used is no longer qualitative (serveur.R: modele with a_jour, atelier.R: aligner_categories). |
| `regression_seulement` | Only for a regression. | The model isn't a regression (LM, LMM, GLM, GAM). |
| `composantes_seulement` | This isn't for this model: a regression or another analysis doesn't have the components and scores of a PCA or a PLS. | A request that needs components and scores (serveur.R: SANS_REGRESSION: hca, permutations, scores_en_variables, appartenance, contributions, spectres, biplot, ipls) names a regression or another analysis in modele or modeles; checked before the request runs. |
| `variable_introuvable` | Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). |
| `identifiant_inconnu` | Unknown identifier: &lt;id:k&gt; | "id:k" names no identifier of the observations (variables.R: colonne_ou_identifiant). |
| `format_inconnu` | Unrecognised file format: .&lt;extension&gt; | The file's extension isn't one Nuée reads. |
| `pas_de_tableau` | The clipboard holds no table (at least two rows and two columns) | The pasted text has no tab, semicolon or comma, or a single line. |
| `demande_inconnue` | Unknown request: &lt;name&gt; | /api/&lt;name&gt; names no request of the engine (an engine older than the request: see depuis). |
| `jeton_refuse` | Request refused: it doesn't come from Nuée's window. (status 403) | The request doesn't carry the session's token in the cookie nuee_&lt;port&gt;. |
| `fichier_introuvable` | File not found: &lt;path&gt; | The file named doesn't exist. |
| `fichier_occupe` | Couldn't replace &lt;path&gt;: another program is using it (sync, antivirus, the file open elsewhere), or it is read-only. The file was left as it was; try again, or save under another name. | The file just written couldn't take the place of the old one within five seconds (nouveau.R: ecrire_sans_risque). |
| `tableau_vide` | The table is empty | The table read has no row or no column (nouveau.R: projet_depuis_tableau). |
| `valeurs_decalees` | &lt;lines&gt;: more values than the first line has columns (&lt;number&gt;). The values would be shifted: a value may contain the separator (&lt;separator&gt;) or a quote, or a column name is missing from the first line. | Lines of a delimited text have more values than its first line has names (formats.R). |
| `pas_un_export` | &lt;file&gt; isn't a KoboToolbox or ODK Central export: it doesn't have their columns (\_id, \_uuid, \_submission_time, \_index…; SubmissionDate, KEY, meta-instanceID…). Open it with File › Open. | The file was given as an export but doesn't have two of their metadata columns (kobo.R: kobo_refus_export). |
| `formulaire_pas_export` | &lt;file&gt; is an XLSForm form (survey and choices sheets), not the data: give it as the form, beside the export of the data. | A workbook with survey and choices sheets was given as the export. |
| `zip_sans_csv` | &lt;file&gt; holds no CSV file: it isn't an ODK Central export. | A .zip without a CSV file (outside media/) was given as an ODK Central export. |
| `table_export_inconnue` | Unknown table in the export: “&lt;table&gt;” (it has: “&lt;table&gt;”, …). | table isn't one of the export's tables. |
| `metadonnee_inconnue` | Unknown metadata: &lt;role&gt; (one can keep: envoi, auteur, identifiant, releve, validation). | garder names a role that isn't one of these. |
| `pas_un_formulaire` | &lt;file&gt; isn't an XLSForm form: an Excel workbook with a “survey” sheet (and “choices”) is needed. | formulaire isn't an Excel workbook with a survey sheet. |
| `langue_formulaire_inconnue` | The form doesn't have the language “&lt;language&gt;”; it has: “&lt;language&gt;”, …. | langue isn't one of the form's label languages. |
| `formulaire_sans_rapport` | The form &lt;file&gt; doesn't match this export: none of its questions is a column of the table “&lt;table&gt;”. | The form given isn't the one the export was made with. |
| `table_parente_inconnue` | “&lt;table&gt;” isn't a parent table of “&lt;table&gt;” (its parent tables: “&lt;table&gt;”, …). | parents names a table that isn't a parent of the imported table. |
| `colonne_parente_inconnue` | The table “&lt;table&gt;” has no column “&lt;column&gt;”. | parents names a column that the parent table's import doesn't give. |
| `sans_coordonnees` | This project has no coordinates: choose them in Plots › Map (two variables and their system). | The request needs the observations' coordinates and the project has none set, or they no longer hold: a variable gone or made qualitative (spatial.R: coordonnees_projet with exiger). |
| `variable_sans_donnees` | Variable without data: &lt;number&gt; | A variable's number isn't a column of the project's data (variables.R: modifier_valeur, atelier.R: coller_cellules). |
| `nom_variable_manquant` | Give the new variable a name | nom is empty, or only spaces. |
| `nom_variable_pris` | A variable is already called “&lt;name&gt;” | nom, without the spaces around it, is the name of one of the project's variables. |
| `format_export_inconnu` | Unrecognised export format: .&lt;extension&gt; | The extension of chemin (or format) isn't one the engine writes: xlsx, csv, tsv, txt, rds, rdata, rda, sav, dta, xpt, omv, dbf, arff (formats.R: ecrire_tableau). |
| `classe_requise` | A PLS-DA needs a class variable | A PLS-DA or OPLS-DA definition without classe. |
| `echelle_inconnue` | Unknown scaling: &lt;scaling&gt; | echelle or a value of echelles isn't UV, Par, Ctr or none. |
| `transformation_inconnue` | Unknown transformation: &lt;transformation&gt; | A value of transformations isn't one of the forms Nuée reads (nouveau.R: lire_transformation). |
| `croise_hors_x` | A cross term can only use X variables | A cross term of a PCA, PLS or OPLS uses a variable that isn't in x. |
| `condition_refusee` | "&lt;function&gt;" isn't allowed in a condition: comparisons (==, !=, &lt;, &gt;=…), &amp;, \|, !, %in%, is.na(), ifelse(), arithmetic (+, log10()…) and the group and lag functions of New variable formulas (group_mean(), previous()…). | The condition calls another function; it may also be unreadable, name an unknown variable or not give one logical value per observation (condition_observations lists those messages). |
| `condition_aucune` | None of the &lt;n&gt; ticked observations meets the condition &lt;condition&gt;. | The condition keeps none of the model's ticked observations. |
| `sans_test` | This analysis doesn't predict left-out observations: it takes no test set. | A test set for an analysis that doesn't predict (RDA, CA…). |
| `regression_une_reponse` | A regression needs a single Y variable, a quantitative one. | A regression's y is empty, has several variables or a qualitative one. |
| `mixte_sans_aleatoire` | A mixed model needs at least one random effect. | An LMM without aleatoires. |
| `loi_inconnue` | Unknown distribution of the response. | famille isn't one of the distributions Nuée fits. |
| `vc_un_groupe` | Cross-validation by "&lt;groups&gt;": the model's observations have a single value, so a single group. At least two are needed. | vc's identifier or variable has one value on the model's observations. |
| `aucune_variation` | No component is possible: the data have no variation (constant variables, or all zero after the spectral preprocessing). | A PCA, PLS or OPLS whose data leave nothing to describe. |
| `trop_de_composantes` | Component &lt;a&gt; is not possible: no variation is left to describe after &lt;n&gt; component(s). This model can have at most &lt;n&gt;. | A was set above what the data allow. |
| `ne_predit_pas` | This analysis doesn't predict new observations. | The model is an analysis that doesn't predict (Modele's prediction false: RDA, CA, PCoA…). |
| `temps_refuse` | The time (&lt;time&gt;) must be numbers (the year) or dates (2019-05-14, 14/05/2019)&lt;, not both&gt;: &lt;why&gt; | A value of the time is neither a number nor a date, or numbers and dates are mixed (variables.R: temps_en_nombres, refus_temps); &lt;why&gt; names the first value refused: “&lt;value&gt;” is a number among dates, a date among numbers, isn't a date that exists, or is neither a number nor a date. |
| `test_inconnu` | Unknown test: &lt;test&gt; | test isn't one of the request's tests. |
| `cle_en_double` | &lt;level&gt; has several rows for the same value of &lt;key&gt; (&lt;values&gt;): the pairing key must identify one row per level | In a paired test, one of the two levels compared has two rows with the same key (comparaisons.R: paires_cle). |
| `permutations_requises` | These tests need permutations: choose a number of them (Edit model). | The RDA or CCA was fitted without permutations (its options' permutations is 0). |
| `rda_seulement` | Only for an RDA or a CCA. | modele isn't a redundancy analysis (RDA) or a canonical correspondence analysis (CCA). |
| `kmeans_seulement` | Only for k-means. | modele isn't a k-means model (KMEANS). |
| `projet_manquant` | Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). |
| `modele_manquant` | Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). |
| `variables_changees` | &lt;variable&gt; is called &lt;name&gt; since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a variable it used was renamed since its fit; one deleted gets “&lt;variable&gt; is no longer in the data since &lt;model&gt; was fitted: it can't be fitted again without it (Ctrl+Z brings it back).”, one turned into categories or numbers “&lt;variable&gt; is &lt;categories\|numbers&gt; since &lt;model&gt; was fitted: fit it again (…)” (atelier.R: variables_changees). |
| `type_inconnu` | Unknown model type: &lt;type&gt;. | type isn't one of the definition's types (nouveau.R: definir_modele). |
| `calcul_arrete` | Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. |
| `code_interdit` | R-code models are turned off on this computer by your organisation's policy (DisableCustomCode). | The engine was started with NUEE_SANS_CODE=1 (etat's code_interdit) and the request defines, fits or exports an R-code model, or asks a code's fingerprint (code.R: refuser_si_interdit). |
| `code_non_approuve` | Model &lt;name&gt; has R code written elsewhere, not approved on this computer: it doesn't run until you have read and approved it (Show the code, Run it). | An R-code model's code isn't among the fingerprints approved on this computer (code_approuve), whoever wrote it (code.R: exiger_code_permis, before anything runs it). Besides erreur, the answer carries code_non_approuve: true and empreinte, the code's fingerprint, to approve before fitting again. |
| `erreur_code` | The R code stopped at line &lt;n&gt;: &lt;R's message&gt; | An R-code model's code stopped with an error or doesn't parse, or a plugin's code failed (code.R: erreur_code; greffons.R: erreur_greffon). Besides erreur, the answer carries ligne, the line of the top-level expression that stopped, null when unknown ("The R code stopped: &lt;R's message&gt;", a last value NULL, a plugin's error, whose file and line are in the message). A missing package adds how to install it. |
| `greffon_refuse` | Model &lt;name&gt; is made with the plugin "&lt;plugin&gt;" &lt;version&gt;, which isn't installed or approved on this computer: it doesn't run until you have read and installed it (Show, Install, in the bar above the project). | A plugin's model is fitted, predicts or is exported to R, and its plugin's fingerprint isn't approved here (besides erreur: greffon_non_approuve: true, empreinte, id, version); under the DisableCustomCode policy, a plugin that isn't the organisation's gets “Model &lt;name&gt; is made with the plugin "&lt;plugin&gt;" &lt;version&gt;: on this computer, your organisation's policy (DisableCustomCode) only lets the plugins it provides run.” (greffon_interdit: true); a fit without the packages it needs “The plugin "&lt;plugin&gt;" needs packages that aren't installed for the R Nuée uses (R &lt;version&gt;): &lt;packages&gt;. Install them (File › Plugins…, or install.packages(…) in R), then fit again.” (paquets_manquants, always an array, and id) (greffons.R: exiger_greffon_permis, exiger_paquets_greffon). |
| `source_introuvable` | The source file isn't found: &lt;path&gt; | The project's source file (etapes' source) is neither at its path from the folder of the project's file nor at its absolute path, &lt;path&gt; (etapes.R: fichier_retrouve). |
| `formulaire_introuvable` | The import's XLSForm form isn't found: &lt;path&gt; | The source is a KoboToolbox or ODK import made with an XLSForm form, and the form is neither at its relative nor at its absolute path. |
| `colonne_observations_absente` | The table no longer has the column “&lt;column&gt;”, which named the observations: they can't be matched. | The file read lacks the column that named the observations when the source was first read (its decisions, nouveau.R: projet_depuis_tableau). |
| `lecteur_inconnu` | Unknown file reader: &lt;reader&gt; | The project's source is read in a way this version doesn't know (a project saved by a newer Nuée). |
| `greffon_sans_manifeste` | No plugin.json in &lt;folder&gt;. | chemin is a folder without plugin.json. |
| `pas_un_greffon` | This isn't a plugin: a .nueeplugin is a zip archive holding plugin.json. | chemin is a file, other than a plugin.json, that isn't a zip archive, or an empty one. |
| `archive_hors_dossier` | The archive has paths outside its folder: it is refused. | A path in the archive is absolute or goes up (..). |
| `archive_trop_grande` | The archive is too large for a plugin (5 MB, 500 files at most). | The archive holds more than 500 entries, or more than 5 MB once unpacked. |
| `archive_sans_manifeste` | The archive has no plugin.json at its root (nor in a single folder). | Neither the archive's root nor exactly one of its folders holds plugin.json. |
| `fichier_greffon_refuse` | &lt;file&gt;: 1 MB at most. | A file of the plugin read (its plugin.json, or any .R file of its folder, even one its manifest doesn't list) is over 1 MB; one with a null byte gets “&lt;file&gt;: it isn't a text file.”, one that isn't UTF-8 “&lt;file&gt;: it isn't in UTF-8.”. |

## The requests

### Session and projects

[`protocole`](#protocole) · [`version`](#version) · [`etat`](#etat) · [`projet`](#projet) · [`projets_modifies`](#projets_modifies) · [`donnees`](#donnees) · [`valeurs`](#valeurs) · [`historique`](#historique) · [`session`](#session) · [`modeles`](#modeles) · [`annuler`](#annuler) · [`retablir`](#retablir) · [`enregistrer`](#enregistrer) · [`sauvegarde_secours`](#sauvegarde_secours) · [`projet_texte`](#projet_texte) · [`fermer_projet`](#fermer_projet) · [`guide`](#guide)

#### `protocole`

Gives the protocol's version, Nuée's version and the names of the requests the engine answers, and with complet this whole description.

Since 2.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `complet` | `boolean` | no | true to also get the full description (moteur/protocole.json) in description. Default: `false`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `version` | `string` | The protocol's version, MAJOR.MINOR ("2.1"). |
| `nuee` | `string` \| `null` | Nuée's version (app/package.json); null when the engine runs without the application's files. |
| `demandes` | `string[]` | The names of the requests this engine answers, sorted (always an array). |
| `description` | `object` | With complet: this description, as moteur/protocole.json holds it. |

**Errors:** none of its own.

#### `version`

Gives the versions of R and of the R packages the engine runs on, and the system it runs on.

Since 1.0. Changes nothing.

No parameters.

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `R` | `string` | R.version.string ("R version 4.6.2 (…)"). |
| `plateforme` | `string` | R's platform (x86_64-w64-mingw32…). |
| `systeme` | `string` | The operating system's version, as R gives it ("" when unknown). |
| `locale` | `string` | R's LC_CTYPE locale. |
| `paquets` | `object` | The version of each of jsonlite, httpuv, openxlsx, haven, readxl, foreign, RSQLite, rmarkdown and knitr, "-" for one that isn't installed. |

**Errors:** none of its own.

#### `etat`

Gives everything the session has open, each project's summary and each fitted model's results, as a new window of the application needs them.

Since 1.0. Changes nothing.

No parameters.

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `projets` | [`Projet`](#type-projet)`[]` | The open projects but the hidden ones (ouvrir's cache), in the order they were opened (always an array). |
| `modeles` | [`Modele`](#type-modele)`[]` | The models fitted in the session but those of hidden projects, in the order they were fitted (always an array). |
| `code_interdit` | `boolean` | Whether R-code models are turned off on this computer by the organisation's policy (DisableCustomCode, given to the engine as NUEE_SANS_CODE=1): the interface then doesn't offer them. Since 2.0. |
| `greffons` | `object` | The plugins, for the interface's menus, as the request greffons gives them (greffons.R: greffons_interface): {dossier, organisation (the user's and the organisation's plugin folders, null without one), interdit (the policy of code_interdit), installation (whether plugins can be installed here), liste (each plugin found, always an array)}. Since 2.0. |

**Errors:** none of its own.

#### `projet`

Gives an open project's summary.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |

**Result:** [`Projet`](#type-projet).

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |

#### `projets_modifies`

Gives the names and ids of the open projects that have changes not saved yet, which the application asks before closing, and its recovery copies every two minutes.

Since 1.0. Changes nothing.

No parameters.

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `noms` | `string[]` | Their names (always an array, empty when every project is saved). |
| `ids` | `string[]` | Their ids, in the same order (always an array). Since 2.0. |

**Errors:** none of its own.

#### `donnees`

Gives a project's data table for the Data view: its observations, its variables and the values of those asked.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `colonnes` | `number[]` | no | The numbers of the variables whose values to send (a view of spectra sends only the first ones, or those its filter keeps); the others come as null. Without it, all of them. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `observations` | `string[]` | The observations' names, in the data table's order. |
| `uid_observations` | `number[]` | Their numbers, in the same order. |
| `variables` | `string[]` | The names of the variables that have data. |
| `uid_variables` | `number[]` | Their numbers, in the same order. |
| `valeurs` | `any[]` | One element per variable, in the same order: its values for each observation (numbers; a qualitative variable's category names; null for a missing value), or null for a variable colonnes leaves out. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |

#### `valeurs`

Gives the values of one variable, or of one identifier of the observations, for the observations asked.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `variable` | `number` | no | The variable's number; required unless identifiant is given. |
| `identifiant` | `integer` | no | Instead of a variable, the rank k of an identifier of the observations (Projet's identifiants' rang; 1 for their names): a number, not "id:k". |
| `observations` | `number[]` | yes | The numbers of the observations whose values to give, in the order wanted; without it, no value. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `variable` | `number` | The variable's number (absent for an identifier). |
| `nom` | `string` | The variable's name (absent for an identifier). |
| `valeurs` | `any[]` | The value of each observation asked, in their order: a number, a category's name or an identifier's text; null for a missing value or an observation the project doesn't have. |
| `qualitative` | `boolean` | Whether the values are text: a qualitative variable, or an identifier (always true). |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |

#### `historique`

Gives the audit trail a .usp project's file kept, one row per event.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |

**Result:** `object`, with the fields below. A table as columns, each always an array, one row per event of the file's work sessions; {} when the file kept no audit trail (projet.R: historique_projet).

| Field | Type | Meaning |
|---|---|---|
| `date` | `string[]` | When it happened, YYYY-MM-DD hh:mm when the file's day/month/year or month/day/year date could be read, else as the file writes it. |
| `logiciel` | `string[]` | The program of the work session. |
| `utilisateur` | `string[]` | Its user. |
| `ordinateur` | `string[]` | Its computer. |
| `modele` | `number[]` | The number of the model the event names ("model no. 4"), null when it names none. |
| `evenement` | `string[]` | The event's text. |
| `details` | `string[]` | Its details. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |

#### `session`

Gives the interface's session kept with a project (its views and fitted models), as the JSON text the interface wrote.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `session` | `string` \| `null` | The session, JSON text only the interface reads; null when the project holds none. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |

#### `modeles`

Lists the models fitted in the session, with their project and name.

Since 1.0. Changes nothing.

No parameters.

**Result:** `object[]`, each with the fields below. One object per fitted model, in the order they were fitted (always an array, empty when none is fitted).

| Field | Type | Meaning |
|---|---|---|
| `id` | `string` | The fitted model's id (M1, M2…). |
| `projet` | `string` | The id of its project. |
| `nom` | `string` | Its definition's name, or its id when it has none. |

**Errors:** none of its own.

#### `annuler`

Undoes the last change of a project that has an undo step, as Edit › Undo (Ctrl+Z) does.

Since 1.0. Changes the project, without an undo step. The project goes back to its state before its last change that has an undo step (20 are kept per project), its data source and data steps included; that step goes to the redo stack (retablir, Edit › Redo; 20 kept too), which the next change with an undo step empties. Its documents, figures and the state of its file stay as they are now, and it counts as modified, but for two changes: undoing a merge (fusionner_finir) gives back the documents, figures and file state it had before the merge when they haven't changed since, and undoing a renamed variable puts back the document references that followed it, even in a document written since. The session's models whose observations came from a condition of the project follow its condition back. A model defined since the change (fitted, which has no undo step of its own) stays in the project, its observations and categories brought to the data back, unless it reads a variable or a category the undo takes away (a computed variable, the BACI variables, merged categories): it goes with them, and Redo brings it back; it keeps its number unless a model back has it. A model the change itself added (baci_modele's mixed model, the other side's in a merge, the console's) goes with the change. A variable number the undo frees isn't given again to another variable.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `projet` | [`Projet`](#type-projet) | The project as it was before that change. |
| `etiquette` | `string` | The label of the change undone ("variable ratio"…), as Projet's annulable gave it. |
| `documents` | `string[]` | The ids of the project's documents whose text the undo changed, for the interface to read again (always an array; empty unless the change undone was a merge or a renamed variable). Since 2.0. |
| `reajuster` | `boolean` | Whether the change undone replaced the project's data (a data update, a removed step: mettre_a_jour, retirer_etape): every model of the session made from the project is then to be fitted again on the data as they now are, as the interface does in each of its windows. Since 2.0. |
| `a_reajuster` | `number[]` | The numbers of the project's models whose fit in the session is on variables that are no longer the project's once the change is undone (a name given back, a variable removed or retyped, a category renamed): reajuster now refuses to fit them as the session has them, so a client fits them again from the project (ajuster with numero). Always an array, sorted. Since 2.0. |

**Errors:**

| Message (in English) | When |
|---|---|
| Nothing to undo | The project has no undo step left (also for an id that is no project's). |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| &lt;R's own message&gt; | projet is left out: the undo stack is read before the project, so the answer is R's attempt to select less than one element in get1index, untranslated, not projet_manquant. |

#### `retablir`

Redoes the last change of a project that Edit › Undo took back, as Edit › Redo (Ctrl+Y or Ctrl+Shift+Z) does.

Since 2.0. Changes the project, without an undo step. The project takes back the state the last undo took it from: its data, source, steps and model definitions as the undo step kept them (so a definition added or changed since the undo, fitted or with a note written, leaves nothing to redo: such a change empties the redo stack, as a change with an undo step does; fitting one of the project's models again by its number, ajuster with numero, doesn't). Its documents, figures, the state of its file and where it saves stay as they are now, but a merge's come back while unchanged since the undo, and a renamed variable's references in the documents follow the new name again; it counts as modified, and the session's models whose condition came from their definition follow it. The entry moves from the redo stack to the undo stack, so Edit › Undo takes it back again, in one step; the redo stack keeps 20 entries per project, empties at any new change that has an undo step (memoriser) and goes when the project closes.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `projet` | [`Projet`](#type-projet) | The project as the change redone left it: its annulable that change again, its retablissable the next one to redo. |
| `etiquette` | `string` | The label of the change redone ("data update", "sugar renamed brix"…), as Projet's retablissable gave it, written when the change was made. |
| `documents` | `string[]` | The ids of the documents whose text the redo changed (a renamed variable's references, a merge's documents), for a client to read them again (always an array). |
| `reajuster` | `boolean` | Whether the change redone replaced the project's data (a data update, a removed step: mettre_a_jour, retirer_etape): every model of the session made from the project is then to be fitted again on the data as they now are, as the interface does in each of its windows. Since 2.0. |
| `a_reajuster` | `number[]` | The numbers of the project's models whose fit in the session is on variables that are no longer the project's once the change is redone (a name given back, a variable removed or retyped, a category renamed): reajuster now refuses to fit them as the session has them, so a client fits them again from the project (ajuster with numero). Always an array, sorted. Since 2.0. |

**Errors:**

| Message (in English) | When |
|---|---|
| Nothing to redo | The project has no change to redo: none was undone since its last change that has an undo step (which empties the redo stack); also for an id that is no project's, or a project closed since. |
| &lt;R's own message&gt; | projet is left out: the redo stack is read before the project, so the answer is R's attempt to select less than one element in get1index, untranslated, not projet_manquant. |

#### `enregistrer`

Saves a project as a .nuee file, with the interface's session.

Since 1.0. Changes the project, without an undo step. With chemin, the file is written first in the engine's temporary folder, then copied into place, replacing an old one only once the new one is whole; that copy stays, the base of a later merge (fusionner). The project then counts as saved, later saves go there, the file's state is kept to see a change made elsewhere, the paths of its data source and step files are written relative to the file's folder, and an old version opened read-only becomes a project like any other. Without chemin, only the file to download is written, and the project stays modified or not as it was. The session given is kept with the project.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `chemin` | `string` | no | The file to write (a .nuee). Without it, &lt;project's name&gt;.nuee in the engine's temporary folder, for a browser to fetch with /api/fichier: a download, which doesn't count as saving the project. |
| `session` | `string` | no | The interface's session (its views and fitted models), JSON text only it reads: kept with the project and saved with it. |
| `ecraser` | `boolean` | no | true to write even when chemin is the project's file and that file changed elsewhere since the project read or wrote it. Default: `false`. |
| `version` | `string` | no | Nuée's version, written in the file (its nuee field) with R's and the packages'. Default: `""`. |
| `auteur` | `string` \| `object` | no | Who saves, as the settings of the versions name them: a name, or {nom, email}, of which only nom is kept. Written in the file (its nuee field's auteur, without the spaces around it), so that a colleague who opened the same file sees who changed it (fichier_change, fusionner); without it, or empty, the file names nobody. Since 2.0. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `chemin` | `string` | The file written. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| This old version is read-only: Save as keeps a copy elsewhere. | The project is an old version opened read-only (ouvrir's lecture_seule) and chemin is the temporary file it was read from. |
| Folder not found: &lt;folder&gt; | chemin's folder doesn't exist. |
| &lt;file&gt; was changed elsewhere since you opened or saved it. | chemin is the file the project was read from or last saved to, it changed since (its size or modification time), and ecraser isn't true. The answer is HTTP 409: {"erreur", "conflit": true, "date": the file's modification time in seconds since 1970, and "fusion": true when the engine kept a copy of the file as the project read or wrote it, so that its changes can be merged (fusionner) rather than written over}. |
| Couldn't replace &lt;path&gt;: another program is using it (sync, antivirus, the file open elsewhere), or it is read-only. The file was left as it was; try again, or save under another name. | The file just written couldn't take the place of the old one within five seconds (nouveau.R: ecrire_sans_risque). ([`fichier_occupe`](#errors)) |
| Couldn't write &lt;chemin&gt; | The file, written first in the engine's temporary folder, can't be copied into chemin's folder (a folder that can't be written). |
| &lt;R's own message&gt; | The file can't be written otherwise ("cannot open the connection", untranslated). |

#### `sauvegarde_secours`

Writes a recovery copy of a project without changing its state.

Since 1.0. Changes nothing. Writes the copy at chemin, the paths of the project's data source and step files relative to the project's own folder; the project, its session and its unsaved state stay as they are.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `chemin` | `string` | yes | The copy to write (a .nuee), in the interface's recovery folder. |
| `session` | `string` | no | The interface's session, JSON text, written in the copy only. |
| `version` | `string` | no | Nuée's version, written in the copy. Default: `""`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `chemin` | `string` | The copy written, as given. |
| `lu` | [`EtatFichier`](#type-etatfichier) \| `null` | The state of the project's file when the project read or last saved it, which the interface keeps with the copy and gives back to ouvrir (lu); null when it never read or saved a .nuee or .json. It may also carry copie (the engine's copy of that file) and sha256, which ouvrir ignores. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Where to write the recovery copy? The request doesn't say (chemin). | chemin is missing or empty. |
| Couldn't replace &lt;path&gt;: another program is using it (sync, antivirus, the file open elsewhere), or it is read-only. The file was left as it was; try again, or save under another name. | The file just written couldn't take the place of the old one within five seconds (nouveau.R: ecrire_sans_risque). ([`fichier_occupe`](#errors)) |
| &lt;R's own message&gt; | chemin can't be written ("cannot open the connection", untranslated). |

#### `projet_texte`

Gives a project as saving would write it, as text, for a problem report.

Since 1.0. Changes nothing. Writes the project in a temporary file, reads it and deletes it.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `version` | `string` | no | Nuée's version, written in the text as in a saved file. Default: `""`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `nom` | `string` | A file name made from the project's name, without extension (characters other than letters, digits, \_, space, brackets and - become \_). |
| `texte` | `string` | The .nuee file's JSON text, the project's session included. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |

#### `fermer_projet`

Closes a project: it leaves the session with the models fitted on it.

Since 1.0. Changes the session, not a project's content. The project and its fitted models leave the session, saved or not, with its redo steps, the copies of its file the engine kept for a merge, and the merges prepared with it; its undo steps stay in the engine, unused.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `ferme` | `string` | The id given. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |

#### `guide`

Gives the whole user guide as text, in French or in English, for the panel of Help › User guide (F1).

Since 2.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `langue` | `string` | no | The guide's language; any other value is French; without it, the request's language (X-Langue). Values: `en` (English (USER-GUIDE.txt)), `fr` (French (LISEZ-MOI-APPLICATION.txt)). |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `texte` | `string` | The guide's file beside the engine, at Nuée's root (an installed copy's resources), read as UTF-8, its lines joined by \\n. |

**Errors:**

| Message (in English) | When |
|---|---|
| cannot open the connection | The guide's file isn't beside the engine (an engine without the application's files): R's own message, untranslated. |

### Opening and importing

[`ouvrir`](#ouvrir) · [`exemple`](#exemple) · [`coller`](#coller) · [`importer_spectres`](#importer_spectres) · [`kobo_apercu`](#kobo_apercu) · [`kobo_importer`](#kobo_importer) · [`paquet_ouvrir`](#paquet_ouvrir)

#### `ouvrir`

Opens a file as a new project: a Nuée project, a .usp project, a table, spectra, or a KoboToolbox or ODK export.

Since 1.0. Changes the session, not a project's content. Opens a new project (with secours or origine, marked modified; with cache, hidden from etat). A .nuee or .json file is first copied into the engine's temporary folder and read from that copy, which is kept as the base of a later merge (fusionner) unless the project is a recovery copy, a read-only old version or hidden. A table, an export imported with kobo, or an ODK archive opened as it is keeps where its data came from (its data source: the file, its table, what the import decided of its columns, its size, date and SHA-256; Projet's a_source) and starts its data steps, which Update the data replays on that file read again. A file with several tables, or a KoboToolbox or ODK export to preview, opens nothing; the export's reading is kept in a cache.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `chemin` | `string` | yes | The file to open, a path the engine can read: a project (.nuee, .json), a .usp project (converted first by convertisseur/export_project), a table (.csv, .txt, .tsv, .tab, .dat, .xlsx, .xlsm, .xls, .rds, .rdata, .rda, .sav, .zsav, .por, .dta, .sas7bdat, .xpt, .omv, .dbf, .arff, .gpkg), spectra (.jdx, .dx, .jcm, .spc) or an ODK Central .zip. |
| `table` | `string` | no | The sheet (Excel), object (.rds, .RData), layer (GeoPackage) or table of an ODK .zip to open, as it is written (a KoboToolbox or ODK export given a table isn't previewed); without it, a file holding several answers choisir. |
| `brut` | `boolean` | no | true to open a KoboToolbox or ODK export as it is written, as any table, instead of answering its import's preview. Default: `false`. |
| `collecte` | `boolean` | no | true when the file must be a KoboToolbox or ODK Central export (File › Import from KoboToolbox or ODK): its import's preview is answered, and any other file refused. Default: `false`. |
| `kobo` | `object` | no | An import's settings, the parameters of the set import_kobo as Projet's kobo gives them ({table, garder, parents, formulaire, langue}): the export is imported with them at once, without its preview (to make an import again after the engine restarted). |
| `secours` | `boolean` | no | true when chemin is a recovery copy (sauvegarde_secours), reopened as the project it keeps: marked modified, and the copy itself never its file. Default: `false`. |
| `origine` | `string` | no | With secours, the project's own file, where it saves or where it came from, which becomes its chemin (a .nuee or .json also its chemin_enregistrement); given without secours, it does the same. |
| `nom` | `string` | no | With secours, origine or lecture_seule, the project's name; by default the name of origine's file, else of the file opened. |
| `lu` | [`EtatFichier`](#type-etatfichier) | no | With secours, the state of origine's file when the project read or last saved it (sauvegarde_secours's lu), so that saving over a file changed elsewhere since still asks first; ignored when its chemin isn't origine's. |
| `lecture_seule` | `object` | no | An old version of a project, written to a temporary file (File › History), opened read-only: {source: the project's file, oid: the version's id in its history, date: its date}, kept as texts ("" for one left out) in Projet's lecture_seule, which the interface shows and uses to restore it. Saving never writes it over the file it was read from; saved elsewhere, it becomes a project like any other. Since 2.0. |
| `cache` | `boolean` | no | true for a project the interface doesn't show (an old version compared with the project of today while its models are refitted): etat leaves it and its models out. Default: `false`. Since 2.0. |

**Result:** [`Projet`](#type-projet) \| `object`, with the fields below. The new project's summary; or, when chemin holds several tables and table isn't given, {choisir, chemin, nom (the file's name)}; or, for a KoboToolbox or ODK export (or with collecte), {kobo, chemin, nom}, its import's preview with the default settings, and nothing opened; or, for a reproducibility package (a .zip that holds a .nuee project and MANIFEST.sha256 at its root or in its one folder), {paquet_ouvrir: {chemin, nom}}, nothing opened: paquet_ouvrir opens it. With kobo, the imported project's summary with collecte.

| Field | Type | Meaning |
|---|---|---|
| `paquet_ouvrir` | `object` | Instead of a project, the reproducibility package to open with paquet_ouvrir: {chemin, nom}. Since 2.2. |
| `note` | `string[]` | What to know about the reading (values read as missing, short lines…) and a project saved by a newer Nuée; always an array, present only when there is something to say, and never with secours, origine, lecture_seule or cache. |
| `collecte` | `object` | With kobo, what the import did: {coordonnees (whether it set the project's coordinates), lignes, colonnes, phrases (one sentence per change, in the request's language; always an array)}. |
| `choisir` | `string[]` | Instead of a project, the file's tables to choose from (always an array), to ask again with table. |
| `kobo` | [`ApercuKobo`](#type-apercukobo) | Instead of a project, the import's preview, to ask kobo_importer or kobo_apercu with the settings chosen, or ouvrir with brut. |

**Errors:**

| Message (in English) | When |
|---|---|
| Unrecognised file format: .&lt;extension&gt; | The file's extension isn't one Nuée reads. ([`format_inconnu`](#errors)) |
| The .usp file couldn't be converted: &lt;the converter's output&gt; | The .usp converter (outils/export_project.exe, or Python with convertisseur/export_project.py) wrote no JSON. |
| This file holds no table (data frame or matrix) | An R file (.rds, .RData) holds no table. |
| Table not found in the file: &lt;table&gt; | table isn't an object of the R file (an unknown sheet of a workbook gets readxl's own Sheet '&lt;table&gt;' not found). |
| Nuée opens .rds and .RData files only with R 4.4 or newer (before, such a file can run code as soon as it is read). This R is version &lt;version&gt;: save the table in another format (CSV, Excel...), or install a newer R. | An R file, with an R older than 4.4. |
| This R file holds an object Nuée doesn't open (environment, function, S4, R5 or R6 object...): reading it could run code. Save a data frame, a matrix or a list of data frames in it. | An R file whose object isn't made only of vectors and lists. |
| The table is empty | The table read has no row or no column (nouveau.R: projet_depuis_tableau). ([`tableau_vide`](#errors)) |
| &lt;lines&gt;: more values than the first line has columns (&lt;number&gt;). The values would be shifted: a value may contain the separator (&lt;separator&gt;) or a quote, or a column name is missing from the first line. | Lines of a delimited text have more values than its first line has names (formats.R). ([`valeurs_decalees`](#errors)) |
| &lt;the reader's own message&gt; | The file doesn't exist or can't be read: R's or its package's message (readxl, haven, jsonlite…), untranslated. |
| &lt;file&gt; isn't a KoboToolbox or ODK Central export: it doesn't have their columns (\_id, \_uuid, \_submission_time, \_index…; SubmissionDate, KEY, meta-instanceID…). Open it with File › Open. | The file was given as an export but doesn't have two of their metadata columns (kobo.R: kobo_refus_export). ([`pas_un_export`](#errors)) |
| &lt;file&gt; is an XLSForm form (survey and choices sheets), not the data: give it as the form, beside the export of the data. | A workbook with survey and choices sheets was given as the export. ([`formulaire_pas_export`](#errors)) |
| &lt;file&gt; holds no CSV file: it isn't an ODK Central export. | A .zip without a CSV file (outside media/) was given as an ODK Central export. ([`zip_sans_csv`](#errors)) |
| File not found: &lt;path&gt; | The file named doesn't exist. ([`fichier_introuvable`](#errors)) |
| Unknown table in the export: “&lt;table&gt;” (it has: “&lt;table&gt;”, …). | table isn't one of the export's tables. ([`table_export_inconnue`](#errors)) |
| Unknown metadata: &lt;role&gt; (one can keep: envoi, auteur, identifiant, releve, validation). | garder names a role that isn't one of these. ([`metadonnee_inconnue`](#errors)) |
| &lt;file&gt; isn't an XLSForm form: an Excel workbook with a “survey” sheet (and “choices”) is needed. | formulaire isn't an Excel workbook with a survey sheet. ([`pas_un_formulaire`](#errors)) |
| The form doesn't have the language “&lt;language&gt;”; it has: “&lt;language&gt;”, …. | langue isn't one of the form's label languages. ([`langue_formulaire_inconnue`](#errors)) |
| The form &lt;file&gt; doesn't match this export: none of its questions is a column of the table “&lt;table&gt;”. | The form given isn't the one the export was made with. ([`formulaire_sans_rapport`](#errors)) |
| “&lt;table&gt;” isn't a parent table of “&lt;table&gt;” (its parent tables: “&lt;table&gt;”, …). | parents names a table that isn't a parent of the imported table. ([`table_parente_inconnue`](#errors)) |
| The table “&lt;table&gt;” has no column “&lt;column&gt;”. | parents names a column that the parent table's import doesn't give. ([`colonne_parente_inconnue`](#errors)) |

#### `exemple`

Opens an example project (made-up or simulated data) in the request's language, as a new project without a file.

Since 1.0. Changes the session, not a project's content. Opens a new project, not modified.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `nom` | `string` | no | Which example (Help › Example projects). Values: `pommes` (the apples of orchards (made-up data), without a session), `alouettes_comptages` (the skylark counts of the monitoring tutorial (simulated data), with their models and views), `alouettes_transects` (the skylark line transects of the monitoring tutorial (simulated data), with their model and views), `baci` (the trout densities of the BACI tutorial (simulated data), with their BACI mixed model and views). Default: `"pommes"`. Since 2.0. |

**Result:** [`Projet`](#type-projet). The file of exemple/ for nom and the request's language: with X-Langue: en, orchards.json, skylark_counts.json, skylark_transects.json or baci_trout.json, else vergers.json, comptages_alouette.json, transects_alouette.json or baci_truite.json (exemple/creer_exemple.js for the apples, exemple/creer_exemples_ecologie.js for the others, which hold the interface's session); its chemin is null, so that saving asks where.

**Errors:**

| Message (in English) | When |
|---|---|
| Unknown example project: &lt;name&gt; | nom isn't one of the values. |

#### `coller`

Opens a table pasted from a spreadsheet as a new project.

Since 1.0. Changes the session, not a project's content. Opens a new project, marked modified (it has no file); the pasted text is kept in a file of the engine's temporary folder, which isn't its data source (Projet's a_source is false), but its data steps start: Update the data, given a file, replays them on it, and that file becomes its source.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `texte` | `string` | yes | The pasted cells: one line per observation, the first with the columns' names, separated by tabs (or semicolons or commas). |
| `nom` | `string` | no | The new project's name. Default: `"Tableau"`. |

**Result:** [`Projet`](#type-projet), with the fields below too.

| Field | Type | Meaning |
|---|---|---|
| `fichier_colle` | `string` | The file of the engine's temporary folder that keeps the pasted text (tableau colle.tsv), which the console's log reads. |
| `note` | `string[]` | What to know about the reading (values read as missing…), always an array. |

**Errors:**

| Message (in English) | When |
|---|---|
| The clipboard holds no table (at least two rows and two columns) | The pasted text has no tab, semicolon or comma, or a single line. ([`pas_de_tableau`](#errors)) |
| The table is empty | The table read has no row or no column (nouveau.R: projet_depuis_tableau). ([`tableau_vide`](#errors)) |
| &lt;lines&gt;: more values than the first line has columns (&lt;number&gt;). The values would be shifted: a value may contain the separator (&lt;separator&gt;) or a quote, or a column name is missing from the first line. | Lines of a delimited text have more values than its first line has names (formats.R). ([`valeurs_decalees`](#errors)) |

#### `importer_spectres`

Opens spectra from one or more files (JCAMP-DX, SPC, text) as one new project, with a table of information on the samples if given.

Since 1.0. Changes the session, not a project's content. Opens a new project, marked modified (it has no file); it has no data source (Projet's a_source is false), but its data steps start: Update the data, given a file, replays them on it, and that file becomes its source.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `chemins` | `string[]` | yes | The spectra files (.jdx, .dx, .jcm, .spc, or text: a column of x values, then one column per spectrum), each giving one spectrum or several. |
| `infos` | `string` | no | A table of information on the samples (concentrations, classes…), any format File › Open reads, its first column the spectra's file names, joined to them. |
| `nom` | `string` | no | The new project's name; by default the file's name for one file, "Spectres (&lt;n&gt; fichiers)" for several. |

**Result:** [`Projet`](#type-projet), with the fields below too.

| Field | Type | Meaning |
|---|---|---|
| `note` | `string[]` | What to know (spectra interpolated on the first one's x values, spectra without information, information without a spectrum), always an array. |

**Errors:**

| Message (in English) | When |
|---|---|
| &lt;file&gt;: &lt;the reader's message&gt; | A file can't be read as spectra. |
| No readable spectrum in &lt;file&gt; | A file holds no spectrum. |
| A spectrum of fewer than two values in &lt;file&gt; | A spectrum has fewer than two x values. |
| Missing or unreadable x values in &lt;file&gt; | A spectrum has x values that aren't finite numbers. |
| The spectra don't have the same units (&lt;x values\|y values&gt;): &lt;units&gt; | The files give different units for the x or y values. |
| The spectra have no x values in common | Spectra on different x values don't overlap over two values. |
| No spectrum | chemins is empty. |
| No name in the first column of the information table matches a spectrum (its file name) | With infos, none of its first column's names is a spectrum's. |

#### `kobo_apercu`

Previews the import of a KoboToolbox or ODK Central export with the given settings, without opening anything.

Since 1.0. Changes the session, not a project's content. Opens nothing: only the readings of the export and of the form are kept in a cache (the last four, by file path, size and date).

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `chemin` | `string` | yes | The export: a KoboToolbox workbook (.xlsx, a sheet per repeat group) or CSV, or an ODK Central .zip (one CSV per table) or submissions CSV. |

Also the parameters of the set [`import_kobo`](#set-import_kobo).

**Result:** [`ApercuKobo`](#type-apercukobo).

**Errors:**

| Message (in English) | When |
|---|---|
| File not found: &lt;path&gt; | The file named doesn't exist. ([`fichier_introuvable`](#errors)) |
| &lt;file&gt; isn't a KoboToolbox or ODK Central export: it doesn't have their columns (\_id, \_uuid, \_submission_time, \_index…; SubmissionDate, KEY, meta-instanceID…). Open it with File › Open. | The file was given as an export but doesn't have two of their metadata columns (kobo.R: kobo_refus_export). ([`pas_un_export`](#errors)) |
| &lt;file&gt; is an XLSForm form (survey and choices sheets), not the data: give it as the form, beside the export of the data. | A workbook with survey and choices sheets was given as the export. ([`formulaire_pas_export`](#errors)) |
| &lt;file&gt; holds no CSV file: it isn't an ODK Central export. | A .zip without a CSV file (outside media/) was given as an ODK Central export. ([`zip_sans_csv`](#errors)) |
| Unknown table in the export: “&lt;table&gt;” (it has: “&lt;table&gt;”, …). | table isn't one of the export's tables. ([`table_export_inconnue`](#errors)) |
| Unknown metadata: &lt;role&gt; (one can keep: envoi, auteur, identifiant, releve, validation). | garder names a role that isn't one of these. ([`metadonnee_inconnue`](#errors)) |
| &lt;file&gt; isn't an XLSForm form: an Excel workbook with a “survey” sheet (and “choices”) is needed. | formulaire isn't an Excel workbook with a survey sheet. ([`pas_un_formulaire`](#errors)) |
| The form doesn't have the language “&lt;language&gt;”; it has: “&lt;language&gt;”, …. | langue isn't one of the form's label languages. ([`langue_formulaire_inconnue`](#errors)) |
| The form &lt;file&gt; doesn't match this export: none of its questions is a column of the table “&lt;table&gt;”. | The form given isn't the one the export was made with. ([`formulaire_sans_rapport`](#errors)) |
| “&lt;table&gt;” isn't a parent table of “&lt;table&gt;” (its parent tables: “&lt;table&gt;”, …). | parents names a table that isn't a parent of the imported table. ([`table_parente_inconnue`](#errors)) |
| The table “&lt;table&gt;” has no column “&lt;column&gt;”. | parents names a column that the parent table's import doesn't give. ([`colonne_parente_inconnue`](#errors)) |

#### `kobo_importer`

Imports a table of a KoboToolbox or ODK Central export as a new project, with the given settings.

Since 1.0. Changes the session, not a project's content. Opens a new project, not modified. Its import's settings are kept with it for the session (Projet's kobo) and, saved with it, in its data source with the export and the XLSForm form (Projet's a_source), which Update the data reads again on the next export; its data steps start.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `chemin` | `string` | yes | The export, as for kobo_apercu. |
| `nom` | `string` | no | The file name the project's name is made from (with a repeat group's name in brackets, "export (observation).xlsx"); chemin's file name by default. |

Also the parameters of the set [`import_kobo`](#set-import_kobo).

**Result:** [`Projet`](#type-projet), with the fields below too. The new project; its kobo gives the settings to make the import again, its collecte (saved with it) where the data came from, for the Methods text.

| Field | Type | Meaning |
|---|---|---|
| `note` | `string[]` | What to know about the reading, always an array, present only when there is something to say. |
| `collecte` | `object` | What the import did: {coordonnees (whether it set the project's coordinates), lignes, colonnes, phrases (one sentence per change, in the request's language; always an array)}. |

**Errors:**

| Message (in English) | When |
|---|---|
| File not found: &lt;path&gt; | The file named doesn't exist. ([`fichier_introuvable`](#errors)) |
| &lt;file&gt; isn't a KoboToolbox or ODK Central export: it doesn't have their columns (\_id, \_uuid, \_submission_time, \_index…; SubmissionDate, KEY, meta-instanceID…). Open it with File › Open. | The file was given as an export but doesn't have two of their metadata columns (kobo.R: kobo_refus_export). ([`pas_un_export`](#errors)) |
| &lt;file&gt; is an XLSForm form (survey and choices sheets), not the data: give it as the form, beside the export of the data. | A workbook with survey and choices sheets was given as the export. ([`formulaire_pas_export`](#errors)) |
| &lt;file&gt; holds no CSV file: it isn't an ODK Central export. | A .zip without a CSV file (outside media/) was given as an ODK Central export. ([`zip_sans_csv`](#errors)) |
| Unknown table in the export: “&lt;table&gt;” (it has: “&lt;table&gt;”, …). | table isn't one of the export's tables. ([`table_export_inconnue`](#errors)) |
| Unknown metadata: &lt;role&gt; (one can keep: envoi, auteur, identifiant, releve, validation). | garder names a role that isn't one of these. ([`metadonnee_inconnue`](#errors)) |
| &lt;file&gt; isn't an XLSForm form: an Excel workbook with a “survey” sheet (and “choices”) is needed. | formulaire isn't an Excel workbook with a survey sheet. ([`pas_un_formulaire`](#errors)) |
| The form doesn't have the language “&lt;language&gt;”; it has: “&lt;language&gt;”, …. | langue isn't one of the form's label languages. ([`langue_formulaire_inconnue`](#errors)) |
| The form &lt;file&gt; doesn't match this export: none of its questions is a column of the table “&lt;table&gt;”. | The form given isn't the one the export was made with. ([`formulaire_sans_rapport`](#errors)) |
| “&lt;table&gt;” isn't a parent table of “&lt;table&gt;” (its parent tables: “&lt;table&gt;”, …). | parents names a table that isn't a parent of the imported table. ([`table_parente_inconnue`](#errors)) |
| The table “&lt;table&gt;” has no column “&lt;column&gt;”. | parents names a column that the parent table's import doesn't give. ([`colonne_parente_inconnue`](#errors)) |
| The table is empty | The table read has no row or no column (nouveau.R: projet_depuis_tableau). ([`tableau_vide`](#errors)) |

#### `paquet_ouvrir`

Opens a reproducibility package again (paquet's .zip): unzips it into a folder of the engine's temporary folder, checks each of its files against its manifest, opens its project as a new project without a file, and gives the results it deposited, which a client compares with its models fitted again (ajuster with numero and A, resultats_cles, comparer_resultats).

Since 2.2. Changes the session, not a project's content. Unzips the package into a new folder of the engine's temporary folder and opens a new project (its .nuee read as ouvrir reads one, then without a file); the .zip doesn't change.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `chemin` | `string` | yes | The .zip: one folder holding a .nuee project (MANIFEST.sha256 beside it), as paquet writes it, or those files at its root; the .zip itself is never written. |
| `nom` | `string` | no | The .zip's name to keep (Projet's depuis_paquet), when chemin's isn't it (a file a browser put in its own folder); by default chemin's. |
| `version` | `string` | no | Nuée's version here, given back in ici; by default NUEE_VERSION's. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `projet` | [`Projet`](#type-projet) | The project opened: named after the package's folder (&lt;folder&gt;.nuee), without a file (chemin and chemin_enregistrement null: enregistrer asks where, nothing is written into the package), not modified, its data's source the package's CSV in the unzipped folder (mettre_a_jour reads it again), depuis_paquet set. |
| `titre` | `string` | The package's title (its project's paquet answers), else its folder's name. |
| `doi` | `string` \| `null` | The package's own DOI (its key results', else CITATION.cff's), null without. |
| `fichier` | `string` | The .zip's name (nom, else chemin's). |
| `dossier` | `string` | The unzipped package's folder, in the engine's temporary folder. |
| `manifeste` | `object` | Its files against MANIFEST.sha256 (each SHA-256 computed again): {present (it has one), intact (it has one, and each file it names is there with its hash, and no other), fichiers (how many it names), manquants, changes (there with another hash), ajoutes (there, not named), the three arrays of paths relative to the package's folder, illisibles (lines that aren't "&lt;hash&gt;  &lt;path&gt;")}. |
| `depose` | [`ResultatsDeposes`](#type-resultatsdeposes) \| `null` | The results the package deposited, as its file gives them; null for a package without (Nuée 1.1's) or one whose file doesn't read. |
| `ici` | `object` | What computes here: {nuee (version), R (X.Y.Z), plateforme}. |
| `paquets_differents` | `object[]` | The deposit's R packages installed here in another version, each {paquet, depose, ici} (always an array). |
| `note` | `string[]` \| `null` | What to know about the project's reading (ouvrir's note), or null. |

**Errors:**

| Message (in English) | When |
|---|---|
| File not found: &lt;chemin&gt; | chemin is empty or names no file. |
| This .zip archive can't be read: &lt;the zip package's message&gt; | chemin isn't a .zip archive. |
| This .zip archive has a file outside its folder: &lt;path&gt;. | A path of the archive would land outside the folder it is unzipped in (.., an absolute path, a drive, a control character): nothing is unzipped. |
| &lt;file&gt; isn't a reproducibility package from Nuée: it holds no project (.nuee). | Neither the archive's root nor one of its first folders holds a .nuee. Besides erreur, the answer carries pas_un_paquet: true. |
| &lt;the project's reading error&gt; | The package's .nuee doesn't read, as ouvrir would say. |

### Editing the data

[`modifier_valeur`](#modifier_valeur) · [`coller_cellules`](#coller_cellules) · [`variable_calculee`](#variable_calculee) · [`classes_valeurs`](#classes_valeurs) · [`imputer`](#imputer) · [`classe_selection`](#classe_selection) · [`scores_en_variables`](#scores_en_variables) · [`table_a_joindre`](#table_a_joindre) · [`ajouter_colonnes`](#ajouter_colonnes) · [`raster_infos`](#raster_infos) · [`ajouter_raster`](#ajouter_raster) · [`resumer_groupes`](#resumer_groupes) · [`remplacer`](#remplacer) · [`categories`](#categories) · [`decrire_variables`](#decrire_variables) · [`renommer_variable`](#renommer_variable) · [`supprimer_variable`](#supprimer_variable) · [`type_variable`](#type_variable) · [`coordonnees`](#coordonnees)

#### `modifier_valeur`

Changes one value of the data, as typing in a cell of the Data view does.

Since 1.0. Changes the project, with an undo step that `annuler` takes back. One undo step, labelled in the request's language (edit of &lt;variable&gt;, modification de &lt;variable&gt;), also noted as a data step, which Update the data replays; the models already fitted keep their results until they are fitted again.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `observation` | `number` | yes | The observation's number (the uid of Projet's observations). |
| `variable` | `number` | yes | The variable's number. |
| `valeur` | `string` \| `number` | no | The new value, as text, without the spaces around it: empty (or absent), the value becomes missing; for a qualitative variable, a category's name (one it doesn't have yet is added after the others); for a quantitative one, a finite number, with a decimal point or comma (3.5 or 3,5). Default: `""`. |

**Result:** [`Projet`](#type-projet). The project after the change.

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable without data: &lt;number&gt; | A variable's number isn't a column of the project's data (variables.R: modifier_valeur, atelier.R: coller_cellules). ([`variable_sans_donnees`](#errors)) |
| Unknown observation: &lt;number&gt; | observation isn't one of the project's observations. |
| Non-numeric value for &lt;variable&gt;: &lt;value&gt; | The variable is quantitative and the value isn't a finite number (Inf is refused too). |

#### `coller_cellules`

Pastes a block of cells into the data from a chosen cell, as Ctrl+V in the Data view does.

Since 1.0. Changes the project, with an undo step that `annuler` takes back. One undo step (pasted cells), even when no cell changes, also noted as a data step, which Update the data replays on the observations the data still have.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `observations` | `number[]` | yes | The block's rows: the observations' numbers, in the view's order. |
| `variables` | `number[]` | yes | Its columns: the variables' numbers, in the view's order. |
| `valeurs` | `string[][]` | yes | Its cells as text, one array per row, one element per column; null leaves a cell as it is, "" makes it missing. Each cell as modifier_valeur reads it: a qualitative variable takes a category's name (new ones added in the order they come), a quantitative one a number (3,5 or 3.5). |

**Result:** [`Projet`](#type-projet), with the fields below too. The project after the paste.

| Field | Type | Meaning |
|---|---|---|
| `collage` | `object` | {cellules: the number of cells that changed}. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable without data: &lt;number&gt; | A variable's number isn't a column of the project's data (variables.R: modifier_valeur, atelier.R: coller_cellules). ([`variable_sans_donnees`](#errors)) |
| The pasted block must have one row per observation and one column per variable. | valeurs doesn't have the shape of observations × variables. |
| Unknown observation: &lt;numbers&gt; | Observations aren't the project's (the first five named): nothing changes. |
| Non-numeric value for &lt;variable&gt;: &lt;value&gt; (observation &lt;name&gt;) | A cell of a quantitative variable isn't a number: nothing changes. |

#### `variable_calculee`

Adds a variable computed by an R formula on the project's variables and identifiers (Edit › New variable).

Since 1.0. Changes the project, with an undo step that `annuler` takes back. One undo step (variable &lt;name&gt;), also noted as a data step, which Update the data replays; the variable joins the project's dataset Variables ajoutées and keeps its formula, from which Update the data computes it again when the replayed data lack it.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `nom` | `string` | yes | The new variable's name; the spaces around it are dropped. |
| `formule` | `string` | yes | An R expression, read before it is evaluated (since 1.1: only the project's variables and identifiers, numbers, text, and the calculation, text, date and distribution functions of FONCTIONS_FORMULE; anything else is refused, the rest of R going to the console), evaluated with each variable and each observation identifier under its name (a name with spaces or signs between `backticks`; a qualitative variable as its categories' names, one value per observation), and Nuée's helpers: moyenne_groupe/group_mean, ecart_groupe/group_deviation, precedent/previous, suivant/following, croissance/growth_rate, richesse/richness, shannon, simpson and pielou (variables.R). It must give one value per observation, or a single value for all: numbers make a quantitative variable (non-finite ones missing), text or logical values a qualitative one, its categories sorted. |

**Result:** [`Projet`](#type-projet). The project with the new variable.

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Give the new variable a name | nom is empty, or only spaces. ([`nom_variable_manquant`](#errors)) |
| A variable is already called “&lt;name&gt;” | nom, without the spaces around it, is the name of one of the project's variables. ([`nom_variable_pris`](#errors)) |
| Variable not found: &lt;name&gt; (check the name; a name with spaces or signs is written between `backticks`) | The formula names an object R doesn't find. |
| "&lt;what&gt;" isn't allowed in a New variable formula: arithmetic (+, log10(), ifelse()…), text (paste(), substr()…), dates (as.Date()…), and Nuée's group, lag and diversity functions. For the rest of R, use the R console (View › R console). | The formula calls a function outside FONCTIONS_FORMULE, assigns, uses { } or ;, or names something that is neither a variable, an identifier nor a constant (since 1.1). |
| The formula must give one value per observation (&lt;n&gt;); it gives &lt;k&gt; | The formula's result is neither one value nor one per observation. |
| The formula must give numbers or text | The result is neither numbers, text, logical values nor a factor. |
| &lt;R's message&gt; | The formula fails as R evaluates it (a wrong argument, stop()…), or one of Nuée's helpers refuses its arguments (precedent(): two observations with the same time in a group…): its message, as it is. |

#### `classes_valeurs`

Adds a qualitative variable made of classes of a quantitative variable's values, cut at given limits or at its quantiles.

Since 1.0. Changes the project, with an undo step that `annuler` takes back. One undo step (variable &lt;name&gt;), also noted as a data step, which Update the data replays.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `nom` | `string` | yes | The new variable's name; the spaces around it are dropped. |
| `variable` | `number` | yes | The number of the quantitative variable to cut. |
| `bornes` | `number[]` | no | The limits: 10 and 20 make the classes ≤ 10, ]10 ; 20] and &gt; 20 (each class closed on the right, the limits written to 4 significant digits). One of bornes and n is needed. |
| `n` | `integer` | no | Without bornes: the number of classes, 2 or more (a fraction is cut to its whole part), of about equal counts, their limits at the quantiles (duplicate limits merged). At least 2. |

**Result:** [`Projet`](#type-projet). The project with the new variable.

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Give the new variable a name | nom is empty, or only spaces. ([`nom_variable_manquant`](#errors)) |
| A variable is already called “&lt;name&gt;” | nom, without the spaces around it, is the name of one of the project's variables. ([`nom_variable_pris`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Choose a quantitative variable | The variable is qualitative. |
| Give the classes' limits, or their number (2 or more) | Neither bornes nor n is given, or n is less than 2 or not a number. |
| “&lt;name&gt;” has no value: there are no classes to make from it. | Without bornes, the variable has no value. |
| At least one limit is needed | bornes is given empty. |

#### `imputer`

Fills the missing values of quantitative variables by regularized iterative PCA, as missMDA::imputePCA does.

Since 1.0. Changes the project, with an undo step that `annuler` takes back. One undo step (imputation), also noted as a data step, which Update the data replays in the language it was made in (the same names); the new variables are named in the request's language. The observed values don't change.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `variables` | `number[]` | yes | The numbers of the quantitative variables, at least two: each one is filled from the others; the complete ones only help. |
| `k` | `integer` | no | The number of components; without it, chosen from 0 to 5 by generalized cross-validation of the scaled variables, as missMDA::estim_ncpPCA (0: each variable's mean). A given k is kept at most min(p − 1, n − 2). At least 1. |
| `bornes` | `boolean` | no | Keep each imputed value within its variable's observed range (a 0/1 variable between 0 and 1, an area positive). Default: `true`. |
| `remplacer` | `boolean` | no | true fills the variables themselves; otherwise new variables "&lt;name&gt; (imputed)" ("(imputée)" in French) receive the values, updated when imputed again. Default: `false`. |

**Result:** [`Projet`](#type-projet), with the fields below too. The project after the imputation.

| Field | Type | Meaning |
|---|---|---|
| `imputation` | `object` | {valeurs: the cells filled; tours: the iterations; composantes: the components used (0: the means); choisies: true when that number was chosen; laissees: the observations left missing, which lack more than half of the variables}. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Imputation by PCA goes with quantitative variables. | One of the variables is qualitative. |
| Choose at least two variables: the PCA fills each one from the others. | Fewer than two different variables. |
| These variables have no missing values. | Nothing is missing in these variables. |
| A constant or nearly empty variable can't be imputed: &lt;names&gt;. | A variable has no variation (or no values) over the observations that miss at most half of them. |
| No value to fill: each incomplete observation misses more than half of these variables. | Every observation with a missing value lacks more than half of the variables. |
| Too few observations for a PCA: at least 3 are needed that miss at most half of these variables. | Fewer than 3 observations miss at most half of the variables. |

#### `classe_selection`

Puts observations in a category of a qualitative class variable, the variable and the category created if needed.

Since 1.0. Changes the project, with an undo step that `annuler` takes back. One undo step, labelled in the request's language (class &lt;category&gt;, classe &lt;category&gt;); a new variable joins the dataset Variables ajoutées. Also noted as a data step, which Update the data replays on the observations the data still have.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `variable` | `string` | yes | The class variable's name, not its number: a new qualitative variable when no variable has it (a number is taken as a name). |
| `etiquette` | `string` | yes | The category's name, added after the variable's others if it is new. |
| `observations` | `number[]` | yes | The numbers of the observations to put in it; the others keep their category (or none). |

**Result:** [`Projet`](#type-projet). The project after the change.

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| The variable “&lt;name&gt;” already exists and isn't qualitative | A quantitative variable with values has that name. |
| Unknown observation: &lt;numbers&gt; | Observations aren't the project's (the first five named). |

#### `scores_en_variables`

Makes the scores of a fitted PCA or PLS variables of its project, for a hierarchical model that takes them as X.

Since 1.0. Changes the project, with an undo step that `annuler` takes back. One undo step, labelled in the request's language (scores of &lt;model&gt; as variables, scores de &lt;model&gt; en variables). Each score variable has values for the model's observations only; doing it again after refitting the model updates the same variables. Also noted as a data step that keeps the model's definition as it was fitted: Update the data fits the model again on the data replayed so far and takes its scores.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id: a PCA, PLS or OPLS (NIPALS components). |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `projet` | [`Projet`](#type-projet) | The project with the score variables. |
| `noms` | `string[]` | Their names, as .usp files name them: $&lt;model&gt;.t&lt;a&gt;(&lt;title&gt;), $M1.t1(Overview)… (always an array). |
| `modeles` | `string[]` | The names of the project's model definitions that use these variables, to fit again (always an array). |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| This isn't for this model: a regression or another analysis doesn't have the components and scores of a PCA or a PLS. | A request that needs components and scores (serveur.R: SANS_REGRESSION: hca, permutations, scores_en_variables, appartenance, contributions, spectres, biplot, ipls) names a regression or another analysis in modele or modeles; checked before the request runs. ([`composantes_seulement`](#errors)) |

#### `table_a_joindre`

Reads a table whose columns are to be added to the project and gives its tables, its columns and the keys proposed, before ajouter_colonnes adds them (Edit › Add columns from a table).

Since 1.0. Changes nothing. Pasted cells are written to a file of the engine's temporary folder; the project doesn't change.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `chemin` | `string` | no | The table's file, in any format File › Open reads, or a project (.nuee, .json, .usp: its data). Needed without texte. |
| `texte` | `string` | no | Pasted cells instead of a file (tab, semicolon or comma separated, two lines at least): kept in a file of the engine's temporary folder, whose path comes back as chemin. |
| `table` | `string` | no | The file's sheet or table; by default its first (a Variables codebook sheet set aside). |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `chemin` | `string` | The file to give ajouter_colonnes: the one given, or the file holding the pasted cells. |
| `nom` | `string` | The file's name, or "pasted cells" in the request's language. |
| `colle` | `boolean` | Whether the table came from pasted cells. |
| `tables` | `string[]` \| `null` | The file's tables when it has several (sheets, R objects, layers), else null. |
| `table` | `string` \| `null` | The table read (null for a format without tables): give it back to ajouter_colonnes. |
| `n` | `integer` | Its number of rows. |
| `colonnes` | `object` | Its columns as columns: nom, their names; nombres, whether each holds numbers; nom_ajoute, the name each would take in the project ("name (2)" for a name taken); always arrays. |
| `cles` | `object[]` | The key proposed, each {projet: the project's column, a Colonne; table: the table's column}: columns of the same name (capitals and spaces around aside), the combination whose keys are unique in the table where the most observations find theirs; empty without a common name. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| The clipboard holds no table (at least two rows and two columns) | The pasted text has no tab, semicolon or comma, or a single line. ([`pas_de_tableau`](#errors)) |
| Unrecognised file format: .&lt;extension&gt; | The file's extension isn't one Nuée reads. ([`format_inconnu`](#errors)) |
| Table not found in the file: &lt;table&gt; | table isn't one of an R file's tables. |
| The .usp file couldn't be converted: &lt;the converter's output&gt; | A .usp project the converter can't read. |
| &lt;the reader's message&gt; | The file is missing or can't be read: the message of R or of the package that reads it, often untranslated. |

#### `ajouter_colonnes`

Adds columns of a table to the project's observations, each observation taking the values of the row that has its key (Edit › Add columns from a table).

Since 1.0. Changes the project, with an undo step that `annuler` takes back. One undo step (columns from &lt;nom&gt;), also noted as a data step, which Update the data replays: it keeps the table's file as the project keeps its source (its path, size, date and SHA-256), or the pasted cells' text. Numbers make quantitative variables, the rest qualitative ones; the models already fitted don't change.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `chemin` | `string` | yes | The table's file, as table_a_joindre's chemin gives it (for pasted cells, the file that keeps them). |
| `table` | `string` | no | Its sheet or table, as table_a_joindre's table gives it; without it, the file's first (which may be a Variables codebook sheet). |
| `cles_projet` | [`Colonne`](#type-colonne)`[]` | yes | The project's columns that make the key: variables by number, identifiers as "id:k". |
| `cles_table` | `string[]` | yes | The table's columns that answer them, in the same order, read as written ("01" stays "01"). Keys compare as numbers when the project's column is a quantitative variable, else as text without the spaces around, capitals counting. |
| `colonnes` | `string[]` | no | The table's columns to add; by default all but the key's. |
| `nom` | `string` | no | The table's name in the undo label; by default the file's name. |
| `colle` | `boolean` | no | true when chemin keeps pasted cells (table_a_joindre's colle): the data step keeps their text rather than that file, which lasts only as long as the engine. A .tsv, .csv or .txt file of the engine's temporary folder under 5 MB is taken as pasted cells without it. Default: `false`. Since 2.0. |

**Result:** [`Projet`](#type-projet), with the fields below too. The project with the new variables.

| Field | Type | Meaning |
|---|---|---|
| `jointure` | `object` | {colonnes: the table's columns added; noms: the names they took ("name (2)" for a name taken); n: the observations; trouvees: those that found their key; sans_correspondance: those without, their values missing; cles_seules: the table's keys that match no observation; sans_cle: the table's rows without a key, left out; phrases: what happened, in sentences}. A single column or sentence comes as a string, not an array. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Unknown identifier: &lt;id:k&gt; | "id:k" names no identifier of the observations (variables.R: colonne_ou_identifiant). ([`identifiant_inconnu`](#errors)) |
| Each key column needs its column in the project and in the table | cles_table is empty, or cles_projet and cles_table don't have the same length. |
| The table has no column “&lt;name&gt;” | A column of cles_table or colonnes isn't the table's. |
| A column comes only once in the key | A column is twice in cles_projet or in cles_table. |
| A key column isn't added to the project | colonnes names a column of the key. |
| Choose at least one column to add | No column is left to add. |
| The key “&lt;key&gt;”&lt;columns&gt; comes &lt;n&gt; times in the table&lt;, as do &lt;k&gt; other(s)&gt;: each key must come at most once (add to the key the column that tells these rows apart, the year for instance) | A key is on several rows of the table. |
| No observation finds its key&lt;columns&gt; in the table: nothing was added. Keys are compared as written (S1 isn't s1, nor 01 1); the project has &lt;keys&gt;, the table &lt;keys&gt;. | No observation's key is in the table. |
| Unrecognised file format: .&lt;extension&gt; | The file's extension isn't one Nuée reads. ([`format_inconnu`](#errors)) |
| &lt;the reader's message&gt; | The file is missing or can't be read: the message of R or of the package that reads it, often untranslated. |

#### `raster_infos`

Says what each raster file is and how many observations fall inside it, or why it can't be read, before ajouter_raster adds its values.

Since 1.0. Changes nothing. A file that can't be read doesn't fail the request: its entry says why in erreur.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id; its coordinates must be set. |
| `chemins` | `string[]` | yes | The raster files' paths: GeoTIFF (.tif) or ESRI ASCII grids (.asc). |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `fichiers` | `object[]` | One per path, in their order (always an array). A file read: {chemin, nom, taille (bytes), format (GeoTIFF, BigTIFF or ASCII), largeur, hauteur (cells), bandes, type (uint8, int16, float32…, texte_entiers or texte for an ASCII grid), compression ("" without), predicteur, organisation (tiles or strips, in words), bandes_separees, resolution (the cells' size, in words), etendue {xmin, xmax, ymin, ymax} in its system, systeme {code (null when unknown), nom, suppose: taken as the project's for want of its own}, nodata (null without), palette, pixel_point, methode (the method proposed: cellule for classes, bilineaire otherwise), noms (each band's proposed name), noms_libres (the names they would take in the project), dedans (the observations inside), observations (those with coordinates), total, notes (always arrays for noms, noms_libres and notes)}. A file that can't be read: {chemin, nom, taille (null when missing), erreur: why, in the request's language}. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| This project has no coordinates: choose them in Plots › Map (two variables and their system). | The request needs the observations' coordinates and the project has none set, or they no longer hold: a variable gone or made qualitative (spatial.R: coordonnees_projet with exiger). ([`sans_coordonnees`](#errors)) |

#### `ajouter_raster`

Adds the values of raster files at the observations' coordinates as new variables, one per band (Edit › Add raster values).

Since 1.0. Changes the project, with an undo step that `annuler` takes back. One undo step (values from &lt;files&gt;), also noted as a data step, which Update the data replays: it keeps each raster file as the project keeps its source (its path, size, date and SHA-256). Each variable keeps where it came from (its files, bands, method, radius, system and cell size), which the Methods text says.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id; its coordinates must be set. |
| `fichiers` | `object[]` | yes | The files, each {chemin: its path; methode: how a value is read, cellule (the cell holding the observation), bilineaire (interpolated from the four nearest cells), moyenne or majorite (the mean, or the majority class, of the cells within rayon), another or none being the method raster_infos proposes; rayon: the circle's radius in metres, for moyenne and majorite, at most 50000; noms: one name per band, by default the names raster_infos proposes}. Bands of different files given the same name make one variable, as tiles of one raster: they must then share their method and radius. |

**Result:** [`Projet`](#type-projet), with the fields below too. The project with the new variables.

| Field | Type | Meaning |
|---|---|---|
| `raster` | `object` | {variables: one per variable added (always an array), each {nom: its name ("name (2)" for a name taken), demande: the name asked, fichiers (always an array), methode, rayon (null without), valeurs: the observations with a value, sans_coordonnees, hors: outside the raster, sans_valeur: on a cell without a value, note: the files' notes, null, one string or several}; phrases: what happened, in sentences, one alone coming as a string}. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| This project has no coordinates: choose them in Plots › Map (two variables and their system). | The request needs the observations' coordinates and the project has none set, or they no longer hold: a variable gone or made qualitative (spatial.R: coordonnees_projet with exiger). ([`sans_coordonnees`](#errors)) |
| Choose at least one raster file. | fichiers is empty. |
| No observation has coordinates. | Every observation lacks a coordinate. |
| File not found: &lt;path&gt; | A file doesn't exist. |
| &lt;file&gt;: Nuée reads GeoTIFF rasters (.tif) and ESRI ASCII grids (.asc). For another format, export it as GeoTIFF in QGIS (right-click on the layer › Export › Save As…). | A file is neither. |
| &lt;file&gt;: give the circle's radius, in metres. | methode is moyenne or majorite without a positive rayon. |
| &lt;file&gt;: a radius over 50 km isn't computed here. | rayon is over 50000. |
| &lt;file&gt; has &lt;n&gt; band(s): give each one a name. | noms doesn't give one name per band. |
| &lt;file&gt;: give each band a name. | A band's name is empty. |
| The files joined under the name “&lt;name&gt;” must have the same method and radius. | Bands given the same name have different methods or radii. |
| &lt;file&gt; is in &lt;system&gt;, and the project's coordinates are plane, without a stated system: choose their system in Plots › Map. | The raster has a system and the project's coordinates are "plan". |
| &lt;file&gt; : &lt;why the file is refused&gt; | The file can't be read (damaged, a compression or a value type Nuée doesn't read, a coordinate system it doesn't convert…): raster.R's refusals, each naming the file. |

#### `resumer_groupes`

Summarizes a project into a new project of one observation per group of one or more columns (Edit › Summarize by group).

Since 1.0. Changes the session, not a project's content. Opens the new project in the session, without a file and marked modified; like a pasted table, it has no data source, but its data steps start (Update the data, given a file). The project summarized doesn't change.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project to summarize, which doesn't change. |
| `groupes` | [`Colonne`](#type-colonne)`[]` | yes | The columns that make the groups (the sector and the year…): variables by number, identifiers as "id:k". An observation without a value in one of them is left out, and counted. |
| `variables` | `number[]` | no | The quantitative variables to summarize; by default all of them but those of the groups. |
| `statistiques` | `string[]` | no | One or more statistics, each for every variable, in columns "&lt;variable&gt; (&lt;statistic&gt;)" named in the request's language; missing values are ignored. Values: `moyenne` (The mean.), `somme` (The sum.), `mediane` (The median.), `minimum` (The minimum.), `maximum` (The maximum.), `ecart_type` (The standard deviation (missing for a single value).), `n` (The number of known values.). Default: `["moyenne"]`. |

**Result:** [`Projet`](#type-projet), with the fields below too. The new project, named "&lt;project&gt; by &lt;groups&gt;" in the request's language: its observations the groups, named by their values, in the order of their columns; the group columns as they were (variables or identifiers); the qualitative variables and identifiers that don't vary within any group; the summaries.

| Field | Type | Meaning |
|---|---|---|
| `resume` | `object` | {groupes: the number of groups; observations: those summarized; sans_groupe: those left out for a missing group value; gardees: the qualitative variables and identifiers kept; laissees: those left out, as they vary within a group; phrase: that in a sentence}. One name alone in gardees or laissees comes as a string. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Unknown identifier: &lt;id:k&gt; | "id:k" names no identifier of the observations (variables.R: colonne_ou_identifiant). ([`identifiant_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Choose at least one column to make the groups | groupes is empty. |
| Choose at least one statistic | statistiques is empty. |
| Unknown statistic: &lt;name&gt; | A statistic isn't one of the values. |
| “&lt;name&gt;” is qualitative: only quantitative variables are summarized (a qualitative variable is kept when it doesn't vary within a group) | variables names a qualitative variable. |
| Choose at least one quantitative variable to summarize | No quantitative variable is left outside the groups. |
| No observation has a value of &lt;columns&gt; | No observation has a value in every group column. |

#### `remplacer`

Finds and replaces values in one, several or all variables, or previews what the replacement would change (Edit › Find and replace).

Since 1.0. Changes the project, with an undo step that `annuler` takes back. With apercu nothing changes. Otherwise one undo step (replacement of &lt;text&gt;, or of empty cells), also noted as a data step, which Update the data replays; a category replaced by an existing one merges into it, one emptied disappears, and the project's model definitions follow the categories (atelier.R: suivre_categories).

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `variables` | `number[]` | no | The numbers of the variables to search; all of them without it. |
| `chercher` | `string` \| `number` | no | The text or number to find; needed unless vides. A qualitative variable compares it as text (the whole cell, without the spaces around, or with partie a part of it); a quantitative one as a number on the whole cell ("3,5" is 3.5). |
| `remplacer` | `string` \| `number` | no | What replaces it; empty, the cells become empty (missing). A quantitative variable takes only a number or nothing: with a text, its cells stay as they are, and are counted (ignorees). Default: `""`. |
| `vides` | `boolean` | no | Find the empty cells (missing values) instead; chercher is then ignored. Default: `false`. |
| `partie` | `boolean` | no | In a qualitative variable, replace chercher wherever it appears in the text ("," in "3,5"), as written; not with vides. Default: `false`. |
| `casse` | `boolean` | no | Capitals count ("Forêt" isn't "forêt"). Default: `false`. |
| `nombres` | `boolean` | no | A qualitative variable whose values, once replaced, are all numbers becomes quantitative. Default: `true`. |
| `apercu` | `boolean` | no | Only say what would change, changing nothing. Default: `false`. |

**Result:** [`Projet`](#type-projet) \| `object`, with the fields below. With apercu, an object with remplacement only; otherwise the project after the change, with remplacement.

| Field | Type | Meaning |
|---|---|---|
| `remplacement` | `object` | {cellules: the cells changed; variables: one per variable searched (always an array), each {variable, nom, cellules, ignorees: the cells left (a text for a quantitative variable), devient: quantitative, qualitative or "" when it stays what it was, categories_avant, categories_apres, exemples: the first changes, "a → b (n)", one alone as a string}; phrases: what changes, in sentences, one alone as a string}. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Give the text or number to find, or find the empty cells. | chercher is empty without vides. |
| No variable to search. | The project has no variable with data. |
| No cell to replace. | Without apercu, no cell would change. |

#### `categories`

Renames, merges or reorders the categories of a qualitative variable (Edit › Categories).

Since 1.0. Changes the project, with an undo step that `annuler` takes back. One undo step (categories of &lt;name&gt;), also noted as a data step with the categories it renamed, which Update the data replays by their names; the observations keep their category under its new name, the project's model definitions follow, and a model of the session fitted with a category since renamed or merged must be fitted again before it refits or predicts (modele_perime).

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `variable` | `number` | yes | The qualitative variable's number. |
| `noms` | `string[]` | yes | The new name of each of its categories, in their present order (Projet's categories): two given the same name merge into one. No name may be empty. |
| `ordre` | `string[]` | no | The order of the categories that result, by their names, each once; by default the order they come in (a merge takes the place of the first). In a regression, the last category observed is the reference. |

**Result:** [`Projet`](#type-projet), with the fields below too. The project after the change.

| Field | Type | Meaning |
|---|---|---|
| `categories` | `object` | {variable, nom, categories: the categories now (always an array), reference: the last, a regression's reference, phrases: what changed, in sentences}. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| &lt;name&gt; is quantitative: it has no categories. | The variable is quantitative. |
| Give a name to each of the &lt;n&gt; categories of &lt;name&gt;. | noms doesn't give one name per category. |
| A category can't have an empty name (Edit › Find and replace empties cells). | A name is empty. |
| The order must give each category once: &lt;categories&gt;. | ordre doesn't give each resulting category once. |
| The categories didn't change. | The names and order are those of now. |

#### `decrire_variables`

Sets the unit and the description of variables (Edit › Units and descriptions).

Since 1.0. Changes the project, with an undo step that `annuler` takes back. One undo step (units and descriptions), also noted as a data step, which Update the data replays on the variables the data still have.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `descriptions` | `object[]` | yes | One per variable, {variable: its number; unite: its unit, cut to 60 characters; description: cut to 1000}: a field absent or null stays as it is, an empty text removes it; line breaks and other control characters become spaces. |

**Result:** [`Projet`](#type-projet). The project after the change (Projet's variables carry unite and description).

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| No variable to describe. | descriptions is empty. |
| The units and descriptions didn't change. | Every value given is the one already there. |

#### `renommer_variable`

Renames a variable, the project's model definitions, the formulas of computed variables and the documents' references to its terms following, or previews what follows (in the Data view, a column header's right-click menu › Rename).

Since 2.0. Changes the project, with an undo step that `annuler` takes back. With apercu nothing changes. Otherwise one undo step (&lt;old&gt; renamed &lt;new&gt;, in the request's language), also noted as a data step that an update replays in its place: the variable takes the name; the project's model definitions follow (the names of their terms, their condition, a trend power's term, a forest's partial dependence), and so do the formulas of the computed variables that read it and the documents' references to its terms ({{M1.coef.site}}); an R-code model's code isn't rewritten. The session's fits keep their results under the old name, those whose condition came from their definition following it; refitting, predicting with or exporting one that used the variable is refused until it is fitted again from its definition. Edit › Undo takes it all back in one step, the references too, even in a document written since; Edit › Redo makes it again.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `variable` | `number` | yes | The variable's number. |
| `nom` | `string` | yes | The new name; line breaks and other control characters become spaces and the spaces around it are dropped. At most 200 characters, not starting with $, and neither another variable's name nor an identifier's of the observations. |
| `apercu` | `boolean` | no | true to only say what would follow the new name (renommage), changing nothing, or why it is refused, by the same errors: Rename's preview, asked as the name is typed. Default: `false`. |

**Result:** [`Projet`](#type-projet) \| `object`, with the fields below. With apercu, {renommage} only; otherwise the project after the change, with renommage.

| Field | Type | Meaning |
|---|---|---|
| `renommage` | `object` | {variable: its number; ancien, nom: its name before and now; modeles: the project's models whose definitions follow (their terms' names, their condition), R-code models aside; code: the R-code models that name it in their code, which isn't rewritten; formules: the computed variables whose formula follows; documents: the ids of the documents whose references follow (D1…); references: how many references follow; ajustes: the numbers of the project's models that a fit of the session made with the old name, to fit again so that their results take the new one; phrases: what it does, in sentences, in the request's language}; modeles, code, formules, documents, ajustes and phrases always arrays. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Give the variable a name. | nom is left out, empty or only spaces. |
| A variable's name has 200 characters at most. | nom is longer, once trimmed. |
| The name didn't change. | nom is the variable's name now. |
| A name that starts with $ is that of a model's scores as variables ($M1.t1): choose another one. | nom starts with $. |
| A variable is already called “&lt;name&gt;”. | Another variable of the project has that name, capitals counting (unlike nom_variable_pris, the message ends with a full stop). |
| “&lt;name&gt;” is the name of an identifier of the observations: a condition or a formula would no longer know which one it reads. | nom is the name of one of the observations' identifiers, the first included (Observation for a table). |

#### `supprimer_variable`

Deletes a variable and its values from a project, or previews what the deletion leaves (in the Data view, a column header's right-click menu › Delete).

Since 2.0. Changes the project, with an undo step that `annuler` takes back. With apercu nothing changes. Otherwise one undo step (deletion of &lt;name&gt;, in the request's language), also noted as a data step that an update replays: the variable's column and values leave the data and the datasets, and the project's coordinates go when it was one of them; computed variables that read it keep their values. Edit › Undo brings it back in one step, Edit › Redo deletes it again.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `variable` | `number` | yes | The variable's number. |
| `apercu` | `boolean` | no | true to only say what the deletion would leave (suppression), changing nothing; a refusal comes as the same error. Default: `false`. |

**Result:** [`Projet`](#type-projet) \| `object`, with the fields below. With apercu, {suppression} only; otherwise the project after the deletion, with suppression.

| Field | Type | Meaning |
|---|---|---|
| `suppression` | `object` | {variable: its number; nom: its name; code: the R-code models that name it in their code, to change there; formules: the computed variables whose formula reads it, which keep their values but can't be computed again; coordonnee: whether it was one of the project's coordinates, which it then no longer has; phrases: what it does, in sentences, in the request's language}; code, formules and phrases always arrays. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| It's the project's only variable: it can't be deleted. | The project's data have a single variable. |
| &lt;models&gt; uses “&lt;name&gt;”: modify or delete it first (Model › Modify model). | A model of the project uses the variable (a term, its class, a curve, an offset, a random effect, its cross-validation groups, its settings) or names it in its condition; &lt;models&gt; their names ("M1, M2 and M3"). An R-code model doesn't stop it: phrases names it. |

#### `type_variable`

Treats a variable as categories or as numbers, the X terms of the project's model definitions following, or previews it (in the Data view, a column header's right-click menu › Treat as categories, or Treat as numbers).

Since 2.0. Changes the project, with an undo step that `annuler` takes back. With apercu nothing changes. Otherwise one undo step (&lt;name&gt; as categories, or &lt;name&gt; as numbers, in the request's language), also noted as a data step that an update replays. The X terms of the project's model definitions follow: one term per category, with the term's scaling, block and modifier and without its transformation, or a single term in numbers; a definition from a .usp file loses its stored solution. The session's fits keep their results, and refitting, predicting with or exporting one that used the variable is refused until it is fitted again from its definition. Edit › Undo takes it back in one step, Edit › Redo makes it again.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `variable` | `number` | yes | The variable's number. |
| `type` | `string` | yes | What the variable becomes. Values: `qualitative` (categories: each distinct value one (a site coded 1, 2, 3), named by the number written to 15 significant digits, in increasing order), `quantitative` (numbers: each category read as a number (3,5 or 3.5)). |
| `apercu` | `boolean` | no | true to only say what the change would do (type), changing nothing; a refusal comes as the same error. Default: `false`. |

**Result:** [`Projet`](#type-projet) \| `object`, with the fields below. With apercu, {type} only; otherwise the project after the change, with type.

| Field | Type | Meaning |
|---|---|---|
| `type` | `object` | {variable: its number; nom: its name; type: qualitative or quantitative, as asked; categories: its categories now (empty as numbers); modeles: the project's models whose definitions follow; code: the R-code models that will see it otherwise when fitted again; phrases: what it does, in sentences, in the request's language}; categories, modeles, code and phrases always arrays. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Unknown type: &lt;type&gt; (qualitative or quantitative). | type is left out, or is neither qualitative nor quantitative. |
| “&lt;name&gt;” already is categories. | type is qualitative and the variable already is; likewise “&lt;name&gt;” already is numbers. for quantitative. |
| &lt;models&gt; uses “&lt;name&gt;” otherwise than as an X variable (as Y, a class, a curve, a random effect, in its condition or its settings): modify it first (Model › Modify model). | A model of the project has the variable as a Y term, elsewhere than in its X terms, or in its condition; &lt;models&gt; their names ("M1 and M2"). |
| A qualitative variable enters a cross term once at most, neither squared nor cubed: "&lt;term&gt;". | As categories, when a model's X term has the variable twice (its square); &lt;term&gt; its variables' names joined by ×. |
| These categories of “&lt;name&gt;” aren't numbers: “&lt;category&gt;”, …. Replace them first (Edit › Find and replace), or empty them. | As numbers, a category doesn't read as a number; the first five are named. |

#### `coordonnees`

Gives the project's coordinates, a proposal made from its variables' names and the coordinate systems offered, and with regler sets or removes them.

Since 1.0. Changes the project, with an undo step that `annuler` takes back. Only with regler: true, which sets the coordinates (or removes them) as one undo step (coordinates), also noted as a data step, which Update the data replays; without it nothing changes.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `regler` | `boolean` | no | true sets the coordinates to x, y and systeme, or removes them without x. Default: `false`. |
| `x` | `number` | no | With regler: the number of the variable holding x (the east, or the longitude); absent, the coordinates are removed. |
| `y` | `number` | no | With regler and x: the number of the variable holding y (the north, or the latitude). |
| `systeme` | `integer` \| `string` | no | With regler and x: their system, an EPSG code (2154, or "EPSG:2154") among those Nuée knows (WGS 84, ETRS89, RGF93, Lambert-93, CC42 to CC50, Lambert II étendu, LAEA Europe, web Mercator, UTM zones), or "plan" for plane coordinates in metres of an unstated system. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `coordonnees` | [`Coordonnees`](#type-coordonnees) \| `null` | The coordinates set (after the change, with regler); null without. |
| `proposition` | `object` \| `null` | Two quantitative variables whose names look like coordinates (longitude and latitude, X and Y, east and north…), {x, y, systeme: the system their values suggest, 4326, 2154, 27572 or "plan"}; null when none do. |
| `systemes` | `object[]` | The systems the interface lists, each {code, nom} (always an array). |
| `projet` | [`Projet`](#type-projet) \| `null` | With regler, the project after the change; null otherwise. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Unknown coordinate system: &lt;code&gt; | With regler, systeme isn't a system Nuée knows. |
| Coordinates must be two different quantitative variables of the project. | With regler, x or y isn't a quantitative variable of the project, or they are the same. |
| Longitudes and latitudes run from −180 to 180 and from −90 to 90: “&lt;x&gt;” or “&lt;y&gt;” goes beyond. Choose the system of the plane coordinates. | With regler, a longitude and latitude system for values beyond those bounds. |

### Data steps and updates

[`etapes`](#etapes) · [`mettre_a_jour`](#mettre_a_jour) · [`retirer_etape`](#retirer_etape)

#### `etapes`

Gives the file a project's data came from, the changes made to them in Nuée since as steps that can be replayed, and whether the data are still those the file and the steps give (Edit › Data steps).

Since 2.0. Changes the session, not a project's content. No project changes; conformes is kept in a cache for the project's revision of its data (serveur.R: conformite_projet), so that asking again before they change doesn't read them all again.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `conformes` | `boolean` | no | false leaves out conformes and raison, so that the data aren't read through for their fingerprint: the interface asks so when it only checks the source file (the status bar's notice). Default: `true`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `source` | [`FichierLu`](#type-fichierlu) \| `null` | The file the data came from, with three more fields: table, its sheet or table (null for none, and for a KoboToolbox or ODK import, whose settings name it); lecteur, how it is read again, tableau (a table, as File › Open reads it, with the decisions of its first reading on its columns), archive (a table of an ODK Central .zip opened as it is) or collecte (a KoboToolbox or ODK import, with its settings); formulaire, only for an import made with an XLSForm form, that form's FichierLu. null for a project without a source (a pasted table, a summary by group, imported spectra, a .usp project, the example, one from before 1.1): its steps are replayed only on a file given to mettre_a_jour. |
| `etapes` | `object[]` | Its steps, in the order they were made and are replayed (always an array, empty for none), each {n: its number from 1, retirer_etape's n; etiquette: what it does, written from its request and parameters in the request's language ("edit of yield (S1)", "sugar renamed brix"; "merge with Bob: …" for one a merge added; for a request this version doesn't know, the label noted with it); date: when it was made, local time, YYYY-MM-DDThh:mm:ss; demande: the request that made it, one of modifier_valeur, variable_calculee, imputer, classes_valeurs, ajouter_colonnes, ajouter_raster, remplacer, categories, coller_cellules, decrire_variables, renommer_variable, supprimer_variable, type_variable, coordonnees, classe_selection, baci_modele, scores_en_variables, kmeans_variable; fichiers: the files it read (an added table, a raster), each a FichierLu, always an array}. |
| `conformes` | `boolean` \| `null` | Unless conformes is false: whether the data are still those the source and the steps give, by their fingerprint after the last step (etapes.R: donnees_conformes); false when they changed otherwise since (the R console, Nuée 1.0), null for a project without a fingerprint (from before 1.1, a .usp project, the example). |
| `raison` | `string` | Unless conformes is false: what to tell of conformes, in the request's language: for false, that updating the data would bring back those the source and the steps give; for null, that the project's changes from before 1.1 aren't steps; "" when the data conform. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |

#### `mettre_a_jour`

Updates a project's data from its source file read again, or from another file such as next year's, its data steps replayed on it, or previews what the update would change (Edit › Update the data).

Since 2.0. Changes the project, with an undo step that `annuler` takes back. With apercu, or a choisir answer, nothing changes. Otherwise one undo step (data update, in the request's language): the data become those of the file with the steps replayed, matched to the project as the report says (a variable keeps its number, name and categories, new categories going before the last; an observation keeps its number; new ones join, absent ones leave, the prediction sets too; a computed variable missing from the file is computed again from its formula); the model definitions follow (category codes, a term for each new category; the removed observations leave them, the new ones join those that admit them but the models of sans_nouvelles); the file read becomes the source (its relative path written again at the next save); every step stays, those skipped included; the data's fingerprint is renewed, so a change made outside the steps is gone. Documents, figures and the session's fits stay, the fits with the results of the data before until they are fitted again (the interface fits every model again, etapes.js). Edit › Undo gives back the data, the source and the steps in one step, and Edit › Redo makes the update again.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `chemin` | `string` | no | The file to read; by default the project's source, found by its path from the folder of the project's file first, then by its absolute path. Needed for a project without a source, the file then read as File › Open reads a table (a KoboToolbox or ODK export by its import at the default settings); with a source, any file is read the source's way, with its decisions: the column that names the observations, the secondary identifiers, the columns read as numbers, or an import's settings and XLSForm form. |
| `table` | `string` | no | The sheet or table of the file to read (a workbook, an R file, a GeoPackage; the table of an ODK Central .zip opened as it is, by default the source's). For a table, by default the source's when the file has it or has no tables, else the file's only one: a file of several, none of them the source's, answers choisir. Ignored for a KoboToolbox or ODK import, whose settings name the table. |
| `sans_nouvelles` | `string[]` \| `number[]` | no | The project's models that don't take the new observations the file brings, by their numbers (ResumeModele's numero) or names; the report's observations_choisies are those worth asking. By default each model takes those it admits. Default: `[]`. |
| `apercu` | `boolean` | no | true to only get the report of what the update would change, changing nothing. Default: `false`. |

**Result:** [`RapportMiseAJour`](#type-rapportmiseajour) \| `object`, with the fields below. With apercu, the report and apercu; otherwise the report and projet. When the file has several tables, none of them the source's, and table isn't given: {choisir, chemin} only, and nothing changes.

| Field | Type | Meaning |
|---|---|---|
| `apercu` | `boolean` | With apercu: true (nothing changed). |
| `projet` | [`Projet`](#type-projet) | Without apercu: the project after the update; its annulable is the update's label. |
| `choisir` | `string[]` | Instead of a report, the file's tables to choose from (always an array), to ask again with table. |
| `chemin` | `string` | With choisir: the file's absolute path, with / as separator. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| This project has no source file: give the file to read. | chemin is left out and the project has no source (etapes' source is null). |
| The source file isn't found: &lt;path&gt; | The project's source file (etapes' source) is neither at its path from the folder of the project's file nor at its absolute path, &lt;path&gt; (etapes.R: fichier_retrouve). ([`source_introuvable`](#errors)) |
| File not found: &lt;path&gt; | The file named doesn't exist. ([`fichier_introuvable`](#errors)) |
| Unrecognised file format: .&lt;extension&gt; | The file's extension isn't one Nuée reads. ([`format_inconnu`](#errors)) |
| The import's XLSForm form isn't found: &lt;path&gt; | The source is a KoboToolbox or ODK import made with an XLSForm form, and the form is neither at its relative nor at its absolute path. ([`formulaire_introuvable`](#errors)) |
| Unknown file reader: &lt;reader&gt; | The project's source is read in a way this version doesn't know (a project saved by a newer Nuée). ([`lecteur_inconnu`](#errors)) |
| The table no longer has the column “&lt;column&gt;”, which named the observations: they can't be matched. | The file read lacks the column that named the observations when the source was first read (its decisions, nouveau.R: projet_depuis_tableau). ([`colonne_observations_absente`](#errors)) |
| The table is empty | The table read has no row or no column (nouveau.R: projet_depuis_tableau). ([`tableau_vide`](#errors)) |
| &lt;lines&gt;: more values than the first line has columns (&lt;number&gt;). The values would be shifted: a value may contain the separator (&lt;separator&gt;) or a quote, or a column name is missing from the first line. | Lines of a delimited text have more values than its first line has names (formats.R). ([`valeurs_decalees`](#errors)) |
| Table not found in the file: &lt;table&gt; | table isn't one of an R file's tables; an unknown sheet of a workbook gets readxl's own Sheet '&lt;table&gt;' not found. |
| &lt;the reader's own message&gt; | The file can't be read: R's or its package's message (readxl, haven…), untranslated, or one of ouvrir's refusals of an R file (R older than 4.4, an object Nuée doesn't open). |
| &lt;file&gt; isn't a KoboToolbox or ODK Central export: it doesn't have their columns (\_id, \_uuid, \_submission_time, \_index…; SubmissionDate, KEY, meta-instanceID…). Open it with File › Open. | The file was given as an export but doesn't have two of their metadata columns (kobo.R: kobo_refus_export). ([`pas_un_export`](#errors)) |
| &lt;file&gt; is an XLSForm form (survey and choices sheets), not the data: give it as the form, beside the export of the data. | A workbook with survey and choices sheets was given as the export. ([`formulaire_pas_export`](#errors)) |
| &lt;file&gt; holds no CSV file: it isn't an ODK Central export. | A .zip without a CSV file (outside media/) was given as an ODK Central export. ([`zip_sans_csv`](#errors)) |
| Unknown table in the export: “&lt;table&gt;” (it has: “&lt;table&gt;”, …). | table isn't one of the export's tables. ([`table_export_inconnue`](#errors)) |
| Unknown metadata: &lt;role&gt; (one can keep: envoi, auteur, identifiant, releve, validation). | garder names a role that isn't one of these. ([`metadonnee_inconnue`](#errors)) |
| &lt;file&gt; isn't an XLSForm form: an Excel workbook with a “survey” sheet (and “choices”) is needed. | formulaire isn't an Excel workbook with a survey sheet. ([`pas_un_formulaire`](#errors)) |
| The form doesn't have the language “&lt;language&gt;”; it has: “&lt;language&gt;”, …. | langue isn't one of the form's label languages. ([`langue_formulaire_inconnue`](#errors)) |
| The form &lt;file&gt; doesn't match this export: none of its questions is a column of the table “&lt;table&gt;”. | The form given isn't the one the export was made with. ([`formulaire_sans_rapport`](#errors)) |
| “&lt;table&gt;” isn't a parent table of “&lt;table&gt;” (its parent tables: “&lt;table&gt;”, …). | parents names a table that isn't a parent of the imported table. ([`table_parente_inconnue`](#errors)) |
| The table “&lt;table&gt;” has no column “&lt;column&gt;”. | parents names a column that the parent table's import doesn't give. ([`colonne_parente_inconnue`](#errors)) |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |

#### `retirer_etape`

Removes one of a project's data steps, its source file read again and the other steps replayed on it in place of its data, or previews what the removal would change (Edit › Data steps, Remove).

Since 2.0. Changes the project, with an undo step that `annuler` takes back. With apercu, or a choisir answer, nothing changes. Otherwise one undo step (removal of step &lt;n&gt;, in the request's language): the data become those the source file, read again, and the other steps give, matched to the project and its model definitions following as mettre_a_jour does it; the step leaves the list, those after it moving up one number, the source's state is renewed, and a change made outside the steps (conformes false) is gone. The session's fits keep their results until they are fitted again (the interface fits every model again, etapes.js). Edit › Undo gives back the data, the source and the steps, the step removed included, in one step; Edit › Redo removes it again.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `n` | `integer` | yes | The step's number, as etapes gives it (n, from 1); a decimal is cut to its whole part. At least 1. |
| `apercu` | `boolean` | no | true to only get the report of what the removal would change, changing nothing; the preview is made even when the source file changed since it was read, its new data then included. Default: `false`. |
| `table` | `string` | no | The sheet or table of the source file to read when the source's own is no longer one of the file's (the answer without it is choisir), as for mettre_a_jour. |

**Result:** [`RapportMiseAJour`](#type-rapportmiseajour) \| `object`, with the fields below. The report of the data that the source and the other steps give, compared to the project's (its etapes numbered without the step removed): with apercu, the report and apercu; otherwise the report and projet. When the source file has several tables, none of them the source's, and table isn't given: {choisir, chemin} only, and nothing changes.

| Field | Type | Meaning |
|---|---|---|
| `apercu` | `boolean` | With apercu: true (nothing changed). |
| `projet` | [`Projet`](#type-projet) | Without apercu: the project after the removal; its annulable is the removal's label. |
| `choisir` | `string[]` | Instead of a report, the file's tables to choose from (always an array), to ask again with table. |
| `chemin` | `string` | With choisir: the file's absolute path, with / as separator. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Unknown step: &lt;n&gt; | n is left out, isn't a number, or is below 1 or above the number of the project's steps. |
| This project has no source file: its steps can't be redone without it. | The project has no source (etapes' source is null): a pasted table, a summary by group, imported spectra, a .usp project, the example, one from before 1.1. |
| The source file isn't found: &lt;path&gt; | The project's source file (etapes' source) is neither at its path from the folder of the project's file nor at its absolute path, &lt;path&gt; (etapes.R: fichier_retrouve). ([`source_introuvable`](#errors)) |
| The source file changed since it was read: removing the step would also bring in its new data. Update the data first, or look at the removal's preview. | Without apercu, the source file changed since it was read (etapes' source.modifie): its size, or its date and its content. Besides erreur, the answer carries source_modifiee: true. |
| The file &lt;file&gt;, read by a step or by the import, changed since it was read: removing the step would also bring in its new data. Update the data first, or look at the removal's preview. | Without apercu, the import's XLSForm form or a file one of the remaining steps reads (joined columns, a raster) changed since it was read. Besides erreur, the answer carries source_modifiee: true. |
| The import's XLSForm form isn't found: &lt;path&gt; | The source is a KoboToolbox or ODK import made with an XLSForm form, and the form is neither at its relative nor at its absolute path. ([`formulaire_introuvable`](#errors)) |
| Unknown file reader: &lt;reader&gt; | The project's source is read in a way this version doesn't know (a project saved by a newer Nuée). ([`lecteur_inconnu`](#errors)) |
| The table no longer has the column “&lt;column&gt;”, which named the observations: they can't be matched. | The file read lacks the column that named the observations when the source was first read (its decisions, nouveau.R: projet_depuis_tableau). ([`colonne_observations_absente`](#errors)) |
| The table is empty | The table read has no row or no column (nouveau.R: projet_depuis_tableau). ([`tableau_vide`](#errors)) |
| &lt;lines&gt;: more values than the first line has columns (&lt;number&gt;). The values would be shifted: a value may contain the separator (&lt;separator&gt;) or a quote, or a column name is missing from the first line. | Lines of a delimited text have more values than its first line has names (formats.R). ([`valeurs_decalees`](#errors)) |
| Table not found in the file: &lt;table&gt; | table isn't one of an R file's tables; an unknown sheet of a workbook gets readxl's own Sheet '&lt;table&gt;' not found. |
| &lt;the reader's own message&gt; | The source file can't be read (with apercu, as it is now): R's or its package's message, untranslated. |
| &lt;file&gt; isn't a KoboToolbox or ODK Central export: it doesn't have their columns (\_id, \_uuid, \_submission_time, \_index…; SubmissionDate, KEY, meta-instanceID…). Open it with File › Open. | The file was given as an export but doesn't have two of their metadata columns (kobo.R: kobo_refus_export). ([`pas_un_export`](#errors)) |
| &lt;file&gt; is an XLSForm form (survey and choices sheets), not the data: give it as the form, beside the export of the data. | A workbook with survey and choices sheets was given as the export. ([`formulaire_pas_export`](#errors)) |
| &lt;file&gt; holds no CSV file: it isn't an ODK Central export. | A .zip without a CSV file (outside media/) was given as an ODK Central export. ([`zip_sans_csv`](#errors)) |
| Unknown table in the export: “&lt;table&gt;” (it has: “&lt;table&gt;”, …). | table isn't one of the export's tables. ([`table_export_inconnue`](#errors)) |
| Unknown metadata: &lt;role&gt; (one can keep: envoi, auteur, identifiant, releve, validation). | garder names a role that isn't one of these. ([`metadonnee_inconnue`](#errors)) |
| &lt;file&gt; isn't an XLSForm form: an Excel workbook with a “survey” sheet (and “choices”) is needed. | formulaire isn't an Excel workbook with a survey sheet. ([`pas_un_formulaire`](#errors)) |
| The form doesn't have the language “&lt;language&gt;”; it has: “&lt;language&gt;”, …. | langue isn't one of the form's label languages. ([`langue_formulaire_inconnue`](#errors)) |
| The form &lt;file&gt; doesn't match this export: none of its questions is a column of the table “&lt;table&gt;”. | The form given isn't the one the export was made with. ([`formulaire_sans_rapport`](#errors)) |
| “&lt;table&gt;” isn't a parent table of “&lt;table&gt;” (its parent tables: “&lt;table&gt;”, …). | parents names a table that isn't a parent of the imported table. ([`table_parente_inconnue`](#errors)) |
| The table “&lt;table&gt;” has no column “&lt;column&gt;”. | parents names a column that the parent table's import doesn't give. ([`colonne_parente_inconnue`](#errors)) |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |

### Versions and merges

[`fichier_change`](#fichier_change) · [`fusionner`](#fusionner) · [`fusionner_finir`](#fusionner_finir)

#### `fichier_change`

Says whether an open project's file was changed on disk since the project read or saved it (a colleague saving it on a shared folder), and by whom when the file says.

Since 2.0. Changes the session, not a project's content. No project's content changes. When the file's size or date changed but not its content (a sync program touched it, it was copied back), the size and date the engine keeps of the file become its own, so that Save doesn't ask before writing over it; the first comparison of contents also keeps the fingerprint of the copy compared with.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |

**Result:** `object`, with the fields below. For a project the engine never read from a project file nor saved to one (a table opened or pasted, imported spectra…; an example project knows its file of exemple/): {change: false, connu: false, existe: false, base: false}. Otherwise its file's size and date compared with those known, then, when they differ, its content (a date alone changed, the content the same, isn't a change); the interface asks every 30 seconds (versions.js: surveillerDossiers).

| Field | Type | Meaning |
|---|---|---|
| `change` | `boolean` | Whether the file's content is no longer what the project read or last saved; false when the file no longer exists. |
| `connu` | `boolean` | Whether the engine knows a file of the project: a .nuee or .json it was opened from, a file it was saved to, or the file of a recovery copy reopened with its lu. |
| `existe` | `boolean` | Whether that file exists now (false: deleted, moved or renamed). |
| `chemin` | `string` | That file, its absolute path with / as separator; absent when connu is false. |
| `base` | `boolean` | Whether the session keeps a copy of the file as the project read or last saved it, so that fusionner with projet can merge their changes (none for a project reopened from a recovery copy, as after a restart of the engine, for an old version or a hidden project, or when the copy couldn't be written). |
| `taille` | `number` | Its size now, in bytes; absent when it doesn't exist. |
| `date` | `string` | Its modification time now, in the engine's local time, YYYY-MM-DDThh:mm:ss (not seconds since 1970, as EtatFichier and Save's 409 give it); absent when it doesn't exist. |
| `auteur` | `string` | With change, who saved it, when the file says (enregistrer's auteur, written in its nuee); absent otherwise. |
| `version` | `string` | With change, the version of Nuée that saved it, when the file says; absent otherwise. |

**Errors:**

| Message (in English) | When |
|---|---|
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |

#### `fusionner`

Prepares the merge of two versions of a project by meaning, from the last version both had, and says what clashes and what each side brought, without changing anything yet.

Since 2.0. Changes the session, not a project's content. No project changes. The prepared merge is kept in the session under its id until fusionner_finir uses it (the last five prepared are kept: a sixth drops the oldest); with projet, the copy of their file it read is kept with it, in the engine's temporary folder.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | no | My side as the session's project, as it is now, its unsaved changes included (a project on a shared folder: Save's 409 answer says fusion when this is possible). Without base, the base is the copy of its file the session kept when the project read or last saved it; without leur, their side is that file as it is now on disk (someone else may have saved it since). With projet, their side is copied into the engine's temporary folder first, and read from the copy. An old version opened read-only, or a project the interface doesn't show (ouvrir's lecture_seule, cache), is refused. |
| `mien` | `string` | no | Without projet, my side: a project file (.nuee, .json), required then. Ignored with projet. |
| `base` | `string` | no | The version both sides last had, a project file (for a project shared with GitHub or GitLab, the two histories' common version: interface/versions.js). Required without projet; with projet, the copy of its file kept in the session by default. |
| `leur` | `string` | no | Their side, a project file. Required without projet; with projet, the project's own file by default (the one it was read from or last saved to). |
| `dossier` | `string` | no | The folder from which the data's source and the steps' files are found, by their path from it first, then by their absolute path (etapes.R: fichier_retrouve), for a merge by steps; by default the project's folder with projet, else the folder of mien. |

**Result:** `object`, with the fields below. The merge as fusion.R's fusionner_projets makes it with no choice yet, what the interface's merge dialog shows (versions.js: contenuFusion); nothing is merged until fusionner_finir.

| Field | Type | Meaning |
|---|---|---|
| `fusion` | `string` | The prepared merge's id (fusion1, fusion2…): fusionner_finir's fusion. |
| `auto` | `boolean` | true when nothing clashes: fusionner_finir needs no choice. |
| `conflits` | [`Conflit`](#type-conflit)`[]` | The clashes, each needing a choice in fusionner_finir's choix, in the order the merge meets them (the data, coordinates and prediction sets, then models, documents, figures and the project's other fields); always an array, empty when auto. |
| `apports` | `object` | What each side changed since the base, {mien, leur}, each an array of sentences in the request's language (always arrays): models, documents and figures added, removed or changed (a model's changes in a few words, "M3: 2 component(s) → 4 component(s); X: + x5", four at most, then how many more), their model renamed when both sides made one of the same name, the data updated from a file, the steps added, variables and observations added or removed, cells changed, new categories, units, descriptions or formulas. |
| `donnees` | `object` | How the data were merged, {mode, raison}: mode "inchangees" (neither side changed them), "etapes" (both sides only added steps to the base's: replayed from the data's source, read again, so that the data stay what their steps give) or "cellules" (cell by cell, the variables by their names, the observations by their identifiers); raison, with cellules, why not by steps when it is worth saying ("data changed outside the steps", "steps of the base were removed", "the source &lt;file&gt; isn't found on this computer", "the source has several tables", "the step “&lt;step&gt;” no longer applies after the others (&lt;why&gt;)", or the replay's own error), else "" (also for projects without a data source, such as those of Nuée 1.0). |
| `qui` | `string` | Their side, for the sentences: who saved their file, when the file says (enregistrer's auteur, written in its nuee), else that file's name ("partage.nuee"). |
| `notes` | `string[]` | What to know of the merge, in the request's language: a variable merged as quantitative whose text values from one side were left empty (always an array). |

**Errors:**

| Message (in English) | When |
|---|---|
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| This old version is read-only: it isn't merged. | projet is an old version opened read-only, or a project the interface doesn't show (ouvrir's lecture_seule or cache). |
| My side is missing: q$mien (a file) or q$projet. | Neither projet nor mien is given (the interface asks fusionner without parameters once, to know whether the engine merges: any answer but Unknown request says it does). |
| No common version to merge from: this project was neither opened from a file nor saved since Nuée started. | There is no base: base isn't given and projet keeps no copy of its file (it was never opened from a .nuee or .json nor saved since the engine started, it was reopened from a recovery copy, or the copy couldn't be written), or base or that copy isn't an existing file. Without projet, a base missing or not found gets this message too. |
| File not found: &lt;path&gt; | The file named doesn't exist. ([`fichier_introuvable`](#errors)) |
| Not a Nuée project (.nuee): &lt;file name&gt; | base, mien or leur isn't a .nuee or .json file (with projet, leur is read from its copy, named .nuee: another kind of file gets jsonlite's own message instead). |
| &lt;jsonlite's own message&gt; | A file that isn't a project's JSON, untranslated. |

#### `fusionner_finir`

Finishes a merge prepared by fusionner with the side chosen for each clash: the merged project takes the place of the session's project as one undo step, or a merge of three files is written to a file.

Since 2.0. Changes the project, with an undo step that `annuler` takes back. A merge prepared with projet: the merged project takes the place of the session's project under the same id (its name, its file and the interface's session it holds stay mine), counts as modified, and the session's models whose observations came from a condition of the project follow its merged condition; when their side was the project's own file, the project counts as having read that file as fusionner found it, so that Save writes over it without asking. That is one undo step, "merge with &lt;qui&gt;": Edit › Undo (annuler) gives the project back as it was before, with the documents, figures and file state it had when they haven't changed since the merge (the file is then seen changed again, and can be merged again), Edit › Redo (retablir) merges it again, and a new change empties Redo. A merge of three files changes no project and leaves nothing to undo: it only writes the merged project. Either way the prepared merge is used up.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `fusion` | `string` | yes | The prepared merge's id (fusionner's fusion). Its project is the one it was prepared with: a projet sent with it is ignored. |
| `choix` | `object` | no | The side chosen for each clash, by its id: {"&lt;Conflit's id&gt;": "mien" \| "leur"}, mien keeping what my side made of what clashes, leur taking theirs; one for every clash, any other value counting as none. The merge is made again with these choices. Default: `{}`. |
| `chemin` | `string` | no | Where to write the merged project (a .nuee). For a merge of three files, without it &lt;my side's project name&gt;.nuee (fusion.nuee without a name) in a new folder of the engine's temporary folder, for a browser to fetch with /api/fichier; its relative paths are written from fusionner's dossier. For a merge prepared with projet, the merged project is also written there once it has taken the project's place, its relative paths from the project's folder; the project still counts as modified and keeps the state it knew of its file, so that when chemin is the project's own file, the next save over it asks first (409). |
| `version` | `string` | no | Nuée's version, written in the file with R's and the packages', when a file is written. Default: `""`. |
| `auteur` | `string` \| `object` | no | Who merges, written in the file as enregistrer writes it (its nuee.auteur, which fichier_change and fusionner give as auteur and qui): a name, or {nom, email} of which only nom is written; none without it. |

**Result:** `object`, with the fields below. A merge of three files (fusionner without projet): {chemin}. A merge prepared with projet: {projet, etiquette, documents}, with chemin too when chemin was given.

| Field | Type | Meaning |
|---|---|---|
| `chemin` | `string` | The file written. |
| `projet` | [`Projet`](#type-projet) | The merged project, under the same id: modified, its annulable the merge's label. |
| `etiquette` | `string` | The label of its undo step, "merge with &lt;qui&gt;" (fusionner's qui), in the request's language. |
| `documents` | `string[]` | For a merge prepared with projet: the ids of the project's documents whose text the merge changed, for a client to read them again in the editors it has open (always an array). Since 2.0. |

**Errors:**

| Message (in English) | When |
|---|---|
| Unknown merge: &lt;id&gt; | fusion isn't a merge prepared in this session and not yet finished: finished already, dropped (only the last five prepared are kept), or prepared before the engine restarted. |
| Choose mine or theirs for: &lt;titles&gt; | choix doesn't give mien or leur for every clash of the merge made again; &lt;titles&gt; are those clashes' titles, separated by commas. |
| Unknown project: &lt;id&gt; | The project the merge was prepared with (fusionner's projet) was closed since. |
| The project changed since the merge was prepared: it must be done again. | A merge prepared with projet, and the project changed since fusionner (in anything but its unsaved state and what fichier_change keeps of its file). |
| Couldn't replace &lt;path&gt;: another program is using it (sync, antivirus, the file open elsewhere), or it is read-only. The file was left as it was; try again, or save under another name. | The file just written couldn't take the place of the old one within five seconds (nouveau.R: ecrire_sans_risque). ([`fichier_occupe`](#errors)) |
| &lt;R's own message&gt; | The file can't be written, its folder missing for instance ("cannot open the connection", untranslated). With projet, the merge has already taken the project's place, its undo step kept, when chemin fails. |

### Defining and fitting models

[`verifier`](#verifier) · [`condition_observations`](#condition_observations) · [`jeu_test`](#jeu_test) · [`formule`](#formule) · [`lire_formule`](#lire_formule) · [`ajuster`](#ajuster) · [`reajuster`](#reajuster) · [`modele`](#modele) · [`supprimer_modele`](#supprimer_modele) · [`note_modele`](#note_modele) · [`modeles_classe`](#modeles_classe) · [`par_reponse`](#par_reponse) · [`baci_modele`](#baci_modele) · [`empreinte_code`](#empreinte_code) · [`code_approuve`](#code_approuve) · [`code_modeles`](#code_modeles)

#### `verifier`

Checks a model's data before it is fitted: the variables and observations with more than half of their values missing, and the constant variables.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |

Also the parameters of the set [`definition`](#set-definition).

**Result:** `object`, with the fields below. Only the problems; TRIM and DISTANCE check their own data when fitted, and an R-code or plugin model has its code do it: they give two empty tables written {}.

| Field | Type | Meaning |
|---|---|---|
| `variables` | `object` | A table of the variables that have more than half of their values missing or are constant: uid, nom, manquantes (the share missing, from 0 to 1, of its worst term), constante; its columns are empty arrays when there is none. |
| `observations` | `object` | A table of the observations that miss more than half of their X values: uid, nom, manquantes (a share, from 0 to 1). |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Unknown model type: &lt;type&gt;. | type isn't one of the definition's types (nouveau.R: definir_modele). ([`type_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| A PLS-DA needs a class variable | A PLS-DA or OPLS-DA definition without classe. ([`classe_requise`](#errors)) |
| Unknown scaling: &lt;scaling&gt; | echelle or a value of echelles isn't UV, Par, Ctr or none. ([`echelle_inconnue`](#errors)) |
| Unknown transformation: &lt;transformation&gt; | A value of transformations isn't one of the forms Nuée reads (nouveau.R: lire_transformation). ([`transformation_inconnue`](#errors)) |
| A cross term can only use X variables | A cross term of a PCA, PLS or OPLS uses a variable that isn't in x. ([`croise_hors_x`](#errors)) |
| "&lt;function&gt;" isn't allowed in a condition: comparisons (==, !=, &lt;, &gt;=…), &amp;, \|, !, %in%, is.na(), ifelse(), arithmetic (+, log10()…) and the group and lag functions of New variable formulas (group_mean(), previous()…). | The condition calls another function; it may also be unreadable, name an unknown variable or not give one logical value per observation (condition_observations lists those messages). ([`condition_refusee`](#errors)) |
| None of the &lt;n&gt; ticked observations meets the condition &lt;condition&gt;. | The condition keeps none of the model's ticked observations. ([`condition_aucune`](#errors)) |
| This analysis doesn't predict left-out observations: it takes no test set. | A test set for an analysis that doesn't predict (RDA, CA…). ([`sans_test`](#errors)) |
| A regression needs a single Y variable, a quantitative one. | A regression's y is empty, has several variables or a qualitative one. ([`regression_une_reponse`](#errors)) |
| A mixed model needs at least one random effect. | An LMM without aleatoires. ([`mixte_sans_aleatoire`](#errors)) |
| Unknown distribution of the response. | famille isn't one of the distributions Nuée fits. ([`loi_inconnue`](#errors)) |
| Cross-validation by "&lt;groups&gt;": the model's observations have a single value, so a single group. At least two are needed. | vc's identifier or variable has one value on the model's observations. ([`vc_un_groupe`](#errors)) |
| No observation is ticked for this model: tick some in the Observations tab (those of the test set aren't part of it). | exclues and test leave the model no observation. |
| An OPLS needs at least one Y variable | An OPLS without y; an OPLS-DA with fewer than two classes gets "An OPLS-DA needs at least two classes", a PLS-DA with a single class "A PLS-DA needs at least two classes". |
| Choose at least one X variable. | A PCA, PLS or OPLS (or PLS-DA, OPLS-DA) without x. |
| Unknown spectral preprocessing: &lt;type&gt; | A step of spectres isn't snv, msc, sg, detrend or ligne_base. |
| Unknown cross-validation: &lt;validation&gt; (sans_fuite, centres or complet). | validation is neither empty nor one of its values. |
| Cross-validation: this project has no ID "&lt;id&gt;". | vc names an identifier the project hasn't. |
| Unknown grouping "&lt;by&gt;" | A random effect's or the CAR1 time's by is neither id nor variable. |
| “&lt;name&gt;” has values too large for the computation&lt; (up to &lt;value&gt;)&gt;: their squares exceed the numbers R can hold. Divide it by a power of 10 (Edit › New variable). | A UV or Pareto term has values beyond about 1e150 in size, or infinite ones; values all below 1e-150 get "“&lt;name&gt;” has values too small for the computation (at most &lt;value&gt;): their squares vanish. Multiply it by a power of 10 (Edit › New variable)." |
| R-code models are turned off on this computer by your organisation's policy (DisableCustomCode). | The engine was started with NUEE_SANS_CODE=1 (etat's code_interdit) and the request defines, fits or exports an R-code model, or asks a code's fingerprint (code.R: refuser_si_interdit). ([`code_interdit`](#errors)) |
| Write the model's R code: its last value is the fitted model. | An R-code model (CODE) whose options' code is left out or blank. |
| &lt;a plugin definition's refusal&gt; | A plugin's model (PLUGIN) whose definition its plugin refuses, in the engine's words: no options' greffon.id (Which plugin? The definition names none (options$greffon$id).), a plugin not installed, a version neither installed nor in the project, a plugin with errors or that needs a newer Nuée, an unknown input or option, a variable an input doesn't accept, too few or too many variables, an option's value of another type or out of its bounds (greffons.R: definir_plugin). |

#### `condition_observations`

Counts the ticked observations that a model's condition keeps, for New model's Observations tab.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `condition` | `string` | no | The condition, an R expression read without running anything (condition.R: lire_condition); empty or absent, every ticked observation counts. |
| `exclues` | `number[]` | no | The unticked observations' numbers, not counted. |
| `test` | `number[]` | no | The test set's observations, not counted either. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `n` | `integer` | The ticked observations where the condition is true (a missing value counts as false). |
| `total` | `integer` | The ticked observations: all of the project's, less exclues and test. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| "&lt;function&gt;" isn't allowed in a condition: comparisons (==, !=, &lt;, &gt;=…), &amp;, \|, !, %in%, is.na(), ifelse(), arithmetic (+, log10()…) and the group and lag functions of New variable formulas (group_mean(), previous()…). | The condition calls another function; it may also be unreadable, name an unknown variable or not give one logical value per observation (condition_observations lists those messages). ([`condition_refusee`](#errors)) |
| Variable not found: &lt;name&gt; (check the name; a name with spaces or signs is written between `backticks`) | The condition names a variable or identifier the project hasn't. |
| Unreadable condition: &lt;R's message&gt; | The text isn't R. |
| A condition is a single expression, without ";": site == "S1" &amp; year &gt;= 2010. | Several expressions. |
| This condition is too long. | More than 5000 characters. |
| In a condition, ":" only goes between two numbers, like year %in% 2005:2010. | A range whose bounds aren't numbers. |
| This ":" range is too long (more than 100000 values). | A range of more than 100,000 values. |
| Incomplete condition: an argument is missing. | An empty argument, c(1, , 2). |
| "&lt;expression&gt;" has no place in a condition. | Something that is neither a call, a name nor a constant. |
| Condition: &lt;R's message&gt; | Evaluating it failed. |
| The condition must give TRUE or FALSE (or 1 and 0) for each observation; it gives text. | It gives text; likewise "; it gives other numbers (&lt;number&gt;)." and "; it gives &lt;class&gt;.". |
| The condition must give one value per observation (&lt;n&gt;); it gives &lt;m&gt;. | It gives another number of values than the project has observations (a summary, a value per group). |

#### `jeu_test`

Proposes a test set: a share of the observations, drawn at random or chosen by Kennard–Stone, by class when a class variable is given.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `methode` | `string` | no | How the test set is chosen. Values: `hasard` (at random, from R's random numbers without a seed: two calls give two sets), `kennard_stone` (Kennard and Stone (1969): the model keeps the observations that cover the data, on the quantitative x, centred and scaled). Default: `"hasard"`. |
| `x` | `number[]` | no | kennard_stone: the X variables whose quantitative ones it works on. |
| `part` | `number` | no | The share of the observations (of each class) set aside. Default: `0.25`. |
| `classe` | `number` | no | A variable whose classes each give their share (a class too small to give one observation and keep two gives none). |
| `exclues` | `number[]` | no | Observations left out of the choice (the unticked ones). |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `test` | `number[]` | The test set's observations, their numbers in increasing order (always an array). |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| "&lt;methode&gt;" isn't a choice of test set method: hasard, kennard_stone. | methode is another value. |

#### `formule`

Writes the R formula of a regression's settings, as lm, mgcv and lme4 write it.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |

Also the parameters of the set [`definition`](#set-definition).

**Result:** `object`, with the fields below. Only type (LM when absent), x, y, transformations, croises, lisses, k_courbes, par_courbes, aleatoires, decalage and lisse_spatial count.

| Field | Type | Meaning |
|---|---|---|
| `formule` | `string` | The formula, "weight \~ log10(diameter) + s(firmness) + (1 \| orchard)"; without y, "\~ x1 + x2"; without x, "y \~ 1". |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Unknown transformation: &lt;transformation&gt; | A value of transformations isn't one of the forms Nuée reads (nouveau.R: lire_transformation). ([`transformation_inconnue`](#errors)) |
| Logit: the lower bound (&lt;C1&gt;) must be smaller than the upper bound (&lt;C2&gt;). | A logit transformation whose bounds are in the wrong order. |
| Unknown grouping "&lt;by&gt;" | A random effect's by is neither id nor variable. |

#### `lire_formule`

Reads an R formula into a regression's settings, without ever evaluating it.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `texte` | `string` | yes | The formula: response \~ terms, with the project's variables and identifiers, numbers, log, log10, log1p, exp, sqrt, I(), s(), offset(log()), random effects as lme4 writes them, ":" and "\*". |
| `type` | `string` | no | The regression's type so far (LM, LMM, GLM or GAM; another value is LM): random effects make an LM mixed and their absence an LMM linear, a curve or a spatial smooth makes it a GAM. Default: `"LM"`. |
| `famille` | `string` | no | The distribution so far: an offset needs a log-link one. |

**Result:** `object`, with the fields below. The settings, in the form of the definition parameters.

| Field | Type | Meaning |
|---|---|---|
| `type` | `string` | The regression's type the formula gives. |
| `x` | `number[]` | The X variables' numbers (always an array). |
| `y` | `number[]` | The response's number, in an array. |
| `transformations` | `object` | By variable number, its transformation. |
| `croises` | `number[][]` | The cross terms and powers (always an array). |
| `lisses` | `number[]` | The variables in a curve (always an array). |
| `k_courbes` | `object` | By variable number, a curve's k. |
| `par_courbes` | `object` | By variable number, the qualitative variable of a curve per category. |
| `aleatoires` | [`Groupement`](#type-groupement)`[]` | The random effects, the widest first (always an array). |
| `decalage` | `number` \| `null` | The effort variable's number. |
| `lisse_spatial` | `object` \| `null` | The spatial smooth, {variables, base, k}. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| This formula is too long. | More than 5000 characters. |
| Unreadable formula: &lt;R's message&gt; | The text isn't R. |
| A formula reads: response \~ terms, for example note \~ sugar + firmness. | No \~, or no response. |
| "&lt;expression&gt;" has no place in a formula here: variables, numbers, log, log10, log1p, exp, sqrt, I(), s(), offset(log()), (1 \| group), ":" and "\*". | Another function or construct. |
| "&lt;name&gt;" is not a variable of the project. | An unknown name. |
| "&lt;name&gt;" is neither an observation ID nor a variable of the project. | A random effect's group is unknown. |
| "&lt;name&gt;" enters the formula twice, transformed two ways: a variable has a single transformation. | A variable transformed two ways. |
| Nuée always keeps the intercept: "+ 0" or "- 1" don't apply. | + 0 or - 1. |
| To remove a term ("- x"), leave it out. Nuée always keeps the intercept. | A term removed with -. |
| Nuée's random effects are nested, the widest first: (1 \| site/sector), or one term per level, (1 + year \| site) + (1 \| site:sector). | Random effects that aren't nested. |
| "&lt;name&gt;" is the response: it can't also be a term. | The response is also a term. |
| The response "&lt;name&gt;" is qualitative: a regression needs a quantitative response. | A qualitative response. |
| The effort (offset) goes with a GLM or GAM with a Poisson, negative binomial, gamma, Tweedie or zero-inflated Poisson distribution: choose the type and the distribution of Y first. | An offset without a log-link GLM or GAM. |
| A curve reads s(x), s(x, k = 5) to set its dimension, or s(x, by = f) for a curve per category of the qualitative variable f. | An s() of another form. |

#### `ajuster`

Fits a model, from its definition or from a model kept in the project, and keeps it in the session.

Since 1.0. Changes the project, without an undo step. The fitted model joins the session under a new id. A definition made by Nuée joins the project, replacing the one of the same name: sent without numero, under a new number; with numero, under its own. The project counts as modified when the definition changed (refitting a saved model leaves it as it was), and a definition added or changed empties the redo stack (retablir), but not one fitted again by its number. A definition sent with meme_numero takes the place of the project's model of that number, whatever its name. A model read from a .usp file only joins the session.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `numero` | `number` \| `string` | no | A model of the project, by its number (ResumeModele's numero) or its name: it is fitted as the project keeps it, and the definition parameters are ignored but A, which replaces its number of components when above 0. Without numero, the definition parameters make the model. |

Also the parameters of the set [`definition`](#set-definition).

**Result:** [`Modele`](#type-modele).

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Unknown model type: &lt;type&gt;. | type isn't one of the definition's types (nouveau.R: definir_modele). ([`type_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| A PLS-DA needs a class variable | A PLS-DA or OPLS-DA definition without classe. ([`classe_requise`](#errors)) |
| Unknown scaling: &lt;scaling&gt; | echelle or a value of echelles isn't UV, Par, Ctr or none. ([`echelle_inconnue`](#errors)) |
| Unknown transformation: &lt;transformation&gt; | A value of transformations isn't one of the forms Nuée reads (nouveau.R: lire_transformation). ([`transformation_inconnue`](#errors)) |
| A cross term can only use X variables | A cross term of a PCA, PLS or OPLS uses a variable that isn't in x. ([`croise_hors_x`](#errors)) |
| "&lt;function&gt;" isn't allowed in a condition: comparisons (==, !=, &lt;, &gt;=…), &amp;, \|, !, %in%, is.na(), ifelse(), arithmetic (+, log10()…) and the group and lag functions of New variable formulas (group_mean(), previous()…). | The condition calls another function; it may also be unreadable, name an unknown variable or not give one logical value per observation (condition_observations lists those messages). ([`condition_refusee`](#errors)) |
| None of the &lt;n&gt; ticked observations meets the condition &lt;condition&gt;. | The condition keeps none of the model's ticked observations. ([`condition_aucune`](#errors)) |
| This analysis doesn't predict left-out observations: it takes no test set. | A test set for an analysis that doesn't predict (RDA, CA…). ([`sans_test`](#errors)) |
| A regression needs a single Y variable, a quantitative one. | A regression's y is empty, has several variables or a qualitative one. ([`regression_une_reponse`](#errors)) |
| A mixed model needs at least one random effect. | An LMM without aleatoires. ([`mixte_sans_aleatoire`](#errors)) |
| Unknown distribution of the response. | famille isn't one of the distributions Nuée fits. ([`loi_inconnue`](#errors)) |
| Cross-validation by "&lt;groups&gt;": the model's observations have a single value, so a single group. At least two are needed. | vc's identifier or variable has one value on the model's observations. ([`vc_un_groupe`](#errors)) |
| No component is possible: the data have no variation (constant variables, or all zero after the spectral preprocessing). | A PCA, PLS or OPLS whose data leave nothing to describe. ([`aucune_variation`](#errors)) |
| Component &lt;a&gt; is not possible: no variation is left to describe after &lt;n&gt; component(s). This model can have at most &lt;n&gt;. | A was set above what the data allow. ([`trop_de_composantes`](#errors)) |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |
| Model not found: &lt;numero&gt; | numero names no model of the project. |
| No observation is ticked for this model: tick some in the Observations tab (those of the test set aren't part of it). | exclues and test leave the model no observation (or a model of the project, with numero, has none). |
| An OPLS needs at least one Y variable | An OPLS without y; an OPLS-DA with fewer than two classes gets "An OPLS-DA needs at least two classes", a PLS-DA with a single class "A PLS-DA needs at least two classes". |
| Choose at least one X variable. | A PCA, PLS or OPLS (or PLS-DA, OPLS-DA) without x. |
| A qualitative variable enters a cross term once at most, neither squared nor cubed: "&lt;term&gt;". | A qualitative variable twice in a cross term. |
| Unknown spectral preprocessing: &lt;type&gt; | A step of spectres isn't snv, msc, sg, detrend or ligne_base; a Savitzky–Golay window that isn't odd and 3 or more, or a degree out of derivee ≤ degre &lt; fenetre, have their own messages. |
| Unknown cross-validation: &lt;validation&gt; (sans_fuite, centres or complet). | validation is neither empty nor one of its values. |
| Cross-validation: this project has no ID "&lt;id&gt;". | vc names an identifier the project hasn't. |
| A linear model has no random effect: choose a mixed model. | An LM with aleatoires. |
| Distributions, curves and offsets need a generalized (GLM) or additive (GAM) model. | famille, lisses, par_courbes, decalage or lisse_spatial for an LM or LMM. |
| Curves need an additive model (GAM). | lisses for a GLM. |
| A curve is made on a quantitative X variable. | A variable of lisses that isn't a quantitative X term on its own. |
| An offset (the effort) goes with a log-link distribution: Poisson, negative binomial, gamma, Tweedie or zero-inflated Poisson. | decalage with another distribution. |
| Three nested random effects at most. | More than three random effects. |
| A random effect is made from an ID or a qualitative variable. | A random effect on a quantitative variable. |
| "&lt;variable&gt;" has a random slope: it must also be an X term of the model, quantitative and on its own (transformed or not), the fixed slope around which the groups' slopes vary. | A random slope on a variable that isn't such a term. |
| Autocorrelation in time works within the groups of a random effect. | correlation without random effects. |
| Autocorrelation in time needs a single time per group: several observations have the same time in the same group. | CAR1 with repeated times in a group. |
| A Poisson or negative binomial distribution needs counts (whole numbers, zero or more). | The response doesn't suit the distribution; the binomial, gamma, beta, Tweedie and zero-inflated Poisson distributions have their own messages. |
| Too few complete observations (&lt;n&gt;) for &lt;k&gt; term(s): at least &lt;m&gt; are needed, with the response and every X variable known. | A regression of k terms with fewer than k + 2 complete observations. |
| The response “&lt;name&gt;” is &lt;value&gt; for each of the model's &lt;n&gt; observations: there is nothing to explain. Choose another response, or other observations. | The response of a regression, a regression tree, a forest or boosted trees has a single value over the model's observations. |
| No value of “&lt;y&gt;” among the model's observations: there is no Y to predict. | A PLS or OPLS whose Y variables have no value among its observations. |
| “&lt;name&gt;” has values too large for the computation&lt; (up to &lt;value&gt;)&gt;: their squares exceed the numbers R can hold. Divide it by a power of 10 (Edit › New variable). | A variable the fit squares has values beyond about 1e150 in size, or infinite ones: a UV or Pareto term of a PCA, PLS or OPLS, a regression's response or term, the response of a regression tree, a forest or boosted trees, an LDA's X; values all below 1e-150 get "“&lt;name&gt;” has values too small for the computation (at most &lt;value&gt;): their squares vanish. Multiply it by a power of 10 (Edit › New variable)." |
| The standard error of “&lt;term&gt;” can't be computed: the values of the model's terms are too far apart in size (billions of times), and the computation lost its precision. Bring them near 1 by a power of 10 (Edit › New variable), or scale them (UV). | A GLM or GAM (mgcv) whose terms are so far apart in size that a coefficient's standard error comes out zero or not a number. |
| R-code models are turned off on this computer by your organisation's policy (DisableCustomCode). | The engine was started with NUEE_SANS_CODE=1 (etat's code_interdit) and the request defines, fits or exports an R-code model, or asks a code's fingerprint (code.R: refuser_si_interdit). ([`code_interdit`](#errors)) |
| Write the model's R code: its last value is the fitted model. | An R-code model (CODE) whose options' code is left out or blank. |
| &lt;a plugin definition's refusal&gt; | A plugin's model (PLUGIN) whose definition its plugin refuses, in the engine's words: no options' greffon.id (Which plugin? The definition names none (options$greffon$id).), a plugin not installed, a version neither installed nor in the project, a plugin with errors or that needs a newer Nuée, an unknown input or option, a variable an input doesn't accept, too few or too many variables, an option's value of another type or out of its bounds (greffons.R: definir_plugin). |
| Model &lt;name&gt; has R code written elsewhere, not approved on this computer: it doesn't run until you have read and approved it (Show the code, Run it). | An R-code model's code isn't among the fingerprints approved on this computer (code_approuve), whoever wrote it (code.R: exiger_code_permis, before anything runs it). Besides erreur, the answer carries code_non_approuve: true and empreinte, the code's fingerprint, to approve before fitting again. ([`code_non_approuve`](#errors)) |
| The R code stopped at line &lt;n&gt;: &lt;R's message&gt; | An R-code model's code stopped with an error or doesn't parse, or a plugin's code failed (code.R: erreur_code; greffons.R: erreur_greffon). Besides erreur, the answer carries ligne, the line of the top-level expression that stopped, null when unknown ("The R code stopped: &lt;R's message&gt;", a last value NULL, a plugin's error, whose file and line are in the message). A missing package adds how to install it. ([`erreur_code`](#errors)) |
| `valeurs`: named numbers, such as c(density = 3.2, slope = -0.4). | An R-code or plugin model's code returned a list whose valeurs or tableaux aren't what they must be: numbers without a name, names that repeat (Names of `valeurs` repeat: &lt;names&gt;.), tableaux that aren't named data.frames (`tableaux`: named data.frames, such as list(counts = d).). |
| Model &lt;name&gt; is made with the plugin "&lt;plugin&gt;" &lt;version&gt;, which isn't installed or approved on this computer: it doesn't run until you have read and installed it (Show, Install, in the bar above the project). | A plugin's model is fitted, predicts or is exported to R, and its plugin's fingerprint isn't approved here (besides erreur: greffon_non_approuve: true, empreinte, id, version); under the DisableCustomCode policy, a plugin that isn't the organisation's gets “Model &lt;name&gt; is made with the plugin "&lt;plugin&gt;" &lt;version&gt;: on this computer, your organisation's policy (DisableCustomCode) only lets the plugins it provides run.” (greffon_interdit: true); a fit without the packages it needs “The plugin "&lt;plugin&gt;" needs packages that aren't installed for the R Nuée uses (R &lt;version&gt;): &lt;packages&gt;. Install them (File › Plugins…, or install.packages(…) in R), then fit again.” (paquets_manquants, always an array, and id) (greffons.R: exiger_greffon_permis, exiger_paquets_greffon). ([`greffon_refuse`](#errors)) |

#### `reajuster`

Fits a fitted model again with a change: observations left out, another number of components, the terms kept by the VIP, a model of its AIC selection, a CAR1 autocorrelation or a quantitative X variable centred.

Since 1.0. Changes the project, without an undo step. The refitted model joins the session, and its definition, named "&lt;name&gt; (&lt;n&gt;)" with a new number, the project, which counts as modified.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id. |
| `exclues` | `number[]` | no | Observations to leave out, by their numbers, besides those the model already leaves out. |
| `A` | `integer` | no | PCA and PLS: the number of components; without it, 0 (the automatic fit) when exclues or garder is given, else the model's. |
| `garder` | `string[]` | no | PCA and PLS: the names of the X terms to keep (the VIP's), with the factors of their cross terms. |
| `groupes_aic` | `string[]` | no | Regressions: the groups of terms of a model of the AIC selection (selection_aic's groupes), the terms kept (Fit this model). |
| `car1` | `object` | no | Regressions with random effects: {temps: a Groupement, the time; groupe: {by: "effets"}, its random effects' groups, optionally followed by a finer level's Groupement}: the same model with a CAR1 autocorrelation (Fit with CAR1). |
| `centrer` | `number[]` | no | Regressions: the numbers of quantitative X variables, terms of the model on their own, centred on their mean (their scaling becomes Ctr; their cross terms keep theirs), as the model's checks propose (Fit again with “&lt;name&gt;” centred). Since 2.0. |

**Result:** [`Modele`](#type-modele). The new model, named "&lt;name&gt; (&lt;n&gt;)".

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| &lt;variable&gt; is called &lt;name&gt; since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a variable it used was renamed since its fit; one deleted gets “&lt;variable&gt; is no longer in the data since &lt;model&gt; was fitted: it can't be fitted again without it (Ctrl+Z brings it back).”, one turned into categories or numbers “&lt;variable&gt; is &lt;categories\|numbers&gt; since &lt;model&gt; was fitted: fit it again (…)” (atelier.R: variables_changees). ([`variables_changees`](#errors)) |
| The categories of &lt;variable&gt; changed since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a category the model saw was renamed, merged or emptied since, or a variable it used is no longer qualitative (serveur.R: modele with a_jour, atelier.R: aligner_categories). ([`modele_perime`](#errors)) |
| Only for a regression. | The model isn't a regression (LM, LMM, GLM, GAM). ([`regression_seulement`](#errors)) |
| No component is possible: the data have no variation (constant variables, or all zero after the spectral preprocessing). | A PCA, PLS or OPLS whose data leave nothing to describe. ([`aucune_variation`](#errors)) |
| Component &lt;a&gt; is not possible: no variation is left to describe after &lt;n&gt; component(s). This model can have at most &lt;n&gt;. | A was set above what the data allow. ([`trop_de_composantes`](#errors)) |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |
| A regression has no VIP. | garder for a regression; another analysis gets "This analysis has no VIP.". |
| Unknown group of terms in this model: &lt;names&gt; | groupes_aic names a group the model hasn't. |
| This model has no random effect: a CAR1 autocorrelation works within the groups of a random effect. | car1 for a model without random effects. |
| This model already has a CAR1 autocorrelation. | car1 for a model with one. |
| Choose the time (a year, a day...): an ID or a quantitative variable. | car1 without temps. |
| A CAR1 autocorrelation works within the groups of the model's random effects, or of a finer level within them: choose its random effects as the groups. | car1's groupe doesn't start with {by: "effets"}, or has more than one finer level. |
| Autocorrelation in time needs a single time per group: several observations have the same time in the same group. | The CAR1 time repeats within a group. |
| This variable isn't a quantitative X variable of the model. | centrer names no quantitative X variable that is a term of the model on its own. |
| None of the terms to keep is an X term of the model. | garder names none of the model's X terms. |
| No observation is ticked for this model: tick some in the Observations tab (those of the test set aren't part of it). | exclues leaves the model no observation. |
| The response “&lt;name&gt;” is &lt;value&gt; for each of the model's &lt;n&gt; observations: there is nothing to explain. Choose another response, or other observations. | The response of a regression (a regression tree, a forest, boosted trees) has a single value over the observations left. |
| R-code models are turned off on this computer by your organisation's policy (DisableCustomCode). | The engine was started with NUEE_SANS_CODE=1 (etat's code_interdit) and the request defines, fits or exports an R-code model, or asks a code's fingerprint (code.R: refuser_si_interdit). ([`code_interdit`](#errors)) |
| Model &lt;name&gt; has R code written elsewhere, not approved on this computer: it doesn't run until you have read and approved it (Show the code, Run it). | An R-code model's code isn't among the fingerprints approved on this computer (code_approuve), whoever wrote it (code.R: exiger_code_permis, before anything runs it). Besides erreur, the answer carries code_non_approuve: true and empreinte, the code's fingerprint, to approve before fitting again. ([`code_non_approuve`](#errors)) |
| The R code stopped at line &lt;n&gt;: &lt;R's message&gt; | An R-code model's code stopped with an error or doesn't parse, or a plugin's code failed (code.R: erreur_code; greffons.R: erreur_greffon). Besides erreur, the answer carries ligne, the line of the top-level expression that stopped, null when unknown ("The R code stopped: &lt;R's message&gt;", a last value NULL, a plugin's error, whose file and line are in the message). A missing package adds how to install it. ([`erreur_code`](#errors)) |
| `valeurs`: named numbers, such as c(density = 3.2, slope = -0.4). | An R-code or plugin model's code returned a list whose valeurs or tableaux aren't what they must be: numbers without a name, names that repeat (Names of `valeurs` repeat: &lt;names&gt;.), tableaux that aren't named data.frames (`tableaux`: named data.frames, such as list(counts = d).). |
| Model &lt;name&gt; is made with the plugin "&lt;plugin&gt;" &lt;version&gt;, which isn't installed or approved on this computer: it doesn't run until you have read and installed it (Show, Install, in the bar above the project). | A plugin's model is fitted, predicts or is exported to R, and its plugin's fingerprint isn't approved here (besides erreur: greffon_non_approuve: true, empreinte, id, version); under the DisableCustomCode policy, a plugin that isn't the organisation's gets “Model &lt;name&gt; is made with the plugin "&lt;plugin&gt;" &lt;version&gt;: on this computer, your organisation's policy (DisableCustomCode) only lets the plugins it provides run.” (greffon_interdit: true); a fit without the packages it needs “The plugin "&lt;plugin&gt;" needs packages that aren't installed for the R Nuée uses (R &lt;version&gt;): &lt;packages&gt;. Install them (File › Plugins…, or install.packages(…) in R), then fit again.” (paquets_manquants, always an array, and id) (greffons.R: exiger_greffon_permis, exiger_paquets_greffon). ([`greffon_refuse`](#errors)) |

#### `modele`

Gives a fitted model's results.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id. |

**Result:** [`Modele`](#type-modele).

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |

#### `supprimer_modele`

Removes a fitted model from the session, and with definition its definition from the project.

Since 1.0. Changes the project, with an undo step that `annuler` takes back. Without definition, the fitted model only leaves the session (other fits of the same definition stay). With definition: true, the definition leaves the project too, which counts as modified, with one undo step.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id. |
| `definition` | `boolean` | no | true to also remove its definition (every one of its name) from the project. Default: `false`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `supprime` | `string` | The id of the model removed. |
| `projet` | [`Projet`](#type-projet) | Its project's summary. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |

#### `note_modele`

Sets a fitted model's free note, kept with its definition in the project.

Since 1.0. Changes the project, without an undo step. The note is kept with the fitted model and with every definition of its name in the project, which counts as modified when the note changed.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id. |
| `note` | `string` | no | The note; "" removes it. Default: `""`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `note` | `string` | The note kept. |
| `projet` | [`Projet`](#type-projet) | The model's project's summary. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |

#### `modeles_classe`

Fits class models: a PCA for each category of a class variable, on that class's observations.

Since 1.0. Changes the project, without an undo step. Each class model joins the session, and its definition the project, which counts as modified.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `classe` | `number` | yes | The qualitative variable whose categories are the classes; it replaces the definition's classe, and the type is always PCA. |

Also the parameters of the set [`definition`](#set-definition).

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modeles` | [`Modele`](#type-modele)`[]` | A model per class with three observations or more, named "&lt;nom&gt;.&lt;class&gt;", their groupe the name nom and their classe_modele the class (always an array). |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Unknown scaling: &lt;scaling&gt; | echelle or a value of echelles isn't UV, Par, Ctr or none. ([`echelle_inconnue`](#errors)) |
| Unknown transformation: &lt;transformation&gt; | A value of transformations isn't one of the forms Nuée reads (nouveau.R: lire_transformation). ([`transformation_inconnue`](#errors)) |
| "&lt;function&gt;" isn't allowed in a condition: comparisons (==, !=, &lt;, &gt;=…), &amp;, \|, !, %in%, is.na(), ifelse(), arithmetic (+, log10()…) and the group and lag functions of New variable formulas (group_mean(), previous()…). | The condition calls another function; it may also be unreadable, name an unknown variable or not give one logical value per observation (condition_observations lists those messages). ([`condition_refusee`](#errors)) |
| None of the &lt;n&gt; ticked observations meets the condition &lt;condition&gt;. | The condition keeps none of the model's ticked observations. ([`condition_aucune`](#errors)) |
| No component is possible: the data have no variation (constant variables, or all zero after the spectral preprocessing). | A PCA, PLS or OPLS whose data leave nothing to describe. ([`aucune_variation`](#errors)) |
| Component &lt;a&gt; is not possible: no variation is left to describe after &lt;n&gt; component(s). This model can have at most &lt;n&gt;. | A was set above what the data allow. ([`trop_de_composantes`](#errors)) |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |
| Class models need a class variable (Class role). | classe is missing. |
| The class variable must be qualitative | classe is a quantitative variable. |
| No class has enough observations (3 or more) | Every class has fewer than three observations. |
| No observation is ticked for this model: tick some in the Observations tab (those of the test set aren't part of it). | exclues and test leave no observation. |
| Choose at least one X variable. | x is empty. |
| Unknown cross-validation: &lt;validation&gt; (sans_fuite, centres or complet). | validation is neither empty nor one of its values. |
| “&lt;name&gt;” has values too large for the computation&lt; (up to &lt;value&gt;)&gt;: their squares exceed the numbers R can hold. Divide it by a power of 10 (Edit › New variable). | A UV or Pareto term has values beyond about 1e150 in size, or infinite ones; values all below 1e-150 get "“&lt;name&gt;” has values too small for the computation (at most &lt;value&gt;): their squares vanish. Multiply it by a power of 10 (Edit › New variable)." |

#### `par_reponse`

Fits a regression again for each of several responses and sets their coefficients, tests and quality side by side.

Since 1.0. Changes the project, without an undo step. Only with garder: each response's model that was fitted joins the session, and its definition, named "&lt;name&gt; · &lt;response&gt;", the project, which counts as modified; without garder nothing is kept.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted regression's id: its terms, random effects, distribution, curves and observations are kept, its response's transformation applied to each. |
| `reponses` | `number[]` | yes | The responses' variable numbers (quantitative, none of the model's X). |
| `correction` | `string` | no | The correction of each term's p over the responses. Values: `BH` (Benjamini and Hochberg (1995)), `holm` (Holm (1979)), `bonferroni` (Bonferroni). Default: `"BH"`. |
| `garder` | `boolean` | no | Also keep each response's model in the session and the project. Default: `false`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The id given. |
| `tableau` | `object` \| `null` | A table, one row per term and response, terme by terme: reponse, terme, estimation, erreur_type, bas, haut, p, edf (a curve's), p_corrige; null when no response could be fitted. |
| `resume` | `object` | A table, one row per response: reponse, N, AIC, R2, Q2, erreur ("" when fitted, else why it couldn't be: a response that fails isn't an error of the request). |
| `effets` | `object[]` | For each response fitted, {reponse, effets}: its terms' effect curves, as effets gives them. |
| `correction` | `string` | The correction used. |
| `resultats` | [`Modele`](#type-modele)`[]` \| `null` | With garder: the models kept. |
| `projet` | [`Projet`](#type-projet) \| `null` | With garder: the project's summary. |
| `methodes` | `string` | The Methods sentence, in the request's language. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| &lt;variable&gt; is called &lt;name&gt; since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a variable it used was renamed since its fit; one deleted gets “&lt;variable&gt; is no longer in the data since &lt;model&gt; was fitted: it can't be fitted again without it (Ctrl+Z brings it back).”, one turned into categories or numbers “&lt;variable&gt; is &lt;categories\|numbers&gt; since &lt;model&gt; was fitted: fit it again (…)” (atelier.R: variables_changees). ([`variables_changees`](#errors)) |
| The categories of &lt;variable&gt; changed since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a category the model saw was renamed, merged or emptied since, or a variable it used is no longer qualitative (serveur.R: modele with a_jour, atelier.R: aligner_categories). ([`modele_perime`](#errors)) |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |
| One model per response is made from a regression (LM, LMM, GLM or GAM). | The model isn't a regression. |
| Choose at least one response. | reponses is empty. |
| An X variable can't also be a response: &lt;names&gt;. | A response is one of the model's X. |
| "&lt;correction&gt;" isn't a choice of p-value correction: BH, holm, bonferroni. | correction is another value. |

#### `baci_modele`

Fits the BACI mixed model, y \~ period × impact + (1 | unit) with CAR1 if asked, adding its two 0/1 variables to the project when they don't exist yet.

Since 1.0. Changes the project, with an undo step that `annuler` takes back. The two 0/1 variables, when they don't exist yet with these values, are added to the project as one undo step (variable &lt;names&gt;), which annuler takes back with the model's definition, and noted as a data step (BACI variables), which Update the data replays; the model joins the session and its definition the project. Reusing existing variables leaves no undo step.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `y` | `number` | yes | The response, a quantitative variable. |
| `temps` | [`Colonne`](#type-colonne) | yes | The time: a variable or "id:k" (k ≥ 2) of numbers (the year) or dates. |
| `unite` | [`Colonne`](#type-colonne) | yes | The unit (the site or sector): a qualitative variable or "id:k" (k ≥ 2). |
| `impact` | `string[]` | yes | The impact units: categories of unite. |
| `temoins` | `string[]` | yes | The control units: other categories of unite. |
| `evenement` | `number` \| `string` | yes | The first time counted after: a number, or a date when the time is made of dates. |
| `fin` | `number` \| `string` | no | The last time of the after period, included; the later times are left out. |
| `car1` | `boolean` | no | With a CAR1 autocorrelation along the time within each unit (a single row per unit and time). Default: `false`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | [`Modele`](#type-modele) | The model, named "BACI &lt;y&gt;" (the model of the same settings is replaced, keeping its name and number; another takes "BACI &lt;y&gt; (2)"…). |
| `variables` | `string[]` | The names of the period and impact variables, "BACI period (2008)" and "BACI impact (S1 vs S2, S3)" (always an array). |
| `nouvelles` | `string[]` | Those of them just added to the project (always an array, empty when both existed with these values). |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Unknown identifier: &lt;id:k&gt; | "id:k" names no identifier of the observations (variables.R: colonne_ou_identifiant). ([`identifiant_inconnu`](#errors)) |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |
| Enter the event, the first time counted as “after”: the mixed model compares the two periods. | No evenement. |
| The response, the time and the unit must be three different variables | Two of y, temps and unite are the same. |
| The response must be a quantitative variable | y is qualitative. |
| The time (&lt;name&gt;) must be numbers (the year) or dates (2019-05-14, 14/05/2019): &lt;why&gt; | A value of the time is neither a number nor a date (", not both" for a mix). |
| The unit must be a qualitative variable or an ID | unite is quantitative. |
| Choose at least one impact unit and one control unit | impact or temoins is empty. |
| &lt;unit&gt; has no level &lt;levels&gt; | impact or temoins names a category unite hasn't. |
| A unit can't be both impact and control: &lt;units&gt; | A unit in both. |
| No value of &lt;time&gt; where both the impact and the controls have a value of &lt;y&gt; | The impact and the controls share no time. |
| “&lt;y&gt;” has values too large for the computation&lt; (up to &lt;value&gt;)&gt;: their squares exceed the numbers R can hold. Divide it by a power of 10 (Edit › New variable). | y has values beyond about 1e150 in size, or infinite ones, on the rows compared; values all below 1e-150 get "“&lt;y&gt;” has values too small for the computation (at most &lt;value&gt;): their squares vanish. Multiply it by a power of 10 (Edit › New variable)." |
| The event time must be a number or a date | evenement can't be read (fin: "The end of the after period must be a number or a date"). |
| The end of the after period (&lt;end&gt;) comes before the event (&lt;event&gt;) | fin before evenement. |
| At least two values of &lt;time&gt; are needed in each period, before and after &lt;event&gt;: there are &lt;n&gt; before and &lt;m&gt; after | A period with fewer than two times. |
| The gap is the same at every time: nothing to compare | The impact − control gap never varies. |
| Several rows of the same unit at the same time (&lt;units&gt;): CAR1 autocorrelation needs a single time per unit. Fit the model without CAR1, or choose a finer unit (the sector rather than the site). | car1 with repeated times in a unit. |

#### `empreinte_code`

Gives the fingerprint of an R code, which code_approuve takes to approve it (New model asks it before fitting the code just written).

Since 2.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `code` | `string` \| `string[]` | no | The R code, as an R-code model's definition holds it (its options' code): its text in UTF-8, its \\r\\n and \\r line ends counting as \\n (a code pasted from Windows has the fingerprint of the same code typed); an array is its lines, joined with \\n. Default: `""`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `empreinte` | `string` | Its SHA-256, 64 lowercase hexadecimal characters, the same whatever the R (code.R's sha256_r on an R older than 4.5, which has no tools::sha256sum): what code_approuve takes, and ResumeModele's code_r.empreinte for the same code. |

**Errors:**

| Message (in English) | When |
|---|---|
| R-code models are turned off on this computer by your organisation's policy (DisableCustomCode). | The engine was started with NUEE_SANS_CODE=1 (etat's code_interdit) and the request defines, fits or exports an R-code model, or asks a code's fingerprint (code.R: refuser_si_interdit). ([`code_interdit`](#errors)) |

#### `code_approuve`

Gives the engine the fingerprints of the R-code models' and plugins' code this user approved on this computer, the only such code it runs.

Since 2.0. Changes the session, not a project's content. The fingerprints the engine holds approved, for as long as it runs: with a secret, exactly those given; without one, those given added to them. An R-code model (type CODE) or a plugin runs only when its fingerprint is among them (ajuster, reajuster, script_r and the replays of Update the data refuse the others), and ResumeModele's code_r.approuve and code_modeles' approuve say whether it is. No project changes.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `empreintes` | `string[]` | yes | The fingerprints approved (empreinte_code's, ResumeModele's code_r.empreinte, a plugin's), each 64 lowercase hexadecimal characters; any other value is left out, without an error. From the application (the engine started with a secret, the request carrying it in the header X-Nuee-Code), the whole list it keeps (the latest 10,000), which replaces the engine's; to an engine started by hand, those to add. Without it, none: with a secret, every approval is withdrawn. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `n` | `integer` | The number of fingerprints the engine now holds approved. |

**Errors:**

| Message (in English) | When |
|---|---|
| Only the application approves R code (Run it, in the bar above the project). | The engine was started with a secret (NUEE_CODE_SECRET in its environment: the application draws one at each start, and the engine removes it from its environment once read) and the request doesn't carry it in the header X-Nuee-Code: the windows, which have the token but not the secret, can't approve code themselves. The answer is HTTP 403, given before the parameters are read; nothing changes. |

#### `code_modeles`

Gives the code of a project's R-code models, with each one's fingerprint and whether it is approved on this computer, to read before running it (Show the code, in the bar above the project).

Since 2.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modeles` | `object[]` | One per R-code model (type CODE) of the project, in its order, each {nom, numero (ResumeModele's numero, null without one), titre, code (as the engine keeps and runs it: UTF-8, \\n line ends), empreinte (its fingerprint, as empreinte_code gives it), approuve (whether its fingerprint is among the approved ones)}; always an array, empty without such a model. A plugin's models aren't listed. |

**Errors:**

| Message (in English) | When |
|---|---|
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |

### Plugins

[`greffons`](#greffons) · [`greffon_lire`](#greffon_lire) · [`greffon_installer`](#greffon_installer) · [`greffon_retirer`](#greffon_retirer) · [`greffon_projet`](#greffon_projet) · [`greffon_mettre_a_jour`](#greffon_mettre_a_jour)

#### `greffons`

Lists the plugins of the user's and of the organisation's plugins folders, as File › Plugins… shows them, with those folders and whether plugins can be installed here.

Since 2.0. Changes nothing.

No parameters.

**Result:** `object`, with the fields below. What the interface knows of the plugins (greffons.R: greffons_interface); etat gives the same in its field greffons.

| Field | Type | Meaning |
|---|---|---|
| `dossier` | `string` \| `null` | The user's plugins folder, where greffon_installer writes (NUEE_GREFFONS: the desktop application gives &lt;userData&gt;/greffons); null without it, an engine started by hand. |
| `organisation` | `string` \| `null` | The organisation's plugins folder, read only (NUEE_GREFFONS_ORGANISATION: the PluginsFolder policy); null without one. |
| `interdit` | `boolean` | Whether the DisableCustomCode policy is on (NUEE_SANS_CODE=1): only the organisation's plugins run, and none installs. |
| `installation` | `boolean` | Whether plugins can be installed here: a user's plugins folder, and the policy off. |
| `liste` | [`Greffon`](#type-greffon)`[]` | The plugins of both folders, the organisation's first, one per sub-folder whose name doesn't start with a dot (one that can't be read too, invalide with its error), without their files (always an array). |

**Errors:** none of its own.

#### `greffon_lire`

Reads a plugin from a file without running anything, before it is installed: its manifest, its code, its errors, whether it can be installed, and the installed version of the same id.

Since 2.0. Changes nothing. Nothing runs: the manifest is read and the R files only parsed. A .nueeplugin is unpacked (its plugin.json and .R files) into a temporary folder, deleted after.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `chemin` | `string` | yes | A .nueeplugin (a zip archive with plugin.json at its root or in its single folder), the plugin.json of a plugin's folder, or that folder (File › Plugins… › Show reads an installed one's folder this way). |

**Result:** `object`, with the fields below. What the install dialog shows (greffons.R: installation_greffon).

| Field | Type | Meaning |
|---|---|---|
| `greffon` | [`Greffon`](#type-greffon) | The plugin read, source fichier, with its fichiers: plugin.json, then the R files its manifest lists (every .R file read when the manifest has errors). |
| `peut` | `boolean` | Whether greffon_installer would install it. |
| `raison` | `string` | Why it can't, in the request's language, the first that applies (greffon_installer's refusals: it has errors; the DisableCustomCode policy; no user's plugins folder; the organisation already gives that id; it needs a newer Nuée); "" when it can. |
| `installe` | `object` \| `null` | The valid plugin of the same id already in a plugins folder, the organisation's first: {version, empreinte, source (utilisateur or organisation), ordre (1 when the one read is newer, -1 older, 0 the same version; null when either has no version), meme (whether they are the same files, the same fingerprint)}; null when there is none. |

**Errors:**

| Message (in English) | When |
|---|---|
| File not found: &lt;path&gt; | The file named doesn't exist. ([`fichier_introuvable`](#errors)) |
| No plugin.json in &lt;folder&gt;. | chemin is a folder without plugin.json. ([`greffon_sans_manifeste`](#errors)) |
| This isn't a plugin: a .nueeplugin is a zip archive holding plugin.json. | chemin is a file, other than a plugin.json, that isn't a zip archive, or an empty one. ([`pas_un_greffon`](#errors)) |
| The archive has paths outside its folder: it is refused. | A path in the archive is absolute or goes up (..). ([`archive_hors_dossier`](#errors)) |
| The archive is too large for a plugin (5 MB, 500 files at most). | The archive holds more than 500 entries, or more than 5 MB once unpacked. ([`archive_trop_grande`](#errors)) |
| The archive has no plugin.json at its root (nor in a single folder). | Neither the archive's root nor exactly one of its folders holds plugin.json. ([`archive_sans_manifeste`](#errors)) |
| &lt;file&gt;: 1 MB at most. | A file of the plugin read (its plugin.json, or any .R file of its folder, even one its manifest doesn't list) is over 1 MB; one with a null byte gets “&lt;file&gt;: it isn't a text file.”, one that isn't UTF-8 “&lt;file&gt;: it isn't in UTF-8.”. ([`fichier_greffon_refuse`](#errors)) |

#### `greffon_installer`

Installs a plugin read from a file, or carried by a model of a project, into the user's plugins folder, in place of the version of the same id.

Since 2.0. Changes the session, not a project's content. Writes files outside any project: the plugin's plugin.json and the R files its manifest lists go into a hidden folder of the user's plugins folder (created when missing), renamed &lt;id&gt; once whole; the folder of that name before it is moved aside, then deleted (put back when the rename fails). No project changes; the plugin is in the lists of greffons and etat from then on. It isn't approved by this: until its fingerprint is approved (code_approuve, which the application sends first), its permis is false.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `chemin` | `string` | no | The plugin's file, as greffon_lire takes it (a .nueeplugin, a plugin.json, a plugin's folder); required unless projet and numero are given. |
| `projet` | `string` | no | Without chemin: the project whose model carries the plugin (Install and run, in the bar above the project). |
| `numero` | `number` \| `string` | no | With projet: that model, by its number (ResumeModele's numero) or its name. |
| `empreinte` | `string` | yes | The fingerprint the user read and approved (greffon_lire's or greffon_projet's Greffon empreinte): a plugin whose files changed since is refused. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `greffon` | [`Greffon`](#type-greffon) | The plugin as now installed, read from its new folder (source utilisateur), without its files. |
| `remplace` | `string` \| `null` | The version of the user's plugin it replaced; null when none was installed. |

**Errors:**

| Message (in English) | When |
|---|---|
| File not found: &lt;path&gt; | The file named doesn't exist. ([`fichier_introuvable`](#errors)) |
| No plugin.json in &lt;folder&gt;. | chemin is a folder without plugin.json. ([`greffon_sans_manifeste`](#errors)) |
| This isn't a plugin: a .nueeplugin is a zip archive holding plugin.json. | chemin is a file, other than a plugin.json, that isn't a zip archive, or an empty one. ([`pas_un_greffon`](#errors)) |
| The archive has paths outside its folder: it is refused. | A path in the archive is absolute or goes up (..). ([`archive_hors_dossier`](#errors)) |
| The archive is too large for a plugin (5 MB, 500 files at most). | The archive holds more than 500 entries, or more than 5 MB once unpacked. ([`archive_trop_grande`](#errors)) |
| The archive has no plugin.json at its root (nor in a single folder). | Neither the archive's root nor exactly one of its folders holds plugin.json. ([`archive_sans_manifeste`](#errors)) |
| &lt;file&gt;: 1 MB at most. | A file of the plugin read (its plugin.json, or any .R file of its folder, even one its manifest doesn't list) is over 1 MB; one with a null byte gets “&lt;file&gt;: it isn't a text file.”, one that isn't UTF-8 “&lt;file&gt;: it isn't in UTF-8.”. ([`fichier_greffon_refuse`](#errors)) |
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Model not found: &lt;numero&gt; | Without chemin, numero names no model of the project. |
| Model &lt;name&gt; doesn't have its plugin. | That model isn't a plugin's (no copy of a plugin in its definition). |
| The plugin has errors: it can't be installed. | Its manifest or its code has errors (Greffon's erreurs); greffon_lire's raison says this refusal and the four that follow beforehand. |
| Your organisation's policy (DisableCustomCode) only allows the plugins it provides. | The DisableCustomCode policy is on. |
| Plugins are installed from the desktop application. | The engine has no user's plugins folder (NUEE_GREFFONS empty: started by hand). |
| Your organisation already provides this plugin (version &lt;version&gt;): its own is used. | The organisation's folder has a valid plugin of that id. |
| It needs Nuée &lt;version&gt; or newer (this one: &lt;version&gt;). | Its manifest's nuee is above this Nuée's version. |
| The plugin changed since you read it: read it again before installing it. | empreinte is missing or isn't the fingerprint of the plugin as read now. |
| The installed version of &lt;id&gt; can't be replaced (a file in use?). | The folder of the version installed couldn't be moved aside; nothing changed. |
| The plugin &lt;id&gt; can't be written into &lt;folder&gt;. | The new folder couldn't take the name &lt;id&gt;; the version installed is put back. |
| cannot open the connection | A file can't be written into the user's plugins folder (R's own message, untranslated). |

#### `greffon_retirer`

Removes one of the user's plugins, as File › Plugins… › Remove does.

Since 2.0. Changes the session, not a project's content. Deletes the plugin's folder from the user's plugins folder, outside any project: it leaves greffons, etat and New model. No project changes: its approval stays, and the models that carry it are still fitted with their own copy.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `id` | `string` | yes | The plugin's id (Greffon's id), a plugin of the user's plugins folder; the organisation's aren't removed. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `retire` | `string` | The id removed. |

**Errors:**

| Message (in English) | When |
|---|---|
| No plugin &lt;id&gt; among yours (your organisation's aren't removed here). | No folder of the user's plugins folder holds a plugin of that id, valid or not (also without id). |
| The folder &lt;folder&gt; can't be deleted (a file in use?). | The plugin's folder is still there after its deletion; some of its files may be gone. |

#### `greffon_projet`

Gives the plugins a project's models carry, with their code, their models, whether they may run here and the installed version of the same id, for Show in the bar above the project.

Since 2.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `greffons` | [`Greffon`](#type-greffon)`[]` | One per plugin its PLUGIN models carry, one per fingerprint (a copy with errors too, etat invalide; a definition without its copy is left out), each source projet with its fichiers and three fields more: modeles, its models in the project, [{nom, numero (null without)}] (always an array); interdit, whether the DisableCustomCode policy keeps it from running here (it isn't the organisation's); installe, the usable installed plugin of the same id (the organisation's when it gives one), {version, empreinte, source, ordre (1 when the project's version is newer, -1 older, 0 the same), meme (the same files)}, null when there is none. Always an array. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |

#### `greffon_mettre_a_jour`

Moves a project's models made with a plugin to its installed version, each redefined with its inputs and options, to be fitted again (Use version &lt;version&gt;, in the bar above the project).

Since 2.0. Changes the project, with an undo step that `annuler` takes back. When at least one definition changed: one undo step, labelled update to the plugin &lt;id&gt; &lt;version&gt;, and the project counts as modified. Each changed definition carries the installed version's copy (its fingerprint, version and files), and keeps the inputs and options that version still has (the others dropped, its new options at their defaults); the definitions already at that version, those numeros leaves out and those refused stay as they were. No model is fitted again: the session's fitted models stay as they were, until each definition is fitted with ajuster and its numero.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `id` | `string` | yes | The plugin's id; its usable installed version is the one taken (the organisation's when it gives one). |
| `numeros` | `number[]` | no | Only the models of these numbers (ResumeModele's numero); without it, every model of the project made with that plugin. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `numeros` | `number[]` | The numbers of the definitions changed, to fit again (ajuster with numero); always an array, empty when none changed. |
| `refus` | `object[]` | The definitions the installed version doesn't take, each {nom, erreur}: its name, and why in the request's language (ajuster's refusals of a plugin's definition: a variable an input no longer accepts, a new input without a variable…); always an array. |
| `version` | `string` | The installed version. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| The plugin &lt;id&gt; isn't installed. | No usable plugin of that id is installed (Greffon's etat ok; also without id). |

### A model's results and diagnostics

[`predire`](#predire) · [`predire_fichier`](#predire_fichier) · [`hca`](#hca) · [`permutations`](#permutations) · [`contributions`](#contributions) · [`appartenance`](#appartenance) · [`spectres`](#spectres) · [`biplot`](#biplot) · [`ipls`](#ipls) · [`effets`](#effets) · [`seuils`](#seuils) · [`apports`](#apports) · [`selection_aic`](#selection_aic) · [`autocorrelation_residus`](#autocorrelation_residus) · [`rapports_vraisemblance`](#rapports_vraisemblance) · [`coefficients_modeles`](#coefficients_modeles) · [`puissance_tendance`](#puissance_tendance) · [`moyennes`](#moyennes) · [`verifications_modele`](#verifications_modele) · [`resultats_texte`](#resultats_texte) · [`methodes`](#methodes) · [`resultats_cles`](#resultats_cles) · [`comparer_resultats`](#comparer_resultats) · [`arbre_stabilite`](#arbre_stabilite) · [`dependance_partielle`](#dependance_partielle) · [`kmeans_choix`](#kmeans_choix) · [`kmeans_variable`](#kmeans_variable) · [`rda_tests`](#rda_tests) · [`rda_selection`](#rda_selection) · [`rda_partition`](#rda_partition)

#### `predire`

Predicts the project's observations, or one of its prediction sets, with a fitted model.

Since 1.0. Changes the session, not a project's content. The predictions are kept in the session, the last ones of each model, for exporter_prediction.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id. It predicts on the project as it saw it: each qualitative variable's categories in their order at its fit (those added since after them), with the definition of that fit, so that categories put in another order since don't change its predictions. |
| `jeu` | `integer` | no | A prediction set of a .usp project, by its rank in Projet's predictions, from 1; without it, every observation of the project. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The id given. |
| `observations` | `object` | A table, one row per observation predicted. PCA and PLS: t1…tA, T2, DModX, DModX_pondere, "&lt;y&gt; prédit" (PLS), classe_predite (PLS-DA); a regression: "&lt;y&gt; prédit" on the modelled scale, "&lt;y&gt; observé" when known, categorie_inconnue (a category the model hasn't seen, left without a prediction) when there is one; another analysis: its own (a k-means: "groupe prédit", "distance au centre"); then hors_domaine (outside the model's domain: DModX above DCrit or T2 above T2Crit(95%), a regression's term outside its range), uid, nom, dans_modele (an observation of the model). |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| &lt;variable&gt; is called &lt;name&gt; since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a variable it used was renamed since its fit; one deleted gets “&lt;variable&gt; is no longer in the data since &lt;model&gt; was fitted: it can't be fitted again without it (Ctrl+Z brings it back).”, one turned into categories or numbers “&lt;variable&gt; is &lt;categories\|numbers&gt; since &lt;model&gt; was fitted: fit it again (…)” (atelier.R: variables_changees). ([`variables_changees`](#errors)) |
| The categories of &lt;variable&gt; changed since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a category the model saw was renamed, merged or emptied since, or a variable it used is no longer qualitative (serveur.R: modele with a_jour, atelier.R: aligner_categories). ([`modele_perime`](#errors)) |
| This analysis doesn't predict new observations. | The model is an analysis that doesn't predict (Modele's prediction false: RDA, CA, PCoA…). ([`ne_predit_pas`](#errors)) |
| Unknown prediction set: &lt;jeu&gt; (the project has &lt;n&gt;). | jeu isn't the rank of one of the project's prediction sets. |
| predict() didn't predict these observations: &lt;R's message&gt; | An R-code or plugin model whose fit's predict() fails on these observations, or doesn't give one prediction per observation (predict() didn't give one prediction per observation.). |
| Model &lt;name&gt; is made with the plugin "&lt;plugin&gt;" &lt;version&gt;, which isn't installed or approved on this computer: it doesn't run until you have read and installed it (Show, Install, in the bar above the project). | A plugin's model is fitted, predicts or is exported to R, and its plugin's fingerprint isn't approved here (besides erreur: greffon_non_approuve: true, empreinte, id, version); under the DisableCustomCode policy, a plugin that isn't the organisation's gets “Model &lt;name&gt; is made with the plugin "&lt;plugin&gt;" &lt;version&gt;: on this computer, your organisation's policy (DisableCustomCode) only lets the plugins it provides run.” (greffon_interdit: true); a fit without the packages it needs “The plugin "&lt;plugin&gt;" needs packages that aren't installed for the R Nuée uses (R &lt;version&gt;): &lt;packages&gt;. Install them (File › Plugins…, or install.packages(…) in R), then fit again.” (paquets_manquants, always an array, and id) (greffons.R: exiger_greffon_permis, exiger_paquets_greffon). ([`greffon_refuse`](#errors)) |

#### `predire_fichier`

Predicts the rows of a table file with a fitted model, the file's columns matched to the model's variables by name.

Since 1.0. Changes the session, not a project's content. The predictions are kept in the session, the last ones of each model, for exporter_prediction.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id. It reads the file on the project as it saw it: the categories in their order at its fit, with the definition of that fit. |
| `chemin` | `string` | yes | The file on the engine's computer, in a format File › Open reads (CSV, Excel…); from a browser, the path /api/deposer gives. Its columns are found by the variables' names, case and surrounding spaces aside, a qualitative variable's values by its categories' names; a model's quantitative variable is read as numbers. A hierarchical model's missing scores of base models ($M1.t1…) are predicted by the latest model of that name fitted in the session, refused as the model itself would be when its variables or categories changed since its fit. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The id given. |
| `observations` | `object` | A table, one row per row of the file: the columns of predire's, without uid, nom the rows' names as the file gives them, dans_modele false. |
| `absentes` | `string[]` | The model's X variables the file hasn't, left missing (one name may come as a single string). |
| `n` | `integer` | The file's number of rows. |
| `fichier` | `string` | The file's name. |
| `note` | `string[]` | What to know of the reading: values read as missing in the model's X columns, a regression's effort missing from the file (predictions for an effort of 1) (always an array). |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| &lt;variable&gt; is called &lt;name&gt; since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a variable it used was renamed since its fit; one deleted gets “&lt;variable&gt; is no longer in the data since &lt;model&gt; was fitted: it can't be fitted again without it (Ctrl+Z brings it back).”, one turned into categories or numbers “&lt;variable&gt; is &lt;categories\|numbers&gt; since &lt;model&gt; was fitted: fit it again (…)” (atelier.R: variables_changees). ([`variables_changees`](#errors)) |
| The categories of &lt;variable&gt; changed since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a category the model saw was renamed, merged or emptied since, or a variable it used is no longer qualitative (serveur.R: modele with a_jour, atelier.R: aligner_categories). ([`modele_perime`](#errors)) |
| This analysis doesn't predict new observations. | The model is an analysis that doesn't predict (Modele's prediction false: RDA, CA, PCoA…). ([`ne_predit_pas`](#errors)) |
| File not found: &lt;path&gt; | The file named doesn't exist. ([`fichier_introuvable`](#errors)) |
| Unrecognised file format: .&lt;extension&gt; | The file's extension isn't one Nuée reads. ([`format_inconnu`](#errors)) |
| &lt;lines&gt;: more values than the first line has columns (&lt;number&gt;). The values would be shifted: a value may contain the separator (&lt;separator&gt;) or a quote, or a column name is missing from the first line. | Lines of a delimited text have more values than its first line has names (formats.R). ([`valeurs_decalees`](#errors)) |
| The table is empty | The table read has no row or no column (nouveau.R: projet_depuis_tableau). ([`tableau_vide`](#errors)) |
| This file holds no table (data frame or matrix) | An R file (.rds, .RData) holds no table. |
| cannot open the connection | The file exists but can't be read (R's own message). |
| predict() didn't predict these observations: &lt;R's message&gt; | An R-code or plugin model whose fit's predict() fails on these observations, or doesn't give one prediction per observation (predict() didn't give one prediction per observation.). |
| Model &lt;name&gt; is made with the plugin "&lt;plugin&gt;" &lt;version&gt;, which isn't installed or approved on this computer: it doesn't run until you have read and installed it (Show, Install, in the bar above the project). | A plugin's model is fitted, predicts or is exported to R, and its plugin's fingerprint isn't approved here (besides erreur: greffon_non_approuve: true, empreinte, id, version); under the DisableCustomCode policy, a plugin that isn't the organisation's gets “Model &lt;name&gt; is made with the plugin "&lt;plugin&gt;" &lt;version&gt;: on this computer, your organisation's policy (DisableCustomCode) only lets the plugins it provides run.” (greffon_interdit: true); a fit without the packages it needs “The plugin "&lt;plugin&gt;" needs packages that aren't installed for the R Nuée uses (R &lt;version&gt;): &lt;packages&gt;. Install them (File › Plugins…, or install.packages(…) in R), then fit again.” (paquets_manquants, always an array, and id) (greffons.R: exiger_greffon_permis, exiger_paquets_greffon). ([`greffon_refuse`](#errors)) |

#### `hca`

Clusters a PCA or PLS model's observations by Ward's method on their scores, and cuts the tree into groups.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id: a PCA, PLS or OPLS (not a regression or another analysis). |
| `composantes` | `integer` | no | How many components' scores (t1, t2…) make the Euclidean distances between observations, from 1 to the model's A; the model's A by default. At least 1. |
| `groupes` | `integer` | no | The number of clusters to cut the tree into; brought back to the number of observations in the tree when larger. At least 1. Default: `3`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id, as given. |
| `composantes` | `integer` | The number of components whose scores were used. |
| `merge` | `integer[][]` | hclust's merge matrix, one [a, b] row per fusion: a negative number -j is the observation at position j of uid, a positive one the cluster made at that earlier row. |
| `hauteurs` | `number[]` | Each fusion's Ward height, the increase of the within-group sum of squares of the scores (hclust's ward.D2 height squared over 2); they add up to the scores' total sum of squares. |
| `ordre` | `integer[]` | The order of the leaves for drawing the tree, as positions in uid. |
| `noms` | `string[]` | The observations' names, in the order of uid. |
| `uid` | `number[]` | The numbers of the model's observations that have all the scores used (the others are left out), in the model's order. |
| `groupes` | `integer[]` | Each observation's cluster, 1 the largest, in the order of uid. |
| `k` | `integer` | The number of clusters: groupes, at most the number of observations in the tree. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| This isn't for this model: a regression or another analysis doesn't have the components and scores of a PCA or a PLS. | A request that needs components and scores (serveur.R: SANS_REGRESSION: hca, permutations, scores_en_variables, appartenance, contributions, spectres, biplot, ipls) names a regression or another analysis in modele or modeles; checked before the request runs. ([`composantes_seulement`](#errors)) |
| The dendrogram is made on 1 to &lt;A&gt; components (the model's): &lt;composantes&gt;. | composantes is below 1 or above the model's number of components. |
| elements of 'k' must be between 1 and &lt;n&gt; | groupes is below 1 (R's own message from cutree, untranslated). |

#### `permutations`

Tests a PLS model by refitting it with its Y permuted among its observations, and gives each refit's R² and Q² with their regression lines on the correlation to the original Y.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id: a PLS, PLS-DA, OPLS or OPLS-DA. |
| `y` | `integer` | no | Which Y is permuted and measured, by its position among the model's Y (1 the first, their number at most); for a PLS-DA, every class is permuted together and y chooses the class indicator whose R² and Q² are measured. At least 1. Default: `1`. |
| `n` | `integer` | no | The number of permutations; a value outside 1 to 999 is brought back within it. Each refit redoes the whole model (scaling and cross-validation included) with the same number of components; the permutations come from a fixed seed, the same at each call. From 1 to 999. Default: `20`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id, as given. |
| `y` | `string` | The name of the Y measured. |
| `n` | `integer` | The number of permutations made. |
| `points` | `object` | A table (columns), the model itself first then one row per permutation: correlation (the absolute correlation between the original and the permuted Y, 1 for the model), R2 and Q2 (that Y's cumulative R² and Q²), origine (true for the model itself). |
| `ordonnee_R2` | `number` | The intercept of the regression line of R2 on the correlation: with ordonnee_Q2, a measure of overfitting (a valid model usually has R2 below 0.3 to 0.4 and Q2 below 0.05 there). |
| `pente_R2` | `number` | The slope of that line. |
| `ordonnee_Q2` | `number` | The intercept of the regression line of Q2 on the correlation. |
| `pente_Q2` | `number` | The slope of that line. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| &lt;variable&gt; is called &lt;name&gt; since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a variable it used was renamed since its fit; one deleted gets “&lt;variable&gt; is no longer in the data since &lt;model&gt; was fitted: it can't be fitted again without it (Ctrl+Z brings it back).”, one turned into categories or numbers “&lt;variable&gt; is &lt;categories\|numbers&gt; since &lt;model&gt; was fitted: fit it again (…)” (atelier.R: variables_changees). ([`variables_changees`](#errors)) |
| The categories of &lt;variable&gt; changed since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a category the model saw was renamed, merged or emptied since, or a variable it used is no longer qualitative (serveur.R: modele with a_jour, atelier.R: aligner_categories). ([`modele_perime`](#errors)) |
| This isn't for this model: a regression or another analysis doesn't have the components and scores of a PCA or a PLS. | A request that needs components and scores (serveur.R: SANS_REGRESSION: hca, permutations, scores_en_variables, appartenance, contributions, spectres, biplot, ipls) names a regression or another analysis in modele or modeles; checked before the request runs. ([`composantes_seulement`](#errors)) |
| The permutation test only applies to PLS models | The model is a PCA. |
| The model has no Y number &lt;y&gt;: it has &lt;n&gt;. | y isn't one of 1 to the model's number of Y (for a PLS-DA, of classes). |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |

#### `contributions`

Gives each X term's contribution to how far an observation, or the mean of several, lies from a PCA or PLS model (DModX) or from the model's centre (a score, T²).

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id: a PCA, PLS or OPLS. |
| `observations` | `number[]` | yes | The observations' numbers; several give the contributions to their mean, and those that aren't in the model are left out. |
| `type` | `string` | no | What the contributions explain. Values: `DModX` (Each term's scaled residual, as in the distance to the model), `score` (The distance to the mean observation in scaled units, times the absolute weight of the component composante (w\* in a PLS, p in a PCA)), `T2` (The same with the weights of all the components together, for an observation outside the ellipse). Default: `"DModX"`. |
| `composante` | `integer` | no | With type score: the component, from 1 to the model's A. At least 1. Default: `1`. |
| `ponderee` | `boolean` | no | With type DModX: each residual multiplied by its variable's modelling power, as the weighted DModX. Default: `true`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id, as given. |
| `terme` | `string[]` | The model's X terms, in its order. |
| `contribution` | `number[]` | Each term's contribution, in the order of terme. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| This isn't for this model: a regression or another analysis doesn't have the components and scores of a PCA or a PLS. | A request that needs components and scores (serveur.R: SANS_REGRESSION: hca, permutations, scores_en_variables, appartenance, contributions, spectres, biplot, ipls) names a regression or another analysis in modele or modeles; checked before the request runs. ([`composantes_seulement`](#errors)) |
| None of the chosen observations is in the model | No observation given is one of the model's. |
| "&lt;type&gt;" isn't a choice of contributions: DModX, score, T2. | type is none of its values. |
| Component &lt;composante&gt;: the model has &lt;A&gt;. | With type score, composante is below 1 or beyond the model's number of components. |

#### `appartenance`

Gives each observation's distance to every model of a group of class models, the classes whose model accepts it, and the share of each known class that each model accepts.

Since 1.0. Changes nothing. A model of the group that isn't fitted in the session is fitted for the request, and not kept.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The id of one fitted model of the group (made by modeles_classe); the group's other models are those of the session, the latest of each name, or fitted for this request when there are none. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `observations` | `object` | A table (columns), one row per observation of the project, those no model saw included: uid, nom, classe (its known class, null without one), one column "DModX &lt;class&gt;" per class (its distance to that class's model), classes (the classes whose model accepts it, DModX at most the model's DCrit at 5 %, comma-separated; "" for none). |
| `classes` | `string[]` | The classes of the group, one per model. |
| `dcrit` | `number[]` | Each model's critical distance DCrit at 5 %, in the order of classes (the names R gives it are lost in JSON). |
| `table` | `object[]` | One row per known class, then "?" for the observations without one: {classe, n (its observations), parts (the share of them each model accepts, in the order of classes; always an array)}. |
| `variable` | `string` | The class variable's name. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| This isn't for this model: a regression or another analysis doesn't have the components and scores of a PCA or a PLS. | A request that needs components and scores (serveur.R: SANS_REGRESSION: hca, permutations, scores_en_variables, appartenance, contributions, spectres, biplot, ipls) names a regression or another analysis in modele or modeles; checked before the request runs. ([`composantes_seulement`](#errors)) |
| This model isn't part of a group of class models | The model wasn't made by modeles_classe. |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |

#### `spectres`

Gives a model's spectra, its quantitative X variables for its observations, as read and after its spectral preprocessing.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id: a PCA, PLS or OPLS. |
| `quoi` | `string` | no | Which spectra to send; both without it (any other value gives neither). Values: `bruts` (The spectra as read only (each value transformed as the model transforms it)), `pretraites` (The preprocessed spectra only (SNV, MSC, derivatives…; the same as read without preprocessing)), `noms` (Neither: only the variables). |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id, as given. |
| `noms` | `string[]` | The quantitative X terms, in the model's order (always an array). |
| `variables` | `number[]` | Their variables' numbers (always an array). |
| `uid` | `number[]` | The model's observations, in its order: the rows of bruts and pretraites. |
| `bruts` | `number[][]` \| `null` | One row per observation, one value per term, rounded to 6 significant digits; null unless asked. |
| `pretraites` | `number[][]` \| `null` | The same after the model's spectral preprocessing; null unless asked. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| This isn't for this model: a regression or another analysis doesn't have the components and scores of a PCA or a PLS. | A request that needs components and scores (serveur.R: SANS_REGRESSION: hca, permutations, scores_en_variables, appartenance, contributions, spectres, biplot, ipls) names a regression or another analysis in modele or modeles; checked before the request runs. ([`composantes_seulement`](#errors)) |

#### `biplot`

Gives a PCA or PLS model's scores and loadings scaled to correlations, for a biplot within the unit circle.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id: a PCA, PLS or OPLS. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id, as given. |
| `t` | `number[][]` | One array per component: each observation's score divided by the norm of that component's scores (null for an observation without scores). |
| `p` | `number[][]` | One array per component: each X term's correlation with the scores, on the observations where it has a value (null under 3 of them, or without variation). |
| `c` | `number[][]` \| `null` | The same for each Y of a PLS; null for a PCA. |
| `noms_x` | `string[]` | The X terms, in the order of p's arrays (always an array). |
| `noms_y` | `string[]` | The Y terms, in the order of c's arrays (always an array, empty for a PCA). |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| This isn't for this model: a regression or another analysis doesn't have the components and scores of a PCA or a PLS. | A request that needs components and scores (serveur.R: SANS_REGRESSION: hca, permutations, scores_en_variables, appartenance, contributions, spectres, biplot, ipls) names a regression or another analysis in modele or modeles; checked before the request runs. ([`composantes_seulement`](#errors)) |

#### `ipls`

Fits interval PLS models on consecutive ranges of a model's spectrum and gives each range's RMSEcv for 1 to A components (iPLS, Nørgaard et al. 2000).

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id: a PLS, PLS-DA or OPLS whose quantitative X make a spectrum (an OPLS is fitted as a PLS). |
| `n` | `integer` | no | The number of intervals of about the same width, at most the number of spectrum variables (brought back within that range). At least 1. Default: `20`. |
| `A` | `integer` | no | Up to how many components each interval's PLS is fitted with; the model's A by default. At least 1. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id, as given. |
| `intervalles` | `object[]` | One per interval, in the spectrum's order: {termes, variables (its terms and their variables' numbers), rmsecv (for 1, 2… A components, null where even one component doesn't fit), A (the number that gives the smallest RMSEcv, null if none)}; the arrays always arrays. |
| `complet` | `object` | The same for the whole spectrum, {rmsecv, A}: it gives the model back. |
| `A` | `integer` | The largest number of components tried. |
| `spectre` | `string[]` | The spectrum's terms, in order (always an array). |
| `moyen` | `number[]` | The mean preprocessed spectrum, one value per term of spectre (always an array). |
| `autres` | `string[]` | The model's other X terms (qualitative ones, cross terms), in every interval's model (always an array). |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| &lt;variable&gt; is called &lt;name&gt; since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a variable it used was renamed since its fit; one deleted gets “&lt;variable&gt; is no longer in the data since &lt;model&gt; was fitted: it can't be fitted again without it (Ctrl+Z brings it back).”, one turned into categories or numbers “&lt;variable&gt; is &lt;categories\|numbers&gt; since &lt;model&gt; was fitted: fit it again (…)” (atelier.R: variables_changees). ([`variables_changees`](#errors)) |
| The categories of &lt;variable&gt; changed since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a category the model saw was renamed, merged or emptied since, or a variable it used is no longer qualitative (serveur.R: modele with a_jour, atelier.R: aligner_categories). ([`modele_perime`](#errors)) |
| This isn't for this model: a regression or another analysis doesn't have the components and scores of a PCA or a PLS. | A request that needs components and scores (serveur.R: SANS_REGRESSION: hca, permutations, scores_en_variables, appartenance, contributions, spectres, biplot, ipls) names a regression or another analysis in modele or modeles; checked before the request runs. ([`composantes_seulement`](#errors)) |
| iPLS needs a model with Y (PLS, PLS-DA, OPLS). | The model is a PCA. |
| iPLS needs quantitative X variables (a spectrum). | The model has fewer than 2 quantitative X terms. |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |

#### `effets`

Gives the effect of each quantitative term of a regression along its variable's values, on the link's scale with its 95 % band, one per category for a term crossed with categories, or with echelle reponse the response's predicted values.

Since 1.0. Changes the session, not a project's content. Computed once for each scale and kept with the fitted model: later calls give the kept result.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted regression's id. |
| `echelle` | `string` | no | The scale of the result; reponse gives the predicted values, with fields of their own (echelle, commun, and each term's fixes). Values: `lien` (The effects on the link's scale, centred (any other value too)), `reponse` (The predicted values of the response, on its scale (Plots › Effects › Predicted values)). Default: `"lien"`. Since 2.0. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id, as given. |
| `echelle` | `string` | With echelle reponse: "reponse"; absent on the link's scale. Since 2.0. |
| `lien` | `string` | The model's link: identity, log, logit… |
| `effets` | `object[]` | One per quantitative term, main effects first, then the terms that only enter through a cross term with categories. A term alone: {terme, courbe (a smooth curve, else a straight line), x (80 values over the variable's range, in its units, transformed if it is), effet (centred on the variable's mean, the other terms fixed), bas, haut (the 95 % band), valeurs (the observed values)}. A term crossed with categories: {terme, courbe, par (their variables, "a × b"), valeurs, niveaux: one {niveau, x, effet, bas, haut, valeurs, and for a straight line pente, pente_bas, pente_haut per unit of the variable} per category observed}. A curve per category: {terme, courbe: true, par_categorie: true, par, valeurs, niveaux: one {niveau, x, effet, bas, haut, valeurs} per category}. With echelle reponse, the same terms, order and shapes, without slopes: effet, bas and haut are the response's predicted values and their band (the inverse link of the linear predictor and of its ± 1.96 standard errors; the response as modelled for an identity link; a zero-inflated Poisson's expected count), not centred; the model's other quantitative variables at their mean on its observations (before their transformation), its qualitative ones at their reference category, but those crossed with the term, each category of which has its line, x over the term's values in that category; each term adds fixes, what stays fixed for it ({quantitatives: a table nom, valeur; qualitatives: a table nom, categorie}), and each line grille, its grid (for the export to R). |
| `commun` | `object` | With echelle reponse: what stays fixed for every term, {aleatoires (the random effects' levels, at 0: the population; always an array), decalage (the effort variable's name, at 1: densities; null without one), spatial ({nom, x, y}: the spatial smooth at the coordinates' mean point; null without one)}. Since 2.0. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Only for a regression. | The model isn't a regression (LM, LMM, GLM, GAM). ([`regression_seulement`](#errors)) |
| The values of "&lt;term&gt;" have changed since this model was fitted: fit it again. | With echelle reponse, a quantitative variable's values in the project no longer give the model's columns. |

#### `seuils`

Gives a GAM curve's effect and slope with simultaneous 95 % bands, and the ranges where the slope is clearly positive or negative, as thresholds (Simpson 2018).

Since 1.0. Changes the session, not a project's content. Computed once per term and category and kept with the fitted model: later calls give the kept result.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted regression's id: a GAM, or a model with a curve fitted by mgcv. |
| `terme` | `string` | yes | The curve's term, by its name as in the model's coefficients and curves. |
| `categorie` | `string` | no | For a curve per category (s(x, by = f)) or a curve crossed with categories: the category whose curve to read; the first without it. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id, as given. |
| `terme` | `string` | The curve's term. |
| `categorie` | `string` | Only for a curve with categories: the category read. |
| `par` | `string` | Only for a curve with categories: its qualitative variable or variables ("a × b"). |
| `edf` | `number` | The curve's effective degrees of freedom. |
| `p` | `number` | The p of the curve's test (mgcv). |
| `lineaire` | `boolean` | edf below 1.5: the curve is almost a straight line. |
| `amplitude` | `number` | The range of the effect over the grid. |
| `x` | `number[]` | 200 values from the 2nd to the 98th percentile of the variable (in that category), in the variable's units. |
| `effet` | `number[]` | The curve at x, on the link's scale, centred on its mean. |
| `bas` | `number[]` | The lower edge of its simultaneous 95 % band (2000 draws of the coefficients, a fixed seed). |
| `haut` | `number[]` | The upper edge of that band. |
| `x_pente` | `number[]` | The 199 midpoints between consecutive values of x. |
| `pente` | `number[]` | The curve's slope at x_pente, per unit of the variable. |
| `pente_bas` | `number[]` | The lower edge of the slope's simultaneous 95 % band. |
| `pente_haut` | `number[]` | Its upper edge. |
| `sens` | `string[]` | At each midpoint: "hausse" (the band of the slope is above 0), "baisse" (below 0) or "ns". |
| `plages_pente` | `object` | A table (columns) of the ranges where the slope is clearly positive or negative: sens ("hausse" or "baisse"), de, a (from, to). |
| `plages_effet` | `object` | A table (columns) of the ranges where the effect is clearly above or below its mean: sens ("au_dessus" or "au_dessous"), de, a. |
| `critique_effet` | `number` | The critical multiplier of the effect's simultaneous band. |
| `critique_pente` | `number` | That of the slope's band. |
| `tirages` | `integer` | The number of draws of the coefficients, 2000. |
| `niveau` | `number` | The bands' level, 0.95. |
| `valeurs` | `number[]` | The variable's observed values (in that category), in its units: a rug. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Only for a regression. | The model isn't a regression (LM, LMM, GLM, GAM). ([`regression_seulement`](#errors)) |
| Thresholds are read on a curve of an additive model. | The regression wasn't fitted by mgcv: an LM, or an LMM fitted by nlme. |
| "&lt;term&gt;" is not a curve of this model. | terme names no curve of the model. |
| "&lt;category&gt;" is not a category of the curve of "&lt;term&gt;". | categorie names no category of that curve, or the curve has none. |

#### `apports`

Gives what each group of terms brings to a regression, from the model refitted by maximum likelihood without it: the change of AIC and the likelihood-ratio test.

Since 1.0. Changes the session, not a project's content. Computed once and kept with the fitted model: later calls give the kept result.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted regression's id. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id, as given. |
| `apports` | `object` | A table (columns), one row per group of terms (its block in New model, else its variable): groupe, termes (how many terms leave with it), delta_aic (the AIC without it minus the full model's, both ML: above 0 the group improves the model, above 2 clearly), khi2, ddl, p (the likelihood-ratio test; null when the refit fails), avec (the groups that leave with it, the cross terms that need it, comma-separated). An empty object {} for a model without terms. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Only for a regression. | The model isn't a regression (LM, LMM, GLM, GAM). ([`regression_seulement`](#errors)) |

#### `selection_aic`

Selects a regression's fixed effects by AICc over every allowed combination of its groups of terms, or by backward elimination beyond 256 combinations, each candidate refitted by maximum likelihood.

Since 1.0. Changes the session, not a project's content. Computed once and kept with the fitted model: methodes then describes the selection and script_r redoes it.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted regression's id. Its random effects, autocorrelation, distribution, offset and curves' settings stay those of the model; a cross term enters only with its main effects; the groups that carry a random slope stay in every candidate. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id, as given. |
| `K` | `integer` | The number of groups chosen among. With 0, the result has only K, N and gardes. |
| `N` | `integer` | The model's number of observations, those of every candidate. |
| `gardes` | `string[]` | The groups kept in every candidate: those carrying a random slope and those they need (always an array). |
| `methode` | `string` | "toutes" (every combination, 2^K at most limite) or "descendante" (backward elimination: at each step, the removal that lowers AICc the most). |
| `limite` | `integer` | The most combinations fitted all together, 256. |
| `groupes` | `string[]` | The groups chosen among, by name (always an array). |
| `termes` | `string[][]` | Each group's terms, in the order of groupes. |
| `requis` | `string[][]` | The groups each group needs (a cross term, its main effects), in the order of groupes. |
| `modeles` | `object` | A table (columns) of the candidates, by increasing AICc (failed fits last): modele (its groups, "a + b", "1" for the null model), n_groupes, k (parameters), logLik, AICc, delta (ΔAICc), poids (Akaike weight), poids_cumule, etape (the step of the backward elimination, null for every combination). |
| `contient` | `integer[][]` | For each group, in the order of groupes: 1 or 0 for each row of modeles, whether it contains that group. |
| `importance` | `object` | A table (columns): groupe, importance (the sum of the weights of the candidates that contain it). |
| `meilleur` | `integer` | The row of modeles of the best candidate, the lowest AICc: 1. |
| `parcimonieux` | `integer` | The row of the most parsimonious candidate: the fewest parameters among those with ΔAICc at most 2. |
| `chemin` | `object` \| `null` | The backward elimination's steps, a table (columns) etape, retire (the group removed), AICc; null for every combination. |
| `echecs` | `integer` | The number of candidates whose fit failed. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Only for a regression. | The model isn't a regression (LM, LMM, GLM, GAM). ([`regression_seulement`](#errors)) |
| No model has an AICc: too few observations for their parameters. | Every candidate's fit failed or has no AICc. |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |

#### `autocorrelation_residus`

Gives the autocorrelation of a regression's residuals along time within groups, pooled as nlme's ACF, to see whether a CAR1 autocorrelation is needed or is enough.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted regression's id. A model with CAR1 uses its own time and groups, and the next two parameters are ignored. |
| `temps` | [`Groupement`](#type-groupement) | no | The time, an identifier ({by: "id", id}) or a variable ({by: "variable", variable}), numbers or dates; required without CAR1. Several: the first. |
| `groupe` | [`Groupement`](#type-groupement) \| [`Groupement`](#type-groupement)`[]` | no | The groups within which the residuals follow each other, nested from the widest to the finest; {by: "effets"} stands for the model's random effects. Without it, all the observations make one series. |
| `type` | `string` | no | The residuals of a model fitted by mgcv alone (a GLM, a GAM); the others' are Pearson's. Values: `pearson` ((y − fitted) / √variance), `deviance` (Each observation's signed part of the deviance). Default: `"pearson"`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id, as given. |
| `car1` | `boolean` | Whether the model has CAR1. |
| `phi` | `number` \| `null` | Its CAR1 φ, null without CAR1. |
| `temps` | `string` | The time's name. |
| `groupe` | `string` \| `null` | The groups' names, "site / sector"; null for one series. |
| `n` | `integer` | The observations used. |
| `groupes` | `integer` | The number of groups. |
| `longueur` | `integer` | The longest series. |
| `laissees` | `integer` | The model's observations left out, without a time or a group. |
| `decalage` | `integer[]` | The lags, 0 to min(10, the longest series minus 1): successive observations of a series, not units of time. |
| `paires` | `number[]` | The pairs of observations at each lag. |
| `bande` | `number[]` | ±1.96/√pairs at each lag: the band of nlme's plot. |
| `series` | `object[]` | {residus, acf (one value per lag)}: the residuals (residus "pearson" or "deviance"), and with CAR1 a second series of its normalized residuals ("normalises"), which show no autocorrelation left when CAR1 accounts for it. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Only for a regression. | The model isn't a regression (LM, LMM, GLM, GAM). ([`regression_seulement`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Choose the time (a year, a date...): an ID or a variable. | temps is missing and the model has no CAR1. |
| This project has no ID "&lt;id&gt;". | A grouping by id names no identifier of the observations. |
| Unknown grouping "&lt;by&gt;" | A grouping's by is none of id and variable (or effets for groupe). |
| The time (&lt;name&gt;) must be numbers (the year) or dates (2019-05-14, 14/05/2019): &lt;why&gt; | The time is made of dates and one of its values isn't a date (variables.R: refus_temps). |
| This model has no random effect. | groupe has {by: "effets"} and the model has no random effect. |
| No observation of the model has both a time and a group. | Every observation lacks its time or its group. |
| Several observations have the same time in the same group: choose finer groups (the sector within its site...). | Two observations of a group share a time. |
| Each group has a single observation: no autocorrelation to compute. | No series has two observations. |
| "&lt;type&gt;" isn't a choice of residuals: pearson, deviance. | type is none of its values for a model fitted by mgcv alone. |

#### `rapports_vraisemblance`

Gives the likelihood-ratio tests between the nested pairs among some regressions that differ only by their fixed effects, as anova(m1, m2) on their maximum-likelihood fits.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modeles` | `string[]` | yes | The fitted models' ids, each a model of the session; those that aren't regressions are left out without an error. Two are compared when they have the same response, observations, distribution, effort, random effects (slopes included) and autocorrelation, and one's fixed terms lie within the other's. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `paires` | `object` | A table (columns), one row per nested pair: grand, petit (the bigger and the smaller model's ids), ddl, khi2 (2 × the difference of their ML log-likelihoods, 0 at least), p, approche (true when one has curves or mgcv random effects: edf, an approximate test). An empty object {} without any pair. |
| `total` | `integer` | The number of nested pairs found. |
| `max` | `integer` | At most that many pairs are given, 2000. |

**Errors:**

| Message (in English) | When |
|---|---|
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |

#### `coefficients_modeles`

Puts several regressions' coefficients side by side with their fit statistics, as the cells of a table for an article.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modeles` | `string[]` | yes | The fitted regressions' ids, in the order of the columns. |
| `erreur` | `string` | no | What follows each estimate; any other value is ic. Values: `ic` (The estimate [its 95 % interval]), `et` (The estimate (its standard error)). Default: `"ic"`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modeles` | `string[]` | The columns' names: the models' names, made unique with a number (always an array). |
| `erreur` | `string` | "ic" or "et". |
| `entete` | `string` | The header of the terms' column ("Estimate [95% CI]", "Estimate (SE)"). |
| `termes` | `string[]` | The rows: the first model's terms, then the next ones' that it hasn't (always an array). |
| `cellules` | `string[][]` | One row per term, one cell per model: the estimate with its interval or standard error, as text with the decimal mark of the request's language; "" when the model hasn't the term. |
| `p` | `number[][]` | Each cell's p, null where the model hasn't the term. |
| `statistiques` | `string[]` | The fit rows that some model has: n, AIC, AICc, R², marginal and conditional R², deviance explained, the distribution's parameters, σ of each random effect and of the residual, ρ of random slopes, φ (CAR1), each curve's edf (always an array). |
| `valeurs` | `string[][]` | One row per fit statistic, one cell per model, as text. |
| `liens` | `string[]` | Each model's link (always an array). |
| `note_lien` | `string` \| `null` | What the estimates mean for a model with a log or logit link; null when every link is the identity. |
| `copie` | `object` | The same table for the clipboard: {entetes (always an array), lignes (rows of text: the terms with their cells, the p's stars added, then the fit rows), notes (the stars' legend and the link's note, always an array)}. |

**Errors:**

| Message (in English) | When |
|---|---|
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Tick at least one regression. | modeles is empty. |
| Only regressions (LM, LMM, GLM, GAM) go side by side. | A model isn't a regression. |

#### `puissance_tendance`

Computes the power of monitoring designs to detect a trend with a linear regression or a linear mixed model's variances, and on request checks one design by simulation.

Since 1.0. Changes the project, without an undo step. The result is kept with the fitted model (methodes, the report and script_r then include it), and its settings and simulation with its definition in the project, saved with it and redone when it is fitted again. With settings given, the project counts as modified; a first computation at the defaults changes the definition without marking it modified.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted regression's id: an LM, or an LMM fitted by nlme (with or without CAR1), with random intercepts only and a quantitative untransformed X for the time. Without any of the parameters below (one this request doesn't take doesn't count), the result of the last computation for this model (its sentence in the request's language), else the defaults. |
| `terme` | `string` | no | The time: the name of a quantitative, untransformed X term of the model. By default the first whose name looks like a time (year, date, time, season, année…), else the CAR1 time's variable, else the first such term. |
| `unite` | `string` | no | The trend's unit; pourcent by default for a response in log10 or ln, else pente (any other value: the default). Values: `pente` (A slope in the response's units per unit of time), `pourcent` (A change in % per year, for a response in log10 or ln). |
| `tendance` | `number` | no | The trend to detect, not 0; by default -3 (% per year), or in pente minus 3 % of the mean response's absolute value, rounded to 2 significant digits. |
| `alpha` | `number` | no | The level of the two-sided test. From 0.001 to 0.2. Default: `0.05`. |
| `annees` | `number[]` | no | The first and last numbers of years of the curves, whole numbers from 3 to 100; by default 5 to the data's span or 30, whichever is larger, 100 at most (another number of values than two: the default). |
| `unites` | `number[]` | no | The numbers of units (the first random effect's, or observations per year for an LM) of the curves, whole numbers from 2 (1 for an LM) to 10000; the data's own number is always added, 10 numbers at most. By default the data's number times 0.5, 1, 1.5 and 2. |
| `simulation` | `object` | no | The simulation's design, {annees (3 to 100, by default the data's span), unites (by default the data's), n (data sets, 10 to 2000, by default 200)}; its expected duration is in estimation. |
| `simuler` | `boolean` | no | true to run the simulation now, unless one with the same settings is kept; refused beyond 20000 observations per data set or about 15 minutes. Default: `false`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id, as given. |
| `reglages` | `object` | The settings used: {terme, unite, tendance, alpha, annees ([first, last], always an array), unites (always an array), simulation {annees, unites, n}}. |
| `termes` | `string[]` | The X terms that can be the time (always an array). |
| `type` | `string` | LM or LMM. |
| `pourcent_possible` | `boolean` | Whether the response is in log10 or ln, so a trend in % can be given. |
| `approchee` | `boolean` | A trend in % on log(a·y + c) with c not 0: an approximation. |
| `reponse` | `string` | The response's name. |
| `transformation` | `string` | The response's transformation, as text ("" without one). |
| `forme` | `object` \| `null` | That transformation as the definition keeps it; null without one. |
| `pente` | `number` | The slope to detect on the response's scale, per unit of time. |
| `pente_modele` | `number` | The model's own slope of the time, in its units, for comparison. |
| `y_modele` | `string` | The response as written with its transformation. |
| `niveaux` | `string[]` | The random effects' levels, from the widest (always an array, empty for an LM). |
| `dans` | `string` \| `null` | The finer group within which CAR1 runs, if any. |
| `temps_car1` | `string` \| `null` | The CAR1 time's name, if any. |
| `croises` | `string[]` | The model's cross terms with the time, left out of the designs (always an array). |
| `plan` | `object` | The data's design: {annees (years spanned), N, unites, emboites and moyennes (nested units per level, rounded and mean; always arrays), dans, moyenne_dans (CAR1 groups in the finest unit), repetitions, moyenne_repetitions (observations per series and year), series_par_unite}. |
| `variances` | `object` | The model's REML variances: {sigma2 (residual), niveaux (each random effect's; always an array), phi (CAR1's φ, null without CAR1)}. |
| `tableau` | `object` | A table (columns), one row per design: annees, unites, observations, erreur_type (of the GLS slope), puissance. |
| `seuil` | `number` | 0.8, the power that annees_80 looks for. |
| `annees_80` | `object` | A table (columns): unites, annees (the fewest years for 80 % power, null when the curves don't reach it). |
| `estimation` | `object` | The simulation's cost: {plan ([years, units, series per unit, repetitions]), par_ajustement (seconds per fit, null for a design too big), duree (seconds expected for n data sets), observations (per data set)}. |
| `simulation` | `object` \| `null` | The simulation once run with these settings, else null: {annees, unites, n, graine, alpha, observations, puissance (the share of p at most alpha), erreur_mc (its Monte Carlo error), echecs, duree (seconds), p (each data set's p), cle (the settings it was run with), analytique (the formula's power at that design)}. |
| `phrase` | `string` | The sentence for a report, at the data's number of units, in the request's language (X-Langue). |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Only for a regression. | The model isn't a regression (LM, LMM, GLM, GAM). ([`regression_seulement`](#errors)) |
| Trend power is computed for a linear regression or a linear mixed model (nlme), with a normal response: not for a generalized or additive model (GLM, GAM), whose distribution and mgcv fit aren't covered. | The model is a GLM or a GAM, or was fitted by mgcv. |
| This model has random slopes: trend power covers random intercepts only. | A random effect has random slopes. |
| This model has no time among its quantitative X (untransformed): the trend is the slope of an X, the year. Add it as an X variable. | No quantitative X term is left untransformed. |
| "&lt;term&gt;" isn't a quantitative X (untransformed) of this model: choose the time among &lt;terms&gt;. | terme names no such term. |
| The trend to detect is a non-zero number. | tendance is 0, or not a finite number. |
| α is between 0.001 and 0.2. | alpha is outside that range. |
| A trend in % per year needs a log10 or ln response (New model › Variables). | unite is pourcent and the response isn't in log10 or ln. |
| A decline of 100% or more has no logarithm. | unite is pourcent and tendance is -100 or less. |
| The CAR1 autocorrelation runs along "&lt;CAR1 time&gt;", the trend along "&lt;term&gt;": their units differ (&lt;b&gt; units of "&lt;CAR1 time&gt;" for one of "&lt;term&gt;"), and the designs count once per unit of time. | The model's CAR1 time and the trend's time aren't in the same units. |
| 10 numbers of units at most. | unites, with the data's number added, has more than 10 numbers. |
| The years go from 3 to 100. | A number of years of annees or of simulation isn't a whole number from 3 to 100. |
| The numbers of units are whole numbers from &lt;2 or 1&gt; to 10000. | A number of units of unites or of simulation is outside that range (at least 2 for a mixed model, 1 for an LM). |
| The simulation draws 10 to 2000 data sets. | simulation's n is outside that range. |
| This design has &lt;n&gt; observations per data set: 20000 at most for the simulation. | simuler with a design too big. |
| &lt;n&gt; data sets would take about &lt;minutes&gt; minutes (&lt;seconds&gt; s per fit): fewer data sets, or a smaller design. | simuler with a simulation expected to take more than 15 minutes. |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |

#### `moyennes`

Gives a regression's adjusted means of the categories of a qualitative X, or a quantitative X's slope in each category, and the pairs of categories compared, as the emmeans package computes them.

Since 1.0. Changes the project, without an undo step. The result is kept with the fitted model (methodes, the report and script_r then include it), and its settings with its definition in the project, saved with it and redone when it is fitted again. With settings given, the project counts as modified; a first computation at the defaults changes the definition without marking it modified.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted regression's id. Without any of the parameters below (one this request doesn't take doesn't count), the settings of its last computation in the session, else the defaults. |
| `facteur` | `number` \| `string` | no | The qualitative X variable whose categories' means are computed, by its number; the model's first qualitative variable by default. |
| `dans` | `number` \| `string` \| `null` | no | A qualitative variable crossed with facteur in a term of the model, by its number: facteur's categories are compared within each of its categories (simple effects). |
| `pente` | `number` \| `string` \| `null` | no | A quantitative variable crossed with facteur in a term, by its number: its slope in each category instead of the means (as emtrends). |
| `ajustement` | `string` | no | How the pairs' p values and intervals are adjusted, within each category of dans. Values: `tukey` (Tukey's studentized range (Tukey–Kramer)), `holm` (Holm's p values and Bonferroni intervals), `aucun` (No adjustment). Default: `"tukey"`. |
| `echelle` | `string` | no | The scale of a model with a log or logit link (slopes stay on the link's scale). Values: `reponse` (On the response's scale: means, and ratios (odds ratios for a logit link) for the pairs), `lien` (On the link's scale). Default: `"reponse"`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id, as given. |
| `reglages` | `object` | The settings used: {facteur (the variable's number as text), dans, pente, ajustement, echelle}. |
| `type` | `string` | The regression's type. |
| `famille` | `string` | Its distribution. |
| `lien` | `string` | Its link. |
| `reponse` | `string` | The response's name. |
| `transformation_y` | `string` | The response's transformation, as text. |
| `forme_y` | `string` \| `null` | The response as written with its transformation, null without one. |
| `log_y` | `boolean` | Whether the response is in log10 or ln. |
| `mode` | `string` | "moyennes" or "pentes". |
| `echelle` | `string` | The scale of the estimates: "reponse", "identite" (an identity link) or "lien". |
| `facteur` | `object` | {uid, nom, niveaux (its observed categories, always an array)}. |
| `dans` | `object` \| `null` | The same for dans, null without it. |
| `pente` | `object` \| `null` | With pente: {uid, nom, terme, delta (the step of the forward difference), forme}; null without it. |
| `grille` | `object` | The reference grid: {lignes, covariables (a table: terme, nom, moyenne, valeur, transformee, courbe; each quantitative variable at its mean), moyennees (the qualitative variables averaged over, always an array), decalage, spatial ({nom, x, y}: a spatial smooth at the mean point)}. |
| `croisee` | `string[]` | The variables crossed with facteur that dans or pente can name (always an array). |
| `ddl` | `object` | {methode ("residuels", "confinement" for nlme, or "asymptotique": z), groupe, groupes (nlme's first random effect and its number of groups)}. |
| `moyennes` | `object` | A table (columns), one row per estimable category (or pair of categories with dans): niveau, groupe (dans's category, null without dans), estimation, erreur_type, ddl, bas, haut (95 %), lien, erreur_type_lien (on the link's scale). |
| `paires` | `object` | A table (columns), one row per estimable pair within each category of dans: groupe, niveau_1, niveau_2, difference, erreur_type_lien, ddl, t, p_brute, p (adjusted), famille (the family size of the adjustment), bas_lien, haut_lien, estimation, erreur_type, bas, haut (on the result's scale). |
| `non_estimables` | `object` | {moyennes, paires}: the categories and pairs left out because no observation has them (always arrays). |
| `options` | `object[]` | The choices for each qualitative variable of the model: {uid, nom, niveaux, dans: [{uid, nom}], pentes: [{uid, nom}]}. |
| `phrases` | `string[]` | A sentence for a report per pair, in the order of paires, in the request's language (X-Langue; always an array). |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| &lt;variable&gt; is called &lt;name&gt; since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a variable it used was renamed since its fit; one deleted gets “&lt;variable&gt; is no longer in the data since &lt;model&gt; was fitted: it can't be fitted again without it (Ctrl+Z brings it back).”, one turned into categories or numbers “&lt;variable&gt; is &lt;categories\|numbers&gt; since &lt;model&gt; was fitted: fit it again (…)” (atelier.R: variables_changees). ([`variables_changees`](#errors)) |
| The categories of &lt;variable&gt; changed since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a category the model saw was renamed, merged or emptied since, or a variable it used is no longer qualitative (serveur.R: modele with a_jour, atelier.R: aligner_categories). ([`modele_perime`](#errors)) |
| Only for a regression. | The model isn't a regression (LM, LMM, GLM, GAM). ([`regression_seulement`](#errors)) |
| This model has no qualitative variable: adjusted means are computed for each category of a qualitative X variable (a main effect, or in a cross term). | The model has no qualitative X. |
| "&lt;facteur&gt;" isn't a qualitative variable of this model. | facteur names no qualitative X of the model. |
| "&lt;dans&gt;" isn't crossed with "&lt;facteur&gt;" in any term of the model: the categories of one aren't compared within each category of the other. | dans isn't crossed with facteur. |
| "&lt;pente&gt;" isn't a quantitative variable crossed with "&lt;facteur&gt;" in a term of the model: its slope doesn't change from one category to another. | pente isn't crossed with facteur. |
| Unknown adjustment: tukey, holm or aucun (none). | ajustement is none of its values. |
| Unknown scale: reponse (response) or lien (link). | echelle is none of its values. |
| The reference grid would have &lt;n&gt; rows (one per combination of the categories of the qualitative variables): 10000 at most. | The model's qualitative variables have too many combinations of categories. |
| The values of "&lt;term&gt;" have changed since this model was fitted: fit it again. | A quantitative variable's values in the project no longer give the model's columns. |
| "&lt;variable&gt;" doesn't vary in the model's observations. | With pente, its variable is constant. |

#### `verifications_modele`

Checks a regression's conditions after its fit and says each in words: the residuals' distribution and spread, influential observations, overdispersion, zeros, separation, collinearity, concurvity, an uncentred X crossed with categories and spatial autocorrelation.

Since 1.0. Changes the session, not a project's content. Computed once per language and kept with the fitted model.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id; for a model that isn't a regression, the list is empty. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `verifications` | `object[]` | One per condition checked, {cle, statut, texte}: cle "loi" (Shapiro–Wilk on the standardized or quantile residuals), "variance" (Spearman between the residuals' size and the fitted values), "influence" (Cook's distance beyond 4/n, an LM), "dispersion" (Poisson), "zeros" (zeros observed against expected), "separation" (a binomial model with fitted probabilities of 0 or 1: terms that separate the presences from the absences), "colinearite" (VIF), "concurvite" (curves), "centrage.&lt;term&gt;" (a quantitative X far from 0, a year, crossed with a qualitative X without being centred: why its categories' coefficients are so large; the entry also has centrer, its variable's number, and nom, the term), "spatial" (Moran's I of the residuals, a project with coordinates and 3000 observations at most); statut "ok", "attention" or "info"; texte, what was seen and what to do, in the request's language (X-Langue). |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |

#### `resultats_texte`

Writes a model's Results text, what it shows in sentences with its numbers, in French or in English.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id. |
| `langue` | `string` | no | The text's language; any other value, or none, is French (this request doesn't follow X-Langue). Values: `en` (English), `fr` (French). Default: `"fr"`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `paragraphes` | `string[]` | The paragraphs. A regression: its fit (R², marginal and conditional R², deviance explained, Q², AIC), each coefficient with its interval and p (a factor or an odds ratio for a log or logit link; for a category crossed with a quantitative X, where it is read: where that X is 0, or at its mean when it is centred), the significant terms, its curves and random effects. A PCA, PLS or OPLS: R²X, R²Y and Q² by component, the gain over the empty model, CV-ANOVA, the variables with VIP above 1. Another analysis: its own text, or its summary figures. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |

#### `methodes`

Writes a model's Methods text for an article, with its references, in French or in English.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id. What was computed and kept with it (the AIC selection, the trend power, the adjusted means) is described too. |
| `langue` | `string` | no | The text's language; any other value, or none, is French (this request doesn't follow X-Langue). Values: `en` (English), `fr` (French). Default: `"fr"`. |
| `version` | `string` | no | Nuée's version, written in the text and in its reference to Nuée; "" leaves it out. Default: `""`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `paragraphes` | `string[]` | The paragraphs: the data, their preprocessing, the method, its validation, the model's quality, the software. |
| `references` | `string[]` | The references the text cites, in APA style, sorted. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |

#### `resultats_cles`

Gives a fitted model's key results, each number with its interval and p-value under the key a document cites, which the What changed view compares.

Since 2.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id. |

**Result:** `object`, with the fields below. The model's summary (resultats_cles.R: resume_resultats), to give back to comparer_resultats as it came.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The id given. |
| `entrees` | [`ResultatCle`](#type-resultatcle)`[]` | Its entries, one per key (the first of a key that repeats), in the model's order (always an array). A PCA, PLS or OPLS: N, K, A, G, R2X, R2Y, Q2, Q2_vide, gain (Q2_honnete, gain_honnete for a hierarchical model), RMSEE and RMSEcv of each Y, bien_classes (PLS-DA), R2X.&lt;a&gt;, R2Y.&lt;a&gt; and Q2.&lt;a&gt; per component, cvanova.F, then per X term vip.&lt;term&gt; and coef.&lt;term&gt; (PLS) or p&lt;a&gt;.&lt;term&gt; and R2V.&lt;term&gt; (PCA), its test set's and the results a .usp file stored. A regression: its fit (N, K, R2, R2_ajuste, R2_marginal, R2_conditionnel, deviance_expliquee, Q2, RMSEcv, AIC, AICc, BIC, dispersion, loi.&lt;parameter&gt;), each coefficient (coef.&lt;term&gt;), curve (courbe.&lt;term&gt;), random effect's standard deviation (sigma.&lt;level&gt;…), phi, its test set's, and the adjusted means and trend power kept with it. Another analysis: its own (resultats_cles.R: entrees_&lt;type&gt;; N alone without one), A (0), and each entry's parts as alias (&lt;key&gt;.ic, .bas, .haut, .p, .pourcent…). Only what fitting always computes: what is computed on request later (rda_tests, selection_aic…) isn't in it. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |

#### `comparer_resultats`

Compares two summaries of resultats_cles entry by entry, by their keys, for the What changed view: how much each value moved, the flags of each change, its importance and, for those that count, a sentence.

Since 2.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `avant` | `object` \| [`ResultatCle`](#type-resultatcle)`[]` | yes | The summary before: resultats_cles's answer as it came ({modele, entrees}), {entrees}, or the array of entries itself; an entry without cle is ignored, a key that repeats counts once. Left out, an empty summary: every entry after is new. |
| `apres` | `object` \| [`ResultatCle`](#type-resultatcle)`[]` | yes | The summary after, in the same forms; left out, every entry before is gone. |
| `langue` | `string` | no | The sentences' language; any other value is French; without it, the request's language (X-Langue). The labels stay those of the summaries: ask them in the same language. Values: `en` (English), `fr` (French). |

**Result:** `object`, with the fields below. The comparison (resultats_cles.R: comparer_resultats).

| Field | Type | Meaning |
|---|---|---|
| `entrees` | `object[]` | Every key of either side, in the order after (an entry gone placed after the last entry before it that stays), each with all its fields, null when it has nothing (always an array): cle, libelle, groupe, alias, forme, axe (the entry's, from after when it is there); avant and apres, the two values; ic_avant and ic_apres, [low, high]; p_avant and p_apres; n_avant and n_apres; classe_avant and classe_apres; classe_texte_avant and classe_texte_apres; ecart, apres − avant; ecart_relatif, ecart over \|avant\| (null when avant is 0); booleans change (anything differs), signe (the value crossed its limite: it changed sign), seuil (its p crossed 0.05; without p, its interval now excludes its limite or no longer does; or the value crossed its repere), ic (the value after is outside the interval before), nouveau, disparu, categorie (its classe changed), retourne (an axe that came out reflected, a PCA component whose sign is arbitrary: apres and ic_apres are given with their sign changed); importance, a number (0 without change); phrase, the change in a sentence as the Results text writes it, in langue, when it counts (a flag, a large move, a whole number that changes…), else null. |
| `phrases` | `object[]` | The sentences of the changes that count, the most important first, each {texte, cles (the keys it speaks of, always an array), importance}: one per entry, except more than 8 per-variable or per-species values, said in one sentence per flag; a reflected component has a sentence of its own (importance 0). Always an array. |
| `resume` | `object` | The counts: {entrees (compared), changees, identiques, nouvelles, disparues, signe, seuil, ic, categorie (the entries with that flag), retournees (the components reflected), phrases}. |

**Errors:** none of its own.

#### `arbre_stabilite`

Gives how stable a decision tree's first split is over bootstrap draws of its groups, computed once and kept with the model.

Since 1.0. Changes the session, not a project's content. Computed at the first request (the groups drawn with replacement, a fixed seed, 2026; each tree grown and pruned again) and kept with the session's model: later requests give it back.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | A decision tree (TREE) fitted in the session. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id. |
| `stabilite` | `object` | A table, as columns, one row per variable that made the first split of a draw's tree, the most frequent first: variable ("(no split)" for trees without a split, in the request's language), tirages, frequence (in %), seuil_median (its median threshold). |
| `tirages` | `number` | The number of draws: the model's setting tirages (200 by default). |
| `echecs` | `number` | The draws whose tree couldn't be grown. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Only for a decision tree. | modele isn't a decision tree (TREE). |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |

#### `dependance_partielle`

Computes the partial dependence of the variables of a random forest or of boosted trees, keeps it with the model and gives it.

Since 1.0. Changes the project, without an undo step. The dependences computed (those of earlier requests kept) stay with the session's model, and the names of their variables go into the model's definition in the project, saved with it and computed again when it opens: the project is marked modified when that list changes.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | A random forest (FOREST) or boosted trees (BOOST) fitted in the session. |
| `variables` | `string[]` | no | The names of the model's X variables wanted (their names, not their numbers); every one by default. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id. |
| `dependance` | `object[]` | One per variable asked, in the order of the model's X (not of variables), each {variable, qualitative, x, tapis, and y, reponse or probabilites}: x, the grid (always an array: the variable's distinct values when at most 25, else 25 values evenly spaced from its minimum to its maximum; a qualitative variable's observed categories); tapis, the rug (always an array: its distinct values, or 300 quantiles beyond 300, or each category's count); y, the mean prediction at each value of the grid (a forest's response, or boosted trees' f on the link's scale); reponse, boosted trees' y on the response's scale; probabilites, a classification forest's mean share of votes, a table with a column per class. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Only for a random forest or boosted trees. | modele is neither a FOREST nor a BOOST. |
| Variable unknown to the model: &lt;names&gt;. | variables names a variable that isn't one of the model's X. |

#### `kmeans_choix`

Gives, to choose k, the within-cluster sum of squares, R² and mean silhouette of the same k-means for k from 2 to 10, computed once and kept with the model.

Since 1.0. Changes the session, not a project's content. Computed at the first request (the model's data and seed) and kept with the session's model: later requests give it back.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | A k-means model (KMEANS) fitted in the session. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id. |
| `choix` | `object` | A table, as columns, one row per k from 2 to 10 (fewer with few distinct observations): k, intra (the within-cluster sum of squares), r2 (between over total), silhouette (the mean silhouette; null beyond 3,000 observations). |
| `k` | `number` | The model's own k. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Only for k-means. | modele isn't a k-means model (KMEANS). ([`kmeans_seulement`](#errors)) |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |

#### `kmeans_variable`

Adds the clusters of a k-means model to its project as a qualitative variable whose categories are 1 to k.

Since 1.0. Changes the project, with an undo step that `annuler` takes back. One undo step, labelled variable &lt;name&gt;. The variable holds the cluster of each of the model's observations, the others missing; a qualitative variable of that name already in the project is replaced (emptied, then given the clusters and the categories 1 to k). The change is also a step of the project's data ("variable &lt;name&gt;, clusters of &lt;model&gt;"), kept with the model's definition: updating the data (mettre_a_jour, retirer_etape) replays it, the k-means fitted again on the data replayed so far; undo takes the step back with the variable.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | A k-means model (KMEANS) fitted in the session; the variable goes to its project. |
| `nom` | `string` | no | The variable's name, trimmed; by default the model's name and "clusters" ("groupes" in French). |

**Result:** [`Projet`](#type-projet). The model's project, with the variable.

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Only for k-means. | modele isn't a k-means model (KMEANS). ([`kmeans_seulement`](#errors)) |
| The variable “&lt;name&gt;” already exists and isn't qualitative | A quantitative variable with values has that name. |
| Unknown observation: &lt;numbers&gt; | An observation of the model is no longer in the project (its data were updated since the fit). |

#### `rda_tests`

Gives the permutation tests of an RDA's or a CCA's axes and terms (for a CCA, also its terms one after another), computed once and kept with the model.

Since 1.0. Changes the session, not a project's content. Computed at the first request with the model's permutations and kept with the session's model (and its exports): later requests give them back.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | An RDA or a CCA fitted in the session with permutations. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id. |
| `axes` | `object` | The constrained axes tested one after another (anova.cca by = "axis"), a table as columns: axe, ddl, inertie, F, p; p never decreases from one axis to the next, null for the axes after one at p = 1; the last row is the residual (labelled in the request's language). |
| `termes` | `object` | Each term tested after all the others (by = "margin"), except those a cross term contains, a table as columns: terme, ddl, inertie, F, p; the last row is the residual. |
| `sequentiels` | `object` \| `null` | A CCA: its terms one after another in the model's order (by = "terms"), the same columns, the residual last; null for an RDA. |
| `permutations` | `number` | The model's number of permutations. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Only for an RDA or a CCA. | modele isn't a redundancy analysis (RDA) or a canonical correspondence analysis (CCA). ([`rda_seulement`](#errors)) |
| These tests need permutations: choose a number of them (Edit model). | The RDA or CCA was fitted without permutations (its options' permutations is 0). ([`permutations_requises`](#errors)) |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |

#### `rda_selection`

Gives the forward selection of an RDA's or a CCA's terms by adjusted R² and permutation tests (ordiR2step's double criterion), computed once and kept with the model.

Since 1.0. Changes the session, not a project's content. Computed at the first request with the model's permutations and kept with the session's model (and its exports): later requests give it back.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | An RDA or a CCA fitted in the session with permutations. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id. |
| `etapes` | `object` \| `null` | The steps, a table as columns: terme (the candidate with the largest adjusted R²), r2_ajuste, F, p (its test against the model without it), retenu (whether it entered); null when no step was made. |
| `retenus` | `string[]` | The terms retained, in the order they entered (always an array). |
| `r2_ajuste_tous` | `number` | The adjusted R² of all the terms. |
| `arret` | `string` | Why the selection stopped: "tous" (no candidate left), "r2" (the best candidate doesn't increase the adjusted R²), "r2_tous" (it would exceed that of all the terms) or "p" (its p is above seuil). |
| `seuil` | `number` | The test's threshold, 0.05. |
| `permutations` | `number` | The model's number of permutations. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Only for an RDA or a CCA. | modele isn't a redundancy analysis (RDA) or a canonical correspondence analysis (CCA). ([`rda_seulement`](#errors)) |
| These tests need permutations: choose a number of them (Edit model). | The RDA or CCA was fitted without permutations (its options' permutations is 0). ([`permutations_requises`](#errors)) |
| No selection: the adjusted R² of all the terms doesn't exist (too many terms for the observations). | The model's adjusted R² is missing. |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |

#### `rda_partition`

Gives the partitioning of an RDA's variation between two or three sets of X variables, the blocks of its Variables tab, computed once and kept with the model.

Since 1.0. Changes the session, not a project's content. Computed at the first request and kept with the session's model (and its exports): later requests give it back.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | An RDA fitted in the session, without covariates Z, its X in two or three blocks (the terms without a block making one). |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `modele` | `string` | The model's id. |
| `ensembles` | `object` | A table, as columns, of each set and each union of sets (vegan's varpart): ensemble ("A + B"…), ddl, R2, r2_ajuste. |
| `fractions` | `object` | A table, as columns: fraction ([a], [b]…), sens (what it is, "A \| B", "A ∩ B" or the residuals, in the request's language), r2_ajuste. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Only for an RDA or a CCA. | modele isn't a redundancy analysis (RDA) or a canonical correspondence analysis (CCA). ([`rda_seulement`](#errors)) |
| Variation partitioning is done for an RDA, not for a CCA. | modele is a CCA. |
| Variance partitioning is done without covariates Z. | The RDA has covariates Z. |
| Variance partitioning needs two or three sets of X variables: give them a block (Block column of the Variables tab). | The X variables make one set, or more than three. |

### Statistics and plots of the data

[`statistiques_variables`](#statistiques_variables) · [`correlations_variables`](#correlations_variables) · [`tests_variables`](#tests_variables) · [`test_deux_variables`](#test_deux_variables) · [`niveaux`](#niveaux) · [`baci`](#baci) · [`series`](#series) · [`rarefaction`](#rarefaction) · [`carte`](#carte) · [`autocorrelation_spatiale`](#autocorrelation_spatiale)

#### `statistiques_variables`

Gives the descriptive statistics of each variable of a project, with the log transformation it suggests for a variable skewed to the right.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |

**Result:** `object` \| `null`, with the fields below. A table, as columns: one row per variable with data, in the project's order (diagnostic.R: statistiques_variables); every statistic is null for a qualitative variable. null for a project without variables.

| Field | Type | Meaning |
|---|---|---|
| `uid` | `number[]` | The variable's number. |
| `nom` | `string[]` | Its name. |
| `qualitative` | `boolean[]` | Whether it is qualitative. |
| `categories` | `number[]` | Its number of categories (0 for a quantitative variable). |
| `N` | `number[]` | Its number of known values. |
| `manquantes` | `number[]` | Its missing values, in % of the observations. |
| `min` | `number[]` | Its smallest value. |
| `max` | `number[]` | Its largest value. |
| `min_max` | `number[]` | Min / Max (null when Max is 0). |
| `moyenne` | `number[]` | Its mean. |
| `mediane` | `number[]` | Its median. |
| `ecart_type` | `number[]` | Its standard deviation (null with one value). |
| `et_moyenne` | `number[]` | Standard deviation / mean (null when the mean is 0). |
| `asymetrie` | `number[]` | Its skewness G1, corrected for the sample's size as Excel's (3 values and a variation at least). |
| `test_asymetrie` | `number[]` | The skewness over its standard error, √(6n(n − 1) / ((n − 2)(n + 1)(n + 3))). |
| `aplatissement` | `number[]` | Its excess kurtosis G2 (4 values at least). |
| `rouge_min_max` | `boolean[]` | 0 ≤ Min/Max &lt; 0.1, which the interface shows in red. |
| `rouge_asymetrie` | `boolean[]` | \|test_asymetrie\| &gt; 2, which the interface shows in red. |
| `suggestion` | `string[]` | The transformation suggested: "log10", or "log10:&lt;C2&gt;" for log10(x + C2), when the skewness is significant to the right (test_asymetrie &gt; 2) and Min/Max without the zeros is under 0.1, with more than 4 values and more than 10 % of the observations; null otherwise. |
| `C2` | `number[]` | The constant of the suggestion: 0 when every value is positive, else half the smallest non-zero absolute value, plus that value when some are negative. |
| `entiers` | `boolean[]` | Whether every known value is a whole number (a count, a presence–absence); null for a qualitative variable. |
| `zeros` | `number[]` | The share of zeros among the known values, in %. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |

#### `correlations_variables`

Gives the correlation matrix of a project's quantitative variables, each pair on the observations where both are known, with its p-values corrected for multiple testing.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `methode` | `string` | no | The correlation. Values: `pearson` (Pearson's r.), `spearman` (Spearman's ρ.). Default: `"pearson"`. |
| `correction` | `string` | no | The correction of the p-values over the K(K − 1)/2 pairs; any other value counts as BH. Values: `BH` (Benjamini and Hochberg (1995), the false discovery rate.), `bonferroni` (Bonferroni.), `aucune` (None.). Default: `"BH"`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `uid` | `number[]` | The numbers of the project's quantitative variables, in its order: the rows and columns of the matrices. |
| `noms` | `string[]` | Their names. |
| `r` | `number[][]` | The correlations, a K × K matrix as an array of rows. |
| `n` | `number[][]` | The number of observations of each pair. |
| `p` | `number[][]` | Each pair's two-sided p-value (the t approximation, n − 2 degrees of freedom); null on the diagonal and for a pair of fewer than 3 observations. |
| `p_ajuste` | `number[][]` | The p-values corrected over the pairs; null on the diagonal. |
| `methode` | `string` | The correlation, as given. |
| `correction` | `string` | The correction, as given. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Unknown method: &lt;methode&gt; | methode is neither pearson nor spearman. |
| At least two quantitative variables are needed | The project has fewer than two quantitative variables with data. |
| The correlation matrix is limited to 300 quantitative variables (this project has &lt;K&gt;): test them two at a time. | The project has more than 300 quantitative variables (spectra). |

#### `tests_variables`

Compares every quantitative variable of a project between the classes of a qualitative variable, one test per variable, with the p-values corrected for multiple testing.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `classe` | `number` | yes | The number of the qualitative variable whose classes are compared (a variable, not "id:k"). |
| `groupes` | `string[]` | no | For a test of two classes: the classes A (the reference) and B, by their names; the first two categories by default. anova and kruskal compare every class and ignore it. |
| `test` | `string` | no | The test; t_apparie by default when appariement is given. Values: `welch` (Welch's t-test, two classes (unequal variances).), `student` (Student's t-test, two classes (equal variances).), `mann_whitney` (The Mann–Whitney test, two classes (on ranks).), `anova` (One-way ANOVA, every class.), `kruskal` (The Kruskal–Wallis test, every class (on ranks).), `t_apparie` (The paired t-test, two classes paired by appariement, on the differences B − A.), `wilcoxon` (The Wilcoxon signed-rank test, two classes paired by appariement.). Default: `"welch"`. |
| `correction` | `string` | no | The correction of the p-values over the variables; any other value counts as BH. Values: `BH` (Benjamini and Hochberg (1995), the false discovery rate.), `bonferroni` (Bonferroni.), `aucune` (None.). Default: `"BH"`. |
| `appariement` | [`Colonne`](#type-colonne) | no | For a paired test: a variable or "id:k" (not "id:1") pairing the rows of A and B that have the same value (the year, for two sectors followed the same years), three pairs at least per variable. A paired test takes it, and it takes a paired test. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `tableau` | `object` | A table, as columns, one row per quantitative variable other than the class and the key: uid, nom, statistique, p and p_ajuste (p corrected over the variables); for two classes, n_a, n_b, moyenne_a, moyenne_b, log2_rapport (the log2 of B's mean over A's, when both are positive) and d (Cohen's d on the pooled standard deviation); for a paired test, n_paires, moyenne_a and moyenne_b on the pairs, log2_rapport and d (Cohen's d_z, the mean difference over the differences' standard deviation); for anova and kruskal, a column "moyenne &lt;class&gt;" per class. statistique and p are null for a variable that couldn't be tested (fewer than 2 values in a class, fewer than 3 pairs). |
| `groupes` | `string[]` | The classes compared: A and B, or every class. |
| `test` | `string` | The test, as given. |
| `correction` | `string` | The correction, as given. |
| `classe` | `string` | The class variable's name. |
| `effectifs` | `object` | Each class's number of observations, by its name. |
| `cle` | `object` | With appariement: the key, {uid, nom, qualitative}. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Unknown identifier: &lt;id:k&gt; | "id:k" names no identifier of the observations (variables.R: colonne_ou_identifiant). ([`identifiant_inconnu`](#errors)) |
| The class variable must be qualitative | classe is a quantitative variable. |
| Two different classes are needed to compare | A test of two classes, and groupes doesn't name two different categories of the class. |
| Unknown test: &lt;test&gt; | test isn't one of the request's tests. ([`test_inconnu`](#errors)) |
| A paired test (paired t, Wilcoxon) takes a pairing key, and a pairing key a paired test | t_apparie or wilcoxon without appariement, or appariement with another test. |
| The pairing key must be another variable than the class | appariement is the class variable. |
| &lt;level&gt; has several rows for the same value of &lt;key&gt; (&lt;values&gt;): the pairing key must identify one row per level | In a paired test, one of the two levels compared has two rows with the same key (comparaisons.R: paires_cle). ([`cle_en_double`](#errors)) |
| No quantitative variable to compare | The project has no quantitative variable besides the class and the key. |

#### `test_deux_variables`

Tests two variables against each other with the tests their types call for, or with a paired test, checks the tests' conditions and says which one to read and why.

Since 1.0. Changes the session, not a project's content. A paired test between the levels of a variable that has three levels or more keeps the table of all its pairs in the session (20 tables at most, by project, data revision and language), so that choosing other levels A and B doesn't compute it again; nothing else changes.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `x` | [`Colonne`](#type-colonne) | yes | The first variable: a variable's number, or "id:k" (not "id:1"), which makes groups as a qualitative variable does. |
| `y` | [`Colonne`](#type-colonne) | yes | The second variable, likewise; not x. |
| `appariement` | [`Colonne`](#type-colonne) | no | For a paired test: with a measurement and a qualitative variable, a third variable or "id:k" (not "id:1") pairing the rows of two of its levels that have the same value (the year, for two sectors); with two measurements, "observation": the same row holds both values (before and after). |
| `niveaux` | `string[]` | no | A paired test between levels: the two levels A and B compared, by name; the first two present by default. |
| `dans` | [`Colonne`](#type-colonne) | no | A paired test between levels: a fourth variable, qualitative, or "id:k" (not "id:1") making groups of levels (the site): the table of all the pairs keeps the pairs of levels of a same group. |

**Result:** `object`, with the fields below. The fields of the kind of comparison (type) and those of its tests (comparaisons.R: test_deux_variables).

| Field | Type | Meaning |
|---|---|---|
| `x` | `object` | The first variable, {uid, nom, qualitative}. |
| `y` | `object` | The second variable, {uid, nom, qualitative}. |
| `type` | `string` | "contingence" (two qualitative variables), "groupes" (a measurement between groups), "correlation" (two measurements) or "apparie" (a paired test). |
| `n` | `integer` | The observations where both are known; for a paired test, the pairs. |
| `manquantes` | `integer` | The observations left out for a missing value; for a paired test between levels, the rows of A and B without a value or a key. |
| `tests` | `object` | A table of the tests, as columns: test, statistique, ddl1, ddl2, p, effet and nom_effet; khi2 (V) and, when Cochran's rule fails or for a 2 × 2 table, fisher (OR for 2 × 2); welch (d) and mann_whitney (r_rb) for two groups; anova (eta2), welch_anova and kruskal (epsilon2) for more; pearson (r) and spearman (rho); t_apparie (dz) and wilcoxon (r_rb). |
| `conseille` | `string` | The test to read, one of tests' test. |
| `raison` | `string` | Why: "" (its conditions hold), "attendus" (expected counts too small for the chi-square), "normalite" (a group's or the differences' normality is doubtful, Shapiro–Wilk p &lt; 0.05), "variances" (the variances differ, Brown–Forsythe p &lt; 0.05), and for a paired test "nombre" (over 5,000 pairs, normality not tested), "constantes" (constant differences: no t-test) or "non_testee" (Shapiro–Wilk couldn't run). |
| `phrase` | `string` | The advised test's result in a sentence for a report, in the request's language; absent when erreur is given. |
| `lignes` | `string[]` | contingence: the categories of x present. |
| `colonnes` | `string[]` | contingence: the categories of y present. |
| `effectifs` | `number[][]` | contingence: the counts, an array of rows. |
| `attendus` | `number[][]` | contingence: the expected counts. |
| `residus` | `number[][]` | contingence: the adjusted residuals (Agresti 2007). |
| `part_attendus_faibles` | `number` | contingence: the share of expected counts under 5. |
| `attendu_min` | `number` | contingence: the smallest expected count. |
| `cochran` | `boolean` | contingence: whether Cochran's rule holds (no expected count under 1, at most 20 % under 5). |
| `fisher_simule` | `boolean` | contingence: whether Fisher's p was simulated (100,000 tables, beyond 2 × 2 and 200 observations). |
| `ic_cotes` | `number[]` \| `null` | contingence: the 95 % interval of the odds ratio of a 2 × 2 table; null otherwise. |
| `groupes` | `object` | groupes: a table of the groups of 2 observations or more, as columns: groupe, n, moyenne, ecart_type, mediane, q1, q3, shapiro_p. |
| `ecartes` | `string[]` | groupes: the groups left out, with fewer than 2 observations. |
| `variances` | `object` | groupes: Brown and Forsythe's test of the variances, {statistique, p}. |
| `valeurs` | `object` | The values plotted, as columns: groupes {groupe, valeur, nom}; correlation {x, y, nom}. |
| `difference` | `number[]` | Two groups: B − A's mean difference and Welch's 95 % interval; a paired test: the mean difference B − A and its 95 % interval (null bounds for constant differences). |
| `posthoc` | `object` | groupes, 3 to 50 groups: the pairwise comparisons {anova: Tukey's, welch_anova: Games and Howell's, kruskal: Dunn's}, each a table: a, b, difference, bas, haut, p (Dunn's: a, b, z, p, p_ajuste by Holm). |
| `note_comparaisons` | `string` | Why there are no pairwise comparisons: more than 50 groups or levels, or no group holding two levels; in the request's language. |
| `ic` | `number[]` \| `null` | correlation: the 95 % interval of Pearson's r. |
| `normalite` | `number[]` | correlation: Shapiro–Wilk's p of x and of y (null beyond 5,000 observations or without variation). |
| `ordonnee` | `number` | correlation: the intercept of the least-squares line of y on x. |
| `pente` | `number` | correlation: its slope. |
| `ligne` | `boolean` | apparie: true for two measurements paired by the observation, false for levels paired by a key. |
| `a` | `string` | apparie: the level A, or the first measurement's name. |
| `b` | `string` | apparie: the level B, or the second measurement's name. |
| `cle` | `object` | apparie: the key, {uid, nom, qualitative}; uid "observation" for two measurements of a row. |
| `paires` | `object` | apparie: the pairs, as columns: cle, a, b, difference (b − a), nom_a, nom_b (the observations' names). |
| `sans_partenaire` | `object` | apparie: the keys of A and of B without a partner, {a, b}. |
| `mediane` | `number` | apparie: the median difference. |
| `shapiro_p` | `number` \| `null` | apparie: Shapiro–Wilk's p of the differences. |
| `exact` | `boolean` | apparie: whether Wilcoxon's p is exact (under 50 differences, none zero, no ties). |
| `toutes` | `object` | apparie, three levels or more: every pair of levels (or of the levels of a same group, with dans), as columns: groupe (with dans), a, b, n, difference, bas, haut, p_t, mediane, p_w, non_teste (why a pair isn't tested, "" otherwise), p_t_holm, p_w_holm. |
| `friedman` | `object` | apparie, three levels or more: Friedman's test of every level (one per group with dans), as columns: groupe (with dans), k, n, laissees, statistique, ddl, p, W (Kendall's), non_teste, p_holm, phrase. |
| `sans_groupe` | `string[]` | apparie with dans: the levels whose rows have no group. |
| `dans` | `object` | apparie with dans: the group variable, {uid, nom, qualitative}. |
| `erreur` | `string` | apparie, three levels or more: why the pair A–B can't be tested (fewer than three shared keys…), the table of all the pairs being given all the same; the other fields of the pair are then absent. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Unknown identifier: &lt;id:k&gt; | "id:k" names no identifier of the observations (variables.R: colonne_ou_identifiant). ([`identifiant_inconnu`](#errors)) |
| Choose two different variables | x and y are the same. |
| Each variable needs at least two categories in these observations | Two qualitative variables, one of which has a single category where both are known. |
| At least two groups of two observations or more are needed | A measurement between groups, with fewer than two groups of two observations. |
| At least three observations with both variables known are needed | Two measurements known together on fewer than three observations. |
| “&lt;variable&gt;” has the same value for the &lt;n&gt; observations where both variables are known: no test can be computed. | Without appariement, a quantitative x or y is constant on the three observations or more where both are known. |
| “&lt;variable&gt;” has values too large for the computation&lt; (up to &lt;value&gt;)&gt;: their squares exceed the numbers R can hold. Divide it by a power of 10 (Edit › New variable). | Without appariement, a quantitative x or y has a value beyond 1e150 where both are known (three observations or more). |
| “&lt;variable&gt;” has values too small for the computation (at most &lt;value&gt;): their squares vanish. Multiply it by a power of 10 (Edit › New variable). | Without appariement, every non-zero value of a quantitative x or y is under 1e-150 where both are known (three observations or more). |
| A paired test compares a measurement between two levels of a qualitative variable, or two measurements of the same observation | appariement with two qualitative variables. |
| Two measurements are paired by the observation: the same row holds both values | Two measurements, and appariement isn't "observation". |
| At least three pairs are needed: only &lt;n&gt; observation(s) have both values | Two measurements paired by the observation, known together on fewer than three rows. |
| A measurement and groups are paired by a third variable (the year, for two sectors) | appariement is "observation" for a measurement and a qualitative variable. |
| The pairing key must be a third variable | appariement is x or y. |
| The group of the pairs must be a fourth variable | dans is x, y or appariement. |
| Pairs are grouped by a qualitative variable or an ID | dans is a quantitative variable. |
| Two different levels are needed to compare | niveaux doesn't name two different levels present. |
| At least three pairs are needed: &lt;A&gt; and &lt;B&gt; share only &lt;n&gt; value(s) of &lt;key&gt; | The levels A and B share fewer than three keys, and there is no table of all the pairs (two levels, or more than 50); when there is one, this comes back in erreur. |
| &lt;level&gt; has several rows for the same value of &lt;key&gt; (&lt;values&gt;): the pairing key must identify one row per level | In a paired test, one of the two levels compared has two rows with the same key (comparaisons.R: paires_cle). ([`cle_en_double`](#errors)) |
| &lt;level&gt; has rows for several values of &lt;group&gt; (&lt;values&gt;): a level must belong to a single group | With dans, a level's rows are in several groups. |
| &lt;A&gt; and &lt;B&gt; have the same value in every pair: nothing to compare | Every difference of the pairs is zero. |

#### `niveaux`

Gives the levels that the observations have of a qualitative variable or an identifier, in the order of the tests, which a paired test offers for A and B.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `variable` | [`Colonne`](#type-colonne) | yes | A qualitative variable's number, or "id:k" (not "id:1"). |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `niveaux` | `string[]` | The levels with at least one observation (always an array): a variable's categories in their order, an identifier's values in the order of numbers when they are numbers, else of text. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Unknown identifier: &lt;id:k&gt; | "id:k" names no identifier of the observations (variables.R: colonne_ou_identifiant). ([`identifiant_inconnu`](#errors)) |
| This variable is not qualitative | variable is a quantitative variable. |

#### `baci`

Gives the series of the gaps between the impact and the control units' means over time and, with an event, tests the gaps after it against those before (before–after control–impact, BACI).

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `y` | [`Colonne`](#type-colonne) | yes | The response: a quantitative variable's number. |
| `temps` | [`Colonne`](#type-colonne) | yes | The time: a variable's number or "id:k" (not "id:1"), made of numbers (the year) or of dates, read as decimal years. |
| `unite` | [`Colonne`](#type-colonne) | yes | The unit (the sector): a qualitative variable's number or "id:k" (not "id:1"). |
| `impact` | `string` \| `string[]` | yes | The impact units: levels of unite. |
| `temoins` | `string` \| `string[]` | yes | The control units: other levels of unite. |
| `evenement` | `number` \| `string` | no | The first time counted after: a number for a time made of numbers; a number (its 1 January) or a date for a time made of dates. Without it, the series of the gaps only. |
| `fin` | `number` \| `string` | no | With evenement: the last time of the after period, included (later times stay out of the test): a number, or a date for a time made of dates. |
| `test` | `string` | no | The test of the gaps after against those before. Values: `welch` (Welch's t-test.), `student` (Student's t-test (equal variances: lm(gap \~ period)).), `mann_whitney` (The Mann–Whitney test (on ranks).). Default: `"welch"`. |

**Result:** `object`, with the fields below. comparaisons.R: baci; the fields from periodes on come only with evenement.

| Field | Type | Meaning |
|---|---|---|
| `type` | `string` | "baci". |
| `y` | `object` | The response, {uid, nom}. |
| `temps` | `object` | The time, {uid, nom}. |
| `dates` | `boolean` | Whether the time is made of dates: times are then decimal years. |
| `unite` | `object` | The unit, {uid, nom}. |
| `impact` | `string[]` | The impact units, without repeats (a single string for one unit). |
| `temoins` | `string[]` | The control units, without repeats (a single string for one unit). |
| `serie` | `object` | A table, as columns, of the times where the impact and the controls both have values: temps, impact and temoins (the means of their rows), n_impact, n_temoins, ecart (impact − temoins) and, with evenement, periode ("avant", "apres", or "hors" after fin). |
| `seuls` | `object` | The times where only the impact, or only the controls, have values: {impact, temoins}. |
| `manquantes` | `integer` | The rows of impact or control units without a value or a time. |
| `repetees` | `object` \| `null` | A table of the units that have several rows at a time, as columns: unite, temps (at how many times); null when none has. |
| `periodes` | `object` | A table of the periods avant and apres, as columns: periode, n, debut, fin, moyenne, ecart_type, mediane, shapiro_p (of their gaps). |
| `tests` | `object` | A table of the three tests, as columns: test (welch, student, mann_whitney), statistique, ddl1, ddl2, p, effet, nom_effet (d, d, r_rb), bas, haut (the 95 % interval of the change). |
| `difference` | `number[]` | The mean gap after minus before, and its 95 % interval (Student's for student, else Welch's). |
| `evenement` | `number` | The event, as a number (a decimal year for dates). |
| `fin` | `number` \| `null` | The end of the after period, or null. |
| `hors` | `integer` | The times after fin, left out of the test. |
| `temps_unites` | `integer` | The smallest number of times of an impact or control unit, up to fin (with three or more, the view's mixed model takes CAR1 by default). |
| `repetees_modele` | `object` \| `null` | repetees among the rows the mixed model would take (up to fin): CAR1 needs one time per unit. |
| `test` | `string` | The test chosen. |
| `phrase` | `string` | The chosen test's result in a sentence for a report, in the request's language. |
| `modeles` | `object[]` | The BACI mixed models of the same settings (with or without CAR1) fitted in the session (baci_modele), oldest first, each {id, nom, car1, estimation, erreur_type, ddl, t, p, bas, haut, phrase}: the period × impact coefficient, the BACI effect (always an array). |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Unknown identifier: &lt;id:k&gt; | "id:k" names no identifier of the observations (variables.R: colonne_ou_identifiant). ([`identifiant_inconnu`](#errors)) |
| The response, the time and the unit must be three different variables | Two of y, temps and unite are the same. |
| Unknown test: &lt;test&gt; | test isn't one of the request's tests. ([`test_inconnu`](#errors)) |
| The response must be a quantitative variable | y is qualitative, or an identifier. |
| The time (&lt;time&gt;) must be numbers (the year) or dates (2019-05-14, 14/05/2019)&lt;, not both&gt;: &lt;why&gt; | A value of the time is neither a number nor a date, or numbers and dates are mixed (variables.R: temps_en_nombres, refus_temps); &lt;why&gt; names the first value refused: “&lt;value&gt;” is a number among dates, a date among numbers, isn't a date that exists, or is neither a number nor a date. ([`temps_refuse`](#errors)) |
| The unit must be a qualitative variable or an ID | unite is quantitative. |
| Choose at least one impact unit and one control unit | impact or temoins is empty. |
| &lt;unit&gt; has no level &lt;levels&gt; | impact or temoins names a level the unit doesn't have. |
| A unit can't be both impact and control: &lt;units&gt; | A level is in both impact and temoins. |
| “&lt;y&gt;” has values too large for the computation&lt; (up to &lt;value&gt;)&gt;: their squares exceed the numbers R can hold. Divide it by a power of 10 (Edit › New variable). | A value of y of the impact and control units' rows with a time is beyond 1e150. |
| “&lt;y&gt;” has values too small for the computation (at most &lt;value&gt;): their squares vanish. Multiply it by a power of 10 (Edit › New variable). | Every non-zero value of y of the impact and control units' rows with a time is under 1e-150. |
| No value of &lt;time&gt; where both the impact and the controls have a value of &lt;y&gt; | The impact and the controls never have values at the same time. |
| The event time must be a number or a date | evenement is neither. |
| The end of the after period must be a number or a date | fin is neither. |
| The time (&lt;time&gt;) is made of numbers: enter the event as a number (2008), not a date | A date as evenement for a time made of numbers (likewise for fin: enter the end as a number (2012), not a date). |
| The time (&lt;time&gt;) is made of dates: enter the end as a date (31/12/2012); a year alone would be its 1 January | A number as fin for a time made of dates. |
| The end of the after period (&lt;end&gt;) comes before the event (&lt;event&gt;) | fin &lt; evenement. |
| At least two values of &lt;time&gt; are needed in each period, before and after &lt;event&gt;: there are &lt;n&gt; before and &lt;n&gt; after | A period has fewer than two times with a gap. |
| The gap is the same at every time: nothing to compare | Every gap of the two periods is equal. |

#### `series`

Gives a variable's values over time in long format, by line and panel, with each panel's mean at each time and, with tendances, each line's trend test.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `variable` | [`Colonne`](#type-colonne) | yes | A quantitative variable's number. |
| `temps` | [`Colonne`](#type-colonne) | yes | The time: a variable's number or "id:k" (not "id:1"), made of numbers (the year) or of dates, read as decimal years. |
| `lignes` | [`Colonne`](#type-colonne) | no | What makes the lines (the sector): a qualitative variable's number or "id:k" (not "id:1"); absent or "": one line. |
| `panneaux` | [`Colonne`](#type-colonne) | no | What makes the panels (the site), likewise; absent or "": one panel. |
| `tendances` | `boolean` | no | true for each line's trend: the Mann–Kendall test and Sen's slope, or the seasonal Kendall test with saisons. Default: `false`. |
| `correction` | `string` | no | With tendances: the correction of the p-values over the lines tested; another value comes back as tendances_erreur. Values: `BH` (Benjamini and Hochberg (1995).), `holm` (Holm (1979).). Default: `"BH"`. |
| `autocorrelation` | `boolean` | no | With tendances: Hamed and Rao's (1998) correction of the Mann–Kendall test for autocorrelation (not with saisons). Default: `false`. |
| `saisons` | [`Colonne`](#type-colonne) | no | With tendances only: the seasonal Kendall test, the season of each point being "mois" (the month of a time made of dates), a qualitative variable's number or "id:k" (not "id:1"); absent or "": none. |

**Result:** `object`, with the fields below. series.R: series_variable, and tendances_series with tendances.

| Field | Type | Meaning |
|---|---|---|
| `variable` | `object` | The variable, {uid, nom}. |
| `temps` | `object` | The time, {uid, nom}. |
| `dates` | `boolean` | Whether the time is made of dates: the times are then decimal years (2019.37), which the interface shows as dates. |
| `lignes` | `object` \| `null` | What makes the lines, {uid, nom}; null for one line. |
| `panneaux` | `object` \| `null` | What makes the panels, {uid, nom}; null for one panel. |
| `points` | `object` | A table, as columns, of the observations whose time and value are known (and their line and panel), sorted by panel, line and time: temps, valeur, ligne, panneau (null without lines or panels), uid, nom, and saison with saisons. |
| `niveaux_lignes` | `string[]` | The lines' levels, in their order (always an array, empty for one line). |
| `niveaux_panneaux` | `string[]` | The panels' levels, in their order (always an array). |
| `moyennes` | `object` | A table, as columns, of the mean of each panel's lines at each time: panneau, temps, valeur, n. |
| `sans_temps_valeur` | `integer` | The observations left out for a missing time or value. |
| `sans_groupe` | `integer` | Those left out for a missing line or panel. |
| `doubles` | `object` | The times that come twice in a line (not a series: finer lines needed): {n}, and with n &gt; 0 the first one's ligne, panneau and temps. |
| `saisons` | `object` | With tendances and saisons: what makes the seasons, {uid, nom} (uid "mois" for the months). |
| `sans_saison` | `integer` | With tendances and saisons: the points without a season, left out of the test. |
| `tendances` | `object` | With tendances: a table, as columns, one row per line of each panel: panneau, ligne, n, debut, fin, test (mann_kendall, hamed_rao or saisonnier), pente (Sen's, per unit of time, per year for dates), bas and haut (its 95 % interval), tau, S, var_S, p, n_n_etoile (Hamed and Rao's n/n\*), saisons and medianes (the seasonal test's), non_teste (why a line isn't tested, "" otherwise: fewer than 4 times…), p_ajuste; an empty object when no point is left. |
| `tendances_erreur` | `string` | With tendances: why the trends' options were refused, the series coming without trends (a status 200): Unknown correction: &lt;correction&gt;; Seasons by month of the year need a time made of dates; Seasons are made by a qualitative variable or an ID; The seasonal test takes the seasons as independent: Hamed and Rao's correction for autocorrelation doesn't apply to it; or a seasons variable not found. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Unknown identifier: &lt;id:k&gt; | "id:k" names no identifier of the observations (variables.R: colonne_ou_identifiant). ([`identifiant_inconnu`](#errors)) |
| Choose a quantitative variable | variable is qualitative, or an identifier. |
| The time (&lt;time&gt;) must be numbers (the year) or dates (2019-05-14, 14/05/2019)&lt;, not both&gt;: &lt;why&gt; | A value of the time is neither a number nor a date, or numbers and dates are mixed (variables.R: temps_en_nombres, refus_temps); &lt;why&gt; names the first value refused: “&lt;value&gt;” is a number among dates, a date among numbers, isn't a date that exists, or is neither a number nor a date. ([`temps_refuse`](#errors)) |
| Lines and panels are made by a qualitative variable or an ID | lignes or panneaux is a quantitative variable. |

#### `rarefaction`

Gives the rarefaction of each observation's species counts, the species accumulation over the observations and the richness estimators, overall or by group.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `especes` | `number[]` | yes | The species: the numbers of two quantitative variables or more, counts of individuals (one row per observation). |
| `groupe` | [`Colonne`](#type-colonne) | no | A qualitative variable's number or "id:k" (not "id:1"): an accumulation curve and estimators per group. |
| `taille` | `number` | no | The common size the richness is rarefied to, rounded; the smallest total of an observation by default (also for a value under 1). |
| `pas` | `integer` | no | The step of the rarefaction curves; by default the largest total over 100, rounded up (about 100 points a curve). |

**Result:** `object`, with the fields below. rarefaction.R: rarefaction_projet, as vegan's rarefy, rarecurve, specaccum (exact), estimateR and specpool compute them.

| Field | Type | Meaning |
|---|---|---|
| `especes` | `string[]` | The species' names (always an array). |
| `n` | `integer` | The observations kept: every count known (and the group, with groupe). |
| `entiers` | `boolean` | Whether the counts are whole numbers (to 1e-8): the rarefaction, Chao1 and ACE need them (null in the tables, and courbes null, otherwise). |
| `taille` | `number` | The common size of the rarefaction. |
| `taille_min` | `number` | The smallest total of an observation with individuals (the default size). |
| `pas` | `integer` | The step of the curves. |
| `manquantes` | `integer` | The observations left out for a missing count. |
| `sans_groupe` | `integer` | Those left out for a missing group. |
| `vides` | `integer` | The observations without any individual: no rarefaction, but they count in the accumulation. |
| `petites` | `integer` | The observations whose total is under taille: no rarefied richness. |
| `groupe` | `object` \| `null` | What makes the groups, {uid, nom}; null without groupe. |
| `niveaux` | `string[]` | The groups (always an array, empty without groupe). |
| `observations` | `object` | A table, as columns, one row per observation kept: uid, nom, groupe, N (individuals), S (species), Sm and se (the richness rarefied to taille and its standard deviation), chao1, se_chao1, ace, se_ace (Chao1 corrected for bias, ACE). |
| `courbes` | `object` \| `null` | The rarefaction curves of the observations with individuals, a table as columns: uid, taille (1, 1 + pas… and their total), S, se, and \_row (row names jsonlite writes, without meaning); null without whole counts. |
| `accumulation` | `object` | The exact species accumulation over 1 to n observations (per group), a table as columns: groupe, sites, richesse, et (its standard deviation). |
| `estimateurs` | `object` | The estimators of the richness from the presences, a table as columns, one row per group (one row without groupe): groupe, n, S, chao, se_chao, jack1, se_jack1, jack2, boot, se_boot. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| Unknown identifier: &lt;id:k&gt; | "id:k" names no identifier of the observations (variables.R: colonne_ou_identifiant). ([`identifiant_inconnu`](#errors)) |
| Choose at least two species (columns of counts). | especes has fewer than two variables. |
| The species of the rarefaction: quantitative variables only ("&lt;name&gt;" isn't one). | A species is a qualitative variable. |
| Rarefaction needs counts that are positive or zero. | A count is negative. |
| Groups are made by a qualitative variable or an ID. | groupe is a quantitative variable. |
| Too few observations have all their values. | Fewer than two observations have every count (and a group). |

#### `carte`

Gives each observation's coordinates and their WGS 84 longitude and latitude, for the map (Plots › Map).

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id; its coordinates must be set. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `uid` | `number[]` | The observations' numbers, all of them in the data's order. |
| `nom` | `string[]` | Their names. |
| `x` | `number[]` | Their x in the project's system (null where missing). |
| `y` | `number[]` | Their y (null where missing). |
| `lon` | `number[]` | Their WGS 84 longitude; null for plane coordinates ("plan") and missing values. |
| `lat` | `number[]` | Their WGS 84 latitude, likewise. |
| `coordonnees` | [`Coordonnees`](#type-coordonnees) | The coordinates set. |
| `systeme` | `object` | {code: the EPSG code or "plan"; nom: its name, in the request's language for "plan"; type: lonlat, lcc, tm, laea, merc or plan}. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| This project has no coordinates: choose them in Plots › Map (two variables and their system). | The request needs the observations' coordinates and the project has none set, or they no longer hold: a variable gone or made qualitative (spatial.R: coordonnees_projet with exiger). ([`sans_coordonnees`](#errors)) |

#### `autocorrelation_spatiale`

Measures the spatial autocorrelation of a variable or of a regression's residuals with Moran's I, its correlogram and the semivariogram (Plots › Spatial autocorrelation).

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | no | A fitted model's id: its residuals, or with variable that variable on the model's observations; its project is the one used. |
| `projet` | `string` | no | Without modele, the project's id (its coordinates must be set). |
| `variable` | `number` | no | The number of a quantitative variable; without it, the regression's residuals (modele needed). |
| `residus` | `string` | no | Without variable, which residuals. Values: `standardises` (Those of the model's views: standardized, deviance residuals for mgcv, normalized with CAR1.), `quantiles` (The randomized quantile residuals of a count, presence-absence, positive or proportion model.). Default: `"standardises"`. |
| `voisinage` | `object` | no | The spatial weights (spatial.R: poids_spatiaux), {methode: voisins (the k nearest neighbours, made mutual; the default), bande (1 up to portee, by default the smallest distance that gives each observation a neighbour) or inverse (1/d, up to portee when given); k: 4 by default, kept between 1 and n − 1; portee: in metres; standardiser: false keeps the weights as they are, otherwise each row sums to 1}. |
| `permutations` | `integer` | no | The permutations of Moran's test, from a fixed seed (kept within 0 to 9999; 0: none). From 0 to 9999. Default: `999`. |
| `classes` | `integer` | no | The correlogram's distance classes, of equal width up to half the largest distance (kept within 2 to 30). From 2 to 30. Default: `10`. |
| `par_lieu` | `boolean` | no | Replace the values of the observations at the same place by their mean, one per place; otherwise their zero distances make them nearest neighbours. Default: `false`. |

**Result:** `object`, with the fields below. Distances are in metres: along great circles for longitudes and latitudes, straight for plane coordinates.

| Field | Type | Meaning |
|---|---|---|
| `nom` | `string` | The variable's name, or residuals or quantile residuals, in the request's language. |
| `n` | `integer` | The values tested (the places, with par_lieu). |
| `observations` | `integer` | The observations with coordinates and a value. |
| `laissees` | `integer` | Those left out, without coordinates or a value. |
| `uid` | `number[]` | The observations tested (with par_lieu, the first of each place). |
| `valeurs` | `number[]` | Their values (with par_lieu, each place's mean). |
| `nulles` | `integer` | The pairs of observations at a zero distance. |
| `lieux` | `number[]` \| `null` | With par_lieu, the number of observations at each place; null otherwise. |
| `voisinage` | `object` | The weights used: {methode, k (null but for voisins), portee (the distance used; null for voisins, and for inverse without one), standardiser, voisins: the mean number of neighbours, sans_voisin: the observations without one}. |
| `moran` | `object` | Moran's I under randomization, as spdep::moran.test: {I, esperance, variance, z, p (one-sided, for a positive autocorrelation), n, S0}, with permutations {permutations, p_permutations, as spdep::moran.mc}; for the residuals of a linear model without random effects, modele, the test that accounts for its fit, as spdep::lm.morantest {I, esperance, variance, z, p, n, rang}. |
| `correlogramme` | `object` | A table (columns): classe, de, a (its bounds), distance (the mean of its pairs), paires, I, esperance, z, p (two-sided) and p_holm (corrected over the classes); null values for a class without pairs. |
| `variogramme` | `object` | The empirical semivariogram, as gstat::variogram (15 classes up to a third of the points' bounding diagonal), a table (columns): classe, de, a, paires, distance, gamma; only the classes with pairs. |
| `distance_max` | `number` | The largest distance between two observations tested. |
| `systeme` | `integer` \| `string` | The coordinates' system: an EPSG code or "plan". |

**Errors:**

| Message (in English) | When |
|---|---|
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Variable not found: &lt;number&gt; | A variable's number isn't one of the project's (nouveau.R: variable_du_projet). ([`variable_introuvable`](#errors)) |
| This project has no coordinates: choose them in Plots › Map (two variables and their system). | The request needs the observations' coordinates and the project has none set, or they no longer hold: a variable gone or made qualitative (spatial.R: coordonnees_projet with exiger). ([`sans_coordonnees`](#errors)) |
| Choose a quantitative variable. | variable is qualitative. |
| Residuals are computed here only for a regression: choose a variable. | Without variable, modele is missing or isn't a regression. |
| This model has no quantile residuals (only counts, presence-absence, positive values and proportions). | residus is quantiles for a model without them. |
| At least 5 observations with coordinates and a value are needed. | Fewer than 5 observations have both. |
| At least 5 different places are needed. | With par_lieu, fewer than 5 places. |
| Some observations are at the same place: the inverse of their zero distance doesn't exist. Tick “Mean by place”, or choose another neighbourhood. | voisinage inverse with observations at the same place. |
| No pair of neighbouring observations: widen the neighbourhood. | The weights make no pair of neighbours. |
| The variable is constant: no autocorrelation to measure. | The values don't vary. |
| "&lt;methode&gt;" isn't a choice of neighbourhood: voisins, bande, inverse. | voisinage's methode is another word. |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |

### Exports

[`exporter`](#exporter) · [`exporter_donnees`](#exporter_donnees) · [`exporter_prediction`](#exporter_prediction) · [`exporter_coefficients`](#exporter_coefficients) · [`script_r`](#script_r) · [`paquet`](#paquet)

#### `exporter`

Writes a fitted model's results as tables, in one file or one file per table.

Since 1.0. Changes nothing. Writes the files.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id. |
| `chemin` | `string` | no | The file to write, its extension its format; without it, a file named after the model in a new folder of the engine's temporary folder (for /api/fichier). |
| `format` | `string` | no | Without chemin: the format. Excel gives a sheet per table, R a list, the others a file per table beside the first ("M1 - variables.csv"); a GeoPackage places the observations as points at the project's coordinates. Values: `xlsx` (Excel), `csv` (CSV), `tsv` (tab-separated text), `txt` (tab-separated text), `rds` (R), `rdata` (R), `rda` (R), `sav` (SPSS), `dta` (Stata), `xpt` (SAS transport), `omv` (jamovi), `dbf` (dBase), `arff` (Weka), `gpkg` (GeoPackage). Default: `"xlsx"`. |

**Result:** [`Fichiers`](#type-fichiers). The tables are named, and their columns, in the request's language (X-Langue: en translates the engine's names).

**Errors:**

| Message (in English) | When |
|---|---|
| Which model? The request names none (modele). | modele is left out, null or "" (serveur.R: modele). ([`modele_manquant`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Unrecognised export format: .&lt;extension&gt; | The extension of chemin (or format) isn't one the engine writes: xlsx, csv, tsv, txt, rds, rdata, rda, sav, dta, xpt, omv, dbf, arff (formats.R: ecrire_tableau). ([`format_export_inconnu`](#errors)) |
| This project has no coordinates: choose them in Plots › Map (two variables and their system). | The request needs the observations' coordinates and the project has none set, or they no longer hold: a variable gone or made qualitative (spatial.R: coordonnees_projet with exiger). ([`sans_coordonnees`](#errors)) |
| This model has no table of observations to place on a map. | A GeoPackage of a model whose results have no observations table. |
| The model's observations can't be found in the project: some names repeat. | A GeoPackage, when the project's observation names repeat and the model's rows can't be matched. |

#### `exporter_donnees`

Writes a project's data table in a file, with its variables' units and descriptions where the format has room for them.

Since 1.0. Changes nothing. Writes the file.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `chemin` | `string` | no | The file to write, its extension its format; without it, a file named after the project in a new folder of the engine's temporary folder. |
| `format` | `string` | no | Without chemin: the format, as exporter's (xlsx, csv, tsv, txt, rds, rdata, rda, sav, dta, xpt, omv, dbf, arff, gpkg). The units and descriptions go to a Variables sheet in Excel, a Variables table in a GeoPackage, jamovi's descriptions and R's label and units attributes. Default: `"xlsx"`. |

**Result:** [`Fichiers`](#type-fichiers).

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Unrecognised export format: .&lt;extension&gt; | The extension of chemin (or format) isn't one the engine writes: xlsx, csv, tsv, txt, rds, rdata, rda, sav, dta, xpt, omv, dbf, arff (formats.R: ecrire_tableau). ([`format_export_inconnu`](#errors)) |
| This project has no coordinates: choose them in Plots › Map (two variables and their system). | The request needs the observations' coordinates and the project has none set, or they no longer hold: a variable gone or made qualitative (spatial.R: coordonnees_projet with exiger). ([`sans_coordonnees`](#errors)) |

#### `exporter_prediction`

Writes the last predictions of a model (predire or predire_fichier) in a file.

Since 1.0. Changes nothing. Writes the file.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | yes | The fitted model's id. |
| `chemin` | `string` | no | The file to write; without it, "predictions &lt;model&gt;" in a new folder of the engine's temporary folder. |
| `format` | `string` | no | Without chemin: the format, as exporter_donnees' but GeoPackage (xlsx, csv, tsv, txt, rds, rdata, rda, sav, dta, xpt, omv, dbf, arff). Default: `"xlsx"`. |

**Result:** [`Fichiers`](#type-fichiers). One table: Observation, then the predictions' columns as predire gives them, named in the request's language as exporter's (X-Langue: en translates the engine's names; the variables' and categories' names stay).

**Errors:**

| Message (in English) | When |
|---|---|
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Unrecognised export format: .&lt;extension&gt; | The extension of chemin (or format) isn't one the engine writes: xlsx, csv, tsv, txt, rds, rdata, rda, sav, dta, xpt, omv, dbf, arff (formats.R: ecrire_tableau). ([`format_export_inconnu`](#errors)) |
| No prediction to export for this model | Neither predire nor predire_fichier was asked for this model in the session. |

#### `exporter_coefficients`

Writes several regressions' coefficients side by side to files, as the table is copied and in numbers.

Since 1.0. Changes nothing. Writes files; nothing in the session changes.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modeles` | `string[]` | yes | The fitted regressions' ids, in the order of the columns. |
| `erreur` | `string` | no | What follows each estimate in the copied table; any other value is ic. Values: `ic` (The estimate [its 95 % interval]), `et` (The estimate (its standard error)). Default: `"ic"`. |
| `chemin` | `string` | no | The file to write, its extension choosing the format; without it, coefficients.&lt;format&gt; in a folder of the engine's temporary folder, to fetch with /api/fichier. |
| `format` | `string` | no | Without chemin, the format by its extension: xlsx, csv, tsv, txt, rds, rdata, rda, sav, dta, xpt, omv, dbf or arff. Default: `"xlsx"`. |

**Result:** [`Fichiers`](#type-fichiers). Two tables, cote_a_cote (the cells as copied, with the p's stars, then the legend) and coefficients (each model's coefficients in numbers: modele, terme, estimation, erreur_type, ddl, t, p, bas, haut): two sheets of one Excel workbook, a named list in an R file, else two files, the second named "&lt;file&gt; - coefficients.&lt;extension&gt;".

**Errors:**

| Message (in English) | When |
|---|---|
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Unrecognised export format: .&lt;extension&gt; | The extension of chemin (or format) isn't one the engine writes: xlsx, csv, tsv, txt, rds, rdata, rda, sav, dta, xpt, omv, dbf, arff (formats.R: ecrire_tableau). ([`format_export_inconnu`](#errors)) |
| Tick at least one regression. | modeles is empty. |
| Only regressions (LM, LMM, GLM, GAM) go side by side. | A model isn't a regression. |

#### `script_r`

Exports a fitted model, or several of the same project, to a standalone R script or an R Markdown or Quarto report.

Since 1.0. Changes nothing. Writes the script and the files beside it; the script carries a licence notice (it may be used, changed and shared on any terms, an exception to Nuée's GPL) and how to cite Nuée.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `modele` | `string` | no | The fitted model's id (or modeles); written on the project as it saw it, its categories in their order at its fit, with the definition of that fit. |
| `modeles` | `string[]` | no | Several fitted models of one project: the session in one .R script, the other analyses left aside (they are exported one by one). |
| `chemin` | `string` | no | The script to write, .R, .Rmd or .qmd (any other extension is refused), its extension its format; without it, a file named after the model (the session: the project's source) in a new folder of the engine's temporary folder. |
| `format` | `string` | no | Without chemin, for one model: the format; several models are always a script. Values: `R` (a script), `Rmd` (an R Markdown report), `qmd` (a Quarto report). Default: `"R"`. |
| `donnees` | `string` | no | Where the data go. Values: `script` (in the script), `csv` (in a CSV beside it). Default: `"script"`. |
| `renv` | `boolean` | no | Also write renv.lock (R's and the packages' versions) beside it. Default: `false`. |
| `rdata` | `boolean` | no | Also write the data and the fitted model in a .RData beside it. Default: `false`. |
| `rproj` | `boolean` | no | Also write an RStudio project (.Rproj) beside it. Default: `false`. |
| `version` | `string` | no | Nuée's version, written in the script's header. Default: `""`. |
| `langue` | `string` | no | The language of the script's comments, names and messages (not X-Langue's). Values: `fr` (French), `en` (English). Default: `"fr"`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `script` | `string` | The script's path. |
| `chemins` | `string[]` | Every file written: the script, and its CSV, .RData, renv.lock and .Rproj when asked (a single path comes as a string). |
| `ecart` | `string` \| `null` | What differs between the script's results, run here without the engine before it is written, and the model's; null when they agree. |

**Errors:**

| Message (in English) | When |
|---|---|
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| &lt;variable&gt; is called &lt;name&gt; since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a variable it used was renamed since its fit; one deleted gets “&lt;variable&gt; is no longer in the data since &lt;model&gt; was fitted: it can't be fitted again without it (Ctrl+Z brings it back).”, one turned into categories or numbers “&lt;variable&gt; is &lt;categories\|numbers&gt; since &lt;model&gt; was fitted: fit it again (…)” (atelier.R: variables_changees). ([`variables_changees`](#errors)) |
| The categories of &lt;variable&gt; changed since &lt;model&gt; was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer). | The request uses the model's data (to refit, predict or compute again) and a category the model saw was renamed, merged or emptied since, or a variable it used is no longer qualitative (serveur.R: modele with a_jour, atelier.R: aligner_categories). ([`modele_perime`](#errors)) |
| These analyses are exported one by one. | modeles holds only other analyses, or neither modele nor modeles is given. |
| The models must come from the same project. | modeles from several projects. |
| No export to R for this analysis yet. | An analysis without an Export to R (every type of 1.0 has one). |
| The export to R writes a .R, .Rmd or .qmd file: &lt;file name&gt; | chemin's extension isn't .R, .Rmd or .qmd (in any case). |
| R-code models are turned off on this computer by your organisation's policy (DisableCustomCode). | The engine was started with NUEE_SANS_CODE=1 (etat's code_interdit) and the request defines, fits or exports an R-code model, or asks a code's fingerprint (code.R: refuser_si_interdit). ([`code_interdit`](#errors)) |
| Model &lt;name&gt; has R code written elsewhere, not approved on this computer: it doesn't run until you have read and approved it (Show the code, Run it). | An R-code model's code isn't among the fingerprints approved on this computer (code_approuve), whoever wrote it (code.R: exiger_code_permis, before anything runs it). Besides erreur, the answer carries code_non_approuve: true and empreinte, the code's fingerprint, to approve before fitting again. ([`code_non_approuve`](#errors)) |
| Model &lt;name&gt; is made with the plugin "&lt;plugin&gt;" &lt;version&gt;, which isn't installed or approved on this computer: it doesn't run until you have read and installed it (Show, Install, in the bar above the project). | A plugin's model is fitted, predicts or is exported to R, and its plugin's fingerprint isn't approved here (besides erreur: greffon_non_approuve: true, empreinte, id, version); under the DisableCustomCode policy, a plugin that isn't the organisation's gets “Model &lt;name&gt; is made with the plugin "&lt;plugin&gt;" &lt;version&gt;: on this computer, your organisation's policy (DisableCustomCode) only lets the plugins it provides run.” (greffon_interdit: true); a fit without the packages it needs “The plugin "&lt;plugin&gt;" needs packages that aren't installed for the R Nuée uses (R &lt;version&gt;): &lt;packages&gt;. Install them (File › Plugins…, or install.packages(…) in R), then fit again.” (paquets_manquants, always an array, and id) (greffons.R: exiger_greffon_permis, exiger_paquets_greffon). ([`greffon_refuse`](#errors)) |

#### `paquet`

Writes a project's reproducibility package: one .zip with the project, its data as CSV with a data dictionary, one R script that redoes every model, its documents as reproducible Quarto files, a README, the licence of the data with its text, CITATION.cff, codemeta.json and the SHA-256 of every file.

Since 2.1. Changes the project, without an undo step. Writes the .zip, beside it first, then put in its place; meta, cleaned, is kept in the project (Projet's paquet) and saved with it, the project counting as changed when it differs from the answers it kept. Nothing else in the session changes: the project in the package is a copy, its data's source the package's CSV.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `meta` | `object` | yes | The package's description, cleaned and kept in the project (Projet's paquet): titre (required, one line), auteurs (at least one, an array of {prenom, nom, orcid}, a table for jsonlite; nom required, orcid optional: 16 digits checked by their last, written with its dashes, its https://orcid.org/ address taken), description (empty lines between paragraphs), mots_cles (an array, or one text with commas or semicolons), licence (the data's, by its SPDX id: CC-BY-4.0, the default, CC0-1.0, CC-BY-SA-4.0 or CC-BY-NC-4.0). |
| `modeles` | `string[]` | no | The fitted models' ids (M1…), all of this project, in the order of their numbers: the latest fit of each model. Those of components and the regressions make one session script, each other analysis follows with its own export to R in local(); a model whose categories changed since its fit, or whose script can't be written, is named with why, the others redone without it. |
| `non_ajustes` | `object[]` | no | The project's models the client couldn't fit, each {nom, erreur}: named in the script and the README with why. |
| `documents` | `string` | no | JSON text: the documents as reproducible Quarto files, each {nom (the .qmd's name, without / or \\), titre, qmd (its text), bib (its .bib, or null), csl ({nom, texte}, or null), figures (each {chemin, png: base64}), fichiers and relatifs (the files document_reproductible wrote in the engine's temporary folder, and their paths beside the .qmd), note}; written in the package's documents/. Default: `"[]"`. |
| `doi` | `string` | no | The package's own DOI, the one Zenodo reserved for the draft it will go in (10.5281/zenodo.123, its https://doi.org/ address or doi: taken): the README gives it, with the link https://nuee.io/app/?doi=&lt;doi&gt;, and so do CITATION.cff, codemeta.json and the key results; without it, none. |
| `qr` | `string` | no | With doi, the QR code of that link as an SVG document (the interface draws it: interface/qr.js), written as qr-code.svg (code-qr.svg in French); ignored without doi. Since 2.2. |
| `session` | `string` | no | The interface's session (its views and fitted models), JSON text only it reads, kept in the package's .nuee as enregistrer keeps it. |
| `chemin` | `string` | no | The .zip to write (.zip added when missing); without it, one named after the title in a new folder of the engine's temporary folder, for a browser to fetch with /api/fichier. |
| `version` | `string` | no | Nuée's version, written in the files. Default: `""`. |
| `langue` | `string` | no | The language of the README, the script's comments and names, the data dictionary and the files' names (data/ or donnees/, analysis.R or analyse.R); anything but "en" is French. Values: `fr` (French), `en` (English). Default: `"fr"`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `chemin` | `string` | The .zip written. |
| `chemins` | `string[]` | The same, as an array. |
| `dossier` | `string` | The folder the .zip holds, named after it. |
| `fichiers` | `string[]` | Every file of that folder, relative to it, in the order of their bytes: &lt;project&gt;.nuee (format 3, its data's source its own CSV by its path in the package, without the project's steps nor any path of the computer it was written on), data/&lt;project&gt;.csv and its dictionary, analysis.R, documents/…, results/key_results.json (each model's key results, ResultatsDeposes; since 2.2), qr-code.svg (with doi and qr; since 2.2), README.md, LICENSE, CITATION.cff, codemeta.json, MANIFEST.sha256; French names in French (donnees/, analyse.R, resultats/resultats_cles.json, code-qr.svg). |
| `modeles` | `object[]` | One per model, each {id, nom, objet (its name in the script), erreur (why it isn't redone)}, one of objet and erreur. |
| `ecarts` | `string[]` | The models whose script doesn't give back Nuée's results exactly, and how (always an array). |
| `documents` | `object[]` | Each document written, {nom, titre, note}. |
| `taille` | `number` | The .zip's size, in bytes. |
| `projet` | [`Projet`](#type-projet) | The project, its paquet field the answers kept. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| The models must come from the same project. | A model of modeles was fitted on another project. |
| The package has no title. | meta's titre is empty. |
| The package has no author. | meta's auteurs has no one. |
| An author without a family name: &lt;given names&gt;. | An author has no nom. |
| This isn't an ORCID: &lt;text&gt; (16 digits, 0000-0002-1825-0097). | An orcid isn't 16 digits (the last may be X) whose last checks the others (ISO 7064 11,2). |
| Unknown licence: &lt;licence&gt;. | meta's licence isn't one of the four. |
| This isn't a DOI: &lt;doi&gt;. | doi isn't 10.&lt;4 to 9 digits&gt;/&lt;suffix&gt;. |
| The package's QR code isn't an SVG image. | With doi, qr isn't an SVG document (its &lt;svg&gt; first, after an XML declaration), or holds a script, a link or an event handler. |
| Document name refused in the package: &lt;name&gt; | A document's nom is empty or holds / or \\. |
| Two documents with the same name: &lt;name&gt; | Two documents' nom are the same, in any case. |
| Path refused in the package: &lt;path&gt; | A document's figure or file would go outside the package (.., an absolute path, a control character). |
| File refused in the package: &lt;file&gt; | A document's file isn't one the engine wrote in its temporary folder. |
| A document's files don't match their paths. | A document's fichiers and relatifs differ in length. |
| &lt;jsonlite's message&gt; | documents isn't JSON text. |

### Documents and figures

[`document`](#document) · [`document_enregistrer`](#document_enregistrer) · [`document_supprimer`](#document_supprimer) · [`figure`](#figure) · [`figure_enregistrer`](#figure_enregistrer) · [`figure_supprimer`](#figure_supprimer) · [`bibliographie_fichier`](#bibliographie_fichier) · [`export_r_possible`](#export_r_possible) · [`document_reproductible`](#document_reproductible)

#### `document`

Gives one of a project's documents.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `id` | `string` | yes | The document's id (D1, D2…), as Projet's documents give it. |

**Result:** `object`, with the fields below. The document as the project keeps it.

| Field | Type | Meaning |
|---|---|---|
| `id` | `string` | Its id. |
| `titre` | `string` | Its title. |
| `langue` | `string` | Its language. |
| `texte` | `string` | Its Markdown text, with its {{…}} references to results. |
| `cree` | `string` | When it was made, local time YYYY-MM-DDTHH:MM:SS. |
| `change` | `string` | When a field last changed, the same way; absent before any change. |
| `bib` | `string` | The BibTeX references kept in it; absent when never set. |
| `style` | `string` | The style of its citations; absent when never set. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Document not found | The project has no document of that id. |

#### `document_enregistrer`

Creates a document in a project, or changes the fields given of one of its documents.

Since 1.0. Changes the project, without an undo step. Each field given replaces the document's (a field left out stays; of an array, only its first element is kept); the project counts as modified only when one changed or a document was made. Undo doesn't touch documents, except for two changes it takes back: a variable's new name (renommer_variable), whose references in the documents ({{M1.coef.site}}) it puts back, and a merge (fusionner_finir), whose documents go back to those before it when they haven't changed since.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `id` | `string` | no | The document to change; without it, or with "", a new document is made (D1, D2…: the next number). An id the project doesn't have is refused. |
| `titre` | `string` | no | Its title ("" for a new document). |
| `langue` | `string` | no | Its language, "fr" or "en" ("fr" for a new document). |
| `texte` | `string` | no | Its Markdown text ("" for a new document). |
| `bib` | `string` | no | The BibTeX references kept in the document. |
| `style` | `string` | no | The style of its citations, as the interface names it. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `id` | `string` | The document's id (that of the new one when one was made). |
| `projet` | [`Projet`](#type-projet) | The project's summary. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Document not found: &lt;id&gt; | id names no document of the project. |

#### `document_supprimer`

Removes a document from a project.

Since 1.0. Changes the project, without an undo step. The project counts as modified; undo doesn't bring the document back.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `id` | `string` | yes | The document's id. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `projet` | [`Projet`](#type-projet) | The project's summary. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Document not found: &lt;id&gt; | The project has no document of that id. |

#### `figure`

Gives one of a project's figures for publication.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `id` | `string` | yes | The figure's id (F1, F2…), as Projet's figures give it. |

**Result:** `object`, with the fields below. The figure as the project keeps it.

| Field | Type | Meaning |
|---|---|---|
| `id` | `string` | Its id. |
| `titre` | `string` | Its title. |
| `contenu` | `string` | Its content, JSON text only the interface reads (its panels, size and settings). |
| `cree` | `string` | When it was made, local time YYYY-MM-DDTHH:MM:SS. |
| `change` | `string` | When its title or content last changed, the same way; absent before any change. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Figure not found | The project has no figure of that id. |

#### `figure_enregistrer`

Creates a figure in a project, or changes its title or content.

Since 1.0. Changes the project, without an undo step. The field given replaces the figure's (of an array, only its first element is kept); the project counts as modified only when one changed or a figure was made. Undo doesn't touch figures, except that undoing a merge (fusionner_finir) puts back the figures from before it when they haven't changed since.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `id` | `string` | no | The figure to change; without it, or with "", a new figure is made (F1, F2…: the next number). An id the project doesn't have is refused. |
| `titre` | `string` | no | Its title ("" for a new figure). |
| `contenu` | `string` | no | Its content, JSON text only the interface reads, 5,000,000 bytes at most ("{}" for a new figure). |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `id` | `string` | The figure's id (that of the new one when one was made). |
| `projet` | [`Projet`](#type-projet) | The project's summary. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Figure not found: &lt;id&gt; | id names no figure of the project. |
| Figure too big to be kept in the project | contenu is over 5,000,000 bytes. |

#### `figure_supprimer`

Removes a figure from a project.

Since 1.0. Changes the project, without an undo step. The project counts as modified; undo doesn't bring the figure back.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `id` | `string` | yes | The figure's id. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `projet` | [`Projet`](#type-projet) | The project's summary. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Figure not found: &lt;id&gt; | The project has no figure of that id. |

#### `bibliographie_fichier`

Gives the text of a bibliography file (BibTeX, CSL-JSON) or of a CSL style that a document names.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id: a relative path is relative to the folder of its file (Projet's chemin), as Quarto reads it beside the document. |
| `chemin` | `string` | yes | The file: .bib, .bibtex, .json (CSL-JSON) or .csl, absolute (from a drive letter or a / or \\) or relative. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `chemin` | `string` | The file's absolute path, with / as separator. |
| `texte` | `string` | Its text in UTF-8, without a byte order mark (a file in Latin-1 converted). |
| `date` | `string` | Its modification time, local time YYYY-MM-DDTHH:MM:SS. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Not a bibliography file (.bib, .json) or a style (.csl): &lt;path&gt; | chemin doesn't end in .bib, .bibtex, .json or .csl. |
| A relative path, and the project has no file yet: &lt;path&gt; | chemin is relative and the project has no chemin. |
| File not found: &lt;path&gt; | The file named doesn't exist. ([`fichier_introuvable`](#errors)) |
| File too big (over 50 MB): &lt;path&gt; | The file is over 50,000,000 bytes. |

#### `export_r_possible`

Names the models among those a document cites whose type has no export to R yet, and why.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `noms` | `string[]` | yes | The names of the models the document cites (M1…), as the project's definitions have them. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `sans_export` | `object[]` | Those without an export to R, each {nom, type, raison (why, in the request's language)}; always an array, empty when every one has one. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |

#### `document_reproductible`

Writes the export to R of each model a document cites beside its Quarto file, with what the .qmd writes for each of its references.

Since 1.0. Changes nothing. Writes nuee.R and each model's script and data in &lt;dossier&gt;/&lt;nom&gt;\_R, and runs each script once, in an environment of its own, to compare its values with Nuée's text; a model whose script fails or differs stays written in. Each script is written on the project as its model saw it (the categories in their order at its fit, with the definition of that fit); nuee.R and the scripts carry Nuée's licence notice and how to cite it.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `projet` | `string` | yes | The project's id. |
| `modeles` | `string[]` | no | The ids in the session (M1…) of the fitted models the document cites, all of this project; without them, only nuee.R is written. |
| `dossier` | `string` | no | The folder of the .qmd; without it, a new folder of the engine's temporary folder, whose files a browser fetches with /api/fichier. |
| `references` | `string` | no | JSON text: the document's references in their order, each null (a reference the document doesn't resolve) or {modele (an id of modeles), chemin (the path in its results, an array), genre (valeur, figure, tableau or texte), forme and texte (how Nuée writes the value, and the text it wrote) for a value, vue and variante for a plot, cle for a table}. Default: `"[]"`. |
| `nom` | `string` | no | The document's name: the scripts go in the folder &lt;nom&gt;\_R. Default: `"document"`. |
| `langue` | `string` | no | The document's language, of its numbers, the scripts' comments and names; anything but "en" is French. Values: `en` (English), `fr` (French). Default: `"fr"`. |
| `version` | `string` | no | Nuée's version, written in the scripts. Default: `""`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `dossier_r` | `string` | The scripts' folder, &lt;nom&gt;\_R, beside the .qmd. |
| `fichiers` | `string[]` | The files written, absolute: nuee.R, then each model's script and data (always an array). |
| `relatifs` | `string[]` | The same files relative to the .qmd's folder, &lt;nom&gt;\_R/&lt;file&gt; (always an array). |
| `installation` | `string[]` | The lines of the .qmd's first chunk: it sources nuee.R, makes each model again, and says in comments what stays written in (always an array). |
| `modeles` | `object[]` | One per model cited, each {id, nom, objet (its R object in the .qmd), script (&lt;nom&gt;\_R/&lt;file&gt;.R), erreur (why it can't be made again, so its values stay written in: its variables or categories changed since its fit, its type has no export to R, its script failed…; null when it can)}. |
| `references` | `any[]` | One per reference, in order: {code} an inline R expression; {lignes (always an array), largeur, hauteur} a plot's chunk; {lignes} a table's chunk; {raison} why it stays written in; null for a reference not resolved or of a model not in modeles. |
| `R` | `object` | This R: {bin (its bin folder, for Quarto), knitr, rmarkdown (whether it has those packages)}. |

**Errors:**

| Message (in English) | When |
|---|---|
| Which project? The request names none (projet). | projet is left out, null or "" (serveur.R: projet). ([`projet_manquant`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| The models must come from the same project. | A model of modeles was fitted on another project. |
| &lt;jsonlite's message&gt; | references isn't JSON text. |

### The R console

[`journal`](#journal) · [`console`](#console) · [`completer`](#completer)

#### `journal`

Gives the R console's log entries after a given number: the engine calls the interface made, commands, outputs, warnings, errors and plots.

Since 1.0. Changes nothing.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `depuis` | `number` | no | The number of the last entry already read: the entries after it are given. Default: `0`. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `entrees` | [`EntreeJournal`](#type-entreejournal)`[]` | The entries after depuis still kept, in order (always an array). |
| `dernier` | `integer` | The number of the last entry written (0 before any). |

**Errors:** none of its own.

#### `console`

Runs R code typed in the console in the engine's session, and gives the log entries it added.

Since 1.0. Changes the project, with an undo step that `annuler` takes back. Runs any R code in the engine's session, which can change anything: a project assigned from the console (P1 &lt;- …) is replaced, marked modified, with one undo step; m and p are bound to the model and project given.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `code` | `string` \| `string[]` | yes | The R code to run (a text, or an array of lines). Without it nothing runs: the log gets an empty command. |
| `modele` | `string` | no | The model shown in the application, bound to m in the console; an id that is no fitted model's is refused ("" is ignored). |
| `projet` | `string` | no | The project shown, bound to p (an id that is no project's is refused, "" ignored); P1, M7… are the session's projects and models. |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `journal` | [`EntreeJournal`](#type-entreejournal)`[]` | The entries the code added: the command, then its outputs, warnings, error and plots (always an array). An error stops the code there and is an entry, not an error of the request; Stop, interrupting a long computation of the engine that the code runs, is one (calcul_arrete). |
| `modifies` | `string[]` | The ids of the projects the code replaced (P1 &lt;- …), whose summaries the interface asks again (always an array). |

**Errors:**

| Message (in English) | When |
|---|---|
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
| Computation stopped. | The client wrote the file NUEE_ARRET names while the request computed, and one of its loops saw it (see Answers): besides erreur, the answer carries arrete: true, and the session is as it was before the request. ([`calcul_arrete`](#errors)) |

#### `completer`

Gives the names that can complete the word before the cursor in the R console, without evaluating anything.

Since 1.0. Changes the session, not a project's content. Binds m and p in the console to the model and project given, as console does; nothing is evaluated.

| Parameter | Type | Required | Meaning |
|---|---|---|---|
| `texte` | `string` | no | The console's line as typed. Default: `""`. |
| `curseur` | `integer` | no | The cursor's place: the number of characters of texte before it; the end of texte by default. |
| `modele` | `string` | no | The model shown, bound to m as console binds it (an unknown id is refused, "" ignored). |
| `projet` | `string` | no | The project shown, bound to p (an unknown id is refused, "" ignored). |

**Result:** `object`, with the fields below.

| Field | Type | Meaning |
|---|---|---|
| `debut` | `integer` | The number of characters before the word completed: where the candidate replaces it. |
| `mot` | `string` | The word completed. |
| `candidats` | `string[]` | The names that start with it, 300 at most (always an array): after x$ or x[[", the object's elements; in a call's brackets, its arguments ("name = ") first; else the console's objects, the engine's and R's functions. |

**Errors:**

| Message (in English) | When |
|---|---|
| Unknown model: &lt;id&gt; | modele isn't the id of a model fitted in the session. ([`modele_inconnu`](#errors)) |
| Unknown project: &lt;id&gt; | projet isn't the id of a project open in the session. ([`projet_inconnu`](#errors)) |
