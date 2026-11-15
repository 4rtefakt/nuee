Real answers of Zotero and Better BibTeX for group libraries, for app/tests/essai_zotero.js and
app/tests/essai_bibliographie.mjs
==============================================================================================

Recorded on 2026-11-15 from Zotero 10.0.8 for Windows, run on throwaway profiles (never signed in,
no sync, every outside address sent to a dead proxy), each with its own data folder:
  - simple/: Zotero alone;
  - better-bibtex/: with Better BibTeX 9.0.77 (its default settings: citation keys
    "auth.lower + shorttitle(3,3) + year", unique within a library: keyScope "library").
index.json in each folder gives, for each file, the address asked (GET, header Zotero-API-Version: 3),
the HTTP status and the Content-Type; what Nuée asks Zotero is in app/interface/documents.js
(etatZotero, chercherZotero, collectionsZotero, choisirDansZotero).

The libraries
-------------
Group libraries come from syncing; here they were made locally, at the profile's first start, by a
small test plugin (Zotero's own JavaScript: a Zotero.Group saved with a made-up id, then items and
collections in its library), with the same items in both profiles (their Zotero keys differ):
  - My Library: Wold, Sjöström & Eriksson 2001 (PLS-regression, with its DOI); Béchet & Johnson
    2019, "Population dynamics of the greater flamingo in the Camargue" (Ibis); Legendre &
    Legendre 2012 (book); collections "Thèse" and "Thèse › Flamants".
  - "Équipe Camargue" (group 6011001, editable): Béchet & Germain 2019, "Population dynamics of
    greater flamingos in Mediterranean wetlands" (another work, same first author, title start and
    year: the same citation key as My Library's Béchet, bechetPopulationDynamicsGreater2019 with
    Better BibTeX, bechet_population_2019 without); Wold et al. 2001 (a copy of My Library's);
    Johnson & Cézilly 2007 (book); a report in French (2021); collections "Flamants" and
    "Flamants › Reproduction", and "Beaucoup" (300 made-up articles, to time the answers).
  - "Réseau Perdrix" (group 6011002, read only: its items saved as a sync saves them): Bro &
    Mayot 2019 (title in French with an italic species name), Reitz & Bro 2012; collection "Suivis".
The data are made up: no person's library, no account, no server's name in these files (the URIs of
My Library's items name the throwaway profile's local id).

What they show
--------------
  - /api/users/0/groups lists the group libraries (id, name, number of items), not My Library;
    a group's items and collections are under /api/groups/<id>/..., as on zotero.org. An unknown
    group answers 404 "Not found". With the local API off (simple/api_fermee.txt) every /api/
    address answers 403 "Local API is not enabled". Zotero 7.0.0 already had the same addresses.
  - Keys: Zotero's BibTeX export uses an item's Citation Key field when it has one (Better BibTeX
    fills it in the libraries it can write to), else makes one (author_firstword_year). In a group
    the user can only read, Better BibTeX can't write that field: it keeps its keys itself, gives
    them to Zotero's export once it has made them (at its next start), and until then Zotero's own
    key is exported (bro_survie_2019 for what its picker calls broSurvieHivernalePerdrix2019).
  - format=bibtex exports a list of items in one go, in an order of its own (not the list's): its
    entries can't be matched with the items' keys; format=json&include=bibtex gives each item with
    its library, its key and its own BibTeX (about 4 ms an item: 1.3 s for 300, against 0.1 s).
    Zotero alone adds -1, -2 to keys made alike in one export, and not across exports.
  - Better BibTeX's picker (format=json; asked here with selected=1, the items selected in Zotero's
    window, which gives what its dialog gives without a locator) returns, for each reference, its
    key, the item's URI (http://zotero.org/groups/<id>/items/<key> for a group's) and the item with
    Zotero's libraryID (1 for My Library, 2 and 3 for the groups here, not their group ids). Its
    export of picked references (/better-bibtex/export/item) takes that libraryID (or groupID=);
    an unknown one answers 400. The dialog closed without a pick answers 200 with nothing.
  - Better BibTeX's JSON-RPC (not used by Nuée): user.groups lists every library with its
    libraryID and name; item.citationkey takes "libraryID:itemKey" (with a group id, it finds
    nothing); item.search gives CSL-JSON with each item's library name and key.
