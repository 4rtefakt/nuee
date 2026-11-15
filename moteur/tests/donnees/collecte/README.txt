Real exports of KoboToolbox and ODK Central, for moteur/tests/verifier_kobo.R
=============================================================================

Made on 2026-11-15 with servers installed in Docker for this purpose (then removed):
  - KoboToolbox 2.026.37g (kobo-install 11.0.0, kobo-docker 2.026.37g: kobotoolbox/kpi:2.026.37g,
    enketo-express-extra-widgets 7.6.3);
  - ODK Central v2026.3.1 (backend and frontend v2026.3.1, Enketo 7.6.4, pyxform-http v4.5.0).
The data are made up: no person, no server name, no token is in these files. The only account
names are the test ones that sent the submissions (Kobo: equipe_terrain; ODK: the app user
"Équipe terrain").

The form
--------
A breeding-bird count, in French and English (label::Français (fr), label::English (en),
default_language Français (fr)): start, end, today, deviceid; a group "site" (the site's name,
its habitat: select_one, a nested group "localisation" with a geopoint and the altitude: integer,
notes); the survey date (date), the count's start (dateTime), the wind (decimal), the weather
(select_multiple); a repeat "observation" (species, count, behaviour) holding a repeat
"individu" (sex, age); a group "fin" (general notes); a note.
  - formulaire_odk.xlsx: the form sent to ODK Central (and given back unchanged by it). Its two
    "remarques" questions, in "site" and in "fin", have the same name.
  - formulaire_kobo.xlsx: the form sent to KoboToolbox, which refuses two questions of the same
    name ("Duplicate node name"): the general notes are "remarques_fin" there.
  - formulaire_telecharge_kobo.xlsx: that form as KoboToolbox gives it back (Download XLS):
    types in lower case (datetime), required as "true", settings without form_title.

The submissions (envois.csv, observations.csv, individus.csv)
--------------------------------------------------------------
20 submissions, sent to both servers as a phone sends them (OpenRosa XML filled from each
server's own XForm): ten sites of the Camargue and the Crau, each counted in May and in June
2026; 37 observations, 28 individuals. envois.csv holds what was sent, with:
  - the 6th submission edited afterwards on both servers (its first count 7 → 8, a general note
    added; its new instanceID, the first one in instanceID_origine);
  - the review state set on ODK Central (revue_odk; "edited" for the edited one) and the
    validation status on KoboToolbox (validation_kobo) for six submissions;
  - answers left empty on purpose: the habitat (8th), the GPS point (13th), the altitude (5th),
    the wind (4th; 0 in the 11th), the count's start (16th), the weather (10th), every
    observation of the 19th, the behaviour of one observation, the age of one individual;
  - notes with a semicolon, a comma, quotes and a line break, accents (Sansouïre, Étang).
observations.csv and individus.csv link each row to its submission (envoi: the row of
envois.csv) and observation (its rank in the submission).

The exports
-----------
kobo/ (POST /api/v2/assets/<uid>/exports/, fields_from_all_versions true, include_media_url
false; lang _xml: XML values and headers; hierarchy_in_labels: groups in headers):
  xml_groupes.xlsx                  xls, _xml, groups "/", multiple_select both
  xml_groupes_texte.xlsx            the same with xls_types_as_text (dates and numbers as text)
  xml_sans_groupes_choix_un.xlsx    xls, _xml, no groups, multiple_select summary
  xml_groupes_point_choix_01.xlsx   xls, _xml, groups ".", multiple_select details
  xml_groupes_souligne.xlsx         xls, _xml, groups "_", both
  fr_groupes.xlsx                   xls, Français (fr), groups "/", both
  fr_groupes_point_choix_01.xlsx    xls, Français (fr), groups ".", details
  en_groupes_tiret_choix_un.xlsx    xls, English (en), groups "-", summary
  xml_groupes.csv                   csv, _xml, groups "/", both
  fr_sans_groupes.csv               csv, Français (fr), no groups, both
  en_groupes_point_choix_01.csv     csv, English (en), groups ".", details
odk/ (GET /v1/projects/1/forms/nuee_essai_oiseaux/submissions.csv.zip, attachments and
deletedFields false):
  groupes.zip                       groupPaths true, splitSelectMultiples false (the defaults)
  sans_groupes.zip                  groupPaths false
  groupes_choix_separes.zip         splitSelectMultiples true
  sans_groupes_choix_separes.zip    groupPaths false, splitSelectMultiples true
  nuee_essai_oiseaux.csv            submissions.csv: the submissions table alone, groupPaths true

What they showed (and the import relies on)
-------------------------------------------
KoboToolbox:
  - the first sheet is named after the form, cut to 28 characters and "..." (31 in all); a
    repeat's sheet has the repeat's name; _parent_table_name holds the parent's full name (the
    form's title, or the repeat's name), _parent_index its _index; every repeat sheet copies the
    submission's metadata as _submission__id, _submission__uuid… and _submission_meta/rootUuid;
  - the submissions' metadata: _id, _uuid, _submission_time, _validation_status (its uid in an
    export in XML values, validation_status_approved; its label in an export in labels,
    Approved), _status, _submitted_by, __version__, meta/rootUuid, _index; no meta/instanceID,
    formhub/uuid, _notes, _tags or _geolocation; "meta/" keeps "/" whatever the separator;
  - the chosen group separator also separates a select_multiple from its choices (meteo.soleil,
    Météo.Soleil), groups in headers or not; labels exports use it between group labels too
    (Site.Localisation.Altitude (m));
  - a geopoint: a text column named after the question alone (gps, Position), and
    <groups>/_gps_latitude… (labels: <group labels>/_Position_latitude…);
  - an unanswered select_multiple leaves its 0/1 cells empty; a summary in labels joins the
    labels with spaces (Nuages Pluie Vent);
  - xls: dates and numbers are Excel cells (dateTime too, formatted yyyy-mm-dd, the phone's
    local time without its time zone), or text with xls_types_as_text; everything else is text;
  - csv: the submissions table only, ";" separated, every field quoted, CRLF, no BOM;
  - lang _default (labels in the default language) wrote XML values for this form, whose
    languages are all named.
ODK Central:
  - comma separated, LF, no BOM; the newest submission first; repeats in
    <form>-<repeat>.csv, a nested repeat by its own name (nuee_essai_oiseaux-individu.csv);
  - groups in dashes (site-localisation-gps-Latitude); without group paths, two questions of
    the same name give two columns of the same name, and meta-instanceID becomes instanceID;
  - split multiple choices: <question>/<choice>, after the question's own column, only for the
    choices picked in at least one submission, in alphabetical order, and 0 (not empty) for an
    unanswered question;
  - an edited submission: KEY keeps the first instanceID (and PARENT_KEY in its repeats),
    meta-instanceID the new one; Edits 1, ReviewState edited.
