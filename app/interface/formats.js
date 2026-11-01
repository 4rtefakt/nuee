// File formats the engine reads and writes (moteur/R/formats.R), for the file dialogs and the
// export dialog. `cle` names the format in i18n.js.
import { t } from './i18n.js';

export const LECTURE = [
  { cle: 'format_projet', ext: ['json'] },
  { cle: 'format_usp', ext: ['usp'] },
  { cle: 'format_excel', ext: ['xlsx', 'xlsm', 'xls'] },
  { cle: 'format_texte', ext: ['csv', 'txt', 'tsv', 'tab', 'dat'] },
  { cle: 'format_r', ext: ['rds', 'RData', 'rda'] },
  { cle: 'format_spss', ext: ['sav', 'zsav', 'por'] },
  { cle: 'format_stata', ext: ['dta'] },
  { cle: 'format_sas', ext: ['sas7bdat', 'xpt'] },
  { cle: 'format_jamovi', ext: ['omv'] },
  { cle: 'format_dbase', ext: ['dbf'] },
  { cle: 'format_arff', ext: ['arff'] },
  { cle: 'format_spectres', ext: ['jdx', 'dx', 'jcm', 'spc'] },
];

// Files of spectra (moteur/R/fichiers_spectres.R): JCAMP-DX, SPC, or text (x, then one column per spectrum).
const SPECTRES = [LECTURE.find((f) => f.cle === 'format_spectres'), { cle: 'format_texte', ext: ['csv', 'txt', 'tsv', 'tab', 'dat'] }];
export const filtresSpectres = () => [{ name: t('tous_spectres'), extensions: SPECTRES.flatMap((f) => f.ext) },
  ...SPECTRES.map((f) => ({ name: t(f.cle), extensions: f.ext })), { name: t('tous_fichiers'), extensions: ['*'] }];
export const acceptSpectres = () => SPECTRES.flatMap((f) => f.ext).map((e) => '.' + e).join(',');

// One extension each; `plusieurs`: a model's results make one file per table in this format.
export const ECRITURE = [
  { ext: 'xlsx', cle: 'format_excel' },
  { ext: 'csv', cle: 'format_csv', plusieurs: true },
  { ext: 'tsv', cle: 'format_tsv', plusieurs: true },
  { ext: 'rds', cle: 'format_rds' },
  { ext: 'RData', cle: 'format_rdata' },
  { ext: 'sav', cle: 'format_spss', plusieurs: true },
  { ext: 'dta', cle: 'format_stata', plusieurs: true },
  { ext: 'xpt', cle: 'format_xpt', plusieurs: true },
  { ext: 'omv', cle: 'format_jamovi', plusieurs: true },
  { ext: 'dbf', cle: 'format_dbase', plusieurs: true },
  { ext: 'arff', cle: 'format_arff', plusieurs: true },
];

const tableaux = LECTURE.filter((f) => !['format_projet', 'format_usp'].includes(f.cle));

// Filters of the Open dialog: everything first, then each format.
export function filtresOuverture(seulementTableaux = false) {
  const liste = seulementTableaux ? tableaux : LECTURE;
  return [{ name: t(seulementTableaux ? 'tous_tableaux' : 'tous_formats'), extensions: liste.flatMap((f) => f.ext) },
    ...liste.map((f) => ({ name: t(f.cle), extensions: f.ext })), { name: t('tous_fichiers'), extensions: ['*'] }];
}

// accept attribute of a file input (browser)
export const acceptOuverture = (seulementTableaux = false) => (seulementTableaux ? tableaux : LECTURE).flatMap((f) => f.ext).map((e) => '.' + e).join(',');
