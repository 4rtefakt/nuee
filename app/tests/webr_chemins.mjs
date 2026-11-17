// webR in Node, on Windows: its worker loads R.js with import() and an absolute path, which
// Node's ES module loader refuses there (the drive letter reads as a URL scheme, "c:"). Loaded
// with --import (webr_rscript.mjs gives it to webR's worker), this turns such a path into a
// file: URL for import() only; require() takes paths as they are.
import { registerHooks } from 'node:module';
import { pathToFileURL } from 'node:url';

registerHooks({
  resolve(specifier, context, next) {
    if (/^[A-Za-z]:[\\/]/.test(specifier) && !(context.conditions || []).includes('require')) return next(pathToFileURL(specifier).href, context);
    return next(specifier, context);
  },
});
