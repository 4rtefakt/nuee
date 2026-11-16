// The OAuth client ids of Nuée's sign-in to GitHub and GitLab (comptes.js), one per server. A client
// id isn't a secret: the device flow needs it alone, and no client secret is embedded in Nuée.
// Clément registers the applications (docs/developpement.md, "Signing in to GitHub and GitLab") and
// writes their ids here. Until then these are placeholders: signing in says it isn't set up for that
// server, and a token can still be given. A self-hosted GitLab's id is added from the settings
// (comptes.js: reglerServeur), not here.
module.exports = {
  'github.com': '<to fill>',
  'gitlab.com': '<to fill>',
};
