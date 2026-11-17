// The OAuth client ids of Nuée's sign-in to GitHub and GitLab (comptes.js), one per server. A client
// id isn't a secret: the device flow needs it alone, and no client secret is embedded in Nuée.
// Clément registered the applications (docs/developpement.md, "Signing in to GitHub and GitLab").
// A copy of Nuée built with a placeholder instead (such as "<to fill>") says signing in isn't set up
// for that server, and a token can still be given. A self-hosted GitLab's id is added from the
// settings (comptes.js: reglerServeur), not here.
module.exports = {
  'github.com': 'Ov23lil4KRom3ititJwM',
  'gitlab.com': 'f3357ffdb45b679d080f7e7118b9e53e9cdcad4cecd6268a8ffd614f839a3d58',
};
