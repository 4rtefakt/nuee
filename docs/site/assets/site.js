// The theme the reader chose (light or dark), set before the page is drawn; the button that
// changes it; the guide's list of pages folded on a narrow screen. Without this script the
// pages follow the system's theme.
(function () {
  var cle = 'nuee-site-theme';
  var racine = document.documentElement;
  try {
    var t = localStorage.getItem(cle);
    if (t === 'clair' || t === 'sombre') racine.setAttribute('data-theme', t);
  } catch (e) { /* storage refused: the system's theme */ }
  function sombre() {
    var t = racine.getAttribute('data-theme');
    if (t) return t === 'sombre';
    return !!(window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches);
  }
  document.addEventListener('DOMContentLoaded', function () {
    var b = document.querySelector('.bouton-theme');
    if (b) {
      b.hidden = false;
      var maj = function () { b.setAttribute('aria-pressed', sombre() ? 'true' : 'false'); };
      maj();
      b.addEventListener('click', function () {
        var t = sombre() ? 'clair' : 'sombre';
        racine.setAttribute('data-theme', t);
        try { localStorage.setItem(cle, t); } catch (e) { /* this page only */ }
        maj();
      });
    }
    var d = document.querySelector('.cote details');
    if (d && window.matchMedia && window.matchMedia('(max-width: 60rem)').matches) d.removeAttribute('open');
  });
})();
