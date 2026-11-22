// The phone's layout: a window narrower than 700 px (a phone held upright, or a desktop window made
// that narrow), or a phone on its side (a coarse pointer, at most 500 px high). One group of views
// at a time, as large as the window: dockview's maximized group, in the main dock and in each model's
// page (unAUn), the group in front each time, which dockview leaves when a view is added, moved or
// closed (and so is taken back). The explorer is one of these groups: the menu bar's Explorer button
// brings it to the front and back (app.js, basculerExplorateur), and a node opened from it, one tap on
// a phone (explorateur.js), shows its view in its place. The body has the class `etroit` (style.css:
// the toolbar and the groups' Detach and Maximize hidden, the menu bar scrolling sideways). A saved
// layout is the whole one (dockview writes it without the maximized group). Back to a wider window,
// every group shows again.
//   etroit()                    whether the window is that narrow now
//   unAUn(d)                    the dockview `d` one group at a time while it is; returns what ends it
//   surEtroit(f)                f(etroit) at each change, and once now; returns what ends it
export const REQUETE_ETROITE = '(max-width: 700px), (pointer: coarse) and (max-height: 500px)';
const media = typeof matchMedia === 'function' ? matchMedia(REQUETE_ETROITE) : null;
export const etroit = () => !!(media && media.matches);

const abonnes = new Set();
if (media) {
  const changer = () => {
    document.body.classList.toggle('etroit', etroit());
    for (const f of abonnes) f(etroit());
  };
  if (media.addEventListener) media.addEventListener('change', changer); else media.addListener(changer);
  if (document.body) document.body.classList.toggle('etroit', etroit());
  else document.addEventListener('DOMContentLoaded', () => document.body.classList.toggle('etroit', etroit()));
}
export function surEtroit(f) {
  abonnes.add(f);
  f(etroit());
  return () => abonnes.delete(f);
}

export function unAUn(d) {
  let prevu = false;
  let fini = false;
  const appliquer = () => {
    prevu = false;
    if (fini || !etroit()) return;
    const g = d.activeGroup || (d.activePanel && d.activePanel.group);
    if (!g || d.groups.length < 2 || g.api.isMaximized()) return;
    try { g.api.maximize(); } catch { /* a group going away meanwhile */ }
  };
  // (after dockview's own change is through: a view added, its group made active, then maximized)
  const plusTard = () => { if (!prevu && !fini && etroit()) { prevu = true; setTimeout(appliquer); } };
  const ecoutes = [d.onDidActiveGroupChange(plusTard), d.onDidActivePanelChange(plusTard), d.onDidLayoutChange(plusTard)];
  const fin = surEtroit((oui) => {
    if (oui) plusTard();
    else if (d.hasMaximizedGroup()) d.exitMaximizedGroup();
  });
  return () => { fini = true; fin(); for (const e of ecoutes) e.dispose(); };
}
