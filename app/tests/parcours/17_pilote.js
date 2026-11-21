// The journeys' driver itself (pilote.js): a click waits until its element has stopped moving, and
// presses only while it is still where it was measured. A button that the layout moves away right
// after the driver saw the pointer over it, before the press (as a status line that comes, "Session
// restored", once moved the help's search field from under the pointer on GitHub's macOS arm64
// machine, the press going to the guide's table of contents beside it), is measured again and
// clicked where it went, not on the button that came where it was.
module.exports = {
  async executer(n, verifier) {
    await n.evaluer(`(() => {
      const E = window.__essai;
      let souris = E.souris;
      let bouge = false;
      const bouton = (id, z) => {
        const b = document.createElement('button');
        b.id = id;
        b.textContent = id;
        Object.assign(b.style, { position: 'fixed', left: '200px', top: '200px', width: '120px', height: '32px', zIndex: String(z) });
        b.addEventListener('click', () => { window.__essaiClics = [...(window.__essaiClics || []), id]; });
        document.body.append(b);
        return b;
      };
      bouton('essai-dessous', 99998);
      const cible = bouton('essai-cible', 99999);
      // (the layout moves once, as the driver reads where the page saw the pointer, before it presses)
      Object.defineProperty(E, 'souris', { configurable: true, set(v) { souris = v; }, get() {
        if (!bouge && souris && souris.type === 'mousemove' && souris.cible === cible) { bouge = true; cible.style.top = '300px'; }
        return souris;
      } });
      return true; })()`);
    await n.cliquer("document.getElementById('essai-cible')");
    const clics = await n.attendre('window.__essaiClics && window.__essaiClics.length ? window.__essaiClics : null', 5000);
    verifier('cliqueOuIlEstAlle', JSON.stringify(clics) === JSON.stringify(['essai-cible']), JSON.stringify(clics));
    await n.evaluer(`(() => {
      for (const id of ['essai-cible', 'essai-dessous']) document.getElementById(id).remove();
      const s = window.__essai.souris;
      Object.defineProperty(window.__essai, 'souris', { value: s, writable: true, configurable: true, enumerable: true });
      delete window.__essaiClics;
      return true; })()`);
  },
};
