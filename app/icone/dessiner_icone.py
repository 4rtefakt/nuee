# Draws the Nuée logo: a swarm of points (a score plot) inside its confidence ellipse, on a
# rounded blue square. Writes, next to this script:
#   nuee.png          512 px
#   nuee.ico          16 to 256 px; up to 32 px a simpler drawing (fewer, larger points)
#   installateur.bmp  the side image of the installer's welcome and finish pages (164 x 314)
# and ../interface/nuee.svg, the logo of the interface and the README.
#   python dessiner_icone.py
import math
import random
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

sortie = Path(__file__).resolve().parent

T = 1024  # drawing units; images are drawn at this size, then reduced (smooth edges)
HAUT, BAS = (27, 74, 156), (47, 125, 205)  # gradient of the square, top and bottom
CLAIR = (190, 225, 255)
CX, CY, A, B, DEGRES = 512, 522, 350, 196, 28  # the ellipse, tilted like a first component


def nuee(n, rayon_min, rayon_max, ecart, graine):
    """n points inside the ellipse, kept apart, larger and whiter near the centre."""
    random.seed(graine)
    angle = math.radians(-DEGRES)
    points = []
    for _ in range(50000):
        if len(points) == n:
            break
        u, v = random.gauss(0, 0.42), random.gauss(0, 0.42)
        r = math.hypot(u, v)
        if r > 0.74:
            continue
        x, y = u * A, v * B
        px = CX + x * math.cos(angle) - y * math.sin(angle)
        py = CY + x * math.sin(angle) + y * math.cos(angle)
        rayon = rayon_max - (rayon_max - rayon_min) * min(r / 0.74, 1)
        if all(math.hypot(px - q[0], py - q[1]) > ecart + rayon + q[2] for q in points):
            points.append((px, py, rayon, r < 0.42))
    return points


def carre(taille):
    """The rounded square with its gradient, at `taille` pixels."""
    fond = Image.new('RGBA', (taille, taille))
    for y in range(taille):
        k = y / (taille - 1)
        fond.paste(tuple(int(h + (b - h) * k) for h, b in zip(HAUT, BAS)) + (255,), (0, y, taille, y + 1))
    masque = Image.new('L', (taille, taille), 0)
    m = taille / T
    ImageDraw.Draw(masque).rounded_rectangle((40 * m, 40 * m, (T - 40) * m, (T - 40) * m), radius=220 * m, fill=255)
    img = Image.new('RGBA', (taille, taille), (0, 0, 0, 0))
    img.paste(fond, (0, 0), masque)
    return img


def dessiner(points, trait, fond=True):
    img = carre(T) if fond else Image.new('RGBA', (T, T), (0, 0, 0, 0))
    ellipse = Image.new('RGBA', (T, T), (0, 0, 0, 0))
    ImageDraw.Draw(ellipse).ellipse((CX - A, CY - B, CX + A, CY + B), outline=(255, 255, 255, 150), width=trait)
    img.alpha_composite(ellipse.rotate(DEGRES, resample=Image.BICUBIC, center=(CX, CY)))
    d = ImageDraw.Draw(img)
    for x, y, r, centre in points:
        d.ellipse((x - r, y - r, x + r, y + r), fill=(255, 255, 255, 255) if centre else CLAIR + (255,))
    return img


grands = nuee(17, 15, 27, 26, 11)
petits = nuee(7, 46, 58, 34, 4)
detail = dessiner(grands, 20)
simple = dessiner(petits, 52)

detail.resize((512, 512), Image.LANCZOS).save(sortie / 'nuee.png')
tailles = [16, 24, 32, 48, 64, 128, 256]
images = [(simple if t <= 32 else detail).resize((t, t), Image.LANCZOS) for t in tailles]
images[-1].save(sortie / 'nuee.ico', sizes=[(t, t) for t in tailles], append_images=images[:-1])

# the same logo as SVG
hexa = lambda c: '#%02x%02x%02x' % c
svg = [f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {T} {T}">',
       f'<defs><linearGradient id="f" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="{hexa(HAUT)}"/>'
       f'<stop offset="1" stop-color="{hexa(BAS)}"/></linearGradient></defs>',
       f'<rect x="40" y="40" width="{T - 80}" height="{T - 80}" rx="220" fill="url(#f)"/>',
       f'<ellipse cx="{CX}" cy="{CY}" rx="{A - 10}" ry="{B - 10}" fill="none" stroke="#fff" stroke-opacity="0.59" '
       f'stroke-width="20" transform="rotate({-DEGRES} {CX} {CY})"/>']
svg += [f'<circle cx="{x:.0f}" cy="{y:.0f}" r="{r:.0f}" fill="{"#fff" if c else hexa(CLAIR)}"/>' for x, y, r, c in grands]
(sortie.parent / 'interface' / 'nuee.svg').write_text('\n'.join(svg + ['</svg>']) + '\n', encoding='utf-8')

# installer side image: the logo without its square, the name, what it does
L, H = 164, 314
cote = Image.new('RGB', (L, H))
for y in range(H):
    k = y / (H - 1)
    cote.paste(tuple(int(h + (b - h) * k) for h, b in zip(HAUT, BAS)), (0, y, L, y + 1))
logo = dessiner(grands, 20, fond=False).resize((150, 150), Image.LANCZOS)
cote.paste(logo, ((L - 150) // 2, 44), logo)
d = ImageDraw.Draw(cote)
police = lambda nom, t: ImageFont.truetype(str(Path('C:/Windows/Fonts') / nom), t)
for texte, f, y in (('Nuée', police('seguisb.ttf', 30), 196), ('ACP · PLS · OPLS', police('segoeui.ttf', 13), 240)):
    w = d.textlength(texte, font=f)
    d.text(((L - w) / 2, y), texte, font=f, fill=(255, 255, 255))
cote.save(sortie / 'installateur.bmp')
print('ok', sortie)
