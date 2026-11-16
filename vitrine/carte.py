# The background of the page's map: IGN's Plan IGN, one of the two backgrounds Nuée's Map view
# offers (app/interface/vues_carte.js), between Blois and Orléans. Twelve tiles at zoom 11 are
# fetched once from the Géoplateforme and stitched into src/assets/carte.webp, which the site
# serves itself: a reader's browser asks IGN for nothing. Also writes src/carte.json, the place
# of these tiles, which donnees.R reads to put the sites on the picture.
#   python vitrine/carte.py        (needs Pillow; then Rscript vitrine/donnees.R, and build again)
# Plan IGN: © IGN, under the Licence Ouverte 2.0 (Etalab); the page says "© IGN – Plan IGN".
#
# To show another place: change ZOOM and the first tile (COLONNE, LIGNE), and the sites' longitudes
# and latitudes in donnees.R (made up, like their counts).

import io
import json
import os
import urllib.request

from PIL import Image

ZOOM = 11
COLONNE, LIGNE = 1031, 712   # the top left tile (Web Mercator, as OpenStreetMap numbers them)
COLONNES, LIGNES = 4, 3      # 1024 × 768 pixels, 51 m a pixel at this latitude
TUILE = ('https://data.geopf.fr/wmts?SERVICE=WMTS&REQUEST=GetTile&VERSION=1.0.0&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2'
         '&STYLE=normal&FORMAT=image/png&TILEMATRIXSET=PM&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}')

ici = os.path.dirname(os.path.abspath(__file__))
image = Image.new('RGB', (256 * COLONNES, 256 * LIGNES))
for i in range(COLONNES):
    for j in range(LIGNES):
        demande = urllib.request.Request(TUILE.format(z=ZOOM, x=COLONNE + i, y=LIGNE + j), headers={'User-Agent': 'static-map-build/1.0'})
        with urllib.request.urlopen(demande, timeout=30) as reponse:
            image.paste(Image.open(io.BytesIO(reponse.read())).convert('RGB'), (256 * i, 256 * j))
sortie = os.path.join(ici, 'src', 'assets', 'carte.webp')
image.save(sortie, 'WEBP', quality=82, method=6)
with open(os.path.join(ici, 'src', 'carte.json'), 'w', encoding='utf-8', newline='\n') as f:
    json.dump({'zoom': ZOOM, 'colonne': COLONNE, 'ligne': LIGNE, 'colonnes': COLONNES, 'lignes': LIGNES}, f)
    f.write('\n')
print(f'{sortie}: {image.width} x {image.height}, {os.path.getsize(sortie)} bytes')
