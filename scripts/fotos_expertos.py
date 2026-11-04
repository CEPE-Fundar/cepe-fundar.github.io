#!/usr/bin/env python3
"""Prepara las fotos de la Red de Expertos nadIA para publicar.

Toma la carpeta (o el .zip) que descarga Google Drive con las fotos subidas
al formulario, cuyos archivos se llaman "<nombre original> - <Nombre de la persona>.<ext>",
y para cada persona de redes/expertos.yml:

  1. recorta la foto en cuadrado (centrada) y la achica a 400x400 px,
  2. la guarda como JPEG en redes/fotos/<slug>.jpg SIN metadatos
     (el EXIF puede traer ubicación GPS, modelo de cámara, fecha, etc.),
  3. actualiza el campo `foto:` de esa persona en redes/expertos.yml.

Si alguien no subió archivo pero puso una URL en el formulario ("Foto de Perfil
(Opción 2 - URL)"), se pasa con --url. Sirve un link directo a la imagen o una
página de perfil: en ese caso busca la foto dentro de la página. Las fotos que
en el yml todavía apuntan a una web externa también se bajan al repo.

Uso:
    python scripts/fotos_expertos.py [RUTA_A_CARPETA_O_ZIP] [--mapa "texto en archivo=Nombre en yml" ...]
                                     [--url "Nombre en yml=URL" ...]

Requiere Pillow:  python -m pip install Pillow
"""

import argparse
import re
import sys
import tempfile
import unicodedata
import urllib.parse
import urllib.request
import zipfile
from pathlib import Path

from PIL import Image, ImageOps

RAIZ = Path(__file__).resolve().parent.parent
YML = RAIZ / "redes" / "expertos.yml"
DESTINO = RAIZ / "redes" / "fotos"
LADO = 400
EXTENSIONES = {".jpg", ".jpeg", ".png", ".webp", ".heic", ".gif", ".bmp", ".tif", ".tiff"}


def normalizar(texto: str) -> str:
    texto = unicodedata.normalize("NFKD", texto)
    texto = "".join(c for c in texto if not unicodedata.combining(c))
    return re.sub(r"[^a-z0-9 ]+", " ", texto.lower()).strip()


def slug(texto: str) -> str:
    return re.sub(r"\s+", "-", normalizar(texto))


def nombres_del_yml() -> list[str]:
    return re.findall(r'^- nombre: "([^"]+)"', YML.read_text(encoding="utf-8"), flags=re.M)


def buscar_persona(nombre_archivo: str, nombres: list[str], mapa: dict[str, str]) -> str | None:
    """El formulario agrega " - Nombre" al final del archivo. Se busca la persona
    del yml que contenga todas las palabras de ese nombre (sin tildes ni mayúsculas)."""
    base = Path(nombre_archivo).stem
    for clave, nombre in mapa.items():
        if normalizar(clave) in normalizar(base):
            return nombre
    if " - " not in base:
        return None
    quien = set(normalizar(base.rsplit(" - ", 1)[1]).split())
    candidatos = [n for n in nombres if quien and quien <= set(normalizar(n).split())]
    return candidatos[0] if len(candidatos) == 1 else None


def procesar(origen: Path, destino: Path) -> None:
    with Image.open(origen) as img:
        img = ImageOps.exif_transpose(img)  # respeta la orientación antes de descartar el EXIF
        img = img.convert("RGB")
        img = ImageOps.fit(img, (LADO, LADO), method=Image.Resampling.LANCZOS, centering=(0.5, 0.4))
        # Imagen nueva sin metadatos: no se copia info, EXIF ni ICC del original.
        limpia = Image.frombytes("RGB", img.size, img.tobytes())
        limpia.save(destino, "JPEG", quality=85, optimize=True, progressive=True)


MAX_BYTES = 15 * 1024 * 1024
AGENTE = "Mozilla/5.0 (nadia.ar; foto de perfil)"


def _bajar(url: str) -> tuple[bytes, str]:
    req = urllib.request.Request(url, headers={"User-Agent": AGENTE})
    with urllib.request.urlopen(req, timeout=30) as r:
        datos = r.read(MAX_BYTES + 1)
        if len(datos) > MAX_BYTES:
            raise ValueError("archivo demasiado grande")
        return datos, r.headers.get("Content-Type", "")


def imagen_desde_url(url: str, nombre: str, carpeta: Path) -> Path:
    """Baja la foto de una URL del formulario. Si la URL es una página (perfil
    institucional, web personal), busca og:image o una <img> cuyo archivo
    contenga el apellido de la persona."""
    datos, tipo = _bajar(url)
    if "html" in tipo or datos[:200].lstrip().lower().startswith((b"<!doctype", b"<html")):
        html = datos.decode("utf-8", errors="ignore")
        apellidos = [p for p in normalizar(nombre).split()[1:] if len(p) > 2]
        srcs = re.findall(r'<img[^>]+src="([^"]+)"', html, flags=re.I)
        candidatas = [s for s in srcs if any(a in normalizar(Path(urllib.parse.urlparse(s).path).stem) for a in apellidos)]
        og = re.findall(r'<meta[^>]+property="og:image"[^>]+content="([^"]+)"', html, flags=re.I)
        elegida = (candidatas or og or [None])[0]
        if not elegida:
            raise ValueError("no encontré una foto en la página")
        datos, tipo = _bajar(urllib.parse.urljoin(url, elegida))
    archivo = carpeta / f"{slug(nombre)}.descarga"
    archivo.write_bytes(datos)
    with Image.open(archivo) as prueba:  # falla si no es una imagen
        prueba.verify()
    return archivo


def actualizar_yml(nombre: str, ruta_foto: str) -> bool:
    texto = YML.read_text(encoding="utf-8")
    patron = re.compile(r'(- nombre: "%s"[^\n]*\n(?:  [^\n]*\n)*?  foto: )"[^"]*"' % re.escape(nombre))
    nuevo, n = patron.subn(r'\1"%s"' % ruta_foto, texto, count=1)
    if n:
        YML.write_text(nuevo, encoding="utf-8", newline="\n")
    return bool(n)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("origen", type=Path, nargs="?", help="carpeta o .zip descargado de Drive (opcional)")
    ap.add_argument("--mapa", action="append", default=[],
                    help='asignación manual "texto en el nombre del archivo=Nombre en el yml"')
    ap.add_argument("--url", action="append", default=[],
                    help='foto desde la URL del formulario: "Nombre en el yml=URL" (imagen o página de perfil)')
    args = ap.parse_args()

    mapa = dict(m.split("=", 1) for m in args.mapa)
    urls = dict(u.split("=", 1) for u in args.url)
    nombres = nombres_del_yml()
    DESTINO.mkdir(parents=True, exist_ok=True)

    # Las fotos que hoy apuntan a una web externa también se bajan al repo.
    texto_yml = YML.read_text(encoding="utf-8")
    for nombre, url in re.findall(r'- nombre: "([^"]+)"[^\n]*\n(?:  [^\n]*\n)*?  foto: "(https?://[^"]+)"', texto_yml):
        urls.setdefault(nombre, url)

    with tempfile.TemporaryDirectory() as tmp:
        carpeta = args.origen
        if args.origen is None:
            carpeta = Path(tmp)
        elif args.origen.suffix.lower() == ".zip":
            with zipfile.ZipFile(args.origen) as z:
                for info in z.infolist():
                    if info.is_dir():
                        continue
                    nombre = info.filename
                    if not info.flag_bits & 0x800:  # Drive no marca UTF-8: Python lo leyó como cp437
                        try:
                            nombre = nombre.encode("cp437").decode("utf-8")
                        except UnicodeError:
                            pass
                    # Solo el nombre del archivo, sin carpetas (evita rutas raras dentro del zip).
                    (Path(tmp) / Path(nombre).name).write_bytes(z.read(info))
            carpeta = Path(tmp)
        archivos = [p for p in carpeta.rglob("*") if p.suffix.lower() in EXTENSIONES]

        sin_asignar = []
        for archivo in sorted(archivos):
            persona = buscar_persona(archivo.name, nombres, mapa)
            if not persona:
                sin_asignar.append(archivo.name)
                continue
            destino = DESTINO / f"{slug(persona)}.jpg"
            procesar(archivo, destino)
            ok = actualizar_yml(persona, f"fotos/{destino.name}")
            print(f"OK  {persona:35s} <- {archivo.name}" + ("" if ok else "  (no se encontró en el yml)"))
            urls.pop(persona, None)  # la foto subida al form tiene prioridad sobre la URL

        bajadas = Path(tmp) / "_urls"
        bajadas.mkdir(exist_ok=True)
        for persona, url in urls.items():
            if persona not in nombres:
                print(f"--  {persona:35s} no está en el yml")
                continue
            try:
                archivo = imagen_desde_url(url, persona, bajadas)
                destino = DESTINO / f"{slug(persona)}.jpg"
                procesar(archivo, destino)
                actualizar_yml(persona, f"fotos/{destino.name}")
                print(f"OK  {persona:35s} <- {url}")
            except Exception as e:  # una URL caída no frena al resto
                print(f"ERR {persona:35s} <- {url}  ({e})")

    if sin_asignar:
        print("\nSin asignar (usar --mapa \"texto=Nombre\"):")
        for nombre in sin_asignar:
            print("   " + nombre)
    return 0


if __name__ == "__main__":
    sys.exit(main())
