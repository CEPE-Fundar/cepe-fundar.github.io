#!/usr/bin/env python3
"""Chequeo de datos personales antes de publicar.

Busca correos electrónicos y teléfonos en los archivos del repositorio
(que es público) y falla si encuentra alguno que no esté en la lista de
contactos institucionales permitidos. También verifica que los datos de la
Red de Expertos (redes/*.yml) no tengan campos de contacto.

Uso:
    python scripts/check_datos_personales.py              # todo el repo
    python scripts/check_datos_personales.py --staged     # solo lo que se va a commitear
    python scripts/check_datos_personales.py archivo ...  # archivos puntuales

Lo corren el hook de pre-commit (.githooks/pre-commit) y GitHub Actions
(.github/workflows/datos-personales.yml).
"""

import re
import subprocess
import sys
from pathlib import Path

# Contactos institucionales que sí pueden aparecer publicados.
CORREOS_PERMITIDOS = {
    "datos@fund.ar",
    "cepe@utdt.edu",
}

# Carpetas y extensiones que no se revisan (librerías de Quarto, binarios).
EXCLUIR_PREFIJOS = ("docs/site_libs/", ".git/", ".quarto/")
EXTENSIONES_BINARIAS = {
    ".png", ".jpg", ".jpeg", ".webp", ".gif", ".svg", ".ico", ".pdf",
    ".woff", ".woff2", ".ttf", ".eot", ".zip", ".xlsx", ".docx", ".pptx",
}

CORREO = re.compile(r"[A-Za-z0-9._%+-]+@[A-Za-z0-9-]+(?:\.[A-Za-z0-9-]+)*\.[A-Za-z]{2,}")
# Sufijos de imágenes retina tipo "logo@2x.png" no son correos.
FALSO_CORREO = re.compile(r"@\d+(\.\d+)?x\.", re.IGNORECASE)
# Teléfonos con prefijo internacional (ej.: "+" seguido de código de país y número).
TELEFONO_INTL = re.compile(r"(?<![\w/=])\+\d[\d\s().-]{7,18}\d")
# Enlaces tel: o de WhatsApp con un número (los mailto: ya los detecta CORREO).
ENLACE_CONTACTO = re.compile(r"(tel:\+?\d|wa\.me/\d|api\.whatsapp\.com/send\?phone=\d)", re.IGNORECASE)
# Campos que no pueden existir en los datos de la red.
CAMPO_CONTACTO = re.compile(
    r"^\s*-?\s*(correo|mail|e-?mail|tel[eé]fono|telefono|phone|celular|whatsapp)\s*:",
    re.IGNORECASE,
)


def archivos_del_repo(staged: bool) -> list[str]:
    if staged:
        cmd = ["git", "diff", "--cached", "--name-only", "-z", "--diff-filter=ACMR"]
    else:
        cmd = ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"]
    salida = subprocess.run(cmd, capture_output=True, check=True)
    return [r for r in salida.stdout.decode("utf-8").split("\x00") if r]


def se_revisa(ruta: str) -> bool:
    ruta = ruta.replace("\\", "/")
    if ruta.startswith(EXCLUIR_PREFIJOS):
        return False
    return Path(ruta).suffix.lower() not in EXTENSIONES_BINARIAS


def revisar(ruta: str) -> list[str]:
    problemas = []
    try:
        texto = Path(ruta).read_text(encoding="utf-8", errors="ignore")
    except OSError:
        return problemas
    es_datos_red = ruta.replace("\\", "/").startswith("redes/") and ruta.endswith((".yml", ".yaml", ".csv", ".json"))
    for n, linea in enumerate(texto.splitlines(), 1):
        for m in CORREO.finditer(linea):
            correo = m.group(0)
            if correo.lower() in CORREOS_PERMITIDOS or FALSO_CORREO.search(correo):
                continue
            problemas.append(f"{ruta}:{n}: correo electrónico ({correo})")
        for m in TELEFONO_INTL.finditer(linea):
            problemas.append(f"{ruta}:{n}: posible teléfono ({m.group(0).strip()})")
        if ENLACE_CONTACTO.search(linea):
            problemas.append(f"{ruta}:{n}: enlace de contacto directo (tel/WhatsApp)")
        if es_datos_red and CAMPO_CONTACTO.search(linea):
            problemas.append(f"{ruta}:{n}: campo de contacto en los datos de la red")
    return problemas


def main(argv: list[str]) -> int:
    if hasattr(sys.stdout, "reconfigure"):
        sys.stdout.reconfigure(encoding="utf-8")
    staged = "--staged" in argv
    explicitos = [a for a in argv if not a.startswith("--")]
    rutas = explicitos or archivos_del_repo(staged)
    problemas = []
    for ruta in rutas:
        if se_revisa(ruta):
            problemas.extend(revisar(ruta))
    if problemas:
        print("Se encontraron posibles datos personales. No se publica hasta revisarlos:\n")
        for p in problemas:
            print("  - " + p)
        print(
            "\nSi es un contacto institucional que puede publicarse, agregalo a "
            "CORREOS_PERMITIDOS en scripts/check_datos_personales.py."
        )
        return 1
    print(f"OK: sin correos ni teléfonos personales ({len(rutas)} archivos revisados).")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
