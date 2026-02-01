# nadIA - Nodo Argentino de Inteligencia Artificial

Sitio web de nadIA, una iniciativa de **CEPE-Di Tella** y **Fundar** dedicada a investigación, datos y políticas públicas sobre inteligencia artificial para el desarrollo en Argentina y América Latina.

## 🚀 Tecnología

Proyecto construido con:
- **HTML5** puro
- **CSS3** moderno (variables, grid, flexbox, animaciones, glassmorphism)
- **JavaScript Vanilla** (ES6+)
- **Sin dependencias externas** ni frameworks
- **Sin proceso de build** - funciona directo en el navegador

## 📁 Estructura

```
/
├── index.html              # Home con manifiesto
├── blog.html               # Listado de opiniones
├── investigacion.html      # Proyectos de investigación
├── nosotros.html           # Equipo y misión
├── investigacion/          # Páginas de investigación
│   ├── encuesta-individuos.html
│   └── encuesta-pymes.html
├── posts/                  # Artículos de opinión
│   ├── _TEMPLATE.html      # Template para nuevos posts
│   └── [13 posts].html     # ✅ TODOS MIGRADOS
├── assets/
│   ├── css/
│   │   └── main.css       # Estilos principales
│   └── js/
│       └── main.js        # JavaScript principal
└── images/                # Recursos visuales
```

## 🎨 Características

- ✨ Diseño moderno con animaciones fluidas
- 🎯 Hero section impactante
- 📱 Totalmente responsive
- ♿ Accesible (semantic HTML, ARIA labels)
- ⚡ Performance optimizado
- 🎭 Glassmorphism effects
- 📊 Scroll progress indicator
- ⬆️ Back to top button
- 🔍 SEO optimizado

## 🛠️ Desarrollo Local

1. Clona el repositorio
2. Abre `index.html` en tu navegador
3. O usa un servidor local:

```bash
# Python
python -m http.server 8000

# Node.js (http-server)
npx http-server

# PHP
php -S localhost:8000
```

4. Visita `http://localhost:8000`

## 📝 Agregar Contenido

### Nuevo Post de Opinión

1. Copia `posts/_TEMPLATE.html` con el nuevo nombre
2. Reemplaza: TÍTULO, CATEGORÍA, FECHA, AUTOR, CONTENIDO
3. Agrégalo al listado en `blog.html`

### Nueva Investigación

1. Crea un archivo en `investigacion/nombre-investigacion.html`
2. Usa una de las investigaciones existentes como template
3. Agrégalo al grid en `investigacion.html`

## 🤝 Contribuir

Este proyecto es mantenido por CEPE-Di Tella y Fundar. Para contribuciones, contacta al equipo.

## 📄 Licencia

© 2026 CEPE-Di Tella y Fundar. Todos los derechos reservados.

---

**nadIA** - Inteligencia Artificial para el Desarrollo
