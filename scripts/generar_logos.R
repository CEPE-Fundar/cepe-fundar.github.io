# install.packages("magick")   # si aún no lo tenés
library(magick)




combine_logos <- function(left,
                          right,
                          out = "logos_combinados.png",
                          height = 200,
                          gap = 24,
                          pad = 24,
                          bg = "transparent",         # "transparent", "white", "#FFFFFF", etc.
                          divider = FALSE,
                          divider_thickness = 2,
                          divider_color = "#000000") {
  
  # leer imágenes
  img_l <- image_read(left)
  img_r <- image_read(right)
  
  # recortar bordes transparentes
  img_l <- image_trim(img_l)
  img_r <- image_trim(img_r)
  
  # asegurar fondo transparente para conservar alfa
  img_l <- image_background(img_l, "none")
  img_r <- image_background(img_r, "none")
  
  # escalar por altura (manteniendo proporción de ancho)
  img_l <- image_scale(img_l, paste0("x", height))
  img_r <- image_scale(img_r, paste0("x", height))
  
  # construir separadores
  # "none" = transparente en magick
  spacer_color <- "none"
  
  # si hay divisor, partimos el gap en: spacer_left + divider + spacer_right
  if (divider) {
    if (gap < divider_thickness) {
      warning("gap < divider_thickness; ajustando divider_thickness = gap")
      divider_thickness <- gap
    }
    # repartimos el resto a ambos lados del divisor
    rem <- max(0, gap - divider_thickness)
    spacer_left_w  <- floor(rem / 2)
    spacer_right_w <- rem - spacer_left_w
    
    spacer_left  <- if (spacer_left_w  > 0) image_blank(spacer_left_w,  height, color = spacer_color) else NULL
    divider_img  <- image_blank(divider_thickness, height, color = divider_color)
    spacer_right <- if (spacer_right_w > 0) image_blank(spacer_right_w, height, color = spacer_color) else NULL
    
    parts <- c(list(img_l),
               if (!is.null(spacer_left))  list(spacer_left)  else list(),
               list(divider_img),
               if (!is.null(spacer_right)) list(spacer_right) else list(),
               list(img_r))
  } else {
    # gap simple sin divisor
    spacer <- if (gap > 0) image_blank(gap, height, color = spacer_color) else NULL
    parts <- c(list(img_l), if (!is.null(spacer)) list(spacer) else list(), list(img_r))
  }
  
  # append horizontal
  combined <- image_append(image_join(parts), stack = FALSE)
  
  # padding externo (border). Si bg = "transparent", usamos "none" para borde transparente.
  border_color <- if (tolower(bg) %in% c("transparent", "none")) "none" else bg
  if (pad > 0) {
    combined <- image_border(combined, color = border_color, geometry = paste0(pad, "x", pad))
  }
  
  # si el fondo NO es transparente y querés fondo sólido detrás de todo, extiende el canvas
  # (esto se encarga automáticamente con image_border; si además querés rellenar posibles transparencias internas:)
  if (!(tolower(bg) %in% c("transparent", "none"))) {
    combined <- image_background(combined, color = bg, flatten = TRUE)
  }
  
  image_write(combined, out)
  message(sprintf("Guardado: %s (%dx%d px aprox.)", out,
                  image_info(combined)$width, image_info(combined)$height))
  invisible(out)
}

combine_logos(
  left   = "extras/logo_cepe.png",
  right  = "extras/logo_fundar.png",
  out    = "logos_combinados.png",
  height = 180,
  gap    = 40,
  pad    = 20,
  bg     = "transparent",
  divider = FALSE
)

