# Función para calcular la amplitud (W) redondeando por exceso
calcular_amplitud <- function(datos, num_clases) {
  datos <- na.omit(datos)
  rango <- max(datos) - min(datos)
  amplitud_sin_redondeo <- rango / num_clases
  
  # Detectar variable discreta (todos los valores son enteros)
  es_discreta <- all(datos == floor(datos))
  
  if (es_discreta) {
    # Discreta: redondear al entero superior
    W <- ceiling(amplitud_sin_redondeo)
  } else {
    # Continua: redondear al superior con 4 decimales
    W <- ceiling(amplitud_sin_redondeo * 1e4) / 1e4
  }
  return(W)
}

# Cargar datos desde CSV
ruta_archivo <- "C:\\Users\\dell\\Desktop\\student-mat.csv"
datos_estudiantes <- read.csv(ruta_archivo, header = TRUE)

# Seleccionar calificaciones G1, G2 y G3
calificaciones <- datos_estudiantes[, c("G1", "G2", "G3")]

# Función principal para análisis y gráficos de una calificación
analizar_calificacion <- function(puntuaciones, nombre_nota) {
  # Eliminar NA y preparar
  puntuaciones <- na.omit(puntuaciones)
  total_alumnos <- length(puntuaciones)
  
  # Calcular número de clases
  num_clases <- round(3.3 * log10(total_alumnos) + 1)
  # Calcular amplitud W con redondeo adecuado
  W <- calcular_amplitud(puntuaciones, num_clases)
  # Ajuste de punto medio y límite inferior
  punto_medio_ajuste <- round((W * num_clases - (max(puntuaciones) - min(puntuaciones))) / 2, 4)
  limite_inferior <- min(puntuaciones) - punto_medio_ajuste
  
  # Generar cortes (breaks)
  cortes <- seq(limite_inferior, max(puntuaciones) + W, by = W)
  
  # Crear histograma sin graficar
  histograma <- hist(puntuaciones, breaks = cortes, plot = FALSE)
  
  # Construir tabla de frecuencias
  tabla_frecuencias <- data.frame(
    Intervalo           = paste(head(histograma$breaks, -1),
                                histograma$breaks[-1],
                                sep = " - "),
    Frecuencia          = histograma$counts,
    FrecuenciaAcumulada = cumsum(histograma$counts),
    Porcentaje          = round((histograma$counts / total_alumnos) * 100, 2)
  )
  cat("\nTabla de frecuencias para", nombre_nota, ":\n")
  print(tabla_frecuencias)
  
  # Resumen estadístico (min, Q1, mediana, media, Q3, max)
  cat("\nResumen estadístico para", nombre_nota, ":\n")
  print(summary(puntuaciones))
  
  # Preparar polígono y ojiva
  mids     <- histograma$mids
  pol_x    <- c(mids[1] - W, mids, mids[length(mids)] + W)
  pol_y    <- c(0, histograma$counts, 0)
  acum_y   <- cumsum(histograma$counts)
  
  # Colores vivos distintos por barra
  colores_barras <- rainbow(length(histograma$counts))
  
  # Dibujar gráficos
  par(ps = 7, mex = 0.3, mfrow = c(1, 3))
  
  # 1) Histograma
  h1 <- hist(puntuaciones, breaks = cortes,
             col = colores_barras, border = "black",
             main = paste("Histograma", nombre_nota),
             xlab = "Puntuación", ylab = "Frecuencia")
  text(x = h1$mids, y = h1$counts,
       labels = h1$counts, pos = 3, cex = 0.8)
  
  # 2) Polígono de frecuencia
  h2 <- hist(puntuaciones, breaks = cortes,
             col = colores_barras, border = "black",
             main = paste("Polígono", nombre_nota),
             xlab = "Puntuación", ylab = "Frecuencia",
             xlim = range(pol_x))
  lines(pol_x, pol_y, type = "o", col = "orange", lwd = 2)
  text(x = h2$mids, y = h2$counts,
       labels = h2$counts, pos = 3, cex = 0.8)
  
  # 3) Ojiva (frecuencia acumulada)
  plot(mids, acum_y, type = "o", col = "darkgreen",
       main = paste("Ojiva", nombre_nota),
       xlab = "Puntuación", ylab = "Frecuencia Acumulada")
}

# Ejecutar análisis para G1, G2 y G3
analizar_calificacion(calificaciones$G1, "G1")
analizar_calificacion(calificaciones$G2, "G2")
analizar_calificacion(calificaciones$G3, "G3")
