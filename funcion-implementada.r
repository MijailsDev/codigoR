# Función para calcular la amplitud (W) redondeando por exceso
compute_amplitude <- function(x, n_classes) {
  x <- na.omit(x)
  R  <- max(x) - min(x)
  W0 <- R / n_classes
  
  # Comprobar si todos los valores son enteros
  is_discrete <- all(x == floor(x))
  
  if (is_discrete) {
    # Variable discreta: redondeo por exceso a entero
    W <- ceiling(W0)
  } else {
    # Variable continua: redondeo por exceso a 4 decimales
    W <- ceiling(W0 * 1e4) / 1e4
  }
  return(W)
}

# Leer el archivo CSV
data <- read.csv("C:\\Users\\dell\\Desktop\\student-mat.csv", header = TRUE)

# Extraer las columnas de calificaciones G1, G2, G3
calificaciones <- data[, c("G1", "G2", "G3")]

# Función para analizar las calificaciones (tabla y gráficos)
analyze_grades <- function(grades, grade_name) {
  # Limpiar NAs
  grades_cleaned <- na.omit(grades)
  n <- length(grades_cleaned)
  
  # Rango y número de clases
  R <- max(grades_cleaned) - min(grades_cleaned)
  C <- round(3.3 * log10(n) + 1)
  
  # Amplitud redondeada por exceso
  W <- compute_amplitude(grades_cleaned, C)
  
  # Ajuste de punto medio y límite inferior
  PM   <- round((W * C - R) / 2, 4)
  Finf <- min(grades_cleaned) - PM
  
  # Construir cortes (breaks) dinámicos
  breaks <- seq(Finf, max(grades_cleaned) + W, by = W)
  
  # Histograma sin graficar para obtener datos
  hist_data <- hist(grades_cleaned, breaks = breaks, plot = FALSE)
  
  # Tabla de frecuencias desde hist_data
  freq_table_df <- data.frame(
    Intervalo = paste(head(hist_data$breaks, -1),
                      hist_data$breaks[-1],
                      sep = " - "),
    Frecuencia            = hist_data$counts,
    Frecuencia_Acumulada  = cumsum(hist_data$counts),
    Porcentaje            = round((hist_data$counts / n) * 100, 2)
  )
  cat("\nTabla de frecuencias para", grade_name, ":\n")
  print(freq_table_df)
  
  # Resumen estadístico
  cat("\nResumen estadístico para", grade_name, ":\n")
  print(summary(grades_cleaned))
  
  # Puntos para polígono y ojiva
  poly_midpoints   <- c(hist_data$mids[1] - W,
                        hist_data$mids,
                        hist_data$mids[length(hist_data$mids)] + W)
  poly_counts      <- c(0, hist_data$counts, 0)
  cumulative_freq  <- cumsum(hist_data$counts)
  
  # Colores vivos y distintos para cada barra
  bar_colors <- rainbow(length(hist_data$counts))
  
  # Dibujar los 3 gráficos
  par(ps = 7, mex = 0.3, mfrow = c(1, 3))
  
  # 1) Histograma con valores
  h1 <- hist(grades_cleaned, breaks = breaks,
             col = bar_colors, border = "black",
             main = paste("Histograma de", grade_name),
             xlab = "Calificación", ylab = "Frecuencia")
  text(x = h1$mids, y = h1$counts,
       labels = h1$counts, pos = 3, cex = 0.8)
  
  # 2) Polígono de frecuencia con valores
  h2 <- hist(grades_cleaned, breaks = breaks,
             col = bar_colors, border = "black",
             main = paste("Polígono de frecuencia de", grade_name),
             xlab = "Calificación", ylab = "Frecuencia",
             xlim = range(poly_midpoints))
  lines(poly_midpoints, poly_counts, type = "o",
        col = "orange", lwd = 2)
  text(x = h2$mids, y = h2$counts,
       labels = h2$counts, pos = 3, cex = 0.8)
  
  # 3) Ojiva (frecuencia acumulada)
  plot(hist_data$mids, cumulative_freq,
       type = "o", col = "darkgreen",
       main = paste("Ojiva de", grade_name),
       xlab = "Calificación",
       ylab = "Frecuencia Acumulada")
}

# Ejecutar el análisis para G1, G2 y G3
analyze_grades(calificaciones$G1, "G1")
analyze_grades(calificaciones$G2, "G2")
analyze_grades(calificaciones$G3, "G3")
