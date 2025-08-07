# Leer el archivo CSV
data <- read.csv("C:\\Users\\dell\\Desktop\\student-mat.csv", header = TRUE)

# Extraer las columnas de calificaciones G1, G2, G3
calificaciones <- data[, c("G1", "G2", "G3")]

# Definir intervalos fijos: 10 clases de ancho 2, de 0 a 20
fixed_breaks <- seq(0, 20, by = 2)

# Función modificada: recibe también los breaks fijos
analyze_grades <- function(grades, grade_name, breaks) {
  grades_cleaned <- na.omit(grades)
  n <- length(grades_cleaned)

  # Generar histograma (sin graficar) con los breaks fijos
  hist_data <- hist(grades_cleaned, breaks = breaks, plot = FALSE)

  # Construir tabla de frecuencias desde hist_data
  freq_table_df <- data.frame(
    Intervalo = paste(head(hist_data$breaks, -1),
                      hist_data$breaks[-1],
                      sep = " - "),
    Frecuencia = hist_data$counts,
    Frecuencia_Acumulada = cumsum(hist_data$counts),
    Porcentaje = round((hist_data$counts / n) * 100, 2)
  )
  print(paste("Tabla de frecuencias para", grade_name, ":"))
  print(freq_table_df)

  # Resumen estadístico
  print(paste("Resumen estadístico para", grade_name, ":"))
  print(summary(grades_cleaned))

  # Preparar datos para polígono y ojiva
  W <- breaks[2] - breaks[1]
  poly_midpoints <- c(hist_data$mids[1] - W,
                      hist_data$mids,
                      hist_data$mids[length(hist_data$mids)] + W)
  poly_counts    <- c(0, hist_data$counts, 0)
  cumulative_freq <- cumsum(hist_data$counts)

  # Paleta de colores vivos y distintos
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

# Ejecutar con los breaks fijos para G1, G2 y G3
analyze_grades(calificaciones$G1, "G1", fixed_breaks)
analyze_grades(calificaciones$G2, "G2", fixed_breaks)
analyze_grades(calificaciones$G3, "G3", fixed_breaks)
