# Leer el archivo CSV
data <- read.csv("C:\\Users\\dell\\Desktop\\student-mat.csv", header = TRUE)

# Extraer las columnas de calificaciones G1, G2, G3
calificaciones <- data[, c("G1", "G2", "G3")]

# Función para analizar las calificaciones (tabla de frecuencias y gráficos)
analyze_grades <- function(grades, grade_name) {
  # Eliminar valores nulos
  grades_cleaned <- na.omit(grades)

  # Cálculos estadísticos
  n <- length(grades_cleaned)
  R <- max(grades_cleaned) - min(grades_cleaned)
  C <- round(3.3 * log10(n) + 1)  # Número de clases
  W <- ceiling(R / C)             # Ancho de clase (amplitud)
  PM <- round((W * C - R) / 2)    # Punto medio de ajuste
  Finf <- min(grades_cleaned) - PM  # Límite inferior

  # Crear los intervalos para la tabla de frecuencias
  bins <- seq(Finf, max(grades_cleaned) + W, W)
  freq_table <- cut(grades_cleaned, bins, right = FALSE)
  freq_counts <- table(freq_table)

  # Construcción de la tabla de frecuencias
  intervals <- as.character(bins[-length(bins)])  # Intervalos de clases
  freq_table_df <- data.frame(
    Intervalo = intervals,
    Frecuencia = as.vector(freq_counts),
    Frecuencia_Acumulada = cumsum(as.vector(freq_counts)),
    Porcentaje = round((as.vector(freq_counts) / n) * 100, 2)
  )

  # Mostrar tabla de frecuencias
  print(paste("Tabla de frecuencias para", grade_name, ":"))
  print(freq_table_df)

  # Mostrar resumen estadístico
  print(paste("Resumen estadístico para", grade_name, ":"))
  print(summary(grades_cleaned))

  # Preparar los intervalos para el gráfico
  breakpoints <- bins

  # Obtener el histograma sin graficar para alinear el polígono
  hist_data <- hist(grades_cleaned, breaks = breakpoints, plot = FALSE)

  # Calcular el primer y último punto medio
  first_midpoint <- hist_data$mids[1]
  last_midpoint <- hist_data$mids[length(hist_data$mids)]

  # Extender los puntos para que el polígono se inicie y termine según la amplitud
  poly_midpoints <- c(first_midpoint - W, hist_data$mids, last_midpoint + W)
  poly_counts <- c(0, hist_data$counts, 0)

  # Cálculo de la frecuencia acumulada para la ojiva
  cumulative_freq <- cumsum(as.vector(freq_counts))

  # Crear un vector de colores diferentes para cada barra
  bar_colors <- rainbow(length(freq_counts))

  # Graficar en el orden solicitado
  par(ps = 7, mex = 0.3, mfrow = c(1, 3))

  # 1. Histograma
  hist(
    grades_cleaned, breaks = breakpoints,
    col = bar_colors, border = "black",
    main = paste("Histograma de Calificación", grade_name),
    xlab = "Intervalos de Calificación",
    ylab = "Número de Estudiantes",
    sub = "Elaboración propia"
  )

  # 2. Histograma con polígono de frecuencia superpuesto
  hist(
    grades_cleaned, breaks = breakpoints,
    col = bar_colors, border = "black",
    main = paste("Polígono de Frecuencia de Calificación", grade_name),
    xlab = "Intervalos de Calificación",
    ylab = "Número de Estudiantes",
    sub = "Elaboración propia",
    xlim = c(poly_midpoints[1], poly_midpoints[length(poly_midpoints)])
  )
  lines(poly_midpoints, poly_counts, type = "o", col = "orange", lwd = 2)

  # 3. Ojiva (Frecuencia Acumulada)
  plot(
    hist_data$mids, cumulative_freq,
    type = "o", col = "green",
    main = paste("Ojiva de Calificación", grade_name),
    xlab = "Intervalos de Calificación",
    ylab = "Número de Estudiantes Acumulados",
    sub = "Elaboración propia"
  )
}

# Realizar el análisis para G1, G2 y G3
analyze_grades(calificaciones$G1, "G1")
analyze_grades(calificaciones$G2, "G2")
analyze_grades(calificaciones$G3, "G3")
