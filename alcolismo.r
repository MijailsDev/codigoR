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
  W <- ceiling(R / C)  # Ancho de clase
  PM <- round((W * C - R) / 2)  # Punto medio de ajuste
  Finf <- min(grades_cleaned) - PM  # Límite inferior

  # Crear los intervalos para la tabla de frecuencias
  bins <- seq(Finf, max(grades_cleaned) + W, W)
  freq_table <- cut(grades_cleaned, bins, right = FALSE)
  freq_counts <- table(freq_table)

  # Construcción de la tabla de frecuencias
  intervals <- as.character(bins[-length(bins)]) # Intervalos de clases
  freq_table_df <- data.frame(
    Intervalo = intervals,
    Frecuencia = as.vector(freq_counts),
    Frecuencia_Acumulada = cumsum(as.vector(freq_counts)),
    Porcentaje = round((as.vector(freq_counts) / n) * 100, 2)
  )

  # Mostrar tabla de frecuencias
  print(paste("Tabla de frecuencias para", grade_name, ":"))
  print(freq_table_df)

  # Preparar los intervalos para el gráfico
  breakpoints <- bins
  
  # Gráficos
  par(ps = 7, mex = 0.3, mfrow = c(1, 3))

  # Gráfico 1: Histograma
  hist(grades_cleaned, breaks = breakpoints, col = 'skyblue', border = 'black',
       main = paste("Histograma de", grade_name),
       xlab = "Intervalos de Calificación", ylab = "Frecuencia Absoluta")

  # Gráfico 2: Ojiva
  cumulative_freq <- cumsum(freq_counts)
  plot(breakpoints[-length(breakpoints)] + W / 2, cumulative_freq, type = "o", col = "green",
       main = paste("Ojiva de", grade_name), xlab = "Límites Superiores de Intervalos", ylab = "Frecuencia Acumulada")

  # Gráfico 3: Polígono de Frecuencia
# Obtener los puntos medios de los intervalos
midpoints <- breakpoints[-length(breakpoints)] + W / 2

# Graficar el polígono de frecuencia
plot(midpoints, freq_counts, type = "o", col = "orange", lwd = 2, 
     main = paste("Polígono de Frecuencia de", grade_name),
     xlab = "Intervalos de Calificación", ylab = "Frecuencia Absoluta")

}

# Realizar el análisis para G1, G2 y G3
analyze_grades(calificaciones$G1, "G1")
analyze_grades(calificaciones$G2, "G2")
analyze_grades(calificaciones$G3, "G3")
