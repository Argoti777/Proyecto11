dataset <- read.csv("winequality-red.csv")

alcohol <- dataset$alcohol

# Tabla de Frecuencias
lista <- hist(alcohol, plot = FALSE)
tablaFreq <- table.freq(lista)

# Histograma
histograma <- hist(alcohol, xlab = "ALCOHOL", ylab = "FRECUENCIA",
                   col = "#6666FF", main = "Contenido de alcohol en los vinos")
print("Este histograma indica que en esta muestra de datos de 'Vinho verde' existen una concentración de vinos con un contenido de alcohol moderado la mayoria se encuentra entre 9.5 y 10.5, mientras que los vinos con contenido de alcohol mas bajo o mas alto son menos comunes.")

# Polígono de Frecuencias
plot(density(alcohol), main = "Polígono de Frecuencia", xlab = "Alcohol",
     ylab = "Densidad", col = "#6666FF", lwd = 3)
print("Este polígono de frecuencias indica que el contenido de alcohol más común en los vinos está alrededor del 9.8, con una distribución asimétrica hacia la izquierda, lo que sugiere que hay menos vinos con contenido de alcohol muy alto.")

# Gráfico de Pastel
dioxido<-dataset$fixed.acidity
alcoholV<-alcohol[dioxido]
colores<- c("#FF6666", "#66FF66", "#6666FF", "#FFFF66", "#FF66FF", "#66FFFF", "#FFCC66", "#996699")
# Ver los colores
pie(table(alcoholV), main = "Calidad de los vinos", col = colores)
print("Este grafico de pastel nos indica que la mayoria de los vinos tienen un porcentaje de alcohol de 9 a 10, siendo el mas comun el de 9.4")
legend("bottomright", legend = levels(as.factor(alcoholV)), fill = colores)

# Media
media <- function(vector) {
  return(mean(vector))
}
paste("El promedio de contenido de alcohol en los vinos es:", media(alcohol))

# Mediana
mediana <- function(vector) {
  x <- sort(vector)
  return(median(x))
}
paste("La mediana del contenido de alcohol en los vinos es:", mediana(alcohol))

# Moda
moda <- function(vector) {
  return(as.numeric(names(which.max(table(vector)))))
}
paste("La moda del contenido de alcohol en los vinos es:", moda(alcohol))

# Rango
rango <- function(calificaciones) {
  r <- max(calificaciones) - min(calificaciones)
  return(r)
}
paste("El rango del contenido de alcohol en los vinos es:", rango(alcohol))

# Varianza y Desviación Estándar
varianza <- function(vector) {
  return(var(vector, na.rm = FALSE))
}
desviacionEstandar <- function(vector) {
  return(sd(vector))
}
paste("La varianza del contenido de alcohol en los vinos es:", varianza(alcohol))
paste("La desviación estándar del contenido de alcohol en los vinos es:", desviacionEstandar(alcohol))
print("Estos datos nos sugieren que la dispersión de los datos alrededor de la media no es muy amplia, la mayoría de los datos se encuentran dentro de un rango relativamente cercano a la media.")

# Coeficiente de Variación
coeficiente <- function(desviacion, media) {
  return((desviacion / media) * 100)
}
paste("El coeficiente de variación del contenido de alcohol en los vinos es:", coeficiente(desviacionEstandar(alcohol), media(alcohol)), "%")

# Cuartiles, Deciles y Percentiles
cuartiles <- function(vector) {
  q <- quantile(vector, probs = c(0.25, 0.50, 0.75))
  return(q)
}
cuartiles(alcohol)

deciles <- function(vector) {
  d <- quantile(vector, probs = seq(0.1, 0.9, by = 0.1))
  return(d)
}
deciles(alcohol)

percentiles <- function(vector) {
  p <- quantile(vector, probs = seq(0.01, 0.99, by = 0.01))
  return(p)
}
percentiles(alcohol)

# Diagrama de Cajas
boxplot(alcohol, main = "Contenido de alcohol en los vinos", ylab = "Alcohol", col = "purple")
print("El diagrama de cajas nos da una idea de cómo está distribuido el contenido de alcohol en los vinos. La línea en el medio representa la mediana, mientras que la longitud de la caja nos dice cuánto varía el contenido de alcohol. La forma de la distribución nos dice si la mayoría de los vinos tienen un contenido de alcohol moderado o si hay más extremos. En general, parece que la mayoría de los vinos tienen un contenido de alcohol alrededor de 10.")

# Diagrama de Pareto
pareto.chart(
  alcohol[1:10], 
  ylab = "Frecuencias", 
  col = heat.colors(length(alcohol[1:10])),
  cumperc = seq(0, 100, by = 20),
  ylab2 = "Porcentaje acumulado",
  main = "Contenido de alcohol en los vinos"
)
print("El bloque J es el que representa la mayoría de problemas, por lo tanto hay que priorizar la mejora de ese conjunto")
