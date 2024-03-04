![](https://itq.edu.ec/wp-content/uploads/2023/02/Recurso-6.png)

Datos  |Descripcion  |
------------- | -------------
Docente  | Ing. Sebastián Landázuri  |
Asignatura  | Base de Datos no relacionales    |
Carrera  | Desarollo de Software  | 
Nivel: | Tercer Nivel |
Estudiantes  | Bryan Velasco, Alejandro Argoti, Alexis Velasco , Gabriel Salazar    |


# Proyecto Estadistica descriptiva

## Dataset de vinos

## Descripción:

Los dos conjuntos de datos están relacionados con las variantes tinta y blanca del vino portugués
"Vinho Verde". Para más detalles, consultar la referencia [Cortez et al., 2009]. Debido a
cuestiones de privacidad y logística, sólo están disponibles variables fisicoquímicas (entradas) y
sensoriales (salidas) (por ejemplo, no hay datos sobre tipos de uva, marca de vino, precio de
venta del vino, etc.). Estos conjuntos de datos pueden verse como tareas de clasificación o
regresión. Las clases están ordenadas y no equilibradas (por ejemplo, hay muchos más vinos
normales que excelentes o malos).

Repositorio: https://www.kaggle.com/datasets/uciml/red-wine-quality-cortez-et-al-2009

Realizar:

## Tabla de frecuencias:

### #Tabla de Frecuencias Calidad 
lista<-hist(calidad, plot=FALSE)
tablaFreq<-table.freq(lista)

### #Tabla de Frecuencias alcohol
lista <- hist(alcohol, plot = FALSE)
tablaFreq <- table.freq(lista)

## Histograma:

### #Histograma Calidad
histograma<-hist(calidad, xlab="CALIDAD", ylab="FRECUENCIA",
                col = "purple",main = "Calidad de los vinos")
print("Este histograma indica que en esta muestra de datos de 'Vinho verde'
      existen una concentracion de vinos con una calidad buena o normal,
      mientras que casi no hay vinos terribles o excelentes")

### #Histograma Alcohol
histograma <- hist(alcohol, xlab = "ALCOHOL", ylab = "FRECUENCIA",
                   col = "#6666FF", main = "Contenido de alcohol en los vinos")
print("Este histograma indica que en esta muestra de datos de 'Vinho verde' existen una concentración de vinos con un contenido de alcohol moderado la mayoria se encuentra entre 9.5 y 10.5, mientras que los vinos con contenido de alcohol mas bajo o mas alto son menos comunes.")
      

## Poligono de frecuencia:

### #Poligono de Frecuencias Calidad
plot(density(calidad), main = "Polígono de Frecuencia", xlab = "Calidad",
     ylab = "Densidad", col = "purple", lwd = 3)
print("Este poligono de frecuencias indica que la calidad de vino 5 y 6 
      son las que mas se repiten, ya que esas dos reprentan los 2 picos 
      que se observan, ademas existe una asimetria a la derecha lo que 
      significa que los valores son mas altos en el lado derecho del grafico")

### #Polígono de Frecuencias alcohol
plot(density(alcohol), main = "Polígono de Frecuencia", xlab = "Alcohol",
     ylab = "Densidad", col = "#6666FF", lwd = 3)
print("Este polígono de frecuencias indica que el contenido de alcohol más común en los vinos está alrededor del 9.8, con una distribución asimétrica hacia la izquierda, lo que sugiere que hay menos vinos con contenido de alcohol muy alto.")

## Grafico circular:

### #Grafico de Pastel Calidad
dioxido<-dataset$free.sulfur.dioxide
calidad10<-calidad[dioxido]
pie(table(calidad10), main = "Calidad de los vinos", col = c("purple", "red", "white","pink"))
print("Este grafico de pastel nos indica que la mayoria de los vinos son 
      decentes, ya que la mayoria tiene una calidad de 5 con mas del 50% 
      mientras que las calidad 6 y 7 no son muy comunes.")
legend("bottomright", legend = levels(as.factor(calidad10)), fill = c("purple", "red", "white","pink"))

### #Gráfico de Pastel alcohol
acidez<-dataset$fixed.acidity
alcoholV<-alcohol[acidez]
colores<- c("#FF6666", "#66FF66", "#6666FF", "#FFFF66", "#FF66FF", "#66FFFF", "#FFCC66", "#996699")
pie(table(alcoholV), main = "Calidad de los vinos", col = colores)
print("Este grafico de pastel nos indica que la mayoria de los vinos tienen un porcentaje de alcohol de 9 a 10, siendo el mas comun el de 9.4")
legend("bottomright", legend = levels(as.factor(alcoholV)), fill = colores)

## Media:

### #Media Calidad
media<-function(vector){
  return(mean(vector))
}
paste("El promedio de calidad de los vinos es:",media(calidad),"esto signfica que en su mayoria se encuentran solo vinos decentes pero no excelentes")

### #Media Alcohol
media <- function(vector) {
  return(mean(vector))
}
paste("El promedio de contenido de alcohol en los vinos es:", media(alcohol))

## Mediana:

### #Mediana Calidad
mediana<-function(vector){
  x<-sort(vector)
  return(median(x))
}
paste("La mediana es:",mediana(calidad),"esto indica que la calidad 
      de la mayoria de los vinos es decente")

### #Mediana Alcohol
mediana <- function(vector) {
  x <- sort(vector)
  return(median(x))
}
paste("La mediana del contenido de alcohol en los vinos es:", mediana(alcohol))

## Moda:

### #Moda Calidad
moda <- function(vector){
  return(as.numeric(
    names(which.max(table(vector)))
  ))
}
paste("La calidad que mas se repite es:",moda(calidad),"ya que la mayoria de los vinos son decentes mas no terribles o excelentes")

### #Moda Alcohol
moda <- function(vector) {
  return(as.numeric(names(which.max(table(vector)))))
}
paste("La moda del contenido de alcohol en los vinos es:", moda(alcohol))


## Varianza y desviacion estandar:

### #Varianza y Desviacion Estandar Calidad
varianza<-function(vector){
  return(var(vector, na.rm = FALSE))
}
desviacionEstandar<-function(vector){
  return(sd(vector))
}

paste("La varianza es:",varianza(calidad),"y la desviacion estandar es:",desviacionEstandar(calidad),"estos valores nos indican que los datos no estan muy dispersos por lo que los valores son constantes debido a que los valores de la muestra solo van del 1 al 10")

### #Varianza y Desviación Estándar Alcohol
varianza <- function(vector) {
  return(var(vector, na.rm = FALSE))
}
desviacionEstandar <- function(vector) {
  return(sd(vector))
}
paste("La varianza del contenido de alcohol en los vinos es:", varianza(alcohol))
paste("La desviación estándar del contenido de alcohol en los vinos es:", desviacionEstandar(alcohol))
print("Estos datos nos sugieren que la dispersión de los datos alrededor de la media no es muy amplia, la mayoría de los datos se encuentran dentro de un rango relativamente cercano a la media.")

## Rango:

### #Rango Calidad
rango <-function(calificaciones){
  r <- max(calificaciones)-min(calificaciones)
  return(r)
}
paste("El rango es:",rango(calidad),"nos indica que ")

### #Rango Alcohol
rango <- function(calificaciones) {
  r <- max(calificaciones) - min(calificaciones)
  return(r)
}
paste("El rango del contenido de alcohol en los vinos es:", rango(alcohol))

## Coeficiente de variacion:

### #Coeficiente de variacion Alcohol
coeficiente<-function(desviacion,media){
  return((desviacion/media)*100)
}

paste("el porcentaje en el que los datos varian es del:",coeficiente(desviacionEstandar(calidad),media(calidad)),"%")

### #Coeficiente de Variación Calidad
coeficiente <- function(desviacion, media) {
  return((desviacion / media) * 100)
}
paste("El coeficiente de variación del contenido de alcohol en los vinos es:", coeficiente(desviacionEstandar(alcohol), media(alcohol)), "%")


## Cuartiles, deciles y percentiles:

### #Cuartiles,deciles y percentiles Alcohol
cuartiles <- function(vector){
  q <- quantile(vector,probs = c(0.25, 0.50, 0.75))
  return(q)
}
cuartiles(calidad)


deciles <- function(vector){

  d <- quantile(vector, probs = seq(0.1, 0.9, by=0.1))
  return(d)
}
deciles(calidad)


percentiles <- function(vector){
  p <- quantile(vector, probs = seq(0.01, 0.99, by=0.01))
  return(p)
}
percentiles(calidad)

### #Cuartiles, Deciles y Percentiles Calidad
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

## Diagrama de Cajas:

### #Diagrama de cajas Calidad
boxplot(calidad, main = "Calidad de vinos", ylab = "Calidad",col="purple")

print("El diagrama de cajas nos da una idea de cómo está distribuida la calidad de los vinos. La línea en el medio representa la calidad media, mientras que la longitud de la caja nos dice cuánto varía esa calidad. Los bigotes muestran lo lejos que pueden ir los vinos de la norma, con algunos valores atípicos por ahí. La forma de la distribución nos dice si la mayoría de los vinos son decentes o si hay más extremos. En general, parece que la mayoría de los vinos están en un punto medio decente.")

### #Diagrama de Cajas Alcohol
boxplot(alcohol, main = "Contenido de alcohol en los vinos", ylab = "Alcohol", col = "purple")
print("El diagrama de cajas nos da una idea de cómo está distribuido el contenido de alcohol en los vinos. La línea en el medio representa la mediana, mientras que la longitud de la caja nos dice cuánto varía el contenido de alcohol. La forma de la distribución nos dice si la mayoría de los vinos tienen un contenido de alcohol moderado o si hay más extremos. En general, parece que la mayoría de los vinos tienen un contenido de alcohol alrededor de 10.")

## Diagrama de pareto

### #Diagrama de pareto Alcohol
pareto.chart(
  calidad[1:10], 
  ylab = "Frecuencias", 
  col = heat.colors(length(calidad[1:10])),
  cumperc = seq(0, 100, by = 20),
  ylab2 = "Porcentaje acumulado",
  main = "Calidad de los vinos"
)
print("El bloque H es el que representa la mayoria de problemas, por lo tanto hay que priozar la mejora de ese conjunto")

### #Diagrama de pareto Calidad
pareto.chart(
  alcohol[1:10], 
  ylab = "Frecuencias", 
  col = heat.colors(length(alcohol[1:10])),
  cumperc = seq(0, 100, by = 20),
  ylab2 = "Porcentaje acumulado",
  main = "Contenido de alcohol en los vinos"
)
print("El bloque J es el que representa la mayoría de problemas, por lo tanto hay que priorizar la mejora de ese conjunto")

![](bef88a2e2f07ad84e80b0b888187443b03c183e3r1-195-258v2_uhq.jpg)

