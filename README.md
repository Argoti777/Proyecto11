![](https://itq.edu.ec/wp-content/uploads/2023/02/Recurso-6.png)


Datos  |Descripcion  |
------------- | -------------
Docente  | Ing. Sebastián Landázuri  |
Asignatura  | Base de Datos no relacionales    |
Carrera  | Desarollo de Software  | 
Nivel: | Tercer Nivel |
Estudiantes  | Bryan Velasco, Alejandro Argoti, Alexis Velasco , Gabriel Salazar    |

### Proyecto Estadistica descriptiva

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

#Tabla de Frecuencias
lista<-hist(calidad, plot=FALSE)
tablaFreq<-table.freq(lista)


## Histograma:

#Histograma
histograma<-hist(calidad, xlab="CALIDAD", ylab="FRECUENCIA",
                col = "purple",main = "Calidad de los vinos")
print("Este histograma indica que en esta muestra de datos de 'Vinho verde'
      existen una concentracion de vinos con una calidad buena o normal,
      mientras que casi no hay vinos terribles o excelentes")

## Poligono de frecuencia:

#Poligono de Frecuencias
plot(density(calidad), main = "Polígono de Frecuencia", xlab = "Calidad",
     ylab = "Densidad", col = "purple", lwd = 3)
print("Este poligono de frecuencias indica que la calidad de vino 5 y 6 
      son las que mas se repiten, ya que esas dos reprentan los 2 picos 
      que se observan, ademas existe una asimetria a la derecha lo que 
      significa que los valores son mas altos en el lado derecho del grafico")
      
## Grafico circular:

#Grafico de Pastel
alcohol<-dataset$free.sulfur.dioxide
calidad10<-calidad[alcohol]
pie(table(calidad10), main = "Calidad de los vinos", col = c("purple", "red", "white","pink"))
print("Este grafico de pastel nos indica que la mayoria de los vinos son 
      decentes, ya que la mayoria tiene una calidad de 5 con mas del 50% 
      mientras que las calidad 6 y 7 no son muy comunes.")
legend("bottomright", legend = levels(as.factor(calidad10)), fill = c("purple", "red", "white","pink"))


## Media:

#Media
media<-function(vector){
  return(mean(vector))
}
paste("El promedio de calidad de los vinos es:",media(calidad),"esto signfica que en su mayoria se encuentran solo vinos decentes pero no excelentes")


## Mediana:

#Mediana
mediana<-function(vector){
  x<-sort(vector)
  return(median(x))
}
paste("La mediana es:",mediana(calidad),"esto indica que la calidad 
      de la mayoria de los vinos es decente")

## Moda:

#Moda
moda <- function(vector){
  return(as.numeric(
    names(which.max(table(vector)))
  ))
}
paste("La calidad que mas se repite es:",moda(calidad),"ya que la mayoria de los vinos son decentes mas no terribles o excelentes")

## Varianza y desviacion estandar:

#Varianza y Desviacion Estandar
varianza<-function(vector){
  return(var(vector, na.rm = FALSE))
}
desviacionEstandar<-function(vector){
  return(sd(vector))
}

paste("La varianza es:",varianza(calidad),"y la desviacion estandar es:",desviacionEstandar(calidad),"estos valores nos indican que los datos no estan muy dispersos por lo que los valores son constantes debido a que los valores de la muestra solo van del 1 al 10")

## Rango:

#Rango
rango <-function(calificaciones){
  r <- max(calificaciones)-min(calificaciones)
  return(r)
}
paste("El rango es:",rango(calidad),"nos indica que ")


## Coeficiente de variacion:

#Coeficiente de variacion
coeficiente<-function(desviacion,media){
  return((desviacion/media)*100)
}

paste("el porcentaje en el que los datos varian es del:",coeficiente(desviacionEstandar(calidad),media(calidad)),"%")

## Cuartiles, deciles y percentiles:

#Cuartiles,deciles y percentiles
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


## Diagrama de cajas:

#Diagrama de cajas
boxplot(calidad, main = "Calidad de vinos", ylab = "Calidad",col="purple")

print("El diagrama de cajas nos da una idea de cómo está distribuida la calidad de los vinos. La línea en el medio representa la calidad media, mientras que la longitud de la caja nos dice cuánto varía esa calidad. Los bigotes muestran lo lejos que pueden ir los vinos de la norma, con algunos valores atípicos por ahí. La forma de la distribución nos dice si la mayoría de los vinos son decentes o si hay más extremos. En general, parece que la mayoría de los vinos están en un punto medio decente.")

## Diagrama de pareto

pareto.chart(
  calidad[1:10], 
  ylab = "Frecuencias", 
  col = heat.colors(length(calidad[1:10])),
  cumperc = seq(0, 100, by = 20),
  ylab2 = "Porcentaje acumulado",
  main = "Calidad de los vinos"
)
print("El bloque H es el que representa la mayoria de problemas, por lo tanto hay que priozar la mejora de ese conjunto")
