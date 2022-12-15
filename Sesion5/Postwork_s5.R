#POSTWORK DE LA SESION 5 - JOSÉ BERNAL FONSECA

"El data frame iris contiene información recolectada por Anderson sobre 50
flores de 3 especies distintas (setosa, versicolor y virginca), incluyendo
medidas en centímetros del largo y ancho del sépalo así como de los pétalos.

Estudios recientes sobre las mismas especies muestran que:"

" ---------------------------------------------------------------------------- "

        "I.- En promedio, el largo del sépalo de la especie setosa (Sepal.Length)
        es igual a 5.7 cm " #hipotesis nula

library(dplyr)

var.iris <- iris

str(var.iris)
summary(var.iris)

var.iris.setosa <- var.iris %>% filter(Species == "setosa")

#Según estudios recientes:
#media(sepal.lenght)
"Planteamiento de hipotesis:"

#H0 - Mu - setosa.Sepal.Length == 5.7
#Ha - Mu - setosa.Sepal.Lenght != 5.7

t.test(x = var.iris.setosa$Sepal.Length, alternative = 'two.sided', mu = 5.7)
"One Sample t-test

data:  var.iris.setosa$Sepal.Length
t = -13.922, df = 49, p-value < 2.2e-16
alternative hypothesis: true mean is not equal to 5.7
95 percent confidence interval:
 4.905824 5.106176
sample estimates:
mean of x 
    5.006 
"

"*******************************************************************************
Conclusión: a Niveles de confianza estandar (90%, 95% y 99%), EEE para rechazar
la hipotesis nula en favor de la alternativa, es decir que en promedio, la
longitud del sépalo en la especie setosa, es distinta a 5.7cms.
*******************************************************************************"

" ---------------------------------------------------------------------------- "
        
        "II.- En promedio, el ancho del pétalo de la especie virginica (Petal.Width)
        es menor a 2.1 cm" #hipótesis alternativa

var.iris.virginica <- var.iris %>% filter(Species == "virginica")

#H0 - virginica.Petal.Width >= 2.1
#Ha - virginica.Petal.Widht < 2.1

t.test(x = var.iris.virginica$Petal.Width, alternative = 'less', mu = 2.1)
"data:  var.iris.virginica$Petal.Width
t = -1.9052, df = 49, p-value = 0.03132 <- MENOR A SIGNIFICANCIA 0.05 - 95% DE NC
alternative hypothesis: true mean is less than 2.1
95 percent confidence interval:
    -Inf 2.09112
sample estimates:
mean of x 
    2.026 
"

"*******************************************************************************
Conclusión: a un NC del 99% no EEE para rechazar la hipótesis nula, es decir que,
el ancho del pétalo en la especie virginica, es mayor o igual a 2.1cms.

*******************************************************************************"

" ---------------------------------------------------------------------------- "
#COMPARACIÓN DE DOS MEDIA
        
        "III.- En promedio, el largo del pétalo de la especie virgínica es 1.1 cm
        más grande que el promedio del largo del pétalo de la especie versicolor."
        #Hipotesis alternativa

## --------------- PASO INTERMEDIO -----------------##
# Identificar si las varianzas de cada grupo, son iguales o diferentes en la población
#Ho: Varianzas son iguales (razon = 1)
#Ha: Varianzas son diferentes (razon != 1)

#Ho: razon = 1 - varianzas iguales
#Ha: razón != 1 - varianzas distintas

var.test(var.iris[var.iris$Species == 'virginica', 'Petal.Length'],
         var.iris[var.iris$Species == 'versicolor', 'Petal.Length'],
         ratio = 1, alternative = "two.sided")

"F test to compare two variances

data:  var.iris[var.iris$Species == 'virginica', 'Petal.Length'] and var.iris[var.iris$Species == 'versicolor', 'Petal.Length']
F = 1.3794, num df = 49, denom df = 49, p-value = 0.2637
alternative hypothesis: true ratio of variances is not equal to 1
95 percent confidence interval:
 0.7827605 2.4307127
sample estimates:
ratio of variances 
          1.379372 
"

"Este p-value me da como resultado la igualdad de las varianzas entre los vectores"

## --------------- FIN PASO INTERMEDIO -----------------##

#H0 - virginica.Petal.Length >= (versicolor.PetalLength + 1.1)
#Ha - virginica.Petal.Lenght < (versicolor.PetalLength + 1.1)

t.test(x = var.iris[var.iris$Species == 'virginica', 'Petal.Length'],
       y = var.iris[var.iris$Species == 'versicolor', 'Petal.Length'],
       alternative = 'greater', mu = 1.1, var.equal = TRUE)

"Two Sample t-test

data:  var.iris[var.iris$Species == 'virginica2', 'Petal.Length'] and var.iris[var.iris$Species == 'versicolor', 'Petal.Length']
t = 1.873, df = 98, p-value = 0.03202
alternative hypothesis: true difference in means is greater than 1.1
95 percent confidence interval:
 1.121779      Inf
sample estimates:
mean of x mean of y 
    5.552     4.260 "

"*******************************************************************************
Conclusión: a un NC del 99% no EEE para rechazar la Hipótesis nula, es decir que
el largo del pétalo de la especie virinica es mayor o igual al de la especie
versicolor.
*******************************************************************************"

" ---------------------------------------------------------------------------- "
#ANÁLISIS DE VARIANZA (ANOVA)
        
        "IV.- En promedio, no existe diferencia en el ancho del sépalo entre las
        3 especies." #hipotesis nula

#Ho - prom_sepal_width_setosa = prom_sepal_width_versicolor = prom_sepal_width_virginica
#Ha - al menos una es diferente

anova <- aov(log(Sepal.Width) ~ Species, data = var.iris)
summary(anova)

"Df Sum Sq Mean Sq F value   Pr(>F)    
Species       2  1.181  0.5905   46.47 2.29e-16 ***
Residuals   147  1.868  0.0127                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"


"*******************************************************************************
Conclusión: el p-value resultante es prácticamente 0 por lo que de acuerdo a los
niveles de significancia estandar EEE para rechazar la hipótesis nula en favor
de la alternativa, es decir que, al menos en un caso, la especie determina el 
ancho del sépalo
*******************************************************************************"

" ---------------------------------------------------------------------------- "

"- Utilizando pruebas de inferencia estadística, concluye si existe evidencia
suficiente para concluir que los datos recolectados por Anderson están en línea
con los nuevos estudios."

"- Utiliza 99% de confianza para toda las pruebas, en cada caso realiza el
planteamiento de hipótesis adecuado y concluye."








