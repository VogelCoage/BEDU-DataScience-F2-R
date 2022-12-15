#POSTWORK DE LA SESION 6 - JOSÉ BERNAL FONSECA

"Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre como
mejorar las ventas de un producto particular, y el conjunto de datos con el que
disponemos son datos de publicidad que consisten en las ventas de aquel producto
en 200 diferentes mercados, junto con presupuestos de publicidad para el
producto en cada uno de aquellos mercados para tres medios de comunicación
diferentes: TV, radio, y periódico. No es posible para nuestro cliente
incrementar directamente las ventas del producto. Por otro lado, ellos pueden
controlar el gasto en publicidad para cada uno de los tres medios de
comunicación. Por lo tanto, si determinamos que hay una asociación entre
publicidad y ventas, entonces podemos instruir a nuestro cliente para que ajuste
los presupuestos de publicidad, y así indirectamente incrementar las ventas.

En otras palabras, nuestro objetivo es desarrollar un modelo preciso que pueda
ser usado para predecir las ventas sobre la base de los tres presupuestos de
medios de comunicación. Ajuste modelos de regresión lineal múltiple a los datos
advertisement.csv y elija el modelo más adecuado siguiendo los procedimientos
vistos

Considera:

    Y: Sales (Ventas de un producto)
    X1: TV (Presupuesto de publicidad en TV para el producto)
    X2: Radio (Presupuesto de publicidad en Radio para el producto)
    X3: Newspaper (Presupuesto de publicidad en Periódico para el producto)
"

adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")
summary(adv)

#Matriz de coorelación entre las variables
round(cor(adv), 4)

              "TV  Radio Newspaper  Sales
TV        1.0000 0.0548    0.0566 0.9012
Radio     0.0548 1.0000    0.3541 0.3496
Newspaper 0.0566 0.3541    1.0000 0.1580
Sales     0.9012 0.3496    0.1580 1.0000"

pairs(~ Sales + TV + Radio + Newspaper, 
      data = adv, gap = 0.4, cex.labels = 1.5)

#Gráfica01_s6

attach(adv)

#modelo de regresión lineal
m1 <- lm(Sales ~ TV + Radio + Newspaper)

summary(m1)

"Call:
lm(formula = Sales ~ TV + Radio + Newspaper)

Residuals:
    Min      1Q  Median      3Q     Max 
-7.3034 -0.8244 -0.0008  0.8976  3.7473 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 4.6251241  0.3075012  15.041   <2e-16 ***
TV          0.0544458  0.0013752  39.592   <2e-16 ***
Radio       0.1070012  0.0084896  12.604   <2e-16 ***
Newspaper   0.0003357  0.0057881   0.058    0.954    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.662 on 196 degrees of freedom
Multiple R-squared:  0.9026,	Adjusted R-squared:  0.9011 
F-statistic: 605.4 on 3 and 196 DF,  p-value: < 2.2e-16"

"*******************************************************************************
De acuerdo con los resultados, la variable Newspaper no es determinante en el
comportamiento de las ventas
*******************************************************************************"

"Creamos un nuevo modelo, omitiendo la variable Newspaper que parece no influir
significativamente en las ventas:"
m2 <- update(m1, ~.-Newspaper)

summary(m2)

"Call:
lm(formula = Sales ~ TV + Radio)

Residuals:
    Min      1Q  Median      3Q     Max 
-7.3131 -0.8269  0.0095  0.9022  3.7484 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 4.630879   0.290308   15.95   <2e-16 ***
TV          0.054449   0.001371   39.73   <2e-16 ***
Radio       0.107175   0.007926   13.52   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.657 on 197 degrees of freedom
Multiple R-squared:  0.9026,	Adjusted R-squared:  0.9016 
F-statistic: 912.7 on 2 and 197 DF,  p-value: < 2.2e-16"