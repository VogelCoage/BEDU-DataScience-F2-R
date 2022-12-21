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

#SUPUESTOS DEL MODELO DE REGRESIÓN LINEAL Y PREDICCIÓN

StanRes2 <- rstandard(m2)

par(mfrow = c(2, 2))

plot(TV, StanRes2, ylab = "Residuales Estandarizados")
plot(Radio, StanRes2, ylab = "Residuales Estandarizados")
plot(Newspaper, StanRes2, ylab = "Residuales Estandarizados")

qqnorm(StanRes2)
qqline(StanRes2, col="red")

#Gráfica02_s6

dev.off()

#Shapiro-Wilk normality test

shapiro.test(StanRes2)
"Ho: La variable distribuye como una normal
Ha: La variable no distribuye como una normal"

"Shapiro-Wilk normality test

data:  StanRes2
W = 0.97535, p-value = 0.001365
"

#NC 90% - sigificancia 0.1 - rechazo | 95% sig. 0.05 - rechazo | 99% sig. 0.01 - rechazo

"Una vez validados estos supuestos, podemos realizar utilizar nuestro modelo estimado 
para realizar predicciones y obtener su intervalo de confianza"
data <- data.frame(
  TV = c(148.0, 75.5, 290.4, 1.5),
  Radio = c(23.5, 12.5, 46.8, 3.1),
  Newspaper = c(26.0, 14.5, 112.6, 2.5)
)

predict(m2, newdata = data, interval = "confidence", level = 0.95)
    "fit       lwr      upr
1 15.207928 14.976755 15.43910
2 10.081458  9.741523 10.42139
3 25.458628 24.890054 26.02720
4  5.044794  4.502648  5.58694"

"Para comprender mejor manera el papel de los intervalos en la predicción, vamos a 
estimar un modelo de regresión simple para realizar una representación gráfica"
modelo <- lm(Sales ~ TV)

TV.values <- data.frame(TV = sort(c(50.5, 133.6, 204.9, 276.8), decreasing = FALSE))
pred <- predict(modelo, newdata = TV.values, interval = "confidence", level = 0.95)

plot(TV, Sales, xlab = "TV", 
     ylab = "Sales", pch = 16)
points(TV.values$TV, pred[,1], xlab = "TV", 
       ylab = "Sales", pch = 16, col = "red")
abline(lsfit(TV, Sales), col="orange")

#Gráfica03_s6

plot(TV.values$TV, pred[,1], xlab = "TV", 
     ylab = "Sales", pch = 16, col = "red")
abline(lsfit(TV.values$TV, pred[,1]), col="orange")

lines(TV.values$TV, pred[, 2], lty = 2, lwd = 2, col = "blue")
lines(TV.values$TV, pred[, 3], lty = 2, lwd = 2, col = "blue")

#Gráfica04_s6

modelo2 <- lm(Sales ~ Radio)

Radio.values <- data.frame(Radio = sort(c(12.6, 23.5, 35.8, 48.7), decreasing = FALSE))
pred2 <- predict(modelo2, newdata = Radio.values, interval = "confidence", level = 0.95)

plot(Radio, Sales, xlab = "Radio", 
     ylab = "Sales", pch = 16)
points(Radio.values$Radio, pred2[,1], xlab = "Radio", 
       ylab = "Sales", pch = 16, col = "red")
abline(lsfit(Radio, Sales), col="orange")

#Gráfica05_s6

plot(Radio.values$Radio, pred2[,1], xlab = "Radio", 
     ylab = "Sales", pch = 16, col = "red")
abline(lsfit(Radio.values$Radio, pred2[,1]), col="orange")

lines(Radio.values$Radio, pred2[, 2], lty = 2, lwd = 2, col = "blue")
lines(Radio.values$Radio, pred2[, 3], lty = 2, lwd = 2, col = "blue")

#Gráfica06_s6
