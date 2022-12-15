#POSTWORK DE LA SESION 4 - JOSÉ BERNAL FONSECA
"Utilizando la variable total_intl_charge de la base de datos
telecom_service.csv de la sesión 3, realiza un análisis probabilístico. Para ello,
debes determinar la función de distribución de probabilidad que más se
acerque el comportamiento de los datos. Hint: Puedes apoyarte de medidas
descriptivas o técnicas de visualización."

library(DescTools)
library(ggplot2)

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")
summary(df)

#Observando el comportamiento de la variable de acuerdo a las medidas de tendencia central
#Media
media <- mean(df$total_intl_charge) #2.764581
#Moda
Mode(df$total_intl_charge)
"[1] 2.7
attr(,'freq')
[1] 62"
#Mediana
median(df$total_intl_charge) #2.78


desv.std <- sd(df$total_intl_charge) #0.7537726

set.seed(0202)

"Una vez que hayas seleccionado el modelo, realiza lo siguiente:"

"1. Grafica la distribución teórica de la variable aleatoria total_intl_charge"

hist(df$total_intl_charge, prob = TRUE, main = "Histograma del total cargos internacionales", xlab = "Costo por llamada (dólares)")

#Gráfica01

" ----------------------------------------------------------------------------- "

curve(dnorm(x, mean = media, sd = desv.std), from = 0, to = 5, 
      col = 'red', main = "Densidad de Probabilidad Normal",
      ylab = "f(x)", xlab = "X")
abline(v = media, lwd = 0.5, lty = "dashed", col = 'orange')

#Gráfica02

" ----------------------------------------------------------------------------- "

"2. ¿Cuál es la probabilidad de que el total de cargos internacionales sea
menor a 1.85 usd?"

pnorm(q = 1.85, mean = media, sd = desv.std, lower.tail = TRUE) #0.1125002

x <- seq(-4, 4, 0.01) * desv.std + media
y <- dnorm(x, mean = media, sd = desv.std)

#Gráfica de la densidad y polígono de probabilidad
plot(x, y, type = "l", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 2.76, " y ", sigma == 0.75)))

polygon(c(min(x), x[x<=1.85], 1.85), c(0, y[x<=1.85], 0), col = "red")

#Gráfica03

" ----------------------------------------------------------------------------- "

"3. ¿Cuál es la probabilidad de que el total de cargos internacionales sea
mayor a 3 usd?"

pnorm(q = 3.00, mean = media, sd = desv.std, lower.tail = FALSE) #0.3773985

#Gráfica de la densidad y polígono de probabilidad

plot(x, y, type = "l", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 2.76, " y ", sigma == 0.75)))

polygon(c(3.00, x[x > 3.00], max(x)), c(0, y[x > 3.00], 0), col = "blue")

#Gráfica04

" ----------------------------------------------------------------------------- "


"4. ¿Cuál es la probabilidad de que el total de cargos internacionales esté
entre 2.35usd y 4.85 usd?"

pnorm(4.85, mean = media, sd = desv.std) -
  pnorm(2.3, mean = media, sd = desv.std) #0.7283336

#Gráfica de la densidad y polígono de probabilidad
plot(x, y, type = "l", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 2.76, " y ", sigma == 0.75)))

polygon(c(2.35, x[x >= 2.35 & x <= 4.85], 4.85), c(0, y[x >= 2.35 & x <= 4.85], 0), col = "green")

#Gráfica05

" ----------------------------------------------------------------------------- "


"5. Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales
más alto que podría esperar?"

qnorm(p = 0.48, mean = media, sd = desv.std) #$ 2.73

"6.- ¿Cuáles son los valores del total de cargos internacionales que dejan
exactamente al centro el 80% de probabilidad?"

qnorm(p = 0.1, mean = media, sd = desv.std); qnorm(p = 0.1, mean = media, sd = desv.std, lower.tail = FALSE)
"[1] 1.798583
[1] 3.73058"