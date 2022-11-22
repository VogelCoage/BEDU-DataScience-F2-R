# Postwork Sesión 3

#### Objetivo

#- Realizar un análisis descriptivo de las variables de un dataframe

#### Requisitos

#1. R, RStudio
#2. Haber realizado el prework y seguir el curso de los ejemplos de la sesión
#3. Curiosidad por investigar nuevos tópicos y funciones de R

#### Desarrollo

library(dplyr)
library(DescTools)
library(ggplot2)

"Utilizando el dataframe `boxp.csv` realiza el siguiente análisis descriptivo. No olvides excluir los missing values y transformar las variables a su
tipo y escala correspondiente."
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")

str(df)
summary(df)

df$Categoria <- factor(df$Categoria)

df$Grupo <- factor(df$Grupo, labels = c("A", "B"))

#1) Calcula e interpreta las medidas de tendencia central de la variable `Mediciones`

df.mediciones <- select(df, Mediciones)
View(df.mediciones)

#MEDIA - omitiendo valores nulos o N/A
mean(na.omit(df$Mediciones)) #62.88494

#MEDIANA - omitiendo valores nulos o N/A
median(na.omit(df$Mediciones)) #49.3

#MODA - omitiendo valores nulos o N/A
Mode(na.omit(df$Mediciones)) #23.3 - freq. 6

#2) Con base en tu resultado anterior, ¿qué se puede concluir respecto al sesgo de `Mediciones`?
"R. que claramente tiene un sesgo hacia el lado derecho"

#3) Calcula e interpreta la desviación estándar y los cuartiles de la distribución de `Mediciones`
sd(na.omit(df$Mediciones)) #53.76972

cuartiles <- quantile(na.omit(df$Mediciones), probs = c(0.25,0.5,0.75))
cuartiles

"R. por lo que es posible apreciar, existe una gran dispersión de los datos"

"4) Con ggplot, realiza un histograma separando la distribución de `Mediciones` por `Categoría`
¿Consideras que sólo una categoría está generando el sesgo?"

#histograma con la librería ggplot
k = ceiling(1 + 3.3 * log10(length(na.omit(df$Mediciones))))

ac = (max(na.omit(df$Mediciones))-min(na.omit(df$Mediciones))) / k

bins = seq(min(na.omit(df$Mediciones)), max(na.omit(df$Mediciones)), by = ac)

my_hist <- hist(df$Mediciones, breaks = bins, main = "Histograma")

ggplot(df, aes(Mediciones, fill = Categoria, color = Categoria)) +
  geom_histogram(bins = k) + 
  labs(title = "Histograma de mediciones", 
       x = "Medición",
       y = "Frecuencia") + 
  theme_classic()

"R. Por lo que es posible apreciar, todas las categorías influyen en similar medida al sesgo"

"5) Con ggplot, realiza un boxplot separando la distribución de `Mediciones` por `Categoría` 
y por `Grupo` dentro de cada categoría. ¿Consideras que hay diferencias entre categorías? ¿Los grupos al interior de cada categoría 
podrían estar generando el sesgo?"

boxplot(na.omit(df$Mediciones ~ df$Grupo),
        main = "Mediciones por grupo",
        xlab = "Grupos",
        ylab = "Mediciones")

boxplot(na.omit(df$Mediciones ~ df$Categoria),
        main = "Mediciones por categoría",
        xlab = "Categorías",
        ylab = "Mediciones")

ggplot(df,mapping = aes(x = Categoria, y = Mediciones)) + 
         geom_boxplot() +
         labs(title = "Mediciones por categoría",
              x = "Categoría",
              y = "Mediciones")

ggplot(df,mapping = aes(x = Grupo, y = Mediciones)) + 
  geom_boxplot() +
  labs(title = "Mediciones por grupo",
       x = "Grupo",
       y = "Mediciones")

"R. De acuerdo a lo observado en el caso de los grupos, se aprecia que uno de
ellos, al que se denominó 'B' si influye notablemente en el sesgo, en tanto que
en el caso de las categorías, la categoría 'C1' muestra una mayor influencia
en relación al sesgo."