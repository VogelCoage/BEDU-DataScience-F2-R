#POSTWORK DE LA SESION 8 - JOSÉ BERNAL FONSECA

"Un centro de salud nutricional está interesado en analizar estadísticamente y
probabilísticamente los patrones de gasto en alimentos saludables y no
saludables en los hogares mexicanos con base en su nivel socioeconómico, en si
el hogar tiene recursos financieros extrar al ingreso y en si presenta o no
inseguridad alimentaria. Además, está interesado en un modelo que le permita
identificar los determinantes socioeconómicos de la inseguridad alimentaria."

"La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición
(2012) levantada por el Instituto Nacional de Salud Pública en México. La
mayoría de las personas afirman que los hogares con menor nivel socioeconómico
tienden a gastar más en productos no saludables que las personas con mayores
niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un
hogar presente cierta inseguridad alimentaria."

"La base de datos contiene las siguientes variables:"

"
- nse5f (Nivel socieconómico del hogar): 1 'Bajo', 2 'Medio bajo', 3 'Medio', 4 'Medio alto', 5 'Alto'
- area (Zona geográfica): 0 'Zona urbana', 1 'Zona rural'
- numpeho (Número de persona en el hogar)
- refin (Recursos financieros distintos al ingreso laboral): 0 'no', 1 'sí'
- edadjef (Edad del jefe/a de familia)
- sexoje (Sexo del jefe/a de familia): 0 'Hombre', 1 'Mujer'
- añosedu (Años de educación del jefe de familia)
- ln_als (Logarítmo natural del gasto en alimentos saludables)
- ln_alns (Logarítmo natural del gasto en alimentos no saludables)
- IA (Inseguridad alimentaria en el hogar): 0 'No presenta IA', 1 'Presenta IA'
"

library(dplyr)
library(DescTools)
library(ggplot2)
library(moments)

?dbGetQuery


df<-read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")

summary(df)
View(df)

dfc <-  na.omit(df)
summary(dfc)  # datos sin NA,  20280 observaciones
View(dfc)

df$nse5f <- factor(df$nse5f, labels = c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto"))
df$area <- factor(df$area, labels = c("Zona urbana", "Zona rural"))
df$refin <- factor(df$refin, labels=c("No","Si"))
df$sexojef <- factor(df$sexojef, labels=c("Hombre","Mujer"))
df$IA <- factor(df$IA, labels=c("No presenta IA","Presenta IA"))

muestraConIA <- filter(dfc, IA == 1)
summary(muestraConIA)

"
1.- Plantea el problema del caso
R.- Uno de los problemas que presenta el caso es que el análisis se basa en
variables descriptivas que requieren una mayor interpretación para llegar a 
una conclusión."

"2.- Realiza un análisis descriptivo de la información"

boxplot( ln_als ~ nse5f,
         data = muestraConIA)

boxplot( ln_alns ~ nse5f,
         data = muestraConIA)

#muestra con IA
#media general ln_als = 6.150
#media general ln_alns = 4.033

muestra.bajo <- filter(muestraConIA, nse5f == "Bajo")
meanbajoALs <- mean( muestra.bajo$ln_als )
#media nivel socioeconómico bajo n_als = 5.790

muestra.medio_bajo <- filter(muestraConIA, nse5f == "Medio bajo")
meanMbajoALs  <- mean( muestra.medio_bajo$ln_als )
#media nivel socioeconómico medio bajo n_als = 6.042

muestra.medio <- filter(muestraConIA, nse5f == "Medio")
meanMedioALs  <- mean( muestra.medio$ln_als )
#media nivel socioeconómico medio n_als = 6.188

muestra.medio_alto <- filter(muestraConIA, nse5f == "Medio alto")
meanMaltoALs  <- mean( muestra.medio_alto$ln_als )
#media nivel socioeconómico medio alto n_als = 6.330

muestra.alto <- filter(muestraConIA, nse5f == "Alto")
meanAltoALS  <- mean( muestra.alto$ln_als )
#media nivel socioeconómico alto n_als = 6.517

"
3.- Calcula probabilidades que nos permitan entender el problema en México
4.- Plantea hipótesis estadísticas y concluye sobre ellas para entender el
problema en México
5.- Estima un modelo de regresión, lineal o logístico, para identificiar los
determinantes de la inseguridad alimentaria en México
6.- Escribe tu análisis en un archivo README.MD y tu código en un script de R y
publica ambos en un repositorio de Github.
"

"NOTA: Todo tu planteamiento deberá estár correctamente desarrollado y deberás
analizar e interpretar todos tus resultados para poder dar una conclusión final
al problema planteado."

attach(muestraConIA)

m1 <- lm(IA ~ nse5f + area + numpeho + refin + edadjef + sexojef + añosedu + ln_als + ln_alns)
summary(m1)


" ---------------------------------------------------------------------------- "
#MODELO DE REGRESIÓN LOGÍSTICA
set.seed(2022)
y = df$IA
x = df$nse5f
?glm

logistic.1 <- glm(y ~ x, family = binomial)

summary(logistic.1)
"Call:
glm(formula = y ~ x, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.0237  -1.1626   0.6321   0.7149   1.1923  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  1.90951    0.03169  60.248   <2e-16 ***
xMedio bajo -0.40043    0.04234  -9.458   <2e-16 ***
xMedio      -0.67573    0.04114 -16.425   <2e-16 ***
xMedio alto -1.17719    0.03977 -29.601   <2e-16 ***
xAlto       -1.94441    0.03953 -49.183   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 47125  on 40808  degrees of freedom
Residual deviance: 43690  on 40804  degrees of freedom
AIC: 43700

Number of Fisher Scoring iterations: 4"

exp(coef(logistic.1))
"(Intercept) xMedio bajo      xMedio xMedio alto       xAlto 
  6.7497813   0.6700337   0.5087874   0.3081420   0.1430723 "

par(mfrow = c(3, 3))

plot(IA ~ nse5f, data=df, xlim = c(0,1))

x = df$area

logistic.1 <- glm(y ~ x, family = binomial)

summary(logistic.1)
"Call:
glm(formula = y ~ x, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.8218  -1.5438   0.6497   0.8509   0.8509  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  0.82968    0.01334   62.22   <2e-16 ***
xZona rural  0.61869    0.02519   24.56   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 47125  on 40808  degrees of freedom
Residual deviance: 46489  on 40807  degrees of freedom
AIC: 46493

Number of Fisher Scoring iterations: 4"

exp(coef(logistic.1))
"(Intercept) xZona rural 
   2.292595    1.856495  "

plot(IA ~ area, data=df, xlim = c(0,1))

x = df$numpeho

logistic.1 <- glm(y ~ x, family = binomial)

summary(logistic.1)
"Call:
glm(formula = y ~ x, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.2351  -1.4726   0.7357   0.8190   0.9085  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) 0.547183   0.026416   20.71   <2e-16 ***
x           0.124318   0.006393   19.45   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 47125  on 40808  degrees of freedom
Residual deviance: 46729  on 40807  degrees of freedom
AIC: 46733

Number of Fisher Scoring iterations: 4
"

exp(coef(logistic.1))
"(Intercept)           x 
   1.728377    1.132376 "

plot(IA ~ numpeho, data=df, xlim = c(0,1))

x = df$refin

logistic.1 <- glm(y ~ x, family = binomial)

summary(logistic.1)
"Call:
glm(formula = y ~ x, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.7940  -1.5973   0.8093   0.8093   0.8093  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  0.94820    0.01226   77.33   <2e-16 ***
xSi          0.43777    0.03091   14.16   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 47125  on 40808  degrees of freedom
Residual deviance: 46913  on 40807  degrees of freedom
AIC: 46917

Number of Fisher Scoring iterations: 4"

exp(coef(logistic.1))
"(Intercept)         xSi 
   2.581058    1.549253 "

plot(IA ~ refin, data=df, xlim = c(0,1))

x = df$edadjef

logistic.1 <- glm(y ~ x, family = binomial)

summary(logistic.1)
"Call:
glm(formula = y ~ x, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.6953  -1.6186   0.7691   0.7807   0.7968  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) 0.945796   0.039192  24.133  < 2e-16 ***
x           0.002156   0.000765   2.819  0.00482 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 40946  on 35791  degrees of freedom
Residual deviance: 40938  on 35790  degrees of freedom
  (5017 observations deleted due to missingness)
AIC: 40942

Number of Fisher Scoring iterations: 4"

exp(coef(logistic.1))
"(Intercept)           x 
   2.574863    1.002159 "

plot(IA ~ edadjef, data=df, xlim = c(0,1))

x = df$sexojef

logistic.1 <- glm(y ~ x, family = binomial)

summary(logistic.1)
"Call:
glm(formula = y ~ x, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.6821  -1.6314   0.7834   0.7834   0.7834  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  1.02388    0.01381  74.126  < 2e-16 ***
xMujer       0.11256    0.02836   3.969 7.22e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 40977  on 35817  degrees of freedom
Residual deviance: 40961  on 35816  degrees of freedom
  (4991 observations deleted due to missingness)
AIC: 40965

Number of Fisher Scoring iterations: 4"

exp(coef(logistic.1))
"(Intercept)      xMujer 
    2.78397     1.11914  "

plot(IA ~ sexojef, data=df, xlim = c(0,1))

x = df$añosedu

logistic.1 <- glm(y ~ x, family = binomial)

summary(logistic.1)
"Call:
glm(formula = y ~ x, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.1615  -1.2344   0.7133   0.8235   1.3601  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  2.234342   0.032546   68.65   <2e-16 ***
x           -0.110595   0.002657  -41.62   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 47125  on 40808  degrees of freedom
Residual deviance: 45190  on 40807  degrees of freedom
AIC: 45194

Number of Fisher Scoring iterations: 4"

exp(coef(logistic.1))
"(Intercept)           x 
   9.340334    0.895301  "

plot(IA ~ añosedu, data=df, xlim = c(0,1))
curve(predict(logistic.1, newdata = data.frame(x), type = "response"),
      add = TRUE)

x = df$ln_als

logistic.1 <- glm(y ~ x, family = binomial)

summary(logistic.1)
"Call:
glm(formula = y ~ x, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3180  -1.4591   0.7436   0.8127   1.1862  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  3.27417    0.10399   31.48   <2e-16 ***
x           -0.36731    0.01677  -21.90   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 46157  on 40021  degrees of freedom
Residual deviance: 45641  on 40020  degrees of freedom
  (787 observations deleted due to missingness)
AIC: 45645

Number of Fisher Scoring iterations: 4"

exp(coef(logistic.1))
"(Intercept)           x 
 26.4212710   0.6925959 "

plot(IA ~ ln_als, data=df, xlim = c(0,1))

x = df$ln_alns

logistic.1 <- glm(y ~ x, family = binomial)

summary(logistic.1)
"Call:
glm(formula = y ~ x, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.0717  -1.4014   0.7607   0.8529   1.2091  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  2.02153    0.06070   33.31   <2e-16 ***
x           -0.27329    0.01389  -19.67   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 28216  on 23304  degrees of freedom
Residual deviance: 27824  on 23303  degrees of freedom
  (17504 observations deleted due to missingness)
AIC: 27828

Number of Fisher Scoring iterations: 4"

exp(coef(logistic.1))
"(Intercept)           x 
  7.5498818   0.7608702 "

plot(IA ~ ln_alns, data=df, xlim = c(0,1))

dev.off()


