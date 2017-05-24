
#############REGRESIÓN POISSON######################

#Se considera que la variable respuesta es el número de concubinos 
#y las variables explicativas son: color, estado de la espina central,
#peso y anchura del caparazón. 

########Leemos el conjunto de datos###########
# El  ejemplo  que  vamos  a  desarrollar  en  el  R,  es  un  ejemplo  de  datos
# tomados  sobre  el sistema de apareamiento del cangrejo herradura. Los datos 
# fueron publicados en Ethology (1996) por Jane Brockmann. Los machos de los 
# cangrejos herradura presentan dos tácticas reproductivas.  O  ser  el  macho  
# principal  o  ser  un  macho  "satélite".  Se  relevó  información sobre 4 
# variables que se cree tienen influencia sobre la cantidad de machos satélites 
# que se reúnen  alrededor  de  una  hembra.  El  color,  clasificado  como  
# 1=medio  claro,  2=medio,  3= medio oscuro, 4= oscuro; la condición de las
# espinas, clasificadas como 1= ambas bien, 2= una  gastada  o  rota  o  3=  
#   ambas  gastadas  o  rotas).  También  se  tomaron  los  anchos  de caparazón 
# de  las  hembras  y  su  peso  en  gramos.  Como  variable  respuesta  se  contó
# el número de machos satélites que se hallaron alrededor del par de cópula.
library(ggfortify)
library(dplyr)
library(tidyr)

Datos <- read.table("Poisson/poisson.csv", 
                    header=TRUE, sep=";", na.strings="NA", 
                    dec=",", strip.white=TRUE)
##Veamos el tipo de cada una de las variable##
str(Datos)

##Modificamos la variable "Espina" a cualitativa (la variable "Color" continúa
#siendo cuantitativa discreta, indicando que a mayor valor, es peor el estado)
Datos<- transform(Datos,
                  Espina =  factor(Espina, 
                                   labels = c("Ambas Bien", "1 Mal", "2 Mal")),
                  Color = factor(Color, 
                                 labels = c("Medio Claro", "Medio", 
                                            "Medio Oscuro", "Oscuro")),
                  Ancho_centrado = Ancho - mean(Ancho),
                  Peso_centrado = Peso - mean(Peso)) 



##Mostramos los datos##
show(Datos)


##Elegimos las variables con las cuales vamos a trabajar con el data.frame##
datos<- Datos[, c(1:5,8,9)]

str(datos)

###############################################################################
##################Analisis exploratorio########################################
###Medidas Resúmen#########
#Variables cuantitativas: 
library(RcmdrMisc)
lapply(datos[, 3:5], numSummary, statistics=c("mean", "sd", "IQR", "quantiles", "cv"),
           type=c("2", "1", "3"),quantiles=c(0, .25, .5, .75, 1))

#Un gráfico "violinplot" es una combinación de un box-plot y un gráfico de 
#densidad. Específicamente, comienza con un diagrama de caja. A continuación, 
#agrega un gráfico de densidad rotado a cada lado del box-plot.

datos %>% 
  select(Ancho, Peso, Satelites) %>% 
  gather(Variable, Valor) %>% 
  ggplot(aes(x = 1, y = Valor)) +
  geom_violin() +
  geom_boxplot(width = 0.5) +
  facet_wrap(~Variable, scales = "free_y")

table(datos$Satelites)
#ya sabemos que un box-plot para variables cuantitativas
#discretas no es apropiado, pero claramente se ve la asimetría en el comportamiento
#de los 

#Variables cualitativas:
pie(table(datos$Espina))

###Gráficos################
#Realizamos un gráfico exploratorio de los datos obtenidos. Triángulo 
#superior: correlación entre las variables. Diagonal: 
#estimación de la densidad de la variable. Triángulo inferior: diagrama de 
#dispersión para pares de variables.

library(psych) 
pairs.panels(datos, pch=21,main="Gráfico exploratorio: Matriz de Dispersión, Histograma y Correlación")
with(datos, table(Espina, Color))
#Observando los gráficos:
#Alta correlación lineal entre "Peso_centrado" y "Ancho_centrado" (0.89), a tener en cuenta, pues uno de los
#supuestos de GLM es que las covariables sean independientes.

###############################################################################
#################Análisis Inferencial##########################################
#Selección de variables: Método Stepwise (Devianza)

#TENER EN CUENTA: 
#Criterio exclusivamente estadístico: no se tienen en cuenta otros 
#“conocimientos” sobre las variables más interesantes a incluir (aunque 
#se puede forzar a que algunas variables siempre estén en el modelo);  
#Si hay un conjunto de variables muy correlacionadas, sólo una será 
#seleccionada; 
#No es fácil tener en cuenta interacciones entre variables (los modelos deben
#ser jerárquicos). 

###########Paso 1############### 
#Comenzamos haciendo GLM para el modelo nulo y el modelo que incluye
#sólo cada uno de las covariables 
mnull<-glm(Satelites~1,data=datos,family=poisson(link = "log"))
mnull
#Por defecto considera que la función de enlace es logaritmo natural
mnull<-glm(Satelites~1,data=datos,family=poisson)

m1<-glm(Satelites~Color,data=datos,family=poisson)
m2<-glm(Satelites~Espina,data=datos,family=poisson)
m3<-glm(Satelites~Ancho_centrado,data=datos,family=poisson)
m4<-glm(Satelites~Peso_centrado,data=datos,family=poisson)

#Con la función "anova" comparo el modelo nulo con cada uno de los modelos 
#generados con una sola variable explicativa. La "Null deviance" es la desviación
#para el modelo que no depende de ninguna variable. La "Residual deviance" es la 
#diferencia entre la desviación del modelo que no depende de ninguna variable
#menos la correspondiente al modelo que incluye a la variable width.
#La diferencia entre ambas se distribuye como una distribucion chi-cuadrado 
#con 1 grado de libertad y permite contrastar si el coe?ciente que corresponde
#a cada covariable puede considerarse nulo.

library(car) 
anova(mnull,m1,test="Chisq")
anova(mnull,m2,test="Chisq")
anova(mnull,m3,test="Chisq")
anova(mnull,m4,test="Chisq")

#La variable "Ancho_centrado" es la que produce menor devianza residual (con p<<0.05).
#Por lo tanto, entra al modelo.

#(Hauck and Donner, 1977) examinaron el desempeño del test de Wald y 
#encontraron que a menudo falla para rechazar la hipótesis nula. En ese 
#artículo recomiendan el contraste condicional de razón de verosimilud . 
#Tanto el test de Wald como el de Razón de Verosimilitud requieren el cálculo de la estimación 
#máximo verosímil de ß. Un test que simplifica estos cálculos es el test de 
#Score, basado en las propiedades de las derivadas parciales de la función 
#de verosimilitud. En R podemos utilizar el argumento test=”Rao” dentro de
#la función anova para obtener este contraste. La potencia de cálculo ya no 
#es un problema y el test Score no suele utilizarse.
anova(mnull,m1, test = "Rao")#Vemos aquí que entraría "Color". 
anova(mnull,m2, test = "Rao")#Espina no entra
anova(mnull,m3, test = "Rao")#Ancho_centrado entra
anova(mnull,m4, test = "Rao")#Peso_centrado no entra
#Por lo tanto llegamos a las mismas conclusiones.


###########Paso 2###############
#Partimos de un modelo que incluye la variable ya seleccionada (Ancho_centrado):
#m3 y lo comparamos con un conjunto de modelos que incluyen el resto de las
#variables que se quieren poner a prueba.

m32<-glm(Satelites~Ancho_centrado+Espina,data=datos,family=poisson)
m33<-glm(Satelites~Ancho_centrado+Color,data=datos,family=poisson)
m34<-glm(Satelites~Ancho_centrado+Peso_centrado,data=datos,family=poisson)

#Revisamos las devianzas entre esos modelos:
anova(m3,m32,test="Chisq")
anova(m3,m33,test="Chisq")
anova(m3,m34,test="Chisq")

#¡Resulta que sí son significativos! Entraría el "Peso_centrado" pues
# (valor p=0.004735 < 0.005592)
# pero la correlación entre "Peso_centrado" y "Ancho_centrado" es muy alta!, entonces tomamos
# la decisión de no incluir el "Peso_centrado". Quien ingresa es el "Color".
cor(datos$Ancho_centrado, datos$Peso_centrado)

###########Paso 3###############
#Partimos de un modelo que incluye las variable seleccionadas ("Ancho_centrado" y "Color"):
#m33 y debemos indagar la posibilidad de eliminar alguna de las variables
#Introducidas hasta el Paso 2.
#Para ello, contrastamos el modelo que tenemos hasta el momento (m33):	
#Y = b0 + b1Ancho_centrado + b2Color	
#con los dos modelos resultantes de excluir una variable a la vez.
summary(m33)
anova(m33,m1,test="Chisq")
anova(m33,m3,test="Chisq")

#En este paso vemos que eliminar del modelo la variable “Ancho_centrado“ 
#o la variable "Color", empeora el ajuste ya que en los dos casos,
#su ausencia aumenta significativamente el valor de la devianza residual
#(observar los g.l.).

#Y continúo con el modelo m33. 


###########Paso 4###############

#Partimos de un modelo que incluye las variable seleccionadas ("Ancho_centrado" y "Color"):
#m33 y lo comparamos con un conjunto de modelos que incluyen el resto de las
#variables que se quieren poner a prueba.
m333<-glm(Satelites~Ancho_centrado+Color+Espina,data=datos,family=poisson)
anova(m33,m333, test="Chisq")

#La disminución en la Devianza Residual debido a la inclusión de 
#la variable “Espina” en el modelo no es significativa.
#Por lo tanto decidimos NO incluirla. 

#Nos quedamos con el modelo m33 que involucra las covariables #Ancho_centrado", y "Color".
#Ancho_centrado del Peso_centrado del prosoma del cangrejo. 
#Color del caparazón (más oscuro indica peor estado).

#################El modelo FINAL es m33####################### 
#Y = b0 + b1Ancho_centrado + b2Color
###################################MODELO FINAL: M33#####################################
summary(m33)
m33_sinI <- update(m33, . ~ . -1)
summary(m33_sinI)
#############################################################################
################Interpretación de los Coeficientes##########################
#Extraemos los coeficientes
coef(m33)
exp(coefficients(m33))
#Intercept:
eb0<-exp(coefficients(m33))[1]
eb0
#Coeficiente de Ancho_centrado:
eb1=exp(coefficients(m33))[2]
eb1
#Coeficiente de Color:
eb2<-exp(coefficients(m33))[3]
eb2

#La manera en la cual se interpreta un modelo de conteo depende si se está 
#interesado en el valor esperado de la variable de recuento o en la 
#distribución de los recuentos. Si el interés está en recuento esperado, 
#varios métodos se pueden utilizar para calcular el cambio en la expectativa 
#de un cambio en una independiente variable.
#Si el interés está en la distribución de los recuentos o tal vez sólo la 
#probabilidad de que un recuento específico, la probabilidad de que un 
#recuento para un nivel dado de las variables independientes se puede calcular.

#Objetivo: "Si el agrupamiento depende de alguna característica de las hembras".
#Entonces:
eb0 #=0.0953139 indica que el número esperado de machos satélites que llegan a 
#la playa para reproducirse aumenta en eb0 cuando sin tener en cuenta el ancho
#y el color de la hembra.
eb1 #=1.161338 indica que el número esperado de machos satélites que llegan a
#la playa para reproducirse aumenta en eb1 por cada unidad que cambia
#el ancho del prosoma manteniendo constante el color del cangrejo.
eb2 #= 0.8441681 indica que el número esperado de machos satélites 
#disminuye en eb2 por cada unidad de cambio de color, es decir mientras más 
#oscuro sea el color, más disminuye, en media, la cantidad de machos satélites que se agrupan en 
#la playa, manteniendo constante el ancho del prosoma.


#####Prueba: si pensamos la variable "Color" como categórica, tendríamos un coeficiente
#por cada categoría. Si se realiza ese análisis se obtienen coeficiente para el color:
eb21 #= 0.8189875 
eb22 #= 0.646383       
eb23 #= 0.6393139  
#Luego: eb21<eb22<eb23 indica que el número esperado de machos satélites 
#disminuye en eb21 (color 3), eb22 (color 4), eb23 (color 5) por cada unidad 
#de cambio de color, es decir mientras más oscuro sea el color,
#más disminuye, en media, la cantidad de machos satélites que se agrupan en 
#la playa, manteniendo constante el ancho del prosoma.
#Por otro lado, los coeficientes para el intercept y para el ancho no varían
#demasiado. 
#Este análisis fue para analizar que cuanto mayor es el color (en una unidad),
#menor es  el agrupamiento esperado alrededor de cada hembra.
#####

summary(m33)
#Predicciones
#Se puede predecir la media de la respuesta para el valor de ancho y color que
#queramos con "predict". Para obtener las predicciones del modelo para nuevos 
#datos, indicamos el nuevo data.frame en el argumento newdata  

#Por ejemplo, para una anchura igual a 22.5 y un color de 4:
nuevosdatos<-data.frame(Ancho_centrado = 0,Color = levels(datos$Color))
nuevosdatos
mutate(nuevosdatos, 
       satelites = predict.glm(m33, newdata=nuevosdatos, type="response")
       )

# 1.401001 
summary(m33)
?predict
#############################################################################

#Considerando las dos covariables
#Los valores ajustados son una función exponencial de Ancho (y Color):
par(mar = c(5, 5, 2, 2))
rango <- range(datos$Ancho_centrado)
MyData <- expand.grid(Ancho_centrado = seq(rango[1], rango[2], length.out = 173), 
                     Color=levels(datos$Color))
MyData$Satelites <- predict(m33, newdata = MyData, type = "response")

library(ggplot2)
ggplot(datos, aes(Ancho_centrado, Satelites)) +
  geom_point() +
  geom_line(data = MyData, aes(colour = Color, group = Color))

library(lsmeans)
lsmeans(m33, pairwise ~ Color, adjust = "none")
#############################################################################



##############################################################################
#################Bondad de Ajuste del Modelo##################################
#Analicemos los gráficos diagnósticos y la Sobredispresión
#En R, una vez que hemos calculado los valores in?uyentes, podemos guardarlos 
#en objetos y representarlos de la forma más conveniente, con histogramas, 
#boxplots etcétera. Aunque lo más cómodo es utilizar funciones que vienen 
#implementadas en la instalación base o en algunos paquetes como car. 
#Se puede utilizar la función genérica plot sobre un objeto glm, que dibuja 
#algunos grá?cos de diagnóstico.
##############################
#Gráficos de residuos: 

autoplot(m33, which = 1:6)

par(mfrow = c(2, 2))
plot(m33, cex = 0.6)
#El primer grá?co, empezando por arriba y de izquierda a derecha, 
#se muestran los residuos de la devianza frente al predictor lineal 
#(transformaciones logit). 
#El segundo compara los residuos de la devianza estandarizados con 
#los cuantiles de una normal estándar. 
#El tercer grá?co es de nuevo un grá?co de los residuos frente al predictor lineal, pero tomando la raíz 
#cuadrada del valor absoluto de los residuos de la devianza estandarizados.
#El cuarto grá?co es el más útil para detectar valores in?uyentes, ya 
#que compara los residuos estandarizados de Pearson con los "hat values" 
#y además muestra líneas de contorno para las distancias de Cook. 
#Como ya habíamos visto, no hay ninguna observación que tenga un valor 
#elevado. En el cuarto grá?co se han etiquetado por defecto las 3 
#observaciones con una mayor distancia de Cook. Si sólo estamos 
#interesados en ver las observaciones in?uyentes, podemos utilizar 
#dos grá?cos que no se muestran por defecto, como es el que muestra 
#las distancias de Cook y el que las compara con los "hqq". 
#Para eso, especi?camos la opción "which" dentro de la función plot.
par(mfrow = c(1, 2)) 
plot(m33, which = 4) 
plot(m33, which = 6)


#Los valores de residuos inferiores a 2 en valor absoluto se consideran 
#adecuados para determinar que el ajuste es bueno.
#Luego el ajuste es bueno. 

##¿CONCLUSIONES?

#Sobredispersión
#Comprobar el efecto de la sobredispersión sobre los errores
#estándar de los coeficientes y su significación.
#Habitualmente esta situación se debe a la existencia de heterogeneidad 
#entre las observaciones. Esto se puede interpretar como una mezcla o 
#mixtura de distribuciones de Poisson. No es un problema cuando Y tiene 
#una distribución normal porque la normal tiene un para´metro espec´i?co 
#que modeliza la variabilidad.

#En una distribución de Poisson, la media y la varianza son iguales, pero 
#cuando trabajamos con recuentos reales no suele ser cierta esta hipótesis. 
#Con frecuencia la varianza es mayor que la media. A esto se le llama 
#sobredispersión (over-dispersed).

#ggplot(datos , aes(Satelites)) + geom_histogram()

??aes
# Res.dev/df.res: 
deviance(m33) / m33$df.res   #  3.295302

  
#Más adecuadamente se estima con X2/(n-p) (Faraway 2006, p.45)            
n <- nrow(datos)
p <- length(coef(m33))
             
sum(residuals(m33, type="pearson")^2, na.rm=TRUE)/(n-p)  #   3.228763

#Aquí la varianza es algo más de tres veces la media. La estimación del 
#parámetro de dispersión no es más que la suma de los residuos dividida 
#entre sus grados de libertad.

##############################################################################
##############################################################################
SOLUCIONES
#¿Soluciones? Una es corregir los errores estándar mediante un modelo de 
#quasi-poisson

# Regresión de quasi-Poisson (estima el parámetro de sobredispersión)
mq<-glm(Satelites~Ancho+Color,data=datos,family=quasipoisson)
summary(mq)                           
mq1<-glm(Satelites~Ancho,data=datos,family=quasipoisson)
summary(mq1)                           
anova(mq, mq1, test="Chisq")

#No da significativo el color, lo quito.
 
m1<-glm(Satelites~Ancho,data=datos,family=poisson)
summary(m1)                           
summary(mq1)

#Notemos que las estimaciones de los coeficientes NO cambian.

#La estimación de sobredispersión del modelo es la que hemos hecho antes 
#(con los residuos de Pearson)
summary(m1)$dispersion  #3.182205

##¿BINOMIAL NEGATIVA?
library(MASS)
mbn<-glm.nb(Satelites~Ancho,data=datos)
summary(mbn)
summary(mq1)

par(mfrow = c(2, 2))
plot(mbn, cex = 0.6)


datos <- mutate(datos, 
                Satelite_b = as.logical(Satelites))
mb <- glm(Satelite_b ~ Ancho * Color, data = datos,
          family = "binomial")
summary(mb)
anova(mb)
mb2 <- glm(Satelite_b ~ Ancho + Color, data = datos,
          family = "binomial")
anova(mb, mb2)
