
#############REGRESI�N POISSON######################

#Se considera que la variable respuesta es el n�mero de concubinos 
#y las variables explicativas son: color, estado de la espina central,
#peso y anchura del caparaz�n. 

########Leemos el conjunto de datos###########
# El  ejemplo  que  vamos  a  desarrollar  en  el  R,  es  un  ejemplo  de  datos
# tomados  sobre  el sistema de apareamiento del cangrejo herradura. Los datos 
# fueron publicados en Ethology (1996) por Jane Brockmann. Los machos de los 
# cangrejos herradura presentan dos t�cticas reproductivas.  O  ser  el  macho  
# principal  o  ser  un  macho  "sat�lite".  Se  relev�  informaci�n sobre 4 
# variables que se cree tienen influencia sobre la cantidad de machos sat�lites 
# que se re�nen  alrededor  de  una  hembra.  El  color,  clasificado  como  
# 1=medio  claro,  2=medio,  3= medio oscuro, 4= oscuro; la condici�n de las
# espinas, clasificadas como 1= ambas bien, 2= una  gastada  o  rota  o  3=  
#   ambas  gastadas  o  rotas).  Tambi�n  se  tomaron  los  anchos  de caparaz�n 
# de  las  hembras  y  su  peso  en  gramos.  Como  variable  respuesta  se  cont�
# el n�mero de machos sat�lites que se hallaron alrededor del par de c�pula.
library(ggfortify)
library(dplyr)
library(tidyr)

Datos <- read.table("Poisson/poisson.csv", 
                    header=TRUE, sep=";", na.strings="NA", 
                    dec=",", strip.white=TRUE)
##Veamos el tipo de cada una de las variable##
str(Datos)

##Modificamos la variable "Espina" a cualitativa (la variable "Color" contin�a
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
###Medidas Res�men#########
#Variables cuantitativas: 
library(RcmdrMisc)
lapply(datos[, 3:5], numSummary, statistics=c("mean", "sd", "IQR", "quantiles", "cv"),
           type=c("2", "1", "3"),quantiles=c(0, .25, .5, .75, 1))

#Un gr�fico "violinplot" es una combinaci�n de un box-plot y un gr�fico de 
#densidad. Espec�ficamente, comienza con un diagrama de caja. A continuaci�n, 
#agrega un gr�fico de densidad rotado a cada lado del box-plot.

datos %>% 
  select(Ancho, Peso, Satelites) %>% 
  gather(Variable, Valor) %>% 
  ggplot(aes(x = 1, y = Valor)) +
  geom_violin() +
  geom_boxplot(width = 0.5) +
  facet_wrap(~Variable, scales = "free_y")

table(datos$Satelites)
#ya sabemos que un box-plot para variables cuantitativas
#discretas no es apropiado, pero claramente se ve la asimetr�a en el comportamiento
#de los 

#Variables cualitativas:
pie(table(datos$Espina))

###Gr�ficos################
#Realizamos un gr�fico exploratorio de los datos obtenidos. Tri�ngulo 
#superior: correlaci�n entre las variables. Diagonal: 
#estimaci�n de la densidad de la variable. Tri�ngulo inferior: diagrama de 
#dispersi�n para pares de variables.

library(psych) 
pairs.panels(datos, pch=21,main="Gr�fico exploratorio: Matriz de Dispersi�n, Histograma y Correlaci�n")
with(datos, table(Espina, Color))
#Observando los gr�ficos:
#Alta correlaci�n lineal entre "Peso_centrado" y "Ancho_centrado" (0.89), a tener en cuenta, pues uno de los
#supuestos de GLM es que las covariables sean independientes.

###############################################################################
#################An�lisis Inferencial##########################################
#Selecci�n de variables: M�todo Stepwise (Devianza)

#TENER EN CUENTA: 
#Criterio exclusivamente estad�stico: no se tienen en cuenta otros 
#�conocimientos� sobre las variables m�s interesantes a incluir (aunque 
#se puede forzar a que algunas variables siempre est�n en el modelo);  
#Si hay un conjunto de variables muy correlacionadas, s�lo una ser� 
#seleccionada; 
#No es f�cil tener en cuenta interacciones entre variables (los modelos deben
#ser jer�rquicos). 

###########Paso 1############### 
#Comenzamos haciendo GLM para el modelo nulo y el modelo que incluye
#s�lo cada uno de las covariables 
mnull<-glm(Satelites~1,data=datos,family=poisson(link = "log"))
mnull
#Por defecto considera que la funci�n de enlace es logaritmo natural
mnull<-glm(Satelites~1,data=datos,family=poisson)

m1<-glm(Satelites~Color,data=datos,family=poisson)
m2<-glm(Satelites~Espina,data=datos,family=poisson)
m3<-glm(Satelites~Ancho_centrado,data=datos,family=poisson)
m4<-glm(Satelites~Peso_centrado,data=datos,family=poisson)

#Con la funci�n "anova" comparo el modelo nulo con cada uno de los modelos 
#generados con una sola variable explicativa. La "Null deviance" es la desviaci�n
#para el modelo que no depende de ninguna variable. La "Residual deviance" es la 
#diferencia entre la desviaci�n del modelo que no depende de ninguna variable
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

#(Hauck and Donner, 1977) examinaron el desempe�o del test de Wald y 
#encontraron que a menudo falla para rechazar la hip�tesis nula. En ese 
#art�culo recomiendan el contraste condicional de raz�n de verosimilud . 
#Tanto el test de Wald como el de Raz�n de Verosimilitud requieren el c�lculo de la estimaci�n 
#m�ximo veros�mil de �. Un test que simplifica estos c�lculos es el test de 
#Score, basado en las propiedades de las derivadas parciales de la funci�n 
#de verosimilitud. En R podemos utilizar el argumento test=�Rao� dentro de
#la funci�n anova para obtener este contraste. La potencia de c�lculo ya no 
#es un problema y el test Score no suele utilizarse.
anova(mnull,m1, test = "Rao")#Vemos aqu� que entrar�a "Color". 
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

#�Resulta que s� son significativos! Entrar�a el "Peso_centrado" pues
# (valor p=0.004735 < 0.005592)
# pero la correlaci�n entre "Peso_centrado" y "Ancho_centrado" es muy alta!, entonces tomamos
# la decisi�n de no incluir el "Peso_centrado". Quien ingresa es el "Color".
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

#En este paso vemos que eliminar del modelo la variable �Ancho_centrado� 
#o la variable "Color", empeora el ajuste ya que en los dos casos,
#su ausencia aumenta significativamente el valor de la devianza residual
#(observar los g.l.).

#Y contin�o con el modelo m33. 


###########Paso 4###############

#Partimos de un modelo que incluye las variable seleccionadas ("Ancho_centrado" y "Color"):
#m33 y lo comparamos con un conjunto de modelos que incluyen el resto de las
#variables que se quieren poner a prueba.
m333<-glm(Satelites~Ancho_centrado+Color+Espina,data=datos,family=poisson)
anova(m33,m333, test="Chisq")

#La disminuci�n en la Devianza Residual debido a la inclusi�n de 
#la variable �Espina� en el modelo no es significativa.
#Por lo tanto decidimos NO incluirla. 

#Nos quedamos con el modelo m33 que involucra las covariables #Ancho_centrado", y "Color".
#Ancho_centrado del Peso_centrado del prosoma del cangrejo. 
#Color del caparaz�n (m�s oscuro indica peor estado).

#################El modelo FINAL es m33####################### 
#Y = b0 + b1Ancho_centrado + b2Color
###################################MODELO FINAL: M33#####################################
summary(m33)
m33_sinI <- update(m33, . ~ . -1)
summary(m33_sinI)
#############################################################################
################Interpretaci�n de los Coeficientes##########################
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

#La manera en la cual se interpreta un modelo de conteo depende si se est� 
#interesado en el valor esperado de la variable de recuento o en la 
#distribuci�n de los recuentos. Si el inter�s est� en recuento esperado, 
#varios m�todos se pueden utilizar para calcular el cambio en la expectativa 
#de un cambio en una independiente variable.
#Si el inter�s est� en la distribuci�n de los recuentos o tal vez s�lo la 
#probabilidad de que un recuento espec�fico, la probabilidad de que un 
#recuento para un nivel dado de las variables independientes se puede calcular.

#Objetivo: "Si el agrupamiento depende de alguna caracter�stica de las hembras".
#Entonces:
eb0 #=0.0953139 indica que el n�mero esperado de machos sat�lites que llegan a 
#la playa para reproducirse aumenta en eb0 cuando sin tener en cuenta el ancho
#y el color de la hembra.
eb1 #=1.161338 indica que el n�mero esperado de machos sat�lites que llegan a
#la playa para reproducirse aumenta en eb1 por cada unidad que cambia
#el ancho del prosoma manteniendo constante el color del cangrejo.
eb2 #= 0.8441681 indica que el n�mero esperado de machos sat�lites 
#disminuye en eb2 por cada unidad de cambio de color, es decir mientras m�s 
#oscuro sea el color, m�s disminuye, en media, la cantidad de machos sat�lites que se agrupan en 
#la playa, manteniendo constante el ancho del prosoma.


#####Prueba: si pensamos la variable "Color" como categ�rica, tendr�amos un coeficiente
#por cada categor�a. Si se realiza ese an�lisis se obtienen coeficiente para el color:
eb21 #= 0.8189875 
eb22 #= 0.646383       
eb23 #= 0.6393139  
#Luego: eb21<eb22<eb23 indica que el n�mero esperado de machos sat�lites 
#disminuye en eb21 (color 3), eb22 (color 4), eb23 (color 5) por cada unidad 
#de cambio de color, es decir mientras m�s oscuro sea el color,
#m�s disminuye, en media, la cantidad de machos sat�lites que se agrupan en 
#la playa, manteniendo constante el ancho del prosoma.
#Por otro lado, los coeficientes para el intercept y para el ancho no var�an
#demasiado. 
#Este an�lisis fue para analizar que cuanto mayor es el color (en una unidad),
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
#Los valores ajustados son una funci�n exponencial de Ancho (y Color):
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
#Analicemos los gr�ficos diagn�sticos y la Sobredispresi�n
#En R, una vez que hemos calculado los valores in?uyentes, podemos guardarlos 
#en objetos y representarlos de la forma m�s conveniente, con histogramas, 
#boxplots etc�tera. Aunque lo m�s c�modo es utilizar funciones que vienen 
#implementadas en la instalaci�n base o en algunos paquetes como car. 
#Se puede utilizar la funci�n gen�rica plot sobre un objeto glm, que dibuja 
#algunos gr�?cos de diagn�stico.
##############################
#Gr�ficos de residuos: 

autoplot(m33, which = 1:6)

par(mfrow = c(2, 2))
plot(m33, cex = 0.6)
#El primer gr�?co, empezando por arriba y de izquierda a derecha, 
#se muestran los residuos de la devianza frente al predictor lineal 
#(transformaciones logit). 
#El segundo compara los residuos de la devianza estandarizados con 
#los cuantiles de una normal est�ndar. 
#El tercer gr�?co es de nuevo un gr�?co de los residuos frente al predictor lineal, pero tomando la ra�z 
#cuadrada del valor absoluto de los residuos de la devianza estandarizados.
#El cuarto gr�?co es el m�s �til para detectar valores in?uyentes, ya 
#que compara los residuos estandarizados de Pearson con los "hat values" 
#y adem�s muestra l�neas de contorno para las distancias de Cook. 
#Como ya hab�amos visto, no hay ninguna observaci�n que tenga un valor 
#elevado. En el cuarto gr�?co se han etiquetado por defecto las 3 
#observaciones con una mayor distancia de Cook. Si s�lo estamos 
#interesados en ver las observaciones in?uyentes, podemos utilizar 
#dos gr�?cos que no se muestran por defecto, como es el que muestra 
#las distancias de Cook y el que las compara con los "hqq". 
#Para eso, especi?camos la opci�n "which" dentro de la funci�n plot.
par(mfrow = c(1, 2)) 
plot(m33, which = 4) 
plot(m33, which = 6)


#Los valores de residuos inferiores a 2 en valor absoluto se consideran 
#adecuados para determinar que el ajuste es bueno.
#Luego el ajuste es bueno. 

##�CONCLUSIONES?

#Sobredispersi�n
#Comprobar el efecto de la sobredispersi�n sobre los errores
#est�ndar de los coeficientes y su significaci�n.
#Habitualmente esta situaci�n se debe a la existencia de heterogeneidad 
#entre las observaciones. Esto se puede interpretar como una mezcla o 
#mixtura de distribuciones de Poisson. No es un problema cuando Y tiene 
#una distribuci�n normal porque la normal tiene un para�metro espec�i?co 
#que modeliza la variabilidad.

#En una distribuci�n de Poisson, la media y la varianza son iguales, pero 
#cuando trabajamos con recuentos reales no suele ser cierta esta hip�tesis. 
#Con frecuencia la varianza es mayor que la media. A esto se le llama 
#sobredispersi�n (over-dispersed).

#ggplot(datos , aes(Satelites)) + geom_histogram()

??aes
# Res.dev/df.res: 
deviance(m33) / m33$df.res   #  3.295302

  
#M�s adecuadamente se estima con X2/(n-p) (Faraway 2006, p.45)            
n <- nrow(datos)
p <- length(coef(m33))
             
sum(residuals(m33, type="pearson")^2, na.rm=TRUE)/(n-p)  #   3.228763

#Aqu� la varianza es algo m�s de tres veces la media. La estimaci�n del 
#par�metro de dispersi�n no es m�s que la suma de los residuos dividida 
#entre sus grados de libertad.

##############################################################################
##############################################################################
SOLUCIONES
#�Soluciones? Una es corregir los errores est�ndar mediante un modelo de 
#quasi-poisson

# Regresi�n de quasi-Poisson (estima el par�metro de sobredispersi�n)
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

#La estimaci�n de sobredispersi�n del modelo es la que hemos hecho antes 
#(con los residuos de Pearson)
summary(m1)$dispersion  #3.182205

##�BINOMIAL NEGATIVA?
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
