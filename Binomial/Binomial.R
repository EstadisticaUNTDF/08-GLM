
#############REGRESIÓN LOGÍSTICA BINOMIAL######################

#se cuenta con un grupo de variables mixtas (edad en años, peso de 
#la próstata en gr, condición de hipertenso (SI/NO) y cirugías anteriores 
#(SI/NO)) medidas en 123 pacientes paraguayos, con Hiperplasia Prostática 
#Benigna, sometidos a uno de dos procedimientos quirúrgicos RTUP (Resección 
#Endoscópica Transuretral) y PR (Prostatectomía Radical o Vía Alta) en el 
#Hospital de la Universidad Nacional de Asunción. 

library(tidyverse)

########Leemos el conjunto de datos###########
Datos <- read.table("Binomial/Próstata.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
##Veamos el tipo de cada una de las variable##
str(Datos)


##Mostramos los datos##
show(Datos)


##Elegimos las variables con las cuales vamos a trabajar, sugeridos por el investigador##
datos<- Datos %>% 
  select(mproc, edad, hta, opanterior, peso)
  
str(datos)

####Convertimos la variable respuesta en variable numérica (1=rtup, 0=via)####
#######################################################################
datos <- transform(datos, 
                   mproc = as.numeric(mproc) - 1)

#####Análisis Bivariado: Gráficos
ggplot(datos, aes(peso, mproc)) +
  geom_point(position = position_jitter(height = 0.1)) +
  labs(x = "Peso (g)", y = "Tipo de Cirugía")

ggplot(datos, aes(edad, mproc)) +
  geom_point(position = position_jitter(height = 0.1)) +
  labs(x = "Edad", y = "Tipo de Cirugía")

#Para evitar el solapamiento de los puntos en los gráficos previos, se ha 
#utilizado la función "jitter" que añade un pequeño error aleatorio. 


#Curva ajustada para la edad 
# El enlace por defecto es log(pi/(1-p))
modelo.1<-glm(mproc ~ peso, family=binomial, data=datos) 
summary(modelo.1)

curva <- data.frame(peso = seq(min(datos$peso), max(datos$peso), by = 0.2)) %>% 
  mutate(mproc = 1/(1 + exp(-modelo.1$coefficients[1] - modelo.1$coefficients[2] * peso)))

ggplot(datos, aes(peso, mproc)) +
  geom_point(position = position_jitter(height = 0.1)) +
  labs(x = "Peso (g)", y = "Tipo de Cirugía") +
  geom_line(data = curva)

#####Análisis Inferencial#################
####Selección de modelos automática con stepwise y teniendo en cuenta la bondad de 
#los ajustes con AIC###
#Una alternativa más cómoda: selección automática de variables 
#por pasos (esta se basa en AIC... aunque vulnera el origen de este
#criterio al hacer la selección por pasos)

#Construcción del Modelo: 
modelo.saturado <- glm(mproc ~ edad + peso + hta * opanterior , family=binomial, 
                       data = datos) 
with(datos, table(hta, opanterior, mproc))
summary(modelo.saturado)

#Selección de Modelos Automática: Stepwise (AIC)
modelo.step <- step(modelo.saturado, direction = "both")
      
summary(modelo.step)
#Es muy típico que AIC acabe incorporando muchas variables explicativas, pero
#vemos en este caso que sólo ingresa la variable "Peso".
coef(modelo.step)
exp(coef(modelo.step))
autoplot(modelo.step)


#install.packages("countreg", repos="http://R-Forge.R-project.org")

#Con summary obtenemos el resumen del modelo ajustado
summary(modelo.step)#AIC: 97.83
summary(modelo.inicial)#AIC: 159.93
Explica (199 - 98.2) / 199 = 0.5065%.

##Interpretación de los coeficientes##
#Cuando la variable explicativa es categórica, lo que se analiza 
#con los cocientes de ventaja en términos probabilísticos, es el cambio 
#marginal de pasar de la categoría de referencia a otra categoría de esta variable.
#Si la OR resulta inferior a la unidad se calcula su inversa (OR^(-1)) para una 
#mejor interpretación, siendo ahora la ventaja a favor de la categoría de referencia.

#Los parámetros eßi con i = 0,··· ,p se denominan “odds ratios”, e indican 
#una relación lineal en escala logarítmica entre el cociente de probabilidades 
#(la probabilidad de que ocurra un suceso dividido por la probabilidad de que 
#no ocurra) y las covariables, es decir, cuánto se modifican las probabilidades 
#por unidad de cambio en las variables Xi (i = 1,··· ,r). 
#Como aquí la única covariable en el modelo es el "Peso", resulta: 
exp(coefficients(modelo.step))
exp(coefficients(modelo.step))[1] #=0.0006327392<1 entonces:  indica OR^(-1)=1/0.0006327 
#=1580.528 ¿¿¿¿???? 
exp(coefficients(modelo.step))[2] #= 1.163733 indica la ventaja de elegir la  
#cirugía para rtup (y=1) es 1.63 veces mayor que elegir la cirugía para via (y=0) cuando 
#aumenta el peso de la próstata en un gramo.
#RTUP (Resección Endoscópica Transuretral) y PR (Prostatectomía Radical o Vía Alta).


#######################################################################
###############Bondad de Ajuste#####################
#Test de Hosmer-Lemeshow
#Área bajo la curva de ROC
#Punto de corte y tasa de clasificaciones correctas

#################
###Área bajo la curva de ROC: 
# La curva ROC es una medida de la capacidad de discriminación del modelo, 
# conocida como AUC. Dicha curva grafica la proporción de resultados clasi?cados 
#incorrectamente y la proporción de resultados clasi?cados correctamente, dado 
#cierto punto de corte. El área bajo la curva ROC (AUC, Area Under the Curve), 
#cuyo valor ?uctúa entre 0 y 1, provee una curva de la habilidad del modelo para 
#discriminar entre aquellos individuos que presentan la característica versus 
#aquellos que no (Hosmer and Lemeshow, 2000).

#En este caso:
# El área bajo la curva de ROC (estadístico AUC) representa la probabilidad de
# que un nido de chimango con respuesta 1 (probabilidad de ser exitoso) tenga
# un valor en la escala de medida considerada mayor que un nido con respuesta 0
# (probabilidad de no ser exitoso). 
# El AUC es mejor cuanto más cercano esté a la unidad.
#A modo de guía para interpretar las curvas ROC se han establecido los siguientes
#intervalos para los valores de AUC:
#[0.5, 0.6): Test malo.
#[0.6, 0.75): Test regular.
#[0.75, 0.9): Test bueno.
#[0.9, 0.97): Test muy bueno.
#[0.97, 1): Test excelente.
#El mejor modelo será aquel con mayor área bajo la curva ROC, o aquel cuya curva 
#ROC se sitúa en un punto en la esquina superior izquierda, o coordenada (0,1) 
#del espacio, representando un 100% de sensibilidad (ningún falso negativo) y un 
#100% también de especificidad (ningún falso positivo).

#Según Hosmer, D. y Lemeshow, S. (2000), el modelo es preciso y tiene alta 
# capacidad de discriminación cuando AUC >= 0, 7

library(gplots)
library(ROCR)
yhat<-predict(modelo.step, type="response", se=T)
yhat<-yhat$fit

ROC <- performance(prediction(yhat, datos$mproc),"tpr","fpr") 
plot(ROC)

### donde:
#tpr: es el vector de valores de la tasa de verdaderos aciertos (sensibilidad).
#fpr: es el vector de valores de la tasa de falsos aciertos (especificidad).


### Una vez graficada, se calcula el área bajo la curva (AUC), que es la 
#probabilidad de concordancia entre las predicciones y los resultados, 
#utilizando la siguiente sintaxis:

area <- performance(prediction(yhat, datos$mproc),"auc")
attributes(area)$y.values[[1]]
##0.8975603

########Test de Hosmer y Lemeshow###########
#Test de Hosmer-Lemeshow
#Hosmer y Lemeshow propusieron evaluar la bondad de ajuste del modelo 
#mediante el agrupamiento de las observaciones según las probabilidades 
#estimadas por el modelo. 
#Este test mide cuán alejados están los valores observados de los valores predichos por el modelo.
#Hipótesis del test:
#H0: el modelo ajusta al conjunto de datos
#H1: el modelo no ajusta al conjunto de datos

library(MKmisc)

#Se define la función que realiza el test de Hosmer y Lemeshow 
hosmerlem <-  function (y, yhat, g =10) 
  {   
  cutyhat <- cut(yhat, 
                 breaks = quantile(yhat, 
                                   probs = seq(0, 1, 1/g), type=9), 
                 include.lowest = T)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2/expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
  }
hosmerlem(datos$mproc, yhat, 5)




