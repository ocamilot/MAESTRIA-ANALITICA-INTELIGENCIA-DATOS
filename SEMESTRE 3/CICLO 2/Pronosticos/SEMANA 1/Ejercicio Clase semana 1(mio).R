#### Ejercicio Analítica Financiera: Tipos de Datos
# PARA INSTALAR LIBRERIAS install.packages("tsfeatures", type = "binary")
library(dygraphs)
library(tseries)
library(xts)
library(quantmod)
library(dplyr)
library(PerfomanceAnalytics)
library(fpp3)
library(tsfeatures)
options(warn = - 1)  
######################################################
###Primero, generemos una función que ayude a simplificar los tipos de datos que deseamos de la fuente de 
#información financiera.
#En este ejemplo, los datos de Cierre y Volúmenes, que dependerán del simbolo o ticker 
#del activo y a partir de qué año se consultan:
##Datos:
start<- Sys.Date()-(365*5) # Cinco años atras
end<-Sys.Date()

precios <- function(simbolo)
{
  # Obtener precios stocks de Yahoo Finance
  datos <- getSymbols(simbolo, auto.assign = FALSE, from=start, to=end, src="yahoo")
  # Eliminando valores faltantes
  datos <- na.omit(datos)
  # Mantenemos columnas con Precios de Cierre y Volúmenes, columnas 4 y 5 de cada stock:
  datos <- datos[, 4]
  # Para hacer los datos accesibles, asignamos a Global Environment:
  assign(simbolo, datos, envir = .GlobalEnv)
}

#Llamar al activo
precios("XOM")
tail(XOM)
colnames(XOM) <- "xom"
str(XOM)

Precios <- dygraph(XOM, main = "Precios x") %>%
  dyAxis("y", label = "Precios") %>%
  dyRangeSelector(dateWindow = c("2020-03-28", "2025-03-28")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(5, "Set1"))
Precios 

# Rendimiento Simple: Precio de hoy menos precio de ayer / precio ayer
precio_ayer <- lag.xts(XOM, 1)
rs_b<- ((XOM - precio_ayer)/(precio_ayer))[-1,] #El -1 es para omitir el primer renglon que tiene un valor NA, lo cual es normal con series de tiempo cuando hago el LAG, por que hay una fecha que no tendra valor.
head(rs_b)

# Con libreria quantmode
rs_spy2<- periodReturn(XOM, period = "daily", type = "arithmetic")
head(rs_spy2)

#Rendimiento logaritmico
rl_spy<- diff(log(XOM))[-1]
head(rl_spy)
#Con paqueteria
rl_spy2<- periodReturn(XOM, period = "daily", type = "log")
head(rl_spy2)

#Features
# PARA INSTALAR LIBRERIAS install.packages("tsfeatures", type = "binary")
library(tsfeatures)
tsfeatures(XOM)# De la salida de esta linea tenemos:
#frequency: Nuestra frecuencia es diaria, esto se denota con el valor 1 en la columna
#nperiods: Identifica si hay estacionalidad (Es decir si identifica que cada X tiempo se repite algo). En este caso el valor es cero.
#trend: la tendencia es cercana a uno (0.984) por lo que es una tendencia significativa, lo que quiere decir que la condicion de ESTACIONARIEDAD (media constante, etc) no se va a cumplir, por lo que la serie no es estacional. Se puede ver en la grafica que esta serie siempre va para arriba.
#Linealidad: Linealidad mayor a uno (32.9) -> Quiere decir que los modelos model based (por ejemplo una regresion lienal) son buenos candidatos para explicar la serie
#Entropy: Medida del desorden o no pronosticabilidad (el reuido). En nuestro caso esta medida es baja (0.145), no es cercana a uno.
#EN RESUMEN DE LINEALIDAD Y ENTROPY: La linealidad es alta y el desorden es bajo, con certeza mis modelos model based podrian predecir esta serie de forma exitosa.


adf.test(XOM) #Es una funcion (Augmented Dickey-Fuller Test) que nos va a decir si la serie es estacionaria o no. Nos dice:
#Hipotesis nula: Tienes una raiz unitaria = Tener esta raiz unitaria genera que nuestra varianza y media no sean constantes.Es decir que mi serie no sea estacionaria.
# Hipotesis alterntica: Que no esta presente esa raiz unitaria.Es decir que la serie es estacionaria.
# Para el analisis nos vamos a fijar en el p-value. Si el p-value es mayor a nivel de significancia (que normalmente esta configurado para que sea 5%) no se rechaza la hipotesis nula.
# En nuestro ejemplo p-value = 61.5%, lo cual es mayor a la sognificancia del 5% -> No rechazo hipotesis nula. Por lo tanto mi serie NO ES ESTACIONARIA.

#Ahora quiero hacer el mimos analisis de ADF, pero sobre los precios y no los  rendimeintos de la accion.
#1. Resto los precios del dia anterior.
db=diff(XOM, 1) %>% na.omit()#El 1 en diff(XOM, 1) es para restar el dia anterior, podria ponerlo 2, o 7.TRambien puedo dejarlo sin el 1, es ll mismo, la funcion por defecto deja 1. La ultima parte de la linea es para borrar los NAs.
# Hago la prueba de mi hipotesis
adf.test(db)
# El resultado me muestra que el p-value = 1% < 5% -> Rechazo hipotesis nula, es decir mi serie es Estacionaria.

#Ahora podemos ver los rezagos: 
pacf(db)
# La grafica que se genera me muestra la correlacion que tienen los datos a lo largo del tiempo. Voy a ver las medidas de dependencia que hay entre los eventos en el tiempo.
# Las lienas puntiadas son el nivel de significancia (5%). Podemos ver que hay datos que se salen del nivel de significancia.
# El LAG 0 es al dia de hoy, y despues empiezo a ver la correlacion con datos del pasado, un LAG de 11 se puede ver que hay un nivel de significancia superior, asi como en 20 o 30.
# Puedo aumentar el lag:
pacf(db, 50)
# Debo empezar a buscar patrones, ver donde tengo los rezagos altos: Aca la profe no profundizo tanto por lo que debe entenderlo un poco mas.


#EXTRAER DATOS DE LA FRED -> DATOS EXOGENOS QUE PUEDO USAR PARA ANALIZAR: DATOS COMO GDP, INFLACION, ETC.
precios2 <- function(sim)
{
  datos<- getSymbols.FRED(sim, from=start, to=end, return.class = "xts", auto.assign=FALSE)
  datos<- na.omit(datos)
  datos<- datos[,1]
  #Adiciono una funcion de rendimientos de una
  rend<- periodReturn(datos, period = "daily", type = "arithmetic")
  #Hacer datos accesibles a global
  assign(sim, rend, envir = .GlobalEnv)
  
}

#Extraemos la info por ejemplo el GDP
precios2("GDP")
plot(GDP)
tail(GDP)
