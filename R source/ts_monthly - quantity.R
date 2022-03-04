rm(list=ls())

#library(ggfortify)
#library(ggplot2)
#library(gplots)
#library(caret)
#library(pROC)
#library(mlr)

library(forecast)
library(lubridate)
library(dplyr)
library(ggplot2)
library(xts) # para funcion xts
library(scales)
library(tidyr) # para funcion replace_na
library(zoo) #as.yearmon
library(dygraphs)
library(tseries)

library(ggfortify) # para que funcione autoplot
library(TSA) # para acf

#pruebas de tendencia
library(funtimes)#notrend_test
library(Kendall)#prueba mann-kendall
library(trend)
library(randtests)#prueba Cox Stuart

#pruebas de estacionalidad
library(seastests)

#pruebas de estacionaridad
library(aTSA)
library(forecast)#boxcox
#library(TSA)
library(MTS)

#Media Movil
library(pracma)
library(DescTools)#RMSE MAPE MAE

#Modelamiento Arima
library(readxl)
library(sweep)
library(gridExtra)
library(tidyquant)
library(caschrono)

# Pre-procesamiento ----------------------------------------

setwd("D:/R/codigos_tesis")

data<-read.csv("3dmovil_ventas_depurado.csv",sep=";", header=T)
summary(data)
str(data)

data.m1<-subset(data,select=c("purchase.date","asin","quantity","item.price","cbm"))


pairs(data.m1$cbm ~ data.m1$quantity)
library(nortest)
ad.test(data.m1$cbm)
cor(data.m1$cbm, data.m1$quantity, method = "spearman")
plot(data.m1$cbm ~ data.m1$quantity)
cor.test(data.m1$cbm, data.m1$quantity, method = "spearman")

td2 = as.Date(data.m1$purchase.date,format = "%Y-%m-%d")
td2

data.m1 %>% 
  mutate(purchase.date=ymd(td2)) %>%
  mutate(AÑO = year(purchase.date)) %>% 
  mutate(TRI = quarter(purchase.date)) %>% 
  mutate(MES = month(purchase.date)) %>% 
  mutate(SEM = week(purchase.date)) %>% 
  mutate(SEE = epiweek(purchase.date)) %>% 
  mutate(DIA = day(purchase.date)) %>%
  mutate(AnioMes  = as.yearmon(purchase.date)) %>%
  mutate(dia = yday(purchase.date)) -> data.m1

data.m1
attach(data.m1)


xts(quantity,purchase.date) -> quantity2
xts(cbm,purchase.date) -> cbm


quantity2 %>% apply.daily(sum) -> quantity_daily
quantity2 %>% apply.weekly(sum) -> quantity_weekly
quantity2 %>% apply.monthly(sum) -> quantity_monthly
quantity2 %>% apply.quarterly(sum)
quantity2 %>% apply.yearly(sum)

cbm %>% apply.daily(sum) -> cbm_daily
cbm %>% apply.weekly(sum) -> cbm_weekly
cbm %>% apply.monthly(sum) -> cbm_monthly
cbm %>% apply.quarterly(sum)
cbm %>% apply.yearly(sum)


# Analisis explorativo ----------------------------------------

plot(quantity_daily)
plot(quantity_weekly)
plot(quantity_monthly)

plot(cbm_daily)
plot(cbm_weekly)
plot(cbm_monthly)

quantity2 %>% apply.weekly(sum) %>% 
  plot(main = "Ventas netas 3d Movil")

quantity2 %>% 
  apply.daily(sum) %>% 
  barplot(col="forestgreen")

cbm %>% apply.weekly(sum) %>% 
  plot(main = "Uso de metros cubicos")

cbm %>% 
  apply.daily(sum) %>% 
  barplot(col="forestgreen")


#item.price2 %>% apply.daily(sum) -> item.price_daily
#item.price2 %>% apply.weekly(sum)

#plot(item.price_daily)

# item.price2 %>% apply.weekly(sum) %>% 
#   plot(main = "Ventas netas 3d Movil")
# 
# item.price2 %>% 
#   apply.daily(sum) %>% 
#   barplot(col="forestgreen")

ts_cbm_cantidades=cbind(cbm_daily,quantity_daily)
plot(ts_cbm_cantidades,plot.type = "single",col=c("blue","red"))

ts_quantity <- ts(as.numeric(quantity_monthly), frequency = 12)#convertir xts a ts

plot(decompose(ts_quantity))

adf.test(ts_quantity)

ts_cbm <- ts(as.numeric(cbm_monthly), frequency = 12)

plot(decompose(ts_cbm))

adf.test(ts_cbm)

#library(tsbox)
#ts_item.price <-ts_ts(item.price_daily)
#plot(decompose(ts_item.price))


#dygraph(ts_item.price)


# Graficas de ST - Funcion ggplot2 ----------------------------------------

data.m1 %>% 
  ggplot(aes(x=purchase.date,y=quantity))+
  geom_line(color = "darkblue", size=0.5) + 
  scale_x_date(limits = c(min(data.m1$purchase.date), max(data.m1$purchase.date)),
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "15 days"),
               expand = c(0,0),
               labels = date_format("%d-%b-%y"))+
  scale_y_continuous(breaks = seq(0,10000,1500))+
  labs(x = "Fecha",
       y = "Pacientes",
       title = "Evolucion de ventas 3d Movil",
       subtitle = "",
       caption = "Fuente:Amazon")+
  theme_minimal()+
  theme(axis.text   = element_text(size=8),
        axis.text.x = element_text(angle=45)) -> grafico1

grafico1
#ggsave('grafico1.png',grafico1,width=30,height=20,units="cm")


# Autocorrelaciones --------------------------------------------------------

quantity_monthly %>% 
  stats::acf(main ="Función de autocorrelación",
             plot = F,
             lag = 9)

quantity_monthly %>% 
  stats::acf(main="Función de autocorrelación",lag=9)


# Pruebas de tendencia --------------------------------------------------------

# Regresión lineal --------------------------------------------------------

# 
# quantity_monthly %>% 
#   #mutate(t = 1:nrow(quantity_monthly)) %>% 
#   ggplot(aes(x = 1:nrow(quantity_monthly), y = quantity_monthly)) +
#   geom_line() +
#   geom_smooth(method = "lm")+
#   theme_minimal()
# 
# cbm_monthly %>% 
#   #mutate(t = 1:nrow(quantity_monthly)) %>% 
#   ggplot(aes(x = 1:nrow(cbm_monthly), y = cbm_monthly)) +
#   geom_line() +
#   geom_smooth(method = "lm")+
#   theme_minimal()


# Media móvil -------------------------------------------------------------


# (ma(quantity_monthly, order = 3, centre = F) %>% 
#     zoo(order.by = date(quantity_monthly)) -> movavg1)
# autoplot(cbind(quantity_monthly,movavg1) , facets = FALSE) +
#   labs(title = "Búsquedas del término vacuna",
#        y     = "Interés")+
#   theme(legend.position="bottom") +
#   scale_colour_manual(labels = c("Real", "Media móvil orden 3"), 
#                       values = c("steelblue2", "red")) +
#   theme_minimal()

# Correlograma ------------------------------------------------------------

# quantity_monthly %>% 
#   ts() %>% 
#   TSA::acf(type = "correlation", lag = 26, plot = FALSE) %>% 
#   autoplot() +
#   theme_minimal()

# Correlación de Spearman -------------------------------------------------

quantity_monthly %>% 
  ts %>% 
  cor.test(1:length(quantity_monthly), method = "spearman")
#pvalor: 3.151e-11
#tendencia positiva: 0.7510619 
#Se rechaza H0, esta presente el componente de tendencia

cbm_monthly %>% 
  ts %>% 
  cor.test(1:length(quantity_monthly), method = "spearman")

# Prueba t ----------------------------------------------------------------

notrend_test(quantity_monthly)$p.value
#pvalor: 0.001
#Se rechaza H0, hay tendencia lineal
notrend_test(cbm_monthly)$p.value

# Prueba Mann Kendall -----------------------------------------------------

MannKendall(quantity_monthly)$sl
notrend_test(quantity_monthly,test = "MK", B = 1e4)$p.value
#pvalor: 0
#Se rechaza H0, si hay tendencia monotona

# Prueba de Cox Stuart ----------------------------------------------------

quantity_monthly %>% ts %>% cs.test
#p-value = 0.003892
#Se rechaza H0, si hay tendencia monotoma
quantity_monthly %>% ts %>% cox.stuart.test
#p-value = 0.000145
#Se rechaza H0, si hay tendencia monotoma
quantity_monthly %>% ts %>% cox.stuart.test(alternative = "left.sided")#decreciente
#p-value = 1
#No se rechaza H0, no tiene tendencia descendiente
quantity_monthly %>% ts %>% cox.stuart.test(alternative = "right.sided")#creciente
#pvalor:  7.248e-05
#Se rechaza H0, hay tendencia monotoma y es creciente o positiva

# Prueba de WAK -----------------------------------------------------------

notrend_test(quantity_monthly, test = "WAVK")$p.value
#pvalor:  0 
#Se rechaza H0, si hay tendencia (no estacionario)

# Pruebas de estacionalidad --------------------------------------------------------

data.m1 %>% 
  group_by(AnioMes) %>% 
  summarise(IM = mean(quantity)) -> quantity_AnioMes

quantity_AnioMes %>% 
  ggplot(aes(x=AnioMes,y=IM))+
  geom_bar(stat="identity")+
  labs(x = "Fecha",
       y = "Cantidades",
       title = "Cantidades vendidas por mes",
       caption = "Fuente: Google Trends")+
  theme_minimal()

# Seasonplot --------------------------------------------------------------

quantity_AnioMes$IM %>% 
  ts(frequency = 12, start = c(2018,3)) %>% 
  seasonplot()
#evidencia grafica de no estacionalidad

# Monthplot ---------------------------------------------------------------

quantity_AnioMes$IM %>% 
  ts(frequency = 12, start = c(2018,3)) %>% 
  monthplot()
#El comportamieto es totalmente aleatorio, no detecta al menos un patron, en todos los meses el comportamiento es diferente

# Boxplot -----------------------------------------------------------------

quantity_AnioMes$IM %>% 
  ts(frequency = 12, start = c(2018,3)) -> IM0
boxplot(IM0 ~ cycle(IM0), col = "gold")
#los boxplot se traslapan bastante

# Correlograma ------------------------------------------------------------

quantity_AnioMes$IM %>% 
  ts() %>% 
  TSA::acf(type = "correlation", lag = 36, plot = FALSE) %>% 
  autoplot() +
  theme_minimal()
#No se observa un patron repetitivo, lo cual es un indicativo de no estacionalidad

# Kruskal Wallis ---------------------------------------------------------- 

quantity_AnioMes$IM %>% 
  ts(frequency = 12, start = c(2018,3)) -> IM0
kruskal.test(IM0 ~ cycle(IM0))
#prueba no parametrica
#p-value = 0.9785
#No se rechaza H0, no hay estacionalidad
#No encuentra una diferencia de nivel para un mes, todos los meses presentan un mismo nivel


# Webel y Ollech ----------------------------------------------------------

#prueba de estacionalidad
#supuestos de no independencia

quantity_AnioMes$IM %>% 
  ts(frequency = 12, start = c(2018,3)) %>% 
  wo %>% 
  summary
#P-value:  1 1 0.8082479
#The WO - test does not identify  seasonality

# Pruebas de estacionaridad --------------------------------------------------------


# Prueba KPSS -------------------------------------------------------------

tseries::kpss.test(quantity_monthly,null="Level")#estacionariedad de nivel -> Type1
#Se rechaza H0, la serie no es estacionaria
tseries::kpss.test(quantity_monthly,null="Trend")#estacionariedad de tendencia -> Type3
#No se rechaza H0, la serie es estacionaria
aTSA::stationary.test(quantity_monthly,method = "kpss",lag.short=T)
aTSA::stationary.test(quantity_monthly,method = "kpss",lag.short=F)

aTSA::stationary.test(cbm_monthly,method = "kpss",lag.short=T)
aTSA::stationary.test(cbm_monthly,method = "kpss",lag.short=F)

# Prueba ADF --------------------------------------------------------------

tseries::adf.test(quantity_monthly)
#p-value = 0.4893
#No se rechaza H0, la serie no es estacionaria
aTSA::stationary.test(quantity_monthly,method = "adf")
#No se rechaza H0, la serie no es estacionaria


# Prueba Box Cox ----------------------------------------------------------

(BoxCox.lambda(quantity_monthly) -> l_quantity_monthly)
par(mfrow=c(1,2))
plot(quantity_monthly,type="l")
plot(BoxCox(quantity_monthly,l_quantity_monthly),type="l")

# Prueba McLeod Li --------------------------------------------------------

McLeod.Li.test(y=quantity_monthly)

# Prueba McLeod Li --------------------------------------------------------

archTest(quantity_monthly, lag=10)

# Modelado ----------------------------------------------------------------

# Media móvil  ------------------------------------------------------------

ts_quantity %>% movavg(n = 3, type="s")#media movil de orden 3

# Data split

ntotal = length(ts_quantity)  # longitud de la serie
ntotal
ntrain = 30 # longitud data de entrenamiento
nvalid = 3  # longitud data de validación
ntest  = 3  # longitud data de prueba

train  = ts_quantity %>% window(start = 1, end = 1+(ntrain-1)/12)
valid  = ts_quantity %>% window(start = 1+(ntrain)/12, end = 1+(ntrain+nvalid-1)/12)
test   = ts_quantity %>% window(start = 1+(ntrain+nvalid)/12)

rmse = mape = mae = NULL
for(r in 2:12){
  fit       = movavg(train, n = r, type="s")
  pred      = rep(fit[length(fit)],nvalid)
  rmse[r-1] = RMSE(pred,valid, na.rm=T)#raiz del cuadrado medio del error
  mape[r-1] = MAPE(pred,valid, na.rm=T)
  mae[r-1]  = MAE(pred,valid, na.rm=T)
}

data.frame(r = 2:12, rmse, mape, mae)

y.obs = ts(c(train,valid,NA))
y.sua = movavg(ts(c(train,valid)), n=5, type="s")#9 es el r que minimiza 2 de los 3 indicadores r=9
y.est = ts(c(NA,y.sua))

data.frame(y.obs,y.est)

autoplot(ts.union(y.obs,y.est),
         facets = FALSE) +
  geom_point()+
  labs(x       = "Día", 
       y       = "Número de pruebas", 
       title   = "Pruebas moleculares de detección COVID-19 a nivel nacional",
       caption = "Fuente: MINSA") +
  scale_colour_manual(labels = c("Serie observada","Predicción"), 
                      values = c("darkblue", "red")) + 
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text  = element_text(color = "deepskyblue4"))


#Modelamiento ARIMA

ts_quantity %>% aTSA::stationary.test(method = "kpss",lag.short=T)
ts_quantity %>% aTSA::stationary.test(method = "kpss",lag.short=F)
ts_quantity %>% aTSA::stationary.test(method = "adf")
ts_quantity %>% aTSA::stationary.test(method = "pp",lag.short=F)
ts_quantity %>% aTSA::stationary.test(method = "pp",lag.short=T)
ts_quantity %>% BoxCox.lambda()#aplicar la transformacion
ts_quantity %>% archTest()
ts_quantity %>% McLeod.Li.test(y=.)



ts_quantity %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=T)
ts_quantity %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=F)
ts_quantity %>% diff %>% aTSA::stationary.test(method = "adf")
ts_quantity %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=F)
ts_quantity %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=T)
ts_quantity %>% diff %>% BoxCox.lambda()#aplicar la transformacion dentro de argumento transformacion logaritmica
ts_quantity %>% diff %>% archTest()
ts_quantity %>% diff %>% McLeod.Li.test(y=.)

ts_quantity %>% diff %>% TSA::acf(type = "correlation", lag = 28)#extinción
ts_quantity %>% diff %>% TSA::acf(type = "partial", lag = 28)#truncamiento

# Modelamiento ------------------------------------------------------------

ts_quantity %>% auto.arima#evalua automatica p y q

ntotal = length(ts_quantity)
ntrain = 36
h      = 6 #proximos 6 meses
medidas1 = medidas2 = medidas3 = medidas4 = medidas5 = medidas6 = NULL

for(i in 0:(ntotal-ntrain-h)){
  #training <- ts_quantity %>%  window(start = 1, end = ntrain + i)
  #training <- ts_quantity %>% window(start = 1, end = 1+(ntrain-1)/12)
  training <- log(ts_quantity) %>% window(start = 1, end = (ntrain+i)/12)
  #testing  <- ts_quantity %>%  window(start = ntrain + i + 1, end= ntrain + i + 4)
  #testing  <- ts_quantity %>%  window(start = 1+(ntrain+i)/12)
  testing  <- log(ts_quantity) %>%  window(start = (ntrain+i+1)/12)
  
  modelo1  <- training %>% Arima(order=c(1,1,1))#asumiendo extinciones
  modelo2  <- training %>% Arima(order=c(0,1,1))#autoarima
  modelo3  <- training %>% Arima(order=c(0,1,2))
  modelo4  <- training %>% Arima(order=c(0,1,0))#asumiendo ruido blanco
  modelo5  <- training %>% Arima(order=c(1,1,0))
  pred1    <- modelo1 %>% forecast::forecast(h=6)#se calculan las predicciones 7 periodos hacia adelante
  pred2    <- modelo2 %>% forecast::forecast(h=6)
  pred3    <- modelo3 %>% forecast::forecast(h=6)
  pred4    <- modelo4 %>% forecast::forecast(h=6)
  pred5    <- modelo5 %>% forecast::forecast(h=6)
  medidas1 <- rbind(medidas1, accuracy(pred1,testing)[2,])
  medidas2 <- rbind(medidas2, accuracy(pred2,testing)[2,])
  medidas3 <- rbind(medidas3, accuracy(pred3,testing)[2,])
  medidas4 <- rbind(medidas4, accuracy(pred4,testing)[2,])
  medidas5 <- rbind(medidas5, accuracy(pred5,testing)[2,])
}


medidas1 %>% colMeans#
medidas2 %>% colMeans##
medidas3 %>% colMeans####
medidas4 %>% colMeans#
medidas5 %>% colMeans#

ts_quantity %>% Arima(order = c(0,1,1)) -> modelo_2
ts_quantity %>% Arima(order = c(0,1,2)) -> modelo_3


modelo_2 %>% residuals -> residuales_2
modelo_3 %>% residuals -> residuales_3

modelo_2 %>%
  sw_augment() %>% 
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Residuales del modelo ARIMA(0,1,1)", x = "") + 
  theme_minimal() -> graf_res_2

modelo_3 %>%
  sw_augment() %>% 
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Residuales del modelo ARIMA(0,1,2)", x = "") + 
  theme_minimal()-> graf_res_3

grid.arrange(graf_res_2, graf_res_3)

modelo_2 %>% t_stat
modelo_3 %>% t_stat
modelo_2

modelo_2 %>% residuals %>% t.test#media igual a 0
modelo_3 %>% residuals %>% t.test#media igual a 0

modelo_2 %>% residuals %>% shapiro.test #no normalidad
modelo_3 %>% residuals %>% shapiro.test #no normalidad


library(nortest)

modelo_2 %>% residuals %>% ad.test
modelo_3 %>% residuals %>% ad.test

modelo_2 %>% residuals %>% ks.test("pnorm")
modelo_3 %>% residuals %>% ks.test("pnorm")

residuales_2 %>% hist()
residuales_3 %>% hist()

residuales_2 %>% qqnorm();residuales_2 %>% qqline()
residuales_3 %>% qqnorm();residuales_3 %>% qqline()

library(moments)
residuales_2 %>% kurtosis
residuales_3 %>% kurtosis

residuales_2 %>% TSA::acf(lag=12)#se visualizan 1 autocorrelacion que sale de las bandas pero tiene un desfaso corto al inicio
residuales_3 %>% TSA::acf(lag=12)#se visualizan 1 autocorrelacion que sale de las bandas pero tiene un desfaso corto al inicio

residuales_2 %>% BoxCox.lambda()
residuales_3 %>% BoxCox.lambda()

residuales_2 %>% aTSA::stationary.test(method="kpss")
residuales_3 %>% aTSA::stationary.test(method="kpss")

residuales_2 %>% aTSA::stationary.test(method="adf")
residuales_3 %>% aTSA::stationary.test(method="adf")

residuales_2 %>% aTSA::stationary.test(method="pp")
residuales_3 %>% aTSA::stationary.test(method="pp")



# Prediccion --------------------------------------------------------------

modelo_3 %>% forecast::forecast(h=6)

modelo_3 %>% forecast::forecast(h=6) %>% autoplot +
  labs(x = "Día",
       y = "Y") + 
  theme_minimal()

modelo_3 %>% 
  forecast::forecast(h=6) %>% 
  sw_sweep() %>%
  ggplot(aes(x = index, y = value, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line(size = 1) +
  labs(title = "Predicción Cantidad Vendida", x = "", y = "°C") +
  scale_color_tq() +
  theme_tq()
