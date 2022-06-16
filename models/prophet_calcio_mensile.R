library(prophet)
library(plyr)
library(ggplot2)
library(Metrics)
library(carData)
library(ggfortify)
library(scales)

setwd("C:\\Users\\abete\\OneDrive")
d_mc<-read.csv("calcio_mensile.csv", sep=",")

d_mc$Datetime<-as.Date(d_mc$Datetime)

#Creazione training set CALCIO MENSILE:
ds<-d_mc$Datetime[73:96]
y<-d_mc$totale[73:96]
train<-data.frame(ds,y)

#Modello prophet con previsione di 365 giorni:
m <- prophet(train,interval.width=0.95, n.changepoints = 4)
m
m$changepoints

future <- make_future_dataframe(m, periods = 365)
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#Visualizzazione con previsione
forecast$ds<-as.Date(forecast$ds)
plot(m,forecast)+scale_y_continuous(n.breaks =10)+ labs(y="revenue",x="time")+add_changepoints_to_plot(m)

#visualizzazione singoli componenti della previsione(trend, weekly, yearly, daily)
prophet_plot_components(m, forecast)

group_by()

#Creazione test set
ds_test<-d_mc$Datetime[97:100]
y_test<-d_mc$totale[97:100]
test<-data.frame(ds_test,y_test)

#MAPE PESCA TRIMESTRALE
M=mape(y_test,forecast$yhat)
MAPE=M*100
MAPE
