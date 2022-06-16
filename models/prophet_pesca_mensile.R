library(prophet)
library(plyr)
library(ggplot2)
library(Metrics)
library(carData)
library(ggfortify)
library(scales)

setwd("C:\\Users\\abete\\OneDrive")
d_mp<-read.csv("pesca_mensile.csv", sep=",")

d_mp$Datetime<-as.Date(d_mp$Datetime)

#Creazione training set PESCA MENSILE:
ds<-d_mp$Datetime[73:96]
y<-d_mp$totale[73:96]
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

#Creazione test set
ds_test<-d_mp$Datetime[97:100]
y_test<-d_mp$totale[97:100]
test<-data.frame(ds_test,y_test)


#MAPE PESCA TRIMESTRALE
M=mape(y_test,forecast$yhat)
MAPE=M*100
MAPE
