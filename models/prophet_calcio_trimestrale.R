library(prophet)
library(plyr)
library(ggplot2)
library(Metrics)
library(carData)
library(ggfortify)
library(scales)

setwd("C:\\Users\\abete\\OneDrive\\Desktop\\Università\\Data science lab\\Progetto-Data-Science-Lab\\data")
d<-read.csv("calcio_trimestrale.csv", sep=",")

d$year<-as.Date(d$year)

#Creazione training set CALCIO TRIMESTRALE:
ds<-d$year[25:32]
y<-d$revenue[25:32]
train<-data.frame(ds,y)

#Modello prophet con previsione di 3 mesi:
m <- prophet(train,interval.width=0.95,daily.seasonality=TRUE, n.changepoints = 4)
m
m$changepoints

future <- make_future_dataframe(m, periods = 90)
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#Visualizzazione con previsione
forecast$ds<-as.Date(forecast$ds)
plot(m,forecast)+scale_y_continuous(n.breaks =10)+ labs(y="revenue",x="time")+add_changepoints_to_plot(m)

#visualizzazione singoli componenti della previsione(trend, weekly, yearly, daily)
prophet_plot_components(m, forecast)

#Creazione test set
ds_test<-d$year[33]
y_test<-d$revenue[33]
test<-data.frame(ds_test,y_test)


#MAPE PESCA TRIMESTRALE
M=mape(y_test,forecast$yhat)
MAPE=M*100
MAPE

