library(prophet)
library(plyr)
library(ggplot2)
library(Metrics)
library(carData)
library(numpy)
library(ggfortify)
library(scales)

setwd("C:\\Users\\abete\\OneDrive\\Desktop\\Università\\Data science lab\\Progetto-Data-Science-Lab\\data")
d<-read.csv("serie-storiche-ecommerce-pulito-no-buchi-temporali.csv", sep=",")

d$data<-as.Date(d$data)

#Creazione training set
ds<-d$data[1:12124]
y<-d$totale[1:12124]
train<-data.frame(ds,y)


#Modello prophet con previsione di 365 giorni
m <- prophet(train,interval.width=0.95,daily.seasonality=TRUE, n.changepoints = 4)
m
m$changepoints

future <- make_future_dataframe(m, periods = 365)
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#Visualizzazione con previsione
forecast$ds<-as.Date(forecast$ds)
plot(m,forecast)+scale_y_continuous(n.breaks =10)+labs(y="revenue(dollars)",x="time")+add_changepoints_to_plot(m)
#visualizzazione singoli componenti della previsione(trend, weekly, yearly, daily)
prophet_plot_components(m, forecast)

#Creazione test set
ds_test<-d$data[12125:15155]
y_test<-d$totale[12125:15155]
test<-data.frame(ds_test,y_test)

#Calcolo del root mean squared error
rmse<-rmse(forecast$yhat[1:3031],test$y_test)
mean<-mean(test$y_test)


#calcolo mean absolute percentage errore
mean(abs((test$y_test-forecast$yhat[1:3031])/test$y_test))*100

max(train$y)
