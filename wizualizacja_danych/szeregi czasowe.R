library(ggpmisc)
library(fpp2)
library(forecast)
library(tidyverse)
library(magrittr)
library(ggplot2)

#------przykłady 1----------

#a
AirPassengers
str(AirPassengers)
summary(AirPassengers)
length(AirPassengers)
start(AirPassengers)
end(AirPassengers)
time(AirPassengers)
deltat(AirPassengers)
frequency(AirPassengers)
cycle(AirPassengers)

plot(AirPassengers, xlab = "Year", ylab = "Number of passengers")


#b

miles<-c(412, 480, 683, 1052, 1385, 1418, 1634, NA, NA, 5948, 6109, 5981, 6753,
         8003, 10566, 12528, 14760, 16769, 19819, 22362, 25340, 25343, 29269, 30514)
miles[8:9] <- median(miles, na.rm = TRUE)
plot(miles)
miles_ts<-ts(miles, start=1937, frequency = 1)
plot(miles_ts)
is.ts(miles_ts)
ts.plot(miles_ts, xlab="year", ylab="air passengers miles sum", main="TS plot")

#c
autoplot(AirPassengers)+
  geom_smooth()


ggplot(AirPassengers, as.numeric = FALSE) + geom_line() +
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red",
             vjust = -0.5, x.label.fmt = "%Y") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1, x.label.fmt = "%Y")


#________ZADANIE 1___________

#a
qcement
str(qcement)
summary(qcement)
length(qcement)
start(qcement)
end(qcement)
time(qcement)
deltat(qcement) #odstęp czasowy między obserwacjami
frequency(qcement) #liczba obserwacji w cyklu
cycle(qcement) #dla kazdego elementu szeregu zwraca numer pozycji w cyklu

autoplot(qcement)+
  geom_line()+
  geom_smooth()+
  labs(title="Cement production")

#b
airquality
str(airquality)

ozonet_airquality1 <- airquality[62:153, c("Ozone", "Temp")]
summary(ozonet_airquality1)
str(ozonet_airquality1)

ozonet_airquality <- na.omit(ozonet_airquality1)
str(ozonet_airquality)

ozonet_ts <- ts(ozonet_airquality, start = c(airquality$Month[1], airquality$day[1]), frequency=30  )
ozonet_ts
str(ozonet_ts)

autoplot(ozonet_ts, facets=TRUE)+
  geom_smooth()+
  labs(x="Miesiac")


#c
arrivals
head(arrivals, 10)
head(arrivals[,"Japan"], 10)

arrivals_japan <- arrivals[,"Japan"]

autoplot(arrivals_japan)+
  geom_smooth()

autoplot(arrivals, facets = TRUE)+
  geom_smooth()
  
ggplot(arrivals, facets=TRUE)+
  geom_line()+
  geom_smooth()
#ZLE BO NW JAK ROZDZIELIC DANE 



# Dekompozycja dla jednego kraju
?stl

fit_uk <- stl(arrivals[, "UK"], s.window = "periodic")
autoplot(fit_uk)

fit_japan <- stl(arrivals[,"Japan"], s.window="periodic")
autoplot(fit_japan)+
  geom_smooth()

fit_us <- stl(arrivals[,"US"], s.window="periodic")
autoplot(fit_us)+
  geom_smooth()

fit_nz <- stl(arrivals[,"NZ"], s.window="periodic")
autoplot(fit_nz)+
  geom_smooth()

#_____PRZYKLADY 2_________

#a)
autoplot(AirPassengers)+
  geom_smooth()

#wraz ze wzrostem trendu rosnie zmiennosc sezonowości

acf(AirPassengers)
ggAcf(AirPassengers)
pacf(AirPassengers)
#install.packages("forecast")
library(forecast)
ggAcf(AirPassengers, type="correlation")
ggPacf(AirPassengers)

#b)
white_noise <- arima.sim(model = list(order = c(0, 0, 0)), n = 100)
rw_drift <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = 0.1)
ar <- arima.sim(model = list(ar = 0.5), n = 100)
ma<-arima.sim(model = list(ma =-0.9), n = 100, mean=2)
autoplot(cbind(white_noise, rw_drift, ar, ma))

rw_drift_diff<-diff(rw_drift)
autoplot(cbind(rw_drift, rw_drift_diff))

#c)
ggseasonplot(AirPassengers, year.labels=TRUE)
ggseasonplot(AirPassengers, year.labels=FALSE, col=rainbow(12), polar=TRUE)
ggsubseriesplot(AirPassengers)



#_________ZADANIE 2___________
#a

autoplot(qcement)
ggAcf(qcement)
ggAcf(ozonet_airquality)

acf_10 <- acf(qcement, plot=FALSE)$acf[11] #10 opóźnienie
acf_10

pacf_20 <- pacf(qcement, plot=FALSE)$acf[21]
pacf_20

ggAcf(qcement, lag.max = 30)
ggPacf(qcement, lag.max = 30)

ggseasonplot(qcement)#, year.labels=TRUE)
ggseasonplot(qcement, polar=TRUE)
ggsubseriesplot(qcement)

#b
ma <- arima.sim(model=list(ma = -0.9), n=200, mean=5)
ar <- arima.sim(model=list(ar = 0.9), n=200, mean=-1)

#rw-bladzenie przypadkowe
rw <- arima.sim(model=list(order=c(0,1,0)), n=200)
autoplot(cbind(ma, ar, rw))

autoplot(ma)
ggAcf(ma)
ggPacf(ma)

ggAcf(ar)
ggPacf(ar)

ggAcf(rw)
ggPacf(rw)


#c
visnights
subset_vis <- visnights[, 1:5]
subset_vis

summary(subset_vis)
sapply(subset_vis, function(x) sum(is.na(x)))

autoplot(subset_vis, facets = TRUE)

acf_plots <- list()
pacf_plots <- list()
season_plots <- list()

for (i in 1:5) {
  series <- subset_vis[, i]
  name <- colnames(subset_vis)[i]
  
  acf_plots[[i]] <- ggAcf(series) + ggtitle(paste("ACF:", name))
  pacf_plots[[i]] <- ggPacf(series) + ggtitle(paste("PACF:", name))
  season_plots[[i]] <- ggseasonplot(series) + ggtitle(paste("Seasonality:", name))
}

acf_plots[2]
