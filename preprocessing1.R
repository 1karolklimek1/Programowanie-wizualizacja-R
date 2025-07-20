data=read.csv("C:/Users/karol/Desktop/AGH/semestr 2/R/titanic_new.csv", sep=",")
head(data, 10)
library(dplyr)
data=select(data, -Passenger.Id)
data=select(data, -Name)
data=select(data, -Ticket)
data=select(data, -Fare)
data=select(data, -Cabin)
data=select(data, -Embarked)

data=filter(data, Survived==0 | Survived==1 )
data=filter(data, Sex=='female' | Sex=='male')

typeof(data$Survived)
typeof(data$Pclass)
typeof(data$Age)
typeof(data$Sex)
typeof(data$SibSp)
typeof(data$Parch)

ave_age = mean(data$Age, na.rm = TRUE)
data$Age[is.na(data$Age)] = ave_age
ave_age

View(table(data$Survived))
View(table(data$Pclass))
View(table(data$Sex))
View(table(data$SibSp))
View(table(data$Parch))

mean(data$Age)
mean(data$SibSp)
mean(data$Parch)

library(e1071)
kurtosis(data$Age)
kurtosis(data$SibSp)

sd(data$Age)
min(data$Age)
max(data$Age)

skewness(data$Age)
skewness(data$SibSp)
skewness(data$Parch)

cor(data$Survived, data$Age)
cor(data$Survived, data$Pclass)
cor(data$Survived, data$SibSp)
cor(data$Survived, data$Parch)

cor.test(data$Survived, data$Age)
cor.test(data$Survived, data$Pclass)
cor.test(data$Survived, data$SibSp)
cor.test(data$Survived, data$Parch)

cor(data$Age,data$SibSp)
cor(data$Age, data$Parch)
cor.test(data$Age,data$SibSp)
cor.test(data$Age, data$Parch)
