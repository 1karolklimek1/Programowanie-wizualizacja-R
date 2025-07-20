install.packages('palmerpenguins')
library(palmerpenguins)
library(MASS)

#-----------------ZADANIE 1----------------------

#------a-------
par(mfrow=c(1,1))
plot(Boston$medv, Boston$crim, main="Przestępstwa na 1000 mieszkańców, a mediana wartości domów",
     xlab="Mediana wartości domów w 1000$", ylab="Wskaźnik przestępstw na mieszkańca")

trend <- lm( medv ~ crim, Boston)
mean_crim <- mean(Boston$crim)
mean_medv <- mean(Boston$medv)

#help(abline)
#help(lines)
trend$coefficients

abline(trend, col="green", lty=3)
abline(h = mean_crim, col="chocolate")
abline(v = mean_medv, col="magenta")


#help(mtext)
#help(coef)
#help(paste0)

wsp <- coef(trend)
rownanie <- paste0("y = ", wsp[2], "x + ", wsp[1])
rownanie2 <- paste0( "y = ", round(wsp[2], 2), "x + ", round(wsp[1],2) )
rownanie2
mtext(rownanie2, side=3, col="green")

#------b--------

#help(seq)
x <- seq(0, 4*pi, length.out = 40)
length(x)
sinus <- sin(x)
cosinus <- cos(x)

par(mfrow=c(1,2))
plot(x, sinus, main="Wykres sin", type="b", col="mediumpurple")
#lines(x, sinus, lty=1, col="blue")

plot(x, cosinus, type="l", main="Wykres cos", col="orange")

#---------c----------

help(dotchart)
par(mfrow=c(1,1))
dotchart(sinus, col="#009ACD", pch=16, main="Dotchart sinus")




#---------------ZADANIE 3------------------

#PRZYKLADY
liczby <- c(1, 2, 3, 4, 5)
wynik <- ifelse(liczby > 3, "większa niż 3", "mniejsza lub równa 3")
print(wynik)


wynik <- character(length(liczby))
wynik
for (i in seq_along(liczby)) {
  if (liczby[i] > 3) {
    wynik[i] <- "większa niż 3"
  } else {
    wynik[i] <- "mniejsza lub równa 3"
  }
}
wynik


par(mfrow=c(1,1))
plot(Boston$crim, Boston$nox, main = "Wykres punktowy: crim vs nox",
     xlab = "Crim", ylab = "Nox", col="green", pch=17)
legend("topright", c("Dane Boston"), pch = 17, col = "green")

#----------a-----------

#Dane whiteside. Wykonaj wykres zużycia gazu od temperatury. Kolor i kształt punktów
#powinien się różnić w zależności od etykiety zmiennej Insul. Dodaj legendę na wykresie oraz
#odpowiednie opisy osi i tytuł. Porównaj z wizualizacją z poprzednich zajęć. 

par(mfrow=c(1,1))

color <- ifelse(whiteside$Insul == "Before", "seagreen1" , "steelblue1" )
points <- ifelse(whiteside$Insul == "Before", 1, 2)

color
points

plot( whiteside$Temp, whiteside$Gas, main="Zużycie gazu od temperatury", pch=points, col=color,
      xlab="Temperatura", ylab="Gaz")

legend("topright", c("Insul = before", "Insul = after"), pch=c(1,2), 
    col=c("seagreen1" , "steelblue1") )

#-------b-----------


hist(whiteside$Gas[whiteside$Insul == "Before"], 
     col = "red", xlab = "Gaz", ylab = "Frequency", main = "Histogram gazów: Przed")

hist(whiteside$Gas[whiteside$Insul == "After"], 
      col = "blue", add=TRUE)

legend("topright", legend = c("Przed", "Po"), fill = c("red", "blue"))

jpeg("histogram.jpg", width = 800, height = 600)
hist(whiteside$Gas[whiteside$Insul == "Before"], 
     col = "red", xlab = "Gaz", ylab = "Frequency", main = "Histogram gazów: Przed")

hist(whiteside$Gas[whiteside$Insul == "After"], 
     col = "blue", add=TRUE)

legend("topright", legend = c("Przed", "Po"), fill = c("red", "blue"))
dev.off()


#---------c-----------

set.seed(66)

boston_data = Boston[sample(nrow(Boston), 40, replace=FALSE), ]
selected = boston_data[, c("crim", "zn", "indus", "nox")]

pdf("stars.pdf")
stars(selected, main="Wykres stars")
dev.off()

boston_data
selected


#--------d---------

tabela <- table(Boston$chas, Boston$rad)

mosaicplot(tabela, main="Mosaic plot chas vs rad", xlab="chas", ylab="rad", color=c("green", "orange"))

tabela


