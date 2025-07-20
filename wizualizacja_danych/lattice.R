library(lattice)
library(latticeExtra)
library(ggplot2)
library(tidyverse)

cancer <- USCancerRates

str(cancer)

str(airquality)

histogram(~ Ozone, data = airquality,
          nint=10,
          type="count")

xyplot(Ozone ~ Solar.R, data = airquality,
       main = "Air Quality",
       xlab = "Temperature (Fahrenheit)",
       ylab = "Ozone (ppb)"
)


histogram(~Ozone|factor(Month),
          data = airquality,
          layout=c(5,1),
          xlab="Ozone (ppb)")


xyplot(Ozone ~ Temp, airquality, groups = Month,
       # Complete the legend spec
       auto.key = list(space = "right",
                       title = "Month",
                       text = month.name[5:9]))

#___________ZADANIE 1__________

#---a-----

xyplot(Sepal.Length ~ Petal.Length, iris,
       main="Iris scatterplot", xlab="Petal length", ylab="Sepal length")


#--b--
str(iris)
xyplot(Sepal.Length ~ Petal.Length, iris, groups = Species,
       main="Iris scatterplot", xlab="Petal length", ylab="Sepal length",
       auto.key = list(space="right", 
                       title="Species"))

#--c--

histogram(~Petal.Width|factor(Species), iris,
          nint=15, layout=c(3,1), col="darkgreen")


#--d--

cloud(Petal.Length ~ Sepal.Length * Petal.Width, iris, groups = Species,
      fontsize=0.8, screen=c(x=-60, y=0, z=10))

#--e--

bwplot(Petal.Width~Species, iris, groups = Species, 
       xlab="Species", ylab="Petal width",
       panel=panel.violin)

#--f--

stripplot(Petal.Width~Species, iris,
          jitter.data=TRUE)

#________________PRZYKLADY 2___________________


xyplot(rate.female ~ rate.male,
       data = USCancerRates,
       grid = TRUE, abline = c(0, 1),
       as.table=TRUE)


bwplot(~ rate.male + rate.female,
       data = USCancerRates,
       outer = TRUE,
       xlab="Rate (per 100,000)",
       strip = strip.custom(factor.levels = c("Male", "Female")))

dens_plot <-
  densityplot(~ rate.male + rate.female,
              data = USCancerRates, outer = TRUE,
              plot.points = FALSE, as.table = TRUE)
print(dens_plot)


update(dens_plot, xlab = "Rate")
dim(dens_plot)
dens_plot[2]

dotplot(mpg ~ hp | cyl + am, data = mtcars,
        as.table = TRUE,
        scales = list(x = list(log=TRUE)),
        par.settings = ggplot2like(),
        lattice.options = ggplot2like.opts())

dotplot(mpg ~ hp | cyl, data = mtcars,
        groups =am, auto.key = list(columns = 2),
        par.settings = simpleTheme(pch = c(3, 1)),
        scales = list(x = list(log = 2, equispaced.log = FALSE)))


plot2<-xyplot(mpg~hp|equal.count(mtcars$wt, 5), data=mtcars)
print(plot2)
plot3<-update(plot2, type=c("p","r"), pch=5)
plot(plot3)
update(plot2, pch=19, col="green")


plot3<-xyplot(mpg~hp, data=mtcars)
plot(plot3, split=c(1,1,2,1))
plot(update(plot3, col="red"), split=c(3,1,4,2), newpage=FALSE)
plot(update(plot3, col="green"), split=c(4,1,4,2),newpage=FALSE)
plot(update(plot3, col="brown"), split=c(2,2,2,2), newpage=FALSE)


plot(plot3, position=c(0,0,1,.5))
plot(update(plot3, col="red"), position=c(0.7,0.5,1,0.8), newpage=FALSE)
plot(update(plot3, col="green"), position=c(0.0, 0.5, 0.7,1),newpage=FALSE)


#---------a--------
str(diamonds)

a<- xyplot(carat ~ log(depth)|factor(color), diamonds,
       grid=TRUE, abline=c(-1.33, 0.515), layout=c(2,4))
print(a)

#--b--

dim(a)

col_E <- a[2]
col_F <- a[3]

print(col_E)
print(col_F)

plot(update(col_E, col="red"))

























