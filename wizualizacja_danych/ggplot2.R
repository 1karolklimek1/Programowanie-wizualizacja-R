install.packages(c("tidyverse", "ggplot2", "ggthemes"))
library(tidyverse)
library(ggplot2)
library(ggthemes)

#przyklady

#a
ggplot(data=mtcars, aes(x=cyl, y=mpg)) +
  geom_point(shape=1, size=4)

#b
ggplot(data=mtcars, aes(x=wt, y=mpg, color = disp, size=disp)) +
  geom_point(alpha=0.8)+
  geom_smooth()

#c
pl<-ggplot(mtcars, aes(wt, mpg))
pl+geom_point(aes(alpha=cyl))
pl+geom_point(aes(shape=as.factor(cyl)))+
  scale_x_log10()
pl+geom_text(aes(label=cyl))
pl+geom_point(aes(size= hp/wt))
dgreen<-"#013220"
pl+geom_point(color=dgreen, alpha=0.6, size=10, shape=1)
pl+ geom_text(label=rownames(mtcars))


#d
m_colors <- c("#378AB1", "#E2111C")
ggplot(mtcars, aes(as.factor(cyl), fill = as.factor(am))) +
  geom_bar(position="dodge") +
  labs(x = "Number of Cylinders", y = "Count")
scale_fill_manual("Type", values = m_colors)



#zadanie1

#Zobacz jakie zmienne znajdują się w df diamonds. Następnie wykonaj wykres rozrzutu: 
#na osi X umieść carat, na osi Y price. Dodatkowo dopasuj do danych krzywą
#obrazującą przebieg danych.

#-----a------
str(diamonds)

diamenty <- diamonds[1:20000,]

ggplot(data=diamenty, aes(x=carat, y=price)) +
  geom_point() +
  geom_smooth()

#-----b-----

#Zmodyfikuj wykres z podpunktu a: dodaj atrybut clarity jako kolor do wykresu oraz
#zmień punkty na półprzeźroczyste o wielkości 3 pikseli.

colors <- diamenty$clarity


ggplot(data=diamenty, aes(x=carat, y=price, color = clarity)) +
  geom_point(alpha=0.5, size=3) +
  geom_smooth() + 
  labs(title = "Cena diamentów w zależności od masy",
       x = "Carat",
       y = "Price")
       


?ggplot
#RColorBrewer::display.brewer.all()


#--------C------------

c <- ggplot(data=diamonds, aes(x=carat, y=price, color=clarity, size=cut)) + 
  geom_point() + 
  labs(x="Carat", y="Price", title="Wykres") +
  scale_color_manual(values=rainbow(8))
c


#-----d-----

d <- ggplot(data=diamonds, aes(x=carat, y=price)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() 
d


#--------e----------

e <- ggplot(data=diamonds, aes(x=cut, fill=clarity)) +
  geom_bar()
e

e2 <- ggplot(diamonds, aes(x=cut, fill=clarity)) +
  geom_bar(position="dodge")
e2



#_______PRZYKLADY 2________________
#a)
ggplot(mtcars, aes(mpg, 0)) +
  geom_jitter(color="green") +
  ylim(-2,2)+
  xlab("Miles per gallon")
#b)
ggplot( mtcars , aes(x=mpg, y=wt, color=as.factor(cyl) )) +
  geom_point(size=3) +
  facet_wrap(~cyl)+
  ggtitle("Mtcars plot")
#c)
ggplot( mtcars , aes(x=mpg, y=wt )) +
  geom_point() +
  facet_grid( cyl ~ gear)
#d)
ggplot(mtcars, aes(x=mpg, fill=factor(am))) +
  geom_histogram(binwidth=3) +
  facet_wrap(~ cyl)+
  scale_fill_brewer(type = 'qual')
#e)
p = ggplot(mtcars, aes(x=mpg, fill=factor(am))) + geom_histogram(binwidth=5)
ggsave(filename="auta_przyklad.png", p)



#_________ZADANIE 2______________

#----------a--------

a <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
  geom_point() +
  facet_wrap(~clarity)
a

#------b----------

b <- ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point() +
  labs(title="Diamonds scatterplot", x="Weight carats")
b


#--------c--------

c <- ggplot(diamonds, aes(x=price)) +
  geom_histogram(binwidth = 200) +
  facet_wrap(~clarity)
c  


#------D------

d <- ggplot(diamonds, aes(x=price, color=cut)) +
  geom_density()
d


#--------e_-----------

e <- ggplot(diamonds, aes(x=color, y=price)) + 
  scale_y_log10() +
  geom_boxplot(fill="lightblue", outlier.color = "red")
e


#-----f-----

m_plot <- ggplot(diamonds, aes(x=carat, y=price)) + 
  geom_point()

ggsave(filename="mp_plot.png", m_plot)
ggsave(filename="mj_plot.jpeg", m_plot)


#-----e---------

g <- ggplot(diamonds, aes(x=cut, y=clarity)) + 
  geom_jitter(alpha=0.03, color="lightblue")
g




#__________ZADANIE 3___________

#------a---------

a <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_jitter(alpha=0.5, width=0.2, shape=12 )
a


#------b--------

b <- ggplot(iris, aes(x=Sepal.Length, ..density.. )) +
  geom_histogram(binwidth=1, fill="darkblue", color="orange") 
b


#-------c-------

c <- ggplot(iris, aes(x=Sepal.Width, fill=Species)) +
  geom_histogram(binwidth=0.5, position="identity", alpha=0.2)
c
















