library(tidyverse)
library(ggplot2)
library(gapminder)
library(plotly)

#----przyklady---------

plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)

plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)%>%
  add_markers()

iris%>%
  filter(Species =="setosa")%>%
  plot_ly(x = ~Sepal.Width)%>%
  add_histogram(nbinsx=6, color = I("darkgreen"), opacity = 0.5)

iris%>%
  plot_ly(x=~Sepal.Width, y=~Species)%>%
  add_boxplot()

plot_ly(x = ~Sepal.Length, y = ~Petal.Length, data = iris,
        marker = list(size = 12,
                      color = 'rgba(255, 182, 193, .9)',
                      line = list(color = "rgba(100, 20, 20, .5)",
                                  width = 5)))%>%
  add_markers()%>%
  layout(title = "Scatterplot")

iris %>%
  plot_ly(x = ~Sepal.Length, y = ~Petal.Length, color=~Species) %>%
  add_markers(colors="Dark2")


#_____ZADAANIE 1___________

#a

mtcars %>% 
  plot_ly(x = ~mpg) %>% 
  add_histogram()

#b
mtcars %>% 
  filter( cyl == 4) %>%
  plot_ly(x = ~ disp, y = ~ mpg, color= ~factor(am), colors=c("red", "blue") ) %>%
  add_markers()
  
#c
mtcars %>% 
  plot_ly(x = ~ disp, y = ~ mpg) %>%
  add_histogram2d(nbinsx=3, nbinsy = 3)

#d
mtcars %>% 
  plot_ly(x = ~ cyl, y = ~ mpg) %>%
  add_boxplot() %>%
  layout(title = "Boxplot")

#e

mtcars %>% 
  plot_ly(x = ~ disp, y = ~ mpg, color= ~factor(cyl) ) %>%
  add_markers(colors="Set1") %>%
  layout(title = "New colors")

#f
pal <- c("magenta", "khaki", "cyan")

mtcars %>% 
  plot_ly(x = ~ disp, y = ~ mpg, color= ~factor(cyl), colors=pal ) %>%
  add_markers() %>%
  layout(title = "My colors")



#-------przyklady 2-----------

g_plot<-ggplot(data=iris, aes(x=Sepal.Length, y=Petal.Length, color=Species))+
  geom_point(alpha=0.5)
ggplotly(g_plot)

iris %>%
  plot_ly(x = ~Sepal.Length) %>%
  add_histogram(xbins = list(start=4, end=8, size=1))

iris %>%
  plot_ly(x = ~Sepal.Length, y=~Petal.Length, color=~Species) %>%
  add_markers(marker = list(symbol = "diamond", size = 6))


iris %>%
  filter(Sepal.Length>5)%>%
  count(Species) %>%
  plot_ly(x = ~Species, y = ~n, hoverinfo = "y") %>%
  add_bars()


iris %>%
  plot_ly(x = ~Sepal.Length, y=~Petal.Length, color=~Species) %>%
  add_markers(marker = list(opacity = 0.5)) %>%
  layout(xaxis = list(title="Sepal Length", showgrid=FALSE),
         yaxis = list(title="Petal Length"), paper_bgcolor="#ababab")

model<-lm(Petal.Length~Sepal.Length, data=iris)
iris%>%
  plot_ly(x = ~Sepal.Length, y=~Petal.Length, color=~Species) %>%
  add_markers(showlegend = FALSE) %>%
  add_lines(y = ~fitted(model))


plot1<- iris %>%
  plot_ly(x = ~Sepal.Length, y=~Petal.Length, color=~Species) %>%
  add_markers(marker = list(symbol = "diamond", size=4))
plot2<-iris %>%
  plot_ly(x = ~Sepal.Length, y=~Petal.Length, color=~Species) %>%
  add_markers(marker = list(symbol = "x", size = 4))
subplot(plot1, plot2, nrows=2)

#____ZADANIE 2_____

#a
plot_a <- ggplot(data=diamonds, aes(x=cut, fill=clarity) ) + 
  geom_bar()

ggplotly(plot_a)


#b
minimum = min(diamonds$carat)
maximum = max(diamonds$carat)
bins = ( maximum - minimum ) / 7

diamonds %>%
  filter(color %in% c("G", "H", "I", "J")) %>%
  plot_ly(x = ~ carat, color= ~as.factor(color), colors="Set1") %>%
  add_histogram(xbins = list(
    start = minimum,
    end = maximum,
    size = bins ))


#c
trend <- lm(carat~price, data= diamonds)

diamonds %>% 
  plot_ly(x = ~ price, y = ~ carat) %>%
  add_markers(marker = list(opacity = 0.5)) %>%
  add_lines(y = ~ fitted(trend) ) %>%
  layout(title = "WYKRES",
         xaxis = list(title="CENA DIAMENTU"),
         yaxis = list(title="LICZBA KARATUF"))



#zadanie4

library(ggplot2)  # potrzebne, bo diamonds są w ggplot2
library(plotly)   # do interaktywnych wykresów

data(diamonds)    # wczytujemy dane

# Liczebność
cut_counts <- as.data.frame(table(diamonds$cut))
colnames(cut_counts) <- c("cut", "count")

# Wykres w Plotly
fig_a <- plot_ly(cut_counts, 
                 x = ~cut, 
                 y = ~count, 
                 type = 'bar', 
                 marker = list(color = 'steelblue')) %>%
  layout(title = "Liczebność diamentów w zależności od cut",
         xaxis = list(title = "Jakość szlifu (cut)"),
         yaxis = list(title = "Liczba diamentów"))
fig_a


#b
fig_b <- plot_ly(diamonds, 
                 x = ~cut, 
                 y = ~carat, 
                 type = 'box', 
                 boxpoints = "outliers", 
                 marker = list(color = 'green')) %>%
  layout(title = "Masa (carat) w zależności od jakości szlifu (cut)",
         xaxis = list(title = "Jakość szlifu (cut)"),
         yaxis = list(title = "Masa (carat)"))
fig_b

diamonds$carat


#c
fig_c <- plot_ly(diamonds, 
                 x = ~clarity, 
                 y = ~price, 
                 type = 'box', 
                 boxpoints = "outliers", 
                 marker = list(color = 'tomato')) %>%
  layout(title = "Cena diamentów w zależności od clarity",
         xaxis = list(title = "clarity"),
         yaxis = list(title = "price"))
fig_c















  