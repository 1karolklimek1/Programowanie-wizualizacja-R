library(ggplot2)

#przykłady

ir_pl<-ggplot(iris, aes(Sepal.Width, fill = Species)) +
  geom_bar(position = "fill")+
  scale_fill_brewer()
ir_pl+
  theme(legend.position="bottom")
ir_pl+
  theme(legend.position=c(0.8, 0.7))
ir_pl

#b
theme(
  rect=element_rect(fill="red"),
  axis.text=element_text(color="blue"),
  legend.key = element_rect(color = NA)
)
ir_pl + ggtitle("Iris plot")+
  theme(
    rect = element_rect(fill = "red"),
    legend.key = element_rect(color = "NA"),
    axis.text=element_text(color="dark blue"),
    plot.title=element_text(size=16, face="italic", color="dark blue"),
    legend.margin=margin(10, 30, 20, 20, "pt")
  )

#c
ir_pl+
  theme_dark()



#______ZADANIE 1___________

str(mtcars)

mtcars_plot <- ggplot(mtcars, aes(x=wt, y=mpg, color=as.factor(cyl))) +
  geom_point() +
  labs(title="Scatterplot", y="Miles per gallon", x="WT") 
mtcars_plot
mtcars_plot + theme(
  plot.title = element_text(color = "red", size = 14, face="bold.italic"),
  axis.title.x = element_text(color = "blue", size=14, face="bold"),
  axis.title.y = element_text(color= "#993333", size = 14, face="bold"),
  rect=element_rect(fill="lightblue"),
  panel.grid = element_line(color=NULL, linetype=NULL ),
  panel.background = element_rect("lightyellow" ),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(color="black")
  )

mtcars_plot + theme_dark()
mtcars_plot + theme_light()
mtcars_plot + theme_grey()
mtcars_plot + theme_classic()
mtcars_plot + theme_void()


#________ZADANIE 2__________

#a
iris_plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species))+
  geom_point(size=4)+
  geom_segment(aes(xend=2, yend=Sepal.Width), linewidth=2)+
  theme_classic()

iris_plot

#b
global_mean<-mean(iris$Sepal.Length)
iris_plot+
  geom_vline(xintercept=global_mean, color="black", linetype=4)

#c
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species))+
  geom_point(size=4)+
  stat_smooth(method=loess, se=TRUE)

#d
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species))+
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  coord_fixed(ratio=1)


#------a-------------

a <- ggplot(mtcars, aes(x=qsec, y=mpg, color= am)) +
  geom_point() +
  geom_segment(xend=19)
a

#-------b-----------

mean_mpg <- mean(mtcars$mpg)
median_mpg <- median(mtcars$mpg)
b <- ggplot(mtcars, aes(x=qsec, y=mpg)) + 
  geom_point() +
  geom_hline(yintercept = mean_mpg, color="lightblue") + 
  geom_hline(yintercept = median_mpg, color="orange")
b  


#------c--------

c <- ggplot(mtcars, aes(x=wt, y=mpg, color=factor(cyl))) + 
  geom_point() + 
  stat_smooth(se=FALSE) +
  stat_smooth(method='lm', se=FALSE)
c



#--------d----------

d <- ggplot(mtcars, aes(x=mpg, y=drat, color=as.factor(am))) +
  geom_point() + 
  geom_jitter(height=0.1) +
  coord_fixed(ratio = 3)

d


#_____________ZADANIE 3_____________

#---------a------------

library(ggradar)
library(palmerpenguins)

?mutate_at
?rescale

penguins_clean <- na.omit(penguins)
str(penguins_clean)
a <- ggradar(penguins_clean, aes(x=species, y= bill_length_mm))
a

