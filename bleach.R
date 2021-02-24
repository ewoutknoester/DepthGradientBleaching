###___Bleaching Intensity___###

porites <- read.csv("Bleaching intensity POCILOPORA.csv", header=T, na.strings=c(""))
porites

depth <- rep(1:10, each=12)
depth

split <- rep(1:2, each=60)
split

porites <- cbind(porites, split)
porites <- cbind(porites, depth)

class(porites$depth)
porites$depth <- as.factor(porites$depth)
porites$split <- as.factor(porites$split)

plot(porites$date, porites$bleach, main = "Scatterplot", ylab = "bleaching intensity", xlab = "time")
points(porites$date, porites$bleachR, col=2)
points(porites$date, porites$bleachG, col=3)
points(porites$date, porites$bleachB, col=4)

install.packages("ggplot2")
library(ggplot2)

#RGB
ggplot(porites, aes(x=date,y=bleach,color=depth)) +ggtitle("Bleaching intensity over time") + 
  ylab("bleaching") + xlab("time")+ geom_point(size=2) + geom_line() + 
  scale_x_continuous(breaks = c(1:12)) # can add dates with labels=c("",)

ggplot(porites, aes(x=date,y=bleach,color=depth)) +ggtitle("Bleaching intensity over time") + 
  ylab("bleaching") + xlab("time")+ geom_point(size=2) + geom_line() + scale_x_continuous(breaks = c(1:12)) +
  facet_grid(split ~ .)
#R
ggplot(porites, aes(x=date,y=bleachR,color=depth)) +ggtitle("Bleaching intensity over time") + 
  ylab("bleaching") + xlab("time")+ geom_point(size=2) + geom_line() + 
  scale_x_continuous(breaks = c(1:12)) 
#G
ggplot(porites, aes(x=date,y=bleachG,color=depth)) +ggtitle("Bleaching intensity over time") + 
  ylab("bleaching") + xlab("time")+ geom_point(size=2) + geom_line() + 
  scale_x_continuous(breaks = c(1:12)) 
#B
ggplot(porites, aes(x=date,y=bleachB,color=depth)) +ggtitle("Bleaching intensity over time") + 
  ylab("bleaching") + xlab("time")+ geom_point(size=2) + geom_line() + 
  scale_x_continuous(breaks = c(1:12)) 
#survival
ggplot(porites, aes(x=date,y=survival,color=depth)) +ggtitle("Survival over time") + 
  ylab("survival") + xlab("time")+ geom_point(size=2) + geom_line() + 
  scale_x_continuous(breaks = c(1:12)) + ylim(0,100) 

            
