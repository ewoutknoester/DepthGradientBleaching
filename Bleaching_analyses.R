
###___Quantifying bleaching intensity___###

bleach_data <- read.csv("Bleaching_analyses_depth_gradient.csv", header=T, na.strings=c(""))

head(bleach_data)

install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

###CREATING A DATAFRAME###

#Adding a grouping column 
Structure_no <- rep(c(1:360),each=8)
bleach_data <- cbind(bleach_data, Structure_no)

#Averaging bleaching intensity Green channel (G) across structures
Average <- bleach_data  %>%  
  group_by(Structure_no) %>% 
  summarise(Average_G = mean(G.2, na.rm = TRUE))
#Averaging survival Green channel (G) across structures
Average_s <- bleach_data  %>%  
  group_by(Structure_no) %>% 
  summarise(Average_survival = mean(Survival...., na.rm = TRUE))

#Creating a new data frame
Species_bl <- rep(c("Pocillopora verrucosa", "Porites cylindrica", "Acropora verweyi"), each=120)
Date <- rep(c(1:12), times=30)
Depth_bl <- rep(1:10, each=12, times=3)
Depth_bl <- as.factor(Depth_bl)
Bleach_short <- data.frame(Depth_bl, Species_bl, Date, Average$Average_G, Average_s$Average_survival)

bleach_short <- Bleach_short %>% 
  rename(
    Average_G = Average.Average_G,
    Average_survival = Average_s.Average_survival)

#Creating separate data frames for species 
Pocillopora <- bleach_short %>% 
  filter(Species_bl == "Pocillopora verrucosa")
Porites <- bleach_short %>% 
  filter(Species_bl == "Porites cylindrica")
Verweyi <- bleach_short %>% 
  filter(Species_bl == "Acropora verweyi")

###PLOTTING THE DATA###

#___Change in the green chanel color intensity over time 

#Pocillopora verrucosa
ggplot(Pocillopora, aes(x=Date,y=Average_G,color=Depth_bl)) + ggtitle("Change in colour intensity of P. verrucosa over time") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=2.5) + geom_line(size = .7) + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
  scale_x_continuous(name = "Time", breaks = c(1:12), labels = c("18.3.2020.", "25.3.2020.", "1.4.2020.", "8.4.2020.", "15.4.2020.","22.4.2020.", "29.4.2020.", "6.5.2020.", "13.5.2020.", "3.6.2020.", "17.6.2020.", "15.7.2020.")) + 
  scale_y_continuous(name = "Color intensity", limits = c(1, 250)) + theme(text=element_text(size = 14)) 

#Porites cylindrica 
ggplot(Porites, aes(x=Date,y=Average_G,color=Depth_bl)) + ggtitle("Change in colour intensity of P. cylindrica over time") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=2.5) + geom_line(size = .7) + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
  scale_x_continuous(name = "Time", breaks = c(1:12), labels = c("18.3.2020.", "25.3.2020.", "1.4.2020.", "8.4.2020.", "15.4.2020.","22.4.2020.", "29.4.2020.", "6.5.2020.", "13.5.2020.", "3.6.2020.", "17.6.2020.", "15.7.2020.")) + 
  scale_y_continuous(name = "Color intensity", limits = c(1, 250)) + theme(text=element_text(size = 14))

#Acropora verweyi
ggplot(Verweyi, aes(x=Date,y=Average_G,color=Depth_bl)) + ggtitle("Change in colour intensity of A. verweyi over time") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=2.5) + geom_line(size = .7) + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
  scale_x_continuous(name = "Time", breaks = c(1:12), labels = c("18.3.2020.", "25.3.2020.", "1.4.2020.", "8.4.2020.", "15.4.2020.","22.4.2020.", "29.4.2020.", "6.5.2020.", "13.5.2020.", "3.6.2020.", "17.6.2020.", "15.7.2020.")) + 
  scale_y_continuous(name = "Color intensity", limits = c(1, 250)) + theme(text=element_text(size = 14))


#TEST (all species on one graph) -> no good, too crowded
#ggplot(bleach_short, aes(x=Date,y=Average_G,color=Depth)) + ggtitle("Change in colour intensity of P. verrucosa over time") + theme(plot.title = element_text(hjust = 0.5)) + facet_grid(Species ~ .) +
#  geom_point(size=2.5) + geom_line(size = .7) + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
#  scale_x_continuous(name = "Time", breaks = c(1:12), labels = c("18.3.2020.", "25.3.2020.", "1.4.2020.", "8.4.2020.", "15.4.2020.","22.4.2020.", "29.4.2020.", "6.5.2020.", "13.5.2020.", "3.6.2020.", "17.6.2020.", "15.7.2020.")) + 
#  scale_y_continuous(name = "Color intensity", limits = c(1, 250)) + theme(text=element_text(size = 14)) 

#___Survival over time during the bleaching period

#Pocillopora verrucosa
ggplot(Pocillopora, aes(x=Date,y=Average_survival,color=Depth_bl)) + ggtitle("Survival of P. verrucosa over time") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=2.5) + geom_line(.7) + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
  scale_x_continuous(name = "Time", breaks = c(1:12), labels = c("18.3.2020.", "25.3.2020.", "1.4.2020.", "8.4.2020.", "15.4.2020.","22.4.2020.", "29.4.2020.", "6.5.2020.", "13.5.2020.", "3.6.2020.", "17.6.2020.", "15.7.2020.")) + 
  scale_y_continuous(name = "Survival (%)", limits = c(0, 100)) + theme(text=element_text(size = 14)) 

#Porites cylindrica 
ggplot(Porites, aes(x=Date,y=Average_survival,color=Depth_bl)) + ggtitle("Survival of P. cylindrica over time") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=2.5) + geom_line(.7) + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
  scale_x_continuous(name = "Time", breaks = c(1:12), labels = c("18.3.2020.", "25.3.2020.", "1.4.2020.", "8.4.2020.", "15.4.2020.","22.4.2020.", "29.4.2020.", "6.5.2020.", "13.5.2020.", "3.6.2020.", "17.6.2020.", "15.7.2020.")) + 
  scale_y_continuous(name = "Survival (%)", limits = c(0, 100)) + theme(text=element_text(size = 14))

#Acropora verweyi
ggplot(Verweyi, aes(x=Date,y=Average_survival,color=Depth_bl)) + ggtitle("Survival of A. verweyi over time") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=2.5) + geom_line(.7) + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
  scale_x_continuous(name = "Time", breaks = c(1:12), labels = c("18.3.2020.", "25.3.2020.", "1.4.2020.", "8.4.2020.", "15.4.2020.","22.4.2020.", "29.4.2020.", "6.5.2020.", "13.5.2020.", "3.6.2020.", "17.6.2020.", "15.7.2020.")) + 
  scale_y_continuous(name = "Survival (%)", limits = c(0, 100)) + theme(text=element_text(size = 14))


