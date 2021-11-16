
###___Quantifying bleaching intensity___###

# Setup

rm(list=ls()) # Clear workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set directory at current (make sure file is saved)

library(tidyverse)
library(ggplot2)

# Load and organize data

data.raw <- read.csv("Bleaching_analyses_depth_gradient.csv", header = T, colClasses = c(Depth = "factor",
            Fragment = "factor", Species = "factor", Date_no = "factor"))
data.raw$Date <- as.Date(data.raw$Date, format = "%d/%m/%Y")
data.raw$Depth <- factor(data.raw$Depth, ordered = TRUE, levels = c(1,2,3,4,5,6,7,8,9,10))
levels(data.raw$Species) <- c("Acropora verweyi", "Pocillopora verrucosa", "Porites cylindrica")

data.1 <- data.raw[c(2:6, 15:21)] # Select relevant columns
data.b <- data.1[!(data.1$Cause == "Dead" | data.1$Cause == "dead" | data.1$Cause == "missing"),]
data.b <- data.b[-c(12, 13)]

# TODO: sort on actual groups, not artificial Structure_no

Average.b <- data.b  %>%  
  group_by(Species, Depth, Date) %>% 
  summarise(Brightness_G = mean(G.c, na.rm = TRUE))

# Survival data frame
data.s <- data.1
#data.s$Survival[is.na(data.s$Survival)] <- 0 # Set survival of missing values to 0    

Average.s <- data.s  %>%  
  group_by(Species, Depth, Date) %>% 
  summarise(Survival = mean(Survival, na.rm = TRUE))

# Plots

# Set colour scale for depth:
cg <- c("#01ffce","#00e6d4","#00c4cc","#0096b1","#006e97","#004d7d","#003163","#001c48","#000c2e","#000314")

#TODO:
# Plot Depth x Average Brightness

# Brightness
ggplot(Average.b, aes(x = Date, y = Brightness_G, color = Depth))+
  facet_wrap(Species ~ .)+
  scale_color_manual(values = cg)+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=1.1) + geom_line(size = 1) + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
  scale_y_continuous(name = "Brightness (bpp)", limits = c(50, 230)) + theme(text=element_text(size = 14))+
  theme(
    panel.spacing = unit(1, "lines"), strip.background = element_blank(),  strip.placement = "outside",
    strip.text.x = element_text(size = 12, face = "bold.italic"),
    axis.title.x = element_text(color="black", vjust=-0, size = 12),
    axis.text.x = element_text(color = "black", angle = 0, size=12, face = "bold", vjust=0.5),
    axis.title.y = element_text(color="black" , vjust=2, size = 12),
    axis.text.y=element_text(size=12, color = "black", face = "bold", vjust=0.5),
    panel.background = element_rect(fill = "#EFEFEF"),
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  ) 
ggsave("Brightness.png", width = 23, height = 8, units = "cm", dpi = 600)



# Survival
ggplot(Average.s, aes(x = Date, y = Survival, color = Depth))+
  facet_wrap(Species ~ .)+
  scale_color_manual(values = cg)+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=1.1) + geom_line(size = 1) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  scale_y_continuous(name = "Live coral tissue (%)", limits = c(0, 100)) + theme(text=element_text(size = 14))+
  theme(
    panel.spacing = unit(1, "lines"), strip.background = element_blank(),  strip.placement = "outside",
    strip.text.x = element_text(size = 12, face = "bold.italic"),
    axis.title.x = element_text(color="black", vjust=-0, size = 12),
    axis.text.x = element_text(color = "black", angle = 0, size=12, face = "bold", vjust=0.5),
    axis.title.y = element_text(color="black" , vjust=2, size = 12),
    axis.text.y=element_text(size=12, color = "black", face = "bold", vjust=0.5),
    panel.background = element_rect(fill = "#EFEFEF"),
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  ) 
ggsave("Survival.png", width = 23, height = 8, units = "cm", dpi = 600)
