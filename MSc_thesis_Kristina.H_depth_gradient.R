
###___DEPTH GRADIENT GROWTH DATA___###

growth_data <- read.csv(file.choose(), header=TRUE, na.strings=c(""))
#OR growth_data <- read.csv("Coral_growth_final", header=T, na.strings=c("")) 


install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggrepel") # To add the number of fragments next to points
library(ggrepel)

#Survival = NA when a fragment is missing
growth_data$Survival_0[growth_data$Comments_0 == "Missing"] <- NA
growth_data$Survival_1[growth_data$Comments_1 == "Missing"] <- NA
growth_data$Survival_2[growth_data$Comments_2 == "Missing"] <- NA
growth_data$Survival_3[growth_data$Comments_3 == "Missing"] <- NA

#Survival = 0% when a fragment is dead
growth_data$Survival_0[growth_data$Comments_0 == "Dead"] <- 0
growth_data$Survival_1[growth_data$Comments_1 == "Dead"] <- 0
growth_data$Survival_2[growth_data$Comments_2 == "Dead"] <- 0
growth_data$Survival_3[growth_data$Comments_3 == "Dead"] <- 0

#Removing commas from numbers
growth_data$EV_cm3_3 <- as.numeric(gsub(",", "", growth_data$EV_cm3_3, fixed = TRUE))
growth_data$EV_cm3_2 <- as.numeric(gsub(",", "", growth_data$EV_cm3_2, fixed = TRUE))

#adding a grouping column 
Depth <- rep(c(1:40),each=8)
growth_data <- cbind(growth_data, Depth)


#___Calculating SGR (from corals with > 80% survival)___#

#1. non_bleaching period 30 Oct-25 Feb

#Filter out fragments that had a survival >= 80% on t0 and t1
growth_data_non_bleach_subset <- growth_data %>% 
  filter(Survival_1 >= 80 & Survival_0 >= 80)

#Calculate growth rate per fragment
growth_data_non_bleach_subset$growth_rate <- (log(growth_data_non_bleach_subset$EV_cm3_1/growth_data_non_bleach_subset$EV_cm3_0)/118)

#creating a data frame with number of fragments per structure
number_fragments_nb <- as.data.frame(table(growth_data_non_bleach_subset$Depth))
number_fragments_nb

#Averaging across depth 
average_growth_rate_per_depth_non_bleach <- growth_data_non_bleach_subset %>%  
  group_by(Depth) %>% 
  summarise(non_bleaching = mean(growth_rate, na.rm = TRUE))

#Adding number of fragments
average_growth_rate_per_depth_non_bleach <- cbind(average_growth_rate_per_depth_non_bleach, number_fragments_nb)
average_growth_rate_per_depth_non_bleach <- average_growth_rate_per_depth_non_bleach %>% 
  rename(Freq_nb = Freq)
Species <- rep(c("Acropora formosa", "Acropora verweyi", "Pocillopora verrucosa", "Porites cylindrica"), each=10)
average_growth_rate_per_depth_non_bleach <- cbind(average_growth_rate_per_depth_non_bleach, Species)  


#2. bleaching period 25 Feb - 20 May

#Filter out fragments that had a survival >= 80% on t1 and t2
growth_data_bleach_subset <- growth_data %>% 
  filter(Survival_1 >= 80 & Survival_2 >= 80)

#Calculate growth rate per fragment
growth_data_bleach_subset$growth_rate <- (log(growth_data_bleach_subset$EV_cm3_2/growth_data_bleach_subset$EV_cm3_1)/85)

#creating a data frame with number of fragments per structure
number_fragments_b <- as.data.frame(table(growth_data_bleach_subset$Depth))
number_fragments_b

# Averaging across depth
average_growth_rate_per_depth_bleach <- growth_data_bleach_subset %>%  
  group_by(Depth) %>% 
  summarise(bleaching = mean(growth_rate, na.rm = TRUE))

#Adding number of fragments
average_growth_rate_per_depth_bleach <- cbind(average_growth_rate_per_depth_bleach, number_fragments_b)

#insert missing values for depthts with no data (no fragments with > 80% survival)
average_growth_rate_per_depth_bleach <- average_growth_rate_per_depth_bleach %>% 
  add_row(Depth=23, bleaching=NA, .before = 23 )

average_growth_rate_per_depth_bleach <- average_growth_rate_per_depth_bleach %>% 
  add_row(Depth=31, bleaching=NA, .before = 31 )

average_growth_rate_per_depth_bleach <- average_growth_rate_per_depth_bleach %>% 
  add_row(Depth=32, bleaching=NA, .before = 32 )

#average_growth_rate_per_depth_bleach <- cbind(average_growth_rate_per_depth_bleach, Species)  


#__PLOTING SGR__#

#Non-bleach + bleach period
graph_SGR <- merge(average_growth_rate_per_depth_non_bleach, average_growth_rate_per_depth_bleach, by="Depth")
Depth <- rep(1:10, times = 4)
graph_SGR$Depth <- Depth

#Removing A. formosa
graph_SGR_reduced <- graph_SGR %>% 
  filter(!Species == "Acropora formosa")

graph_SGR_a <- graph_SGR_reduced  %>%
  select(non_bleaching, bleaching, Species, Depth) %>% 
  pivot_longer(., cols = c(non_bleaching, bleaching), names_to = "Condition", values_to = "SGR")
  
graph_SGR_b <- graph_SGR_reduced  %>%
  pivot_longer(., cols = c(Freq_nb, Freq), names_to = "Condition_f", values_to = "Frequency")  

graph_SGR_a$Condition <- as.factor(graph_SGR_a$Condition)  
graph_SGR_b$Condition_f <- as.factor(graph_SGR_b$Condition_f)

  
#GRAPH
SGR <- ggplot(graph_SGR_a, aes(x = Depth, y = graph_SGR_a$SGR, color = graph_SGR_a$Condition)) + 
  geom_point(size=2.5) + geom_line(size = 1, alpha = .5) + facet_wrap(~Species, scales = "free_x") + 
  ggtitle("SGR of four coral species") +  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "SGR (1/d)", breaks = c(-0.0025, 0, 0.0025, 0.005, 0.0075, 0.010, 0.0125, 0.015, 0.0175)) +
  scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + theme(text=element_text(size = 13)) + 
  geom_text_repel(aes(label = Frequency, group = Condition_f), data = graph_SGR_b, size = 4, nudge_y = 0.0009, nudge_x = 0.28, point.size = NA, min.segment.length = Inf) +
  scale_fill_discrete(name = "Period", labels = c("Bleaching", "Non-bleaching")) + 
  scale_color_discrete(name = "Period", labels = c("Bleaching", "Non-bleaching")) +
  theme(legend.title = element_text(face = "bold")) + theme(text=element_text(size = 14)) +
  geom_hline(yintercept = 0, color = "red", alpha = .3, size = .8, linetype = "dashed")

SGR

###A. formosa only ###
graph_SGR_formosa <- graph_SGR %>% 
  filter(Species == "Acropora formosa")

graph_SGR_c <- graph_SGR_formosa  %>%
  select(non_bleaching, bleaching, Species, Depth) %>% 
  pivot_longer(., cols = c(non_bleaching, bleaching), names_to = "Condition", values_to = "SGR")

graph_SGR_d <- graph_SGR_formosa  %>%
  pivot_longer(., cols = c(Freq_nb, Freq), names_to = "Condition_f", values_to = "Frequency")  

graph_SGR_c$Condition <- as.factor(graph_SGR_c$Condition)  
graph_SGR_d$Condition_f <- as.factor(graph_SGR_d$Condition_f)

#GRAPH (A. formosa)
ggplot(graph_SGR_c, aes(x = Depth, y = graph_SGR_c$SGR, color = graph_SGR_c$Condition)) + 
  geom_point(size=2.5) + geom_line(size = 1, alpha = .5) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "SGR (1/d)", breaks = c(-0.0025, 0, 0.0025, 0.005, 0.0075, 0.010, 0.0125, 0.015, 0.0175)) +
  scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + theme(text=element_text(size = 13)) + 
  geom_text_repel(aes(label = Frequency, group = Condition_f), data = graph_SGR_d, size = 4, nudge_y = 0.0009, nudge_x = 0.28, point.size = NA, min.segment.length = Inf) +
  scale_fill_discrete(name = "Condition", labels = c("Bleaching", "Non-bleaching")) + 
  scale_color_discrete(name = "Condition", labels = c("Bleaching", "Non-bleaching")) +
  theme(legend.title = element_text(face = "bold")) + theme(text=element_text(size = 14)) +
  geom_hline(yintercept = 0, color = "red", alpha = .3, size = .8, linetype = "dashed")



###___CALCULATING SURVIVAL RATE___###

#Non-bleach - survival rate for every fragment individualy
growth_data$non_bleaching <- (growth_data$Survival_1/growth_data$Survival_0)*100

#Bleach - survival rate for every fragment individualy
growth_data$bleaching <- (growth_data$Survival_2/growth_data$Survival_1)*100

#Replacing NaN values with 0 ()because those are dead),leaving NA values (because those are missing)
growth_data <- growth_data %>% 
  mutate_all(~replace(., is.nan(.), 0))

# Averaging across depth
average_survival_per_depth <- growth_data %>%  
  group_by(Depth) %>% 
  summarise_at(vars("non_bleaching", "bleaching"), mean, na.rm = TRUE)


#___PLOTTING SURVIVAL RATE___#

#Creating a data frame for the graph
Depth <- rep(1:10, times = 4)
average_survival_per_depth$Depth <- Depth
Species <- rep(c("Acropora formosa", "Acropora verweyi", "Pocillopora verrucosa", "Porites cylindrica"), each=10)
average_survival_per_depth$Species <- Species

#Removing A. formosa
average_survival_per_depth_reduced <- average_survival_per_depth %>% 
  filter(!Species == "Acropora formosa")

graph_survival_rate <- average_survival_per_depth_reduced %>% select(Depth, non_bleaching, bleaching, Species) %>%
  pivot_longer(., cols = c(non_bleaching, bleaching), names_to = "Condition", values_to = "Survival_rate") 

graph_survival_rate$condition <- as.factor(graph_survival_rate$Condition)  

#GRAPH
Survival <- ggplot(graph_survival_rate, aes(x = Depth, y = Survival_rate, color = Condition, group = Condition)) + 
  geom_point(size=2) + geom_line() + facet_wrap(~Species) + 
  scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + scale_y_continuous(name = "Survival rate (%)") + 
  theme(text=element_text(size = 13)) +scale_fill_discrete(name = "Period", labels = c("Bleaching", "Non-bleaching")) + 
  scale_color_discrete(name = "Period", labels = c("Bleaching", "Non-bleaching")) + 
  theme(legend.title = element_text(face = "bold")) + theme(text=element_text(size = 14))

Survival

##__A. formosa only__##
average_survival_per_depth_formosa <- average_survival_per_depth %>% 
  filter(Species == "Acropora formosa")

graph_survival_rate_formosa <- average_survival_per_depth_formosa %>% select(Depth, non_bleaching, bleaching, Species) %>%
  pivot_longer(., cols = c(non_bleaching, bleaching), names_to = "Condition", values_to = "Survival_rate") 

graph_survival_rate_formosa$condition <- as.factor(graph_survival_rate_formosa$Condition)  

#GRAPH
ggplot(graph_survival_rate_formosa, aes(x = Depth, y = Survival_rate, color = Condition, group = Condition)) + 
  geom_point(size=2) + geom_line() + facet_wrap(~Species) + 
  scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + scale_y_continuous(name = "Survival rate (%)") + 
  theme(text=element_text(size = 14))


##__CALCULATING % CHANGE IN LIVE TISSUE__##

#EV = 0 when a fragment is dead
growth_data$EV_cm3_0[growth_data$Comments_0 == "Dead"] <- 0
growth_data$EV_cm3_1[growth_data$Comments_1 == "Dead"] <- 0
growth_data$EV_cm3_2[growth_data$Comments_2 == "Dead"] <- 0
growth_data$EV_cm3_3[growth_data$Comments_3 == "Dead"] <- 0

#1. Non-bleaching

#Calculating % change 
growth_data$pct_non_bleaching <- ((growth_data$EV_cm3_1 - growth_data$EV_cm3_0)/growth_data$EV_cm3_0)*100

#Average across depth
Average_pct_non_bleach <- growth_data %>%  
  group_by(Depth) %>% 
  summarise(non_bleaching = mean(pct_non_bleaching, na.rm = TRUE))
  
#2. Bleaching

#Calculating % change 
growth_data$pct_bleaching <- ((growth_data$EV_cm3_2 - growth_data$EV_cm3_1)/growth_data$EV_cm3_1)*100

#Average across depth
Average_pct_bleach <- growth_data %>%  
  group_by(Depth) %>% 
  summarise(bleaching = mean(pct_bleaching, na.rm = TRUE))
  

##__PLOTTING % CHANGE__##

#Creating a data frame for the graph
graph_pct_change <- merge(Average_pct_non_bleach, Average_pct_bleach, by="Depth")
Depth <- rep(1:10, times = 4)
graph_pct_change$Depth <- Depth
graph_pct_change$Species <- Species

graph_pct_change <- graph_pct_change %>% dplyr::select(Depth, non_bleaching, bleaching, Species) %>%
  pivot_longer(., cols = c(non_bleaching, bleaching), names_to = "condition", values_to = "pct_change") 

graph_pct_change$condition <- as.factor(graph_pct_change$condition)  
  
#Graph
ggplot(graph_pct_change, aes(x = Depth, y = pct_change, color = condition)) + 
  geom_point(size = 2) + geom_line() + facet_wrap(~Species, scales = "free_x") + 
  ggtitle("Production (%)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + 
  scale_y_continuous(name = "Change in live coral tissue (%)", breaks = c(-100, 0, 100, 200, 300, 400, 500, 600), limits = c(-100,650)) +
  scale_fill_discrete(name = "Condition", labels = c("Bleaching", "Non-bleaching")) + 
  scale_color_discrete(name = "Condition", labels = c("Bleaching", "Non-bleaching")) +
  theme(legend.title = element_text(face = "bold")) + theme(text=element_text(size = 14)) +
  geom_hline(yintercept = 0, color = "red", alpha = .3, size = .8, linetype = "dashed") 
 

#___Regression model for production___#

install.packages("broom")
library(broom)
install.packages("psych")
library(psych)
install.packages("stargazer")
library(stargazer)
install.packages("interplot")
library(interplot)
install.packages("car")
library(car)
install.packages("reshape2")
library(reshape2)
install.packages("ggpubr")
library(ggpubr)


# Trying out different models for each species and checking which one is best

#A. formosa
lm_formosa <- lm(pct_change ~ Depth + condition + condition:Depth, data = graph_pct_change[graph_pct_change$Species == "Acropora formosa", ])
lm_formosa_red <- lm(pct_change ~ Depth + condition, data = graph_pct_change[graph_pct_change$Species == "Acropora formosa", ])
lm_formosa_red_red <- lm(pct_change ~ Depth, data = graph_pct_change[graph_pct_change$Species == "Acropora formosa", ])

#A. verweyi
lm_verweyi <- lm(pct_change ~ Depth + condition + condition:Depth , data = graph_pct_change[graph_pct_change$Species == "Acropora verweyi", ])
lm_verweyi_red <- lm(pct_change ~ Depth + condition , data = graph_pct_change[graph_pct_change$Species == "Acropora verweyi", ])#Not significant
lm_verweyi_par <- lm(pct_change ~ Depth + I(Depth^2) + condition + condition:Depth + condition:I(Depth^2), data = graph_pct_change[graph_pct_change$Species == "Acropora verweyi", ])
lm_verweyi_par_red <- lm(pct_change ~ Depth + I(Depth^2) + condition, data = graph_pct_change[graph_pct_change$Species == "Acropora verweyi", ])

#P. verrucosa
lm_pocillopora <- lm(pct_change ~ Depth + condition + condition:Depth, data = graph_pct_change[graph_pct_change$Species == "Pocillopora verrucosa",])

#P. cylindrica
lm_porites <- lm(pct_change ~ Depth + condition + condition:Depth, data = graph_pct_change[graph_pct_change$Species == "Porites cylindrica", ])
lm_porites_red <- lm(pct_change ~ Depth + condition, data = graph_pct_change[graph_pct_change$Species == "Porites cylindrica", ])


#Look at the statistics 
#A. formosa
summary(lm_formosa)
summary(lm_formosa_red)
summary(lm_formosa_red_red)
anova(lm_formosa_red_red, lm_formosa_red) # formosa_red is better

#Checking model assumptions
par(mfrow=c(2,2))
plot(lm_formosa_red)

#A. verweyi
summary(lm_verweyi)
summary(lm_verweyi_red) #bad idea
summary(lm_verweyi_par)
summary(lm_verweyi_par_red)
anova(lm_verweyi_par_red, lm_verweyi_par) # verweyi_par is better 

#Checking model assumptions
plot(lm_verweyi_par_red)

#P. verrucosa
summary(lm_pocillopora)

#Checking model assumptions
plot(lm_pocillopora)

#P. cylindrica
summary(lm_porites)
summary(lm_porites_red)
anova(lm_porites_red, lm_porites) # lm_porites is better

#Checking model assumptions
plot(lm_porites_red)

#Additional checks
anova(lm_formosa_red)
anova(lm_verweyi_par)
anova(lm_pocillopora)
anova(lm_porites)


###___ALL GRAPHS TOGETHER__###

install.packages("gridExtra")
install.packages("grid")
library(grid)
library(gridExtra)
library(cowplot)

##__For the three species___##

# The graphs for survival and SGR from before to have them in one place + estetics modifications 

#SGR
ggplot(graph_SGR_a, aes(x = Depth, y = graph_SGR_a$SGR, color = graph_SGR_a$Condition)) + 
  geom_point(size=2) + geom_line(size = .8, alpha = .3) + facet_wrap(~Species, scales = "free_x") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "SGR (1/d)", limits = c(-0.0025, 0.01), breaks = c(-0.0025, 0, 0.0025, 0.005, 0.0075, 0.01)) +
  scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + theme(text=element_text(size = 13)) + 
  geom_text_repel(aes(label = Frequency, group = Condition_f), data = graph_SGR_b, size = 3, nudge_y = 0.0009, nudge_x = 0.28, point.size = NA, min.segment.length = Inf) +
  scale_fill_discrete(name = "Condition", labels = c("Bleaching", "Non-bleaching")) + 
  scale_color_discrete(name = "Condition", labels = c("Bleaching", "Non-bleaching")) +
  theme(legend.title = element_text(face = "bold")) + theme(text=element_text(size = 11), axis.title.y = element_text(size = 14)) +
  geom_hline(yintercept = 0, color = "red", alpha = .3, size = .8, linetype = "dashed") + #theme(legend.position = "none")
  theme(axis.title.x=element_blank()) + theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y = element_text(vjust=3)) + theme(legend.position = "none")

#Survival rate
ggplot(graph_survival_rate, aes(x = Depth, y = Survival_rate, color = Condition, group = Condition)) + 
  geom_point(size=2) + geom_line(size = .8, alpha = .3) + facet_wrap(~Species) + 
  scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + scale_y_continuous(name = "Survival rate (%)") + 
  theme(text=element_text(size = 13), axis.title.y = element_text(size = 14, vjust=2.8), strip.text = element_text(size=14, colour = "gray17")) +
  theme(legend.position = "none") + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) 
  
# Production (regressions)

# A. verweyi
b <- ggplot(lm_verweyi, aes(x = Depth, y = pct_change, color = condition, group = condition)) +
  geom_point() + scale_x_continuous(name = "Depth", limits = c(1, 10), breaks = c(1:10)) + 
  scale_y_continuous(name = "Production (%)", limits = c(-100, 200), breaks = c(-100, -50, 0, 50, 100, 150, 200)) +
  stat_smooth(aes(group=condition), method="lm", formula = y ~ x + I(x^2), se = F, na.rm = T) + theme(legend.position = "none") +
  geom_hline(yintercept = 0, color = "red", alpha = .3, size = .8, linetype = "dashed") + 
  theme(axis.title.x=element_blank(), axis.title.y = element_text(size = 14, vjust=3)) +
  theme(plot.margin = unit(c(0, 0, 0, .5), "cm"))

#P. verrucosa
c <- ggplot(lm_pocillopora, aes(x = Depth, y = pct_change, color = condition, group = condition)) +
  geom_point() + scale_x_continuous(name = "Depth", limits = c(1, 10), breaks = c(1:10)) + 
  scale_y_continuous(limits = c(-100, 200), breaks = c(-100, -50, 0, 50, 100, 150, 200)) + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  stat_smooth(aes(group=condition), method="lm", se = F, na.rm = T) + theme(legend.position = "none") +
  geom_hline(yintercept = 0, color = "red", alpha = .3, size = .8, linetype = "dashed") + theme(axis.title.x=element_blank()) +
    theme(plot.margin = unit(c(0, 0, 0, .1), "cm"))

#P. cylindrica
d <- ggplot(lm_porites, aes(x = Depth, y = pct_change, color = condition, group = condition)) +
  geom_point() + scale_x_continuous(name = "Depth", limits = c(1, 10), breaks = c(1:10)) + 
  scale_y_continuous(limits = c(-100, 200), breaks = c(-100, -50, 0, 50, 100, 150, 200)) + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  stat_smooth(aes(group=condition), method="lm", se = F, na.rm = T) + theme(legend.position = "none") +
  geom_hline(yintercept = 0, color = "red", alpha = .3, size = .8, linetype = "dashed") + theme(axis.title.x=element_blank()) +
  theme(plot.margin = unit(c(0, 0, 0, .1), "cm"))

#Creating one figure out of the regression graphs
bottom_row <- plot_grid(b, c, d, align = "h", rel_widths = c(1.2, 1, 1), ncol = 3)
x.grob <- textGrob("Depth (m)", hjust = .2,
                   gp=gpar(fontsize=14))
bottom_row
grid.arrange(arrangeGrob(bottom_row,bottom = x.grob))


##__A. Formosa separately__##

#Survival rate
f <- ggplot(graph_survival_rate_formosa, aes(x = Depth, y = Survival_rate, color = Condition, group = Condition)) + 
  geom_point(size=2) + geom_line(size = .8, alpha = .3) + theme(text=element_text(size = 10)) + 
  scale_fill_discrete(name = "Period", labels = c("Bleaching", "Non-bleaching")) + 
  scale_color_discrete(name = "Period", labels = c("Bleaching", "Non-bleaching")) +
  theme(legend.title = element_text(face = "bold")) + theme(text=element_text(size = 11)) +
  scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + scale_y_continuous(name = "Survival rate (%)") 

#SGR
 e <- ggplot(graph_SGR_c, aes(x = Depth, y = graph_SGR_c$SGR, color = graph_SGR_c$Condition)) + 
  geom_point(size=2) + geom_line(size = .8, alpha = .3) + 
  scale_y_continuous(name = "SGR (1/d)", breaks = c(-0.0025, 0, 0.0025, 0.005, 0.0075, 0.010, 0.0125, 0.015, 0.0175)) +
  scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + theme(text=element_text(size = 10)) + 
  geom_text_repel(aes(label = Frequency, group = Condition_f), data = graph_SGR_d, size = 3, nudge_y = 0.0009, nudge_x = 0.28, point.size = NA, min.segment.length = Inf) +
  scale_fill_discrete(name = "Period", labels = c("Bleaching", "Non-bleaching")) + 
  scale_color_discrete(name = "Period", labels = c("Bleaching", "Non-bleaching")) +
  theme(legend.title = element_text(face = "bold")) +
   scale_fill_discrete(name = "Condition", labels = c("Bleaching", "Non-bleaching")) + 
   scale_color_discrete(name = "Condition", labels = c("Bleaching", "Non-bleaching")) +
   theme(legend.title = element_text(face = "bold")) + theme(text=element_text(size = 11))

 #Production (regression)
 a <- ggplot(lm_formosa, aes(x = Depth, y = pct_change, color = condition, group = condition)) +
   geom_point() + scale_x_continuous(name = "Depth (m)", limits = c(1, 10), breaks = c(1:10)) + 
   scale_y_continuous(name = "Production (%)", limits = c(-100, 610)) + 
   scale_fill_discrete(name = "Period", labels = c("Bleaching", "Non-bleaching")) + 
   scale_color_discrete(name = "Period", labels = c("Bleaching", "Non-bleaching")) +
   theme(legend.title = element_text(face = "bold")) + theme(text=element_text(size = 11)) +
   stat_smooth(aes(group=condition), method="lm", se = F, na.rm = T) + theme(text=element_text(size = 11)) +
   geom_hline(yintercept = 0, color = "red", alpha = .3, size = .8, linetype = "dashed")  

#Survival SGR and production of A. formosa in one figure
 figure <- ggarrange(f, e , a, common.legend = TRUE, legend = "right", ncol = 2, nrow = 2)
 figure

 
 ####____One year period____####
 
 
 #___Calculating SGR (from corals with > 80% survival)___#  
 
 #Filter out fragments that had a survival >= 80% on t0 and t3 from the main dataset
 growth_data_year_subset <- growth_data %>% 
   filter(Survival_0 >= 80 & Survival_3 >= 80)
 
 #Calculate SGR for each fragment
 growth_data_year_subset$growth_rate <- (log(growth_data_year_subset$EV_cm3_3/growth_data_year_subset$EV_cm3_0)/343)
 
 #creating a data frame for number of fragments per dept that have a survival >= 80%
 number_fragments_year <- as.data.frame(table(growth_data_year_subset$Depth))
 number_fragments_year
 
 # Calculating the average SGR per depth 
 average_growth_rate_per_depth_year <- growth_data_year_subset %>%  
   group_by(Depth) %>% 
   summarise(year_SGR = mean(growth_rate, na.rm = TRUE))
 
 #Adding the number of fragments to the SGR values
 average_growth_rate_per_depth_year <- cbind(average_growth_rate_per_depth_year, number_fragments_year)
 average_growth_rate_per_depth_year <- average_growth_rate_per_depth_year %>% 
   rename(Freq_year = Freq)
 
 #Adding Species names
 #Species <- rep(c("Acropora formosa", "Acropora verweyi", "Pocillopora verrucosa", "Porites cylindrica"), each=10)
 #average_growth_rate_per_depth_year <- cbind(average_growth_rate_per_depth_year, Species)  
 
 
 #insert missing values for deptths with no data (no fragments with > 80% survival at those depths)
 average_growth_rate_per_depth_year <- average_growth_rate_per_depth_year %>% 
   add_row(Depth=5, year_SGR=NA, Var1=NA, Freq_year=NA, .before = 5 )
 
 average_growth_rate_per_depth_year <- average_growth_rate_per_depth_year %>% 
   add_row(Depth=10, year_SGR=NA, Var1=NA, Freq_year=NA, .before = 10 )
 
 average_growth_rate_per_depth_year <- average_growth_rate_per_depth_year %>% 
   add_row(Depth=16, year_SGR=NA, Var1=NA, Freq_year=NA, .before = 16 )
 
 average_growth_rate_per_depth_year <- average_growth_rate_per_depth_year %>% 
   add_row(Depth=20, year_SGR=NA, Var1=NA, Freq_year=NA, .before = 20 )
 
 average_growth_rate_per_depth_year <- average_growth_rate_per_depth_year %>% 
   add_row(Depth=23, year_SGR=NA, Var1=NA, Freq_year=NA, .before = 23 )
 
 average_growth_rate_per_depth_year <- average_growth_rate_per_depth_year %>% 
   add_row(Depth=24, year_SGR=NA, Var1=NA, Freq_year=NA, .before = 24 )
 
 #Removing one remaining value for Porites cylindrica (Growth data missing for this species due to high mortality)
 average_growth_rate_per_depth_year <- average_growth_rate_per_depth_year[-31, ]
 
 #__PLOTING SGR__#
 
 #Creating a dataframe for the graph
 Depth_3 <- rep(1:10, times = 3)
 Species_3 <- Species <- rep(c("Acropora formosa", "Acropora verweyi", "Pocillopora verrucosa"), each=10)
 average_growth_rate_per_depth_year$Depth <- Depth_3
 average_growth_rate_per_depth_year$Species <- Species_3
 average_growth_rate_per_depth_year$Species <- as.factor(average_growth_rate_per_depth_year$Species)
 
 
 # GRAPH -> SGR over after one year
 ggplot(average_growth_rate_per_depth_year, aes(x = Depth, y = year_SGR)) + 
   geom_point(size=2.5, color="chartreuse3") + geom_line(size = 1, alpha = .5, color="chartreuse3") + facet_wrap(~Species, nrow = 2) + 
   scale_y_continuous(name = "SGR (1/d)", breaks = c(-0.0025, 0, 0.0025, 0.005, 0.0075, 0.010, 0.0125, 0.015, 0.0175)) +
   scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + theme(text=element_text(size = 16)) + 
   geom_text_repel(aes(label = Freq_year), data = average_growth_rate_per_depth_year, size = 4, nudge_y = 0.0003, nudge_x = 0.2, point.size = NA, min.segment.length = Inf, color="darkgreen") +
   geom_hline(yintercept = 0, color = "red", alpha = .3, size = .8, linetype = "dashed") 
 
 
 
 ###___CALCULATING THE SURVIVAL RATE___###
 
 #Calculating survival rate for individual fragments
 growth_data$year_survival <- (growth_data$Survival_3/growth_data$Survival_0)*100
 
 #Replacing NaN values with 0 ()because those are dead from the start),leaving NA values (because those are missing)
 growth_data <- growth_data %>% 
   mutate_all(~replace(., is.nan(.), 0))
 
 # Averaging across depth
 average_survival_per_depth_year <- growth_data %>% 
   group_by(Depth) %>% 
   summarise(survival_rate = mean(year_survival, na.rm = TRUE))
 
 
 #___PLOTTING SURVIVAL RATE___#
 
 #Creating a data frame for the graph
 Depth <- rep(1:10, times = 4)
 average_survival_per_depth_year$Depth <- Depth
 Species <- rep(c("Acropora formosa", "Acropora verweyi", "Pocillopora verrucosa", "Porites cylindrica"), each=10)
 average_survival_per_depth_year$Species <- Species
 
 #GRAPH
 ggplot(average_survival_per_depth_year, aes(x = Depth, y = survival_rate)) + 
   geom_point(size=2.5, color="chartreuse3") + geom_line(size = 1, alpha = .5, color="chartreuse3") + facet_wrap(~Species) + 
   theme(text=element_text(size = 16)) +
   scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) +
   scale_y_continuous(name = "Survival rate (%)")
 
 
 
 ##__CALCULATING PRODUCTION__##
 
 #Calculating % change for individual fragments
 growth_data$pct_year <- ((growth_data$EV_cm3_3 - growth_data$EV_cm3_0)/growth_data$EV_cm3_0)*100
 
 #Average across depth
 Average_pct_year <- growth_data %>%  
   group_by(Depth) %>% 
   summarise(pct = mean(pct_year, na.rm = TRUE))
 
 
 ##__PLOTTING PRODUCTION__##
 
 #Creating a data frame for the graph
 Depth <- rep(1:10, times = 4)
 Average_pct_year$Depth <- Depth
 Species <- rep(c("Acropora formosa", "Acropora verweyi", "Pocillopora verrucosa", "Porites cylindrica"), each=10)
 Average_pct_year$Species <- Species
 
 #Graph
 ggplot(Average_pct_year, aes(x = Depth, y = pct)) +
   geom_point(size=2.5, color="chartreuse3") + geom_line(size = 1, alpha = .5, color="chartreuse3") + facet_wrap(~Species, scales = "free_y") +
   scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + 
   scale_y_continuous(name = "Production (%)") + theme(text=element_text(size = 16)) +
   geom_hline(yintercept = 0, color = "red", alpha = .3, size = .8, linetype = "dashed") 
 
 
 ##__REGRESSION for production of every species individually (over one year)__## -> none of the models are significant (not used in thesis)
 
 #Acropora formosa
 lm_pct_year_formosa <- lm(pct ~ Depth, data = Average_pct_year[Average_pct_year$Species == "Acropora formosa", ])
 summary(lm_pct_year_formosa)
 
 ggplot(lm_pct_year_formosa, aes(x = Depth, y = pct)) +
   geom_point() + scale_x_continuous(name = "Depth", limits = c(1, 10), breaks = c(1:10)) + 
   scale_y_continuous(name = "%") +
   stat_smooth(method="lm", color = "red" , na.rm = T)
 
 #Acropora verweyi
 Average_pct_year$Depth_sq <- Average_pct_year$Depth^2 #Because the data fits a parabola
 
 lm_pct_year_verweyi <- lm(pct ~ Depth + Depth_sq, data = Average_pct_year[Average_pct_year$Species == "Acropora verweyi", ])
 summary(lm_pct_year_verweyi)
 
 ggplot(lm_pct_year_verweyi, aes(x = Depth, y = pct)) +
   geom_point() + scale_x_continuous(name = "Depth", limits = c(1, 10), breaks = c(1:10)) + 
   scale_y_continuous(name = "%") +
   stat_smooth(method="lm", formula = y ~ x + I(x^2), color = "red" , na.rm = T)
 
 #Pocillopora verrucosa
 lm_pct_year_pocillopora <- lm(pct ~ Depth, data = Average_pct_year[Average_pct_year$Species == "Pocillopora verrucosa", ])
 summary(lm_pct_year_pocillopora)
 
 ggplot(lm_pct_year_pocillopora, aes(x = Depth, y = pct)) +
   geom_point() + scale_x_continuous(name = "Depth", limits = c(1, 10), breaks = c(1:10)) + 
   scale_y_continuous(name = "%") +
   stat_smooth(method="lm", color = "red" , na.rm = T)
 
 #Porites cylindrica
 lm_pct_year_porites <- lm(pct ~ Depth, data = Average_pct_year[Average_pct_year$Species == "Porites cylindrica", ])
 summary(lm_pct_year_porites)
 
 ggplot(lm_pct_year_porites, aes(x = Depth, y = pct)) +
   geom_point() + scale_x_continuous(name = "Depth", limits = c(1, 10), breaks = c(1:10)) + 
   scale_y_continuous(name = "%") +
   stat_smooth(method="lm", color = "red" , na.rm = T)
 

  ###___INITIAL FRAGMENT SIZE DIFFERENCES___###
 
 
 install.packages("dunn.test")
 library(dunn.test)
 install.packages("FSA")
 library(FSA)
 
 #Checking the distribution of the data
 hist(growth_data$EV_cm3_0)
 
 #1. Acropora formosa
 
 formosa_EV <- growth_data %>% 
   filter(Species == "Acropora formosa") 
 #To see if variable normally distributed
 hist(formosa_EV$EV_cm3_0)  
 shapiro.test(formosa_EV$EV_cm3_0)
 
 #Boxplot showing fragment sizes at different depths
 ggplot(formosa_EV, aes(x = Structure, y = EV_cm3_0, group = Structure, fill= as.factor(Structure))) + geom_boxplot() + 
   scale_x_discrete(name = "Depth (m)", breaks= c(1:10)) + scale_y_continuous(name = "EV(cm^3)") + 
   theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5)) +
   stat_compare_means(method = "kruskal.test", label.y = 177, label.x = 1.5, size = 6) +theme(text=element_text(size = 14))
 
 #Statistic tests for differences in fragment size between different depths
 kruskal.test(EV_cm3_0 ~ Structure, data = formosa_EV)                       #Is fragment size significantly different between depths? If p < 0.05 than yes -> Dunns test
 #dunnTest(formosa_EV$EV_cm3_0, formosa_EV$Structure, method="bonferroni")   #Which groups are significantly different? (pairwise comparison)
 # no need for Dunns test because Kruskal-W test not significant
 
 
 #2. Acropora verweyi
 
 verweyi_EV <- growth_data %>% 
   filter(Species == "Acropora verweyi") 
 hist(verweyi_EV$EV_cm3_0)
 shapiro.test(verweyi_EV$EV_cm3_0)
 
 #Boxplot showing fragment sizes at different depths
 ggplot(verweyi_EV, aes(x = Structure, y = EV_cm3_0, group = Structure, fill= as.factor(Structure))) + geom_boxplot() + 
   scale_x_discrete(name = "Depth (m)", breaks= c(1:10)) + scale_y_continuous(name = "EV(cm^3)", limits = c(0,200)) + 
   theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5)) +
   stat_compare_means(method = "kruskal.test", label.y = 177, label.x = 1.5, size = 6) +theme(text=element_text(size = 14))
 
 #Statistic tests for differences in fragment size between different depths
 kruskal.test(EV_cm3_0 ~ Structure, data = verweyi_EV)
 dunnTest(verweyi_EV$EV_cm3_0, verweyi_EV$Structure, method="bonferroni")
 
 
 #. Pocillopora verrucosa
 
 pocilopora_EV <- growth_data %>% 
   filter(Species == "Pocillopora verrucosa") 
 hist(pocilopora_EV$EV_cm3_0)
 shapiro.test(pocilopora_EV$EV_cm3_0)
 
 #Boxplot showing fragment sizes at different depths
 ggplot(pocilopora_EV, aes(x = Structure, y = EV_cm3_0, group = Structure, fill= as.factor(Structure))) + geom_boxplot() + 
   scale_x_discrete(name = "Depth (m)", breaks= c(1:10)) + scale_y_continuous(name = "EV(cm^3)") + 
   theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5)) +
   stat_compare_means(method = "kruskal.test", label.y = 177, label.x = 1.5, size = 6) +theme(text=element_text(size = 14))
 
 #Statistic tests for differences in fragment size between different depths
 kruskal.test(EV_cm3_0 ~ Structure, data = pocilopora_EV)
 dunnTest(pocilopora_EV$EV_cm3_0, pocilopora_EV$Structure, method="bonferroni")
 
 
 #4. Porites cylindrica
 
 porites_EV <- growth_data %>% 
   filter(Species == "Porites cylindrica") %>% 
   filter(!Structure == "6") # excluding depth 6 to do a kruskal W test
 hist(porites_EV$EV_cm3_0)
 shapiro.test(porites_EV$EV_cm3_0)
 
 #Boxplot showing fragment sizes at different depths
 ggplot(porites_EV, aes(x = Structure, y = EV_cm3_0, group = Structure, fill= as.factor(Structure))) + geom_boxplot() + 
   scale_x_discrete(name = "Depth (m)", breaks= c(1:10)) + scale_y_continuous(name = "EV(cm^3)", limits = c(0,200)) + 
   theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5)) +
   stat_compare_means(method = "kruskal.test", label.y = 177, label.x = 1.5, size = 6) + theme(text=element_text(size = 14))
 
 #Statistic tests for differences in fragment size between different depths
 kruskal.test(EV_cm3_0 ~ Structure, data = porites_EV)
 #dunnTest(porites_EV$EV_cm3_0, porites_EV$Structure, method="bonferroni")  no need for this test because previous not significant
 
 
 #Calculating the MEAN and SD for EV0 for every species 
 
 group_by(growth_data, Species) %>%
   summarise(
     count = n(),
     mean = mean(EV_cm3_0, na.rm = TRUE),
     sd = sd(EV_cm3_0, na.rm = TRUE))
 
 #Calculating the MEAN and SD for initial lengths for every species (for methods)
 
 group_by(growth_data, Species) %>%
   summarise(
     count = n(),
     mean = mean(Length_cm_0, na.rm = TRUE),
     sd = sd(Length_cm_0, na.rm = TRUE))
 
 
 ####____QUANTIFYING BLEACHING INTENSITY____####
 
 bleach_data <- read.csv("Bleaching_analyses_depth_gradient.csv", header=T, na.strings=c(""))
 
 head(bleach_data)
 
 #__Creating a data frame__#
 
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

 
 #___GRAPHS - change in color intensity of the Green channel over time 
 
 #Pocillopora verrucosa
 ggplot(Pocillopora, aes(x=Date,y=Average_G,color=Depth_bl)) + theme(plot.title = element_text(hjust = 0.5)) +
   geom_point(size=2.5) + geom_line(size = .7) + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
   scale_x_continuous(name = "Date", breaks = c(1:12), labels = c("18.3", "25.3.", "1.4.", "8.4.", "15.4.","22.4.", "29.4.", "6.5.", "13.5.", "3.6.", "17.6.", "15.7.")) + 
   scale_y_continuous(name = "Colour intensity (bpp)", limits = c(1, 250)) + theme(text=element_text(size = 14)) + scale_fill_discrete (name = "Depth (m)") + 
   scale_color_discrete(name = "Depth (m)") + theme(legend.title = element_text(face = "bold")) +
   theme(axis.title.y = element_text(vjust=2))
 
 #Porites cylindrica 
 ggplot(Porites, aes(x=Date,y=Average_G,color=Depth_bl)) + theme(plot.title = element_text(hjust = 0.5)) +
   geom_point(size=2.5) + geom_line(size = .7) + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
   scale_x_continuous(name = "Date", breaks = c(1:12), labels = c("18.3", "25.3.", "1.4.", "8.4.", "15.4.","22.4.", "29.4.", "6.5.", "13.5.", "3.6.", "17.6.", "15.7.")) + 
   scale_y_continuous(name = "Colour intensity (bpp)", limits = c(1, 250)) + theme(text=element_text(size = 14)) + scale_fill_discrete (name = "Depth (m)") + 
   scale_color_discrete(name = "Depth (m)") + theme(legend.title = element_text(face = "bold")) +
   theme(axis.title.y = element_text(vjust=2))
 
 #Acropora verweyi
 ggplot(Verweyi, aes(x=Date,y=Average_G,color=Depth_bl)) + theme(plot.title = element_text(hjust = 0.5)) +
   geom_point(size=2.5) + geom_line(size = .7) + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
   scale_x_continuous(name = "Date", breaks = c(1:12), labels = c("18.3", "25.3.", "1.4.", "8.4.", "15.4.","22.4.", "29.4.", "6.5.", "13.5.", "3.6.", "17.6.", "15.7.")) + 
   scale_y_continuous(name = "Colour intensity (bpp)", limits = c(1, 250)) + theme(text=element_text(size = 14)) + scale_fill_discrete (name = "Depth (m)") + 
   scale_color_discrete(name = "Depth (m)") + theme(legend.title = element_text(face = "bold")) +
   theme(axis.title.y = element_text(vjust=2))
 
 
 #TEST (all species on one graph) -> no good, too crowded
 #ggplot(bleach_short, aes(x=Date,y=Average_G,color=Depth)) + ggtitle("Change in colour intensity of P. verrucosa over time") + theme(plot.title = element_text(hjust = 0.5)) + facet_grid(Species ~ .) +
 #  geom_point(size=2.5) + geom_line(size = .7) + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
 #  scale_x_continuous(name = "Time", breaks = c(1:12), labels = c("18.3.2020.", "25.3.2020.", "1.4.2020.", "8.4.2020.", "15.4.2020.","22.4.2020.", "29.4.2020.", "6.5.2020.", "13.5.2020.", "3.6.2020.", "17.6.2020.", "15.7.2020.")) + 
 #  scale_y_continuous(name = "Color intensity", limits = c(1, 250)) + theme(text=element_text(size = 14)) 
 
 
 #___Survival over time during the bleaching period (not used in thesis)___#
 
 #Pocillopora verrucosa
 ggplot(Pocillopora, aes(x=Date,y=Average_survival,color=Depth_bl)) + ggtitle("Survival of P. verrucosa over time") + theme(plot.title = element_text(hjust = 0.5)) +
   geom_point(size=2.5) + geom_line() + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
   scale_x_continuous(name = "Time", breaks = c(1:12), labels = c("18.3", "25.3.", "1.4.", "8.4.", "15.4.","22.4.", "29.4.", "6.5.", "13.5.", "3.6.", "17.6.", "15.7.")) + 
   scale_y_continuous(name = "Survival (%)", limits = c(0, 100)) + theme(text=element_text(size = 14)) +
   scale_color_discrete(name = "Depth (m)") + theme(legend.title = element_text(face = "bold"))
 
 #Porites cylindrica 
 ggplot(Porites, aes(x=Date,y=Average_survival,color=Depth_bl)) + ggtitle("Survival of P. cylindrica over time") + theme(plot.title = element_text(hjust = 0.5)) +
   geom_point(size=2.5) + geom_line() + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
   scale_x_continuous(name = "Time", breaks = c(1:12), labels = c("18.3", "25.3.", "1.4.", "8.4.", "15.4.","22.4.", "29.4.", "6.5.", "13.5.", "3.6.", "17.6.", "15.7.")) + 
   scale_y_continuous(name = "Survival (%)", limits = c(0, 100)) + theme(text=element_text(size = 14)) + 
   scale_color_discrete(name = "Depth (m)") + theme(legend.title = element_text(face = "bold"))
 
 #Acropora verweyi
 ggplot(Verweyi, aes(x=Date,y=Average_survival,color=Depth_bl)) + ggtitle("Survival of A. verweyi over time") + theme(plot.title = element_text(hjust = 0.5)) +
   geom_point(size=2.5) + geom_line() + theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
   scale_x_continuous(name = "Time", breaks = c(1:12), labels = c("18.3", "25.3.", "1.4.", "8.4.", "15.4.","22.4.", "29.4.", "6.5.", "13.5.", "3.6.", "17.6.", "15.7.")) + 
   scale_y_continuous(name = "Survival (%)", limits = c(0, 100)) + theme(text=element_text(size = 14)) +
   scale_color_discrete(name = "Depth (m)") + theme(legend.title = element_text(face = "bold"))
 
 
 ###___Correlation analyses___###
 
 #Calculating the AVERAGE bleaching intensity per depth from THE FIRST MEASUREMENT TO the MAX bleaching point 
 
 #1.Pocillopora
 Pocillopora_average_start_max <- Pocillopora %>% 
   group_by(Depth_bl) %>% 
   slice(seq_len(min(which.max(Average_G)))) %>% 
   summarise(Average_G = mean(Average_G, na.rm = TRUE))
 
 #TEST
 plot(Pocillopora_average_start_max)
 shapiro.test(Pocillopora_average_start_max$Average_G)
 
 
 #2.Porites
 Porites_average_start_max <- Porites %>% 
   group_by(Depth_bl) %>% 
   slice(seq_len(min(which.max(Average_G)))) %>% 
   summarise(Average_G = mean(Average_G, na.rm = TRUE))
 
 plot(Porites_average_start_max)
 shapiro.test(Porites_average_start_max$Average_G)
 
 #3.Verweyi
 Verweyi_average_start_max <- Verweyi %>% 
   group_by(Depth_bl) %>% 
   slice(seq_len(min(which.max(Average_G)))) %>% 
   summarise(Average_G = mean(Average_G, na.rm = TRUE))
 
 plot(Verweyi_average_start_max)
 shapiro.test(Verweyi_average_start_max$Average_G)
 
 #Calculating the AVERAGE bleaching intensities with the difference in survival and growth 
 #(differences between the bleaching and the non-bleaching per depth)
 
 install.packages("Hmisc")
 library(Hmisc)
 install.packages("PerformanceAnalytics")
 library("PerformanceAnalytics")
 install.packages("psych")
 library(psych)
 
 #Check for normality
 shapiro.test(average_survival_per_depth$bleaching) 
 shapiro.test(average_growth_rate_per_depth_bleach$bleaching) 
 
 #1.Pocilopora
 
 average_survival_per_depth$difference_survival <- average_survival_per_depth$bleaching - average_survival_per_depth$non_bleaching
 difference_SGR <- average_growth_rate_per_depth_bleach$bleaching - average_growth_rate_per_depth_non_bleach$non_bleaching
 
 Pocillopora_correlation_dif <- cbind(Pocillopora_average_start_max, average_survival_per_depth$difference_survival[21:30])
 Pocillopora_correlation_dif <- cbind(Pocillopora_correlation_dif, difference_SGR[21:30])
 Pocillopora_correlation_dif <- Pocillopora_correlation_dif %>% 
   rename(Colour_intensity = Average_G, Survival = `average_survival_per_depth$difference_survival[21:30]`, SGR = `difference_SGR[21:30]`)
 
 coef_pocillopora_dif <- rcorr(as.matrix(Pocillopora_correlation_dif), type = c("spearman"))
 coef_pocillopora_dif
 
 pairs.panels(Pocillopora_correlation_dif[,c(2, 3, 4)], lm = T, density = F, ellipses =F, method = "spearman", stars = TRUE, hist.col = 1) 
 
 
 #2.Porites
 
 Porites_correlation_dif <- cbind(Porites_average_start_max, average_survival_per_depth$difference_survival[31:40])
 Porites_correlation_dif <- cbind(Porites_correlation_dif, difference_SGR[31:40])
 Porites_correlation_dif <- Porites_correlation_dif %>% 
   rename(Colour_intensity = Average_G, Survival = `average_survival_per_depth$difference_survival[31:40]`, SGR = `difference_SGR[31:40]`)
 
 coef_porites_dif <- rcorr(as.matrix(Porites_correlation_dif), type = c("spearman"))
 coef_porites_dif
 
 pairs.panels(Porites_correlation_dif[,c(2, 3, 4)], lm = T, density = F, ellipses =F, method = "spearman", stars = TRUE, hist.col = 1) 
 
 
 #3.Verweyi
 
 Verweyi_correlation_dif <- cbind(Verweyi_average_start_max, average_survival_per_depth$difference_survival[11:20])
 Verweyi_correlation_dif <- cbind(Verweyi_correlation_dif, difference_SGR[11:20])
 Verweyi_correlation_dif <- Verweyi_correlation_dif %>% 
   rename(Colour_intensity = Average_G, Survival = `average_survival_per_depth$difference_survival[11:20]`, SGR = `difference_SGR[11:20]`)
 
 coef_verweyi_dif <- rcorr(as.matrix(Verweyi_correlation_dif), type = c("spearman"))
 coef_verweyi_dif
 
 pairs.panels(Verweyi_correlation_dif[,c(2, 3, 4)], lm = T, density = F, ellipses =F, method = "spearman", stars = TRUE, hist.col = 1) 
 
 
 
 ####____Temperature recorded at 1 and 10 m____####
 
 temp_light <- read.csv(file.choose(), header=TRUE, na.strings=c(""))
 
 temp_light <- temp_light %>% 
   filter(Location == "Pilli Pipa") 
 
 temp_light$Depth <- as.factor(temp_light$Depth)
 
 # set date format
 temp_light$Date.Time <- as.Date(temp_light$Date.Time, format="%m/%d/%Y %H:%M")
 head(temp_light$Date.Time)
 temp_light$Date.Time
 #Graph
 
 ggplot(temp_light, aes(y = Temp, x = Date.Time, color = Depth)) + geom_line(size = 0.8) +
   scale_y_continuous(name = "Temperature (˚C)", limits = c(25, 30)) +
   labs(x = "Month") + theme(legend.title = element_text(face = "bold")) + 
   theme(text=element_text(size = 14)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
   geom_vline(xintercept = as.numeric(temp_light$Date.Time[670]), color = "red", alpha = .3, size = .7) +
   scale_color_discrete(name = "Depth (m)")
 
 
 group_by(temp_light, Depth) %>%
   summarise(
     count = n(),
     mean = mean(Temp, na.rm = TRUE),
     sd = sd(Temp, na.rm = TRUE))  





