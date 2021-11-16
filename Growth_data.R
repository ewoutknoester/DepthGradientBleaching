
###___DEPTH GRADIENT GROWTH DATA___###

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set directory at current (make sure file is saved)

growth_data <- read.csv("Coral_growth_final", header=T, na.strings=c("")) 


install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)

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

graph_SGR_a <- graph_SGR  %>%
  select(non_bleaching, bleaching, Species, Depth) %>% 
  pivot_longer(., cols = c(non_bleaching, bleaching), names_to = "Condition", values_to = "SGR")
  
graph_SGR_b <- graph_SGR  %>%
  pivot_longer(., cols = c(Freq_nb, Freq), names_to = "Condition_f", values_to = "Frequency")  

graph_SGR_a$Condition <- as.factor(graph_SGR_a$Condition)  
graph_SGR_b$Condition_f <- as.factor(graph_SGR_b$Condition_f)

install.packages("ggrepel") # To add the number of fragments next to points
library(ggrepel)
  
#GRAPH
ggplot(graph_SGR_a, aes(x = Depth, y = graph_SGR_a$SGR, color = graph_SGR_a$Condition)) + 
  geom_point(size=2.5) + geom_line(size = 1, alpha = .5) + facet_wrap(~Species, scales = "free_x") + 
  ggtitle("SGR of four coral species") +  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "SGR (1/d)", breaks = c(-0.0025, 0, 0.0025, 0.005, 0.0075, 0.010, 0.0125, 0.015, 0.0175)) +
  scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + theme(text=element_text(size = 13)) + 
  geom_text_repel(aes(label = Frequency, group = Condition_f), data = graph_SGR_b, size = 4, nudge_y = 0.0009, nudge_x = 0.28, point.size = NA, min.segment.length = Inf) +
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

graph_survival_rate <- average_survival_per_depth %>% select(Depth, non_bleaching, bleaching, Species) %>%
  pivot_longer(., cols = c(non_bleaching, bleaching), names_to = "Condition", values_to = "Survival_rate") 

graph_survival_rate$condition <- as.factor(graph_survival_rate$Condition)  

#GRAPH
ggplot(graph_survival_rate, aes(x = Depth, y = Survival_rate, color = Condition, group = Condition)) + 
  geom_point(size=2) + geom_line() + facet_wrap(~Species) + 
  ggtitle("Survival rate of four species during the non-bleaching and bleaching period") + 
  scale_x_continuous(breaks = c(1:10)) + theme(text=element_text(size = 13))



###___CALCULATING THE GROWTH FACTOR (G) (production)___###

#EV = 0 when a fragment is dead
growth_data$EV_cm3_0[growth_data$Comments_0 == "Dead"] <- 0
growth_data$EV_cm3_1[growth_data$Comments_1 == "Dead"] <- 0
growth_data$EV_cm3_2[growth_data$Comments_2 == "Dead"] <- 0
growth_data$EV_cm3_3[growth_data$Comments_3 == "Dead"] <- 0


#1. Non_bleaching

#Calculate the Growth factor per fragment (all fragments included)
growth_data$G_factor_non_bleaching <- exp((log(growth_data$EV_cm3_1/growth_data$EV_cm3_0)/118))

# Averaging across depth 
Average_G_factor_non_bleach <- growth_data %>%  
  group_by(Depth) %>% 
  summarise(non_bleaching = mean(G_factor_non_bleaching, na.rm = TRUE))


#2. Bleaching

#Calculate the Growth factor per fragment (all fragments included)
growth_data$G_factor_bleaching <- exp((log(growth_data$EV_cm3_2/growth_data$EV_cm3_1)/118))

# Averaging across depth 
Average_G_factor_bleach <- growth_data %>%  
  group_by(Depth) %>% 
  summarise(bleaching = mean(G_factor_bleaching, na.rm = TRUE))

#Creating a data frame for the graph
graph_G_factor <- merge(Average_G_factor_non_bleach, Average_G_factor_bleach, by="Depth")
graph_G_factor$Depth <- Depth
graph_G_factor$Species <- Species

graph_G_factor <- graph_G_factor %>% select(Depth, non_bleaching, bleaching, Species) %>%
  pivot_longer(., cols = c(non_bleaching, bleaching), names_to = "Condition", values_to = "Growth_factor") 

graph_G_factor$condition <- as.factor(graph_G_factor$Condition) 

#Transforming in % per day 
graph_G_factor <- graph_G_factor %>% 
  mutate(Growth_factor = (Growth_factor - 1)*100)

#GRAPH
ggplot(graph_G_factor, aes(x = Depth, y = Growth_factor, color = Condition, group = Condition)) + 
  geom_point(size=2) + geom_line() + facet_wrap(~Species) + 
  ggtitle("Growth factor of four species during the non-bleaching and bleaching period") + 
  scale_x_continuous(breaks = c(1:10)) + theme(text=element_text(size = 13))  


##__CALCULATING % CHANGE IN LIVE TISSUE__##

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

graph_pct_change <- graph_pct_change %>% select(Depth, non_bleaching, bleaching, Species) %>%
  pivot_longer(., cols = c(non_bleaching, bleaching), names_to = "condition", values_to = "pct_change") 

graph_pct_change$condition <- as.factor(graph_pct_change$condition)  
  
#Graph
ggplot(graph_pct_change, aes(x = Depth, y = pct_change, color = condition)) + 
  geom_point(size=3) + geom_line(size = 1) + facet_wrap(~Species, scales = "free_x") + 
  ggtitle("% change in live coral tissue for four species") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + 
  scale_y_continuous(name = "Change in live coral tissue (%)", breaks = c(-100, 0, 100, 200, 300, 400, 500, 600), limits = c(-100,650)) +
  scale_fill_discrete(name = "Condition", labels = c("Bleaching", "Non-bleaching")) + 
  scale_color_discrete(name = "Condition", labels = c("Bleaching", "Non-bleaching")) +
  theme(legend.title = element_text(face = "bold")) + theme(text=element_text(size = 14)) +
  geom_hline(yintercept = 0, color = "red", alpha = .3, size = .8, linetype = "dashed") 
 

#___REGRESSION MODEL___#

#Packages for visualising the results
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

# % change during the bleaching and non bleaching perion

#Normal linear regression models
lm_formosa <- lm(pct_change ~ Depth + condition + condition:Depth, data = graph_pct_change[graph_pct_change$Species == "Acropora formosa", ])
lm_verweyi <- lm(pct_change ~ Depth + I(Depth^2) + condition + condition:Depth + condition:I(Depth^2), data = graph_pct_change[graph_pct_change$Species == "Acropora verweyi", ])
lm_pocillopora <- lm(pct_change ~ Depth + condition + condition:Depth, data = graph_pct_change[graph_pct_change$Species == "Pocillopora verrucosa",])
lm_porites <- lm(pct_change ~ Depth + condition + condition:Depth, data = graph_pct_change[graph_pct_change$Species == "Porites cylindrica", ])
#lm_verweyi <- lm(pct_change ~ Depth + condition + condition:Depth , data = graph_pct_change[graph_pct_change$Species == "Acropora verweyi", ]) #Not significant

#Look at the statistics 
summary(lm_formosa)
summary(lm_verweyi)
summary(lm_pocillopora)
summary(lm_porites)

anova(lm_formosa)
anova(lm_verweyi)
anova(lm_pocillopora)
anova(lm_porites)

#GRAPH
a <- ggplot(lm_formosa, aes(x = Depth, y = pct_change, color = condition, group = condition)) +
  geom_point() + scale_x_continuous(name = "Depth", limits = c(1, 10), breaks = c(1:10)) + 
  scale_y_continuous(name = "% change in live coral tissue", limits = c(-100, 610)) +
  stat_smooth(aes(group=condition), method="lm", se = F, na.rm = T)
b <- ggplot(lm_verweyi, aes(x = Depth, y = pct_change, color = condition, group = condition)) +
  geom_point() + scale_x_continuous(name = "Depth", limits = c(1, 10), breaks = c(1:10)) + 
  scale_y_continuous(name = "% change in live coral tissue", limits = c(-100, 610)) +
  stat_smooth(aes(group=condition), method="lm", formula = y ~ x + I(x^2), se = F, na.rm = T)
c <- ggplot(lm_pocillopora, aes(x = Depth, y = pct_change, color = condition, group = condition)) +
  geom_point() + scale_x_continuous(name = "Depth", limits = c(1, 10), breaks = c(1:10)) + 
  scale_y_continuous(name = "% change in live coral tissue", limits = c(-100, 610)) +
  stat_smooth(aes(group=condition), method="lm", se = F, na.rm = T)
d <- ggplot(lm_porites, aes(x = Depth, y = pct_change, color = condition, group = condition)) +
  geom_point() + scale_x_continuous(name = "Depth", limits = c(1, 10), breaks = c(1:10)) + 
  scale_y_continuous(name = "% change in live coral tissue", limits = c(-100, 610)) +
  stat_smooth(aes(group=condition), method="lm", se = F, na.rm = T)

#Joinin all species in one plot
figure <- ggarrange(a+ rremove("ylab") + rremove("xlab"), b+ rremove("ylab") + rremove("xlab"), c+ rremove("ylab") + rremove("xlab"), d+ rremove("ylab") + rremove("xlab"), common.legend = TRUE, legend = "right", labels = c("A", "B", "C", "D"), hjust = -4,
                    vjust = 1.5)
figure
annotate_figure(figure, top = text_grob("Regression analyses of % change in live coral tissue"), left = text_grob("% change in live coral tissue", rot = 90, vjust = 1), bottom = text_grob("Depth"))



##__Linear mixed models__##

install.packages("lme4") # for linear mixed models
library(lme4)
install.packages("afex") # to get p values from a linear mixed output
library(afex)

#Adding Depth squared (for a parabola)
graph_SGR$Depth_sq <- graph_SGR$Depth^2

#Adding the column nursery (for the random effects of nursery)
Nursery <- rep(1:10, each=2, times = 4)
graph_SGR$Nursery <- Nursery

#Linear mixed models
lmix_formosa <- lmer(SGR ~ condition + Depth + condition:Depth + (1|Nursery), data=graph_SGR[graph_SGR$Species == "Acropora formosa", ])
lmix_verweyi <- lmer(SGR ~ condition + Depth + condition:Depth + (1|Nursery), data=graph_SGR[graph_SGR$Species == "Acropora verweyi", ])
lmix_pocillopora <- lmer(SGR ~ condition + Depth + condition:Depth + (1|Nursery), data=graph_SGR[graph_SGR$Species == "Pocillopora verrucosa",])
lmix_porites <- lmer(SGR ~ condition + Depth + condition:Depth + (1|Nursery), data=graph_SGR[graph_SGR$Species == "Acropora formosa", ])
#output
summary(lmix_formosa)
summary(lmix_verweyi)
summary(lmix_pocillopora)
summary(lmix_porites)

#To compare which model is better (the one with lower AIC)
AIC(logLik(lm_formosa))
AIC(logLik(lm_verweyi))
AIC(logLik(lm_pocillopora))
AIC(logLik(lm_porites))
AIC(logLik(lmix_formosa))
AIC(logLik(lmix_verweyi))
AIC(logLik(lmix_pocillopora))
AIC(logLik(lmix_porites))



#POTTING REGRESSION (with a std error band)

lm_formosa %>%
  augment(newdata = data.frame(condition = "non_bleaching",
                               Depth = 1:20), se_fit = TRUE) -> fit.w
lm_formosa %>%
  augment(newdata = data.frame(condition = "bleaching",
                               Depth = 1:20), se_fit = TRUE) -> fit.m
fit.df <- full_join(fit.w, fit.m)  
fit.df <- fit.df %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit) 

ggplot(fit.df, aes(Depth, .fitted, color = condition)) + geom_point(data = regression_formosa, aes(x = Depth, y = SGR, color = condition)) +
  geom_line(size=1.5) + geom_ribbon(aes(ymin = lower, ymax = upper, fill = condition), alpha = .1) +
  scale_x_continuous(name = "Depth", limits = c(1, 10), breaks = c(1:10)) + 
  scale_y_continuous(name = "SGR", limits = c(-0.005, 0.025), breaks = c(0.00, 0.005, 0.01, 0.015, 0.02, 0.025)) +
  ggtitle("Acropora Formosa SGR regression lines") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Condition", labels = c("Non-bleaching", "Bleaching")) + scale_color_discrete(name = "Condition", labels = c("Non-bleaching", "Bleaching")) +
  theme(legend.title = element_text(face = "bold")) + theme(text=element_text(size = 14)) 













