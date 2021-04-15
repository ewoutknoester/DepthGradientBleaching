
##__ONE YEAR PERIOD__##


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

#Removing one remaining value for Porites cylindrica
average_growth_rate_per_depth_year <- average_growth_rate_per_depth_year[-31, ]


#__PLOTING SGR__#

#Creating a dataframe for the graph
Depth_3 <- rep(1:10, times = 3)
Species_3 <- Species <- rep(c("Acropora formosa", "Acropora verweyi", "Pocillopora verrucosa"), each=10)
average_growth_rate_per_depth_year$Depth <- Depth_3
average_growth_rate_per_depth_year$Species <- Species_3
average_growth_rate_per_depth_year$Species <- as.factor(average_growth_rate_per_depth_year$Species)

install.packages("ggrepel") # For the number of fragments next to points
library(ggrepel)

# GRAPH -> SGR over after one year
ggplot(average_growth_rate_per_depth_year, aes(x = Depth, y = year_SGR)) + 
  geom_point(size=1.8, color="chartreuse3") + geom_line(size = 0.5, color="chartreuse3") + facet_wrap(~Species, scales = "free_x", nrow = 2) + 
  ggtitle("SGR of four coral species over one year") +  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "SGR (1/d)", breaks = c(-0.0025, 0, 0.0025, 0.005, 0.0075, 0.010, 0.0125, 0.015, 0.0175)) +
  scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + theme(text=element_text(size = 13)) + 
  geom_text_repel(aes(label = Freq_year), data = average_growth_rate_per_depth_year, size = 4, nudge_y = 0.0003, nudge_x = 0.2, point.size = NA, min.segment.length = Inf, color="darkgreen") +
  geom_hline(yintercept = 0, color = "red", alpha = .3, size = .8, linetype = "dashed")




###___CALCULATING SURVIVAL RATE___###

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
  geom_point(size=1.8, color="chartreuse3") + geom_line(size = 0.5, color="chartreuse3") + facet_wrap(~Species, scales = "free_x") + 
  ggtitle("Survival rate of four species after one year") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + theme(text=element_text(size = 13)) +
  scale_y_continuous(name = "Survival rate (%)")



##__CALCULATING % CHANGE IN LIVE TISSUE__##


#Calculating % change for individual fragments
growth_data$pct_year <- ((growth_data$EV_cm3_3 - growth_data$EV_cm3_0)/growth_data$EV_cm3_0)*100

#Average across depth
Average_pct_year <- growth_data %>%  
  group_by(Depth) %>% 
  summarise(pct = mean(pct_year, na.rm = TRUE))


##__PLOTTING % CHANGE__##


#Creating a data frame for the graph
Depth <- rep(1:10, times = 4)
Average_pct_year$Depth <- Depth
Species <- rep(c("Acropora formosa", "Acropora verweyi", "Pocillopora verrucosa", "Porites cylindrica"), each=10)
Average_pct_year$Species <- Species

#Graph
ggplot(Average_pct_year, aes(x = Depth, y = pct)) +
  geom_point(size=1.8, color="chartreuse3") + geom_line(size = 0.5, color="chartreuse3") + facet_wrap(~Species, scales = "free") + 
  ggtitle("% change in live coral tissue for four species after one year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "Depth (m)", breaks = c(1:10)) + 
  scale_y_continuous(name = "Change in live coral tissue (%)") +
  geom_hline(yintercept = 0, color = "red", alpha = .3, size = .8, linetype = "dashed") 


##__REGRESSION for % change for every species individually (over one year)__## -> none of the models are significant

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
