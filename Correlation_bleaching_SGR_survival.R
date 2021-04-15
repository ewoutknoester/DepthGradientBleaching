
###___Correlation analyses___###

#Calculating the AVERAGE bleaching intensity per depth from THE FIRST MEASUREMENT TO the MAX bleaching point 

#1.Pocillopora
Pocillopora_average_start_max <- Pocillopora %>% 
  group_by(Depth_bl) %>% 
  slice(seq_len(min(which.max(Average_G)))) %>% 
  summarise(Average_G = mean(Average_G, na.rm = TRUE))

#2.Porites
Porites_average_start_max <- Porites %>% 
  group_by(Depth_bl) %>% 
  slice(seq_len(min(which.max(Average_G)))) %>% 
  summarise(Average_G = mean(Average_G, na.rm = TRUE))

#3.Verweyi
Verweyi_average_start_max <- Verweyi %>% 
  group_by(Depth_bl) %>% 
  slice(seq_len(min(which.max(Average_G)))) %>% 
  summarise(Average_G = mean(Average_G, na.rm = TRUE))


#Calculating correlations

install.packages("Hmisc")
library(Hmisc)
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

#1.Pocilopora
Pocillopora_correlation <- cbind(Pocillopora_average_start_max, average_survival_per_depth$bleaching[21:30])
Pocillopora_correlation <- cbind(Pocillopora_correlation, average_growth_rate_per_depth_bleach$bleaching[21:30])
Pocillopora_correlation <- Pocillopora_correlation %>% 
  rename(Colour_intensity = Average_G, Survival = `average_survival_per_depth$bleaching[21:30]`, SGR = `average_growth_rate_per_depth_bleach$bleaching[21:30]`)

coef_pocillopora <- rcorr(as.matrix(Pocillopora_correlation), type = c("spearman"))
coef_pocillopora

chart.Correlation(Pocillopora_correlation[,c(2, 3, 4)], histogram=F, pch=19, method = c("spearman"))


#2.Porites
Porites_correlation <- cbind(Porites_average_start_max, average_survival_per_depth$bleaching[Species == "Porites cylindrica"])
Porites_correlation <- cbind(Porites_correlation, average_growth_rate_per_depth_bleach$bleaching[Species == "Porites cylindrica"])
Porites_correlation <- Porites_correlation %>% 
  rename(Colour_intensity = Average_G, Survival = `average_survival_per_depth$bleaching[Species == "Porites cylindrica"]`, SGR = `average_growth_rate_per_depth_bleach$bleaching[Species == "Porites cylindrica"]`)

coef_porites <- rcorr(as.matrix(Porites_correlation), type = c("spearman"))
coef_porites

chart.Correlation(Porites_correlation[,c(2, 3, 4)], histogram=F, pch=19, method = c("spearman"))


#3.Verweyi
Verweyi_correlation <- cbind(Verweyi_average_start_max, average_survival_per_depth$bleaching[Species == "Acropora verweyi"])
Verweyi_correlation <- cbind(Verweyi_correlation, average_growth_rate_per_depth_bleach$bleaching[Species == "Acropora verweyi"])
Verweyi_correlation <- Verweyi_correlation %>% 
  rename(Colour_intensity = Average_G, Survival = `average_survival_per_depth$bleaching[Species == "Acropora verweyi"]`, SGR = `average_growth_rate_per_depth_bleach$bleaching[Species == "Acropora verweyi"]`)

coef_verweyi <- rcorr(as.matrix(Verweyi_correlation), type = c("spearman"))
coef_verweyi

chart.Correlation(Verweyi_correlation[,c(2, 3, 4)], histogram=F, pch=19, method = c("spearman"))



