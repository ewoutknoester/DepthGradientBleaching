
###__Checking if Fragments  are the same size at all 10 depths for each species__###

#Checking the distribution of the data
hist(growth_data$EV_cm3_0)

install.packages("ggpubr")
library("ggpubr")
install.packages("dunn.test")
library(dunn.test)
install.packages("FSA")
library(FSA)

#1. Acropora formosa

formosa_EV <- growth_data %>% 
  filter(Species == "Acropora formosa") 
hist(formosa_EV$EV_cm3_0)  #To see if variable normally distributed

#Boxplot showing fragment sizes at different depths
ggboxplot(formosa_EV, x = "Structure", y = "EV_cm3_0", ylab = "EV (cm^3)" , xlab = "Depth (m)", 
          title = "A. formosa initial fragment size per depth", bxp.errorbar = T,
          fill = "Structure", legend = "right")

#Statistic tests for differences in fragment size between different depths
kruskal.test(EV_cm3_0 ~ Structure, data = formosa_EV)                       #Is fragment size significantly different between depths? If p < 0.05 than yes -> Dunns test
#dunnTest(formosa_EV$EV_cm3_0, formosa_EV$Structure, method="bonferroni")   #Which groups are significantly different? (pairwise comparison)
# no need for Dunns test because Kruskal-W test not significant


#2. Acropora verweyi

verweyi_EV <- growth_data %>% 
  filter(Species == "Acropora verweyi") 
hist(verweyi_EV$EV_cm3_0)

#Boxplot showing fragment sizes at different depths
ggboxplot(verweyi_EV, x = "Structure", y = "EV_cm3_0", ylab = "EV (cm^3)" , xlab = "Depth (m)", 
          title = "A. verweyi initial fragment size per depth", bxp.errorbar = T,
          fill = "Structure", legend = "right")
          
#Statistic tests for differences in fragment size between different depths
kruskal.test(EV_cm3_0 ~ Structure, data = verweyi_EV)
dunnTest(verweyi_EV$EV_cm3_0, verweyi_EV$Structure, method="bonferroni")



#. Pocillopora verrucosa

pocilopora_EV <- growth_data %>% 
  filter(Species == "Pocillopora verrucosa") 
hist(pocilopora_EV$EV_cm3_0)

#Boxplot showing fragment sizes at different depths
ggboxplot(pocilopora_EV, x = "Structure", y = "EV_cm3_0", ylab = "EV (cm^3)" , xlab = "Depth (m)", 
          title = "P. verrucosa initial fragment size per depth", bxp.errorbar = T,
          fill = "Structure", legend = "right")
          
#Statistic tests for differences in fragment size between different depths
kruskal.test(EV_cm3_0 ~ Structure, data = pocilopora_EV)
dunnTest(pocilopora_EV$EV_cm3_0, pocilopora_EV$Structure, method="bonferroni")



#4. Porites cylindrica

porites_EV <- growth_data %>% 
  filter(Species == "Porites cylindrica") %>% 
  filter(!Structure == "6") # excluding depth 6 to do a kruskal W test
hist(porites_EV$EV_cm3_0)

#Boxplot showing fragment sizes at different depths
ggboxplot(porites_EV, x = "Structure", y = "EV_cm3_0", ylab = "EV (cm^3)" , xlab = "Depth (m)", 
          title = "P. cylindrica initial fragment size per depth", bxp.errorbar = T,
          fill = "Structure", legend = "right") 
         
#Statistic tests for differences in fragment size between different depths
kruskal.test(EV_cm3_0 ~ Structure, data = porites_EV)
#dunnTest(porites_EV$EV_cm3_0, porites_EV$Structure, method="bonferroni")  no need for this test because previous not significant



#Calculating the MEAN and SD for EV0 for every species 

group_by(growth_data, Species) %>%
  summarise(
    count = n(),
    mean = mean(EV_cm3_0, na.rm = TRUE),
    sd = sd(EV_cm3_0, na.rm = TRUE))
