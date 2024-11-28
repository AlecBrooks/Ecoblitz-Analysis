############################################################
### Ecoblitz Flower data 
#############################################################

library(readr)
Flower<- read_csv("~/Desktop/Ecoblitz project/Clean_Flr_Data_Edit .csv")
str(Flower)
library(dplyr)

table(Flower$Year ,Flower$Status)
help(table)

Flower_SP21 <- Flower[Flower$Year ==2021 & Flower$season == "Spring",]
str(Flower_SP21)
Flower_SP22 <- Flower[Flower$Year ==2022 & Flower$season == "Spring",]
str(Flower_SP22)
Flower_SP23 <- Flower[Flower$Year ==2023 & Flower$season == "Spring",]
str(Flower_SP23)

Flower_FA21 <- Flower[Flower$Year ==2021 & Flower$season == "Fall",]
str(Flower_SP21)
Flower_FA22 <- Flower[Flower$Year ==2022 & Flower$season == "Fall",]
str(Flower_SP22)
Flower_FA23 <- Flower[Flower$Year ==2023 & Flower$season == "Fall",]
str(Flower_SP23)

Flower_Spring <- Flower[Flower$season == "Spring",]
Flower_Fall <- Flower[Flower$season == "Fall",]


par(mfrow = c(1, 3))
barplot(prop.table(table(Flower_SP21$Status)))
barplot(prop.table(table(Flower_SP22$Status)))
barplot(prop.table(table(Flower_SP23$Status)))

barplot(prop.table(table(Flower_FA21$Status)))
barplot(prop.table(table(Flower_FA22$Status)))
barplot(prop.table(table(Flower_FA23$Status)))

prop.table(table(Flower_SP21$Status))
prop.table(table(Flower_SP22$Status))
prop.table(table(Flower_SP23$Status))
par(mfrow = c(1, 1))

############# Spring ANOVA ########################
anova_spring_flower <- aov(`Only Buds` ~ factor(Year), data = Flower_Spring)
summary(anova_spring_flower)  ## 6.99e-05
tukey_result_flower_spring <- TukeyHSD(anova_spring_flower)
tukey_result_flower_spring  
## Only Buds = everything is different, especially 2021-2023 

anova_spring_flower_early <- aov(Early ~ factor(Year), data = Flower_Spring )
summary(anova_spring_flower_early)  ## p =  3.29e-05
tukey_result_flower_spring_early <- TukeyHSD(anova_spring_flower_early)
tukey_result_flower_spring_early ## Early = 2023 different  

anova_spring_flower_peak <- aov(Peak ~ factor(Year), data = Flower_Spring)
summary(anova_spring_flower_peak)  ## p = 0.233
tukey_result_flower_spring_peak <- TukeyHSD(anova_spring_flower_peak)
tukey_result_flower_spring_peak ## Peak = all similar 

anova__spring_late <- aov(Late ~ factor(Year), data = Flower_Spring)
summary(anova_spring_flower_late)  ## p = 0.0117
tukey_result_flower_spring_late <- TukeyHSD(anova_spring_flower_late)
tukey_result_flower_spring_late ## Late = 2021 is different   

anova_spring_flower_fruits <- aov(`Only Fruits` ~ factor(Year), data = Flower_Spring)
summary(anova_spring_flower_fruits)  ## p = 0.657
tukey_result_flower_spring_fruits <- TukeyHSD(anova_spring_flower_fruits)
tukey_result_flower_spring_fruits ## Only fruits = all similar.  

######## In spring, 2021-Late and 2023-Early were different. 



###### Fall ANOVA #######

anova_fall_flower <- aov(`Only Buds` ~ factor(Year), data = Flower_Fall)
summary(anova_fall_flower)  ## p = 0.198
tukey_result_flower_fall <- TukeyHSD(anova_fall_flower)
tukey_result_flower_fall ## Only Buds = all similar 

anova_fall_flower_early <- aov(Early ~ factor(Year), data = Flower_Fall)
summary(anova_fall_flower_early)  ## p = 0.0995
tukey_result_flower_fall_early <- TukeyHSD(anova_fall_flower_early)
tukey_result_flower_fall_early ## Early = all similar 

anova_fall_flower_peak <- aov(Peak ~ factor(Year), data = Flower_Fall)
summary(anova_fall_flower_peak)  ## p = 0.813
tukey_result_flower_fall_peak <- TukeyHSD(anova_fall_flower_peak)
tukey_result_flower_fall_peak ## Peak = all similar 

anova_fall_flower_late <- aov(Late ~ factor(Year), data = Flower_Fall)
summary(anova_fall_flower_late)  ## p = 0.372
tukey_result_flower_fall_late <- TukeyHSD(anova_fall_flower_late)
tukey_result_flower_fall_late ## Late = all similar   

anova_fall_flower_fruits <- aov(`Only Fruits` ~ factor(Year), data = Flower_Fall)
summary(anova_fall_flower_fruits)  ## p = 0.245
tukey_result_flower_fall_fruits <- TukeyHSD(anova_fall_flower_fruits)
tukey_result_flower_fall_fruits ## Only fruits = all similar.  


###### Spring varies, but cannot specify the year ########
###### Fall are all similar #######################
