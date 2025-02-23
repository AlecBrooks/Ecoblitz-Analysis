############################################################
### Ecoblitz Flower data 
#############################################################

library(readr)
library(dplyr)
Flower<- read_csv("Pre-Trip/FlowerData_2025222.csv")
str(Flower)

table(Flower$YEAR ,Flower$STATUS)

Flower_SP21 <- Flower[Flower$YEAR ==2021 & Flower$SEASON == "Spring",]
str(Flower_SP21)
Flower_SP22 <- Flower[Flower$YEAR ==2022 & Flower$SEASON == "Spring",]
str(Flower_SP22)
Flower_SP23 <- Flower[Flower$YEAR ==2023 & Flower$SEASON == "Spring",]
str(Flower_SP23)

Flower_FA21 <- Flower[Flower$YEAR ==2021 & Flower$SEASON == "Fall",]
str(Flower_SP21)
Flower_FA22 <- Flower[Flower$YEAR ==2022 & Flower$SEASON == "Fall",]
str(Flower_SP22)
Flower_FA23 <- Flower[Flower$YEAR ==2023 & Flower$SEASON == "Fall",]
str(Flower_SP23)

Flower_Spring <- Flower[Flower$SEASON == "Spring",]
Flower_Fall <- Flower[Flower$SEASON == "Fall",]


par(mfrow = c(1, 3))
barplot(prop.table(table(Flower_SP21$STATUS)))
barplot(prop.table(table(Flower_SP22$STATUS)))
barplot(prop.table(table(Flower_SP23$STATUS)))

barplot(prop.table(table(Flower_FA21$STATUS)))
barplot(prop.table(table(Flower_FA22$STATUS)))
barplot(prop.table(table(Flower_FA23$STATUS)))

prop.table(table(Flower_SP21$STATUS))
prop.table(table(Flower_SP22$STATUS))
prop.table(table(Flower_SP23$STATUS))
par(mfrow = c(1, 1))

############# Spring ANOVA ########################
anova_spring_flower <- aov(`ONLY_BUDS` ~ factor(YEAR), data = Flower_Spring)
summary(anova_spring_flower)  ## 6.99e-05
tukey_result_flower_spring <- TukeyHSD(anova_spring_flower)
tukey_result_flower_spring  
## ONLY_BUDS = everything is different, especially 2021-2023 

anova_spring_flower_early <- aov(EARLY ~ factor(YEAR), data = Flower_Spring )
summary(anova_spring_flower_early)  ## p =  3.29e-05
tukey_result_flower_spring_early <- TukeyHSD(anova_spring_flower_early)
tukey_result_flower_spring_early ## EARLY = 2023 different  

anova_spring_flower_peak <- aov(PEAK ~ factor(YEAR), data = Flower_Spring)
summary(anova_spring_flower_peak)  ## p = 0.233
tukey_result_flower_spring_peak <- TukeyHSD(anova_spring_flower_peak)
tukey_result_flower_spring_peak ## PEAK = all similar 

anova__spring_late <- aov(LATE ~ factor(YEAR), data = Flower_Spring)
summary(anova_spring_flower_late)  ## p = 0.0117
tukey_result_flower_spring_late <- TukeyHSD(anova_spring_flower_late)
tukey_result_flower_spring_late ## LATE = 2021 is different   

anova_spring_flower_fruits <- aov(`ONLY_FRUITS` ~ factor(YEAR), data = Flower_Spring)
summary(anova_spring_flower_fruits)  ## p = 0.657
tukey_result_flower_spring_fruits <- TukeyHSD(anova_spring_flower_fruits)
tukey_result_flower_spring_fruits ## ONLY_FRUITS = all similar.  

######## In spring, 2021-Late and 2023-Early were different. 



###### Fall ANOVA #######

anova_fall_flower <- aov(`ONLY_BUDS` ~ factor(YEAR), data = Flower_Fall)
summary(anova_fall_flower)  ## p = 0.198
tukey_result_flower_fall <- TukeyHSD(anova_fall_flower)
tukey_result_flower_fall ## ONLY_BUDS = all similar 

anova_fall_flower_early <- aov(EARLY ~ factor(YEAR), data = Flower_Fall)
summary(anova_fall_flower_early)  ## p = 0.0995
tukey_result_flower_fall_early <- TukeyHSD(anova_fall_flower_early)
tukey_result_flower_fall_early ## EARLY = all similar 

anova_fall_flower_peak <- aov(PEAK ~ factor(YEAR), data = Flower_Fall)
summary(anova_fall_flower_peak)  ## p = 0.813
tukey_result_flower_fall_peak <- TukeyHSD(anova_fall_flower_peak)
tukey_result_flower_fall_peak ## PEAK = all similar 

anova_fall_flower_late <- aov(LATE ~ factor(YEAR), data = Flower_Fall)
summary(anova_fall_flower_late)  ## p = 0.372
tukey_result_flower_fall_late <- TukeyHSD(anova_fall_flower_late)
tukey_result_flower_fall_late ## LATE = all similar   

anova_fall_flower_fruits <- aov(`ONLY_FRUITS` ~ factor(YEAR), data = Flower_Fall)
summary(anova_fall_flower_fruits)  ## p = 0.245
tukey_result_flower_fall_fruits <- TukeyHSD(anova_fall_flower_fruits)
tukey_result_flower_fall_fruits ## ONLY_FRUITS = all similar.  


###### Spring varies, but cannot specify the YEAR ########
###### Fall are all similar #######################
