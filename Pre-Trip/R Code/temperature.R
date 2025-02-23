########### Reno Weather with big data ################################
library(readr)
library(lubridate)
library(dplyr)

data <- read_csv("WeatherData_20250222.csv")

data$DATE <- as.Date(data$DATE, format="%Y-%m-%d")
### By years
table(data$YEAR)

data_2021_2023 <- data[data$YEAR == "2021"|data$YEAR == "2022"| data$YEAR == "2023" ,]
#data_2021_2023<- data_2021_2023[, -c(1,5,6,7,8,9,10)]


anova_model <- aov(TEMP_AVERAGE ~ YEAR, data = data)
summary(anova_model) ## p> 0.05, p=0.838, fail to reject the null.No significant differnce

qqnorm(residuals(anova_model))
qqline(residuals(anova_model))  # does not look so normal 

#May and Septmber 
# March-May: Spring, July-September: Summer 

### Spring 

Spring2021 <- subset(data, DATE >= as.Date("2021-03-10") & DATE <= as.Date("2021-05-10"))
Spring2022 <- subset(data, DATE >= as.Date("2022-03-10") & DATE <= as.Date("2022-05-10"))
Spring2023 <- subset(data, DATE >= as.Date("2023-03-10") & DATE <= as.Date("2023-05-10"))

Spring <- rbind(Spring2021, Spring2022, Spring2023)
Spring
Spring$YEAR <- year(Spring$DATE)
Spring2021_2023 <- Spring[Spring$YEAR == "2021"|Spring$YEAR == "2022"| Spring$YEAR == "2023" ,]
table(Spring2021_2023$YEAR)

anova_spring_average <- aov(TEMP_AVERAGE ~ factor(YEAR), data = Spring)
summary(anova_spring_average)   #### P= 0.0299, p<0.05 significant

tukey_result_ave_spring <- TukeyHSD(anova_spring_average)
tukey_result_ave_spring  ## 2023 is different 

matplot( Spring2021$DATE,  cbind(Spring2021$TEMP_AVERAGE, 
                                      Spring2022$TEMP_AVERAGE, Spring2023$TEMP_AVERAGE),
         col = c("red", "blue", "brown"), type = "l",
         main = "Average Spring Temp.",
         xlab="date", ylab="Average temp.")
legend(x='topleft', y=70, legend=c('SP21', 'SP22', 'SP23'), fill=c('red', 'blue', 'brown'))



anova_spring_max <- aov(TEMP_MAX ~ factor(YEAR), data = Spring)
summary(anova_spring_max)   ### p = 0.0105, significant 

tukey_result_max_spring <- TukeyHSD(anova_spring_max)
tukey_result_max_spring ## 2023 is different 

matplot( Spring2021$DATE,  cbind(Spring2021$TEMP_MAX, 
                                 Spring2022$TEMP_MAX, Spring2023$TEMP_MAX),
         col = c("red", "blue", "brown"), type = "l",
         main = "Max. Spring Temp.",
         xlab="date", ylab="Max. temp.")

legend(x='topleft', y=80, legend=c('SP21', 'SP22', 'SP23'), fill=c('red', 'blue', 'brown'))



anova_spring_min <- aov(TEMP_MIN ~ factor(YEAR), data = Spring)
summary(anova_spring_min)  ## p= 0.238, NOT significant 

tukey_result_min_spring <- TukeyHSD(anova_spring_min)
tukey_result_min_spring  ## All same/similar 

matplot( Spring2021$DATE,  cbind(Spring2021$TEMP_MIN, 
                                 Spring2022$TEMP_MIN, Spring2023$TEMP_MIN),
         col = c("red", "blue", "brown"), type = "l")

#qqnorm(residuals(anova_spring))
#qqline(residuals(anova_spring)) ## Pretty normal 

## Fall 

Fall2021 <- subset(data, DATE >= as.Date("2021-08-10") & DATE <= as.Date("2021-10-10"))
Fall2022 <- subset(data, DATE >= as.Date("2022-08-10") & DATE <= as.Date("2022-10-10"))
Fall2023 <- subset(data, DATE >= as.Date("2023-08-10") & DATE <= as.Date("2023-10-10"))

Fall <- rbind(Fall2021, Fall2022, Fall2023)
Fall
Fall$YEAR <- year(Fall$DATE)
Fall2021_2023 <- Fall[Fall$YEAR == "2021"|Fall$YEAR == "2022"| Fall$YEAR == "2023" ,]
table(Fall2021_2023$YEAR)

anova_fall <- aov(Fall$TEMP_AVERAGE ~ factor(YEAR), data = Fall)
summary(anova_fall)  # p< 0.05, p= 0.00418, significant 

qqnorm(residuals(anova_fall))
qqline(residuals(anova_fall)) ## Normal 

tukey_result_fall <- TukeyHSD(anova_fall)
tukey_result_fall ## 2022 is different 

## max
anova_fall_max <- aov(Fall$TEMP_MAX ~ factor(YEAR), data = Fall)
summary(anova_fall_max) #p=0.00314 significant

tukey_result_max <- TukeyHSD(anova_fall_max)
tukey_result_max  # 2022 is different 

## min
anova_fall_min <- aov(Fall$TEMP_MIN ~ factor(YEAR), data = Fall)
summary(anova_fall_min) #p=0.00311 significant

tukey_result_min <- TukeyHSD(anova_fall_min)
tukey_result_min  # 2022-2021 is different??

########### Conclusion: Fall 2022 and Spring 2023 were different ##########

#means
mean(Spring$TEMP_AVERAGE[Spring$YEAR == 2021])
mean(Spring$TEMP_AVERAGE[Spring$YEAR == 2022])
mean(Spring$TEMP_AVERAGE[Spring$YEAR == 2023])
### 2023 Spring was cooler -4

mean(Fall$TEMP_AVERAGE[Fall$YEAR == 2021])
mean(Fall$TEMP_AVERAGE[Fall$YEAR == 2022])
mean(Fall$TEMP_AVERAGE[Fall$YEAR == 2023])
### 2022 Fall was warmer +4

### How about Pairwise T test?

#### Spring 
pairwise.t.test(Spring$TEMP_AVERAGE, factor(Spring$YEAR)) 
#P>0.05, but similar result, 2023 is different

pairwise.t.test(Spring$TEMP_MAX , factor(Spring$YEAR)) 
# P<0.05 significant, 2023 is different

pairwise.t.test(Spring$TEMP_MIN , factor(Spring$YEAR)) 
# P>0.05, not significant. All same 

pairwise.t.test(Fall$TEMP_AVERAGE , factor(Fall$YEAR)) 
# 2022 is different

pairwise.t.test(Fall$TEMP_MIN , factor(Fall$YEAR)) 
# 2021-2022 is only different 

pairwise.t.test(Fall$TEMP_MAX , factor(Fall$YEAR)) 
# 2022 is different 

###### Same results from ANOVA

############################################################
###### Ecoblitz day Weather data for 1 week 
###########################################################

### Spring 
Spring2021_week <- subset(data, DATE >= as.Date("2021-05-04") & DATE <= as.Date("2021-05-10"))
Spring2022_week <- subset(data, DATE >= as.Date("2022-05-03") & DATE <= as.Date("2022-05-09"))
Spring2023_week <- subset(data, DATE >= as.Date("2023-05-02") & DATE <= as.Date("2023-05-08"))

Spring_week <- rbind(Spring2021_week, Spring2022_week, Spring2023_week)
Spring_week
Spring_week$YEAR <- year(Spring_week$DATE)
Spring2021_2023_week <- Spring_week[Spring_week$YEAR == "2021"|Spring_week$YEAR == "2022"| Spring_week$YEAR == "2023" ,]
table(Spring2021_2023_week$YEAR)

anova_spring_week_average <- aov(TEMP_AVERAGE ~ factor(YEAR), data = Spring_week)
summary(anova_spring_week_average)   #### P= 0.000484, p<0.05 significant

tukey_result_ave_spring_week <- TukeyHSD(anova_spring_week_average)
tukey_result_ave_spring_week  ## 2021 is different with 90% sig level


matplot( Spring2021_week$DATE,  cbind(Spring2021_week$TEMP_AVERAGE, 
                                      Spring2022_week$TEMP_AVERAGE, Spring2023_week$TEMP_AVERAGE),
         col = c("red", "blue", "brown"), type = "l")
mean(Spring2021_week$TEMP_AVERAGE)
mean(Spring2022_week$TEMP_AVERAGE)
mean(Spring2023_week$TEMP_AVERAGE)


anova_spring_week_max <- aov(TEMP_MAX ~ factor(YEAR), data = Spring_week)
summary(anova_spring_week_max)   ### p = 0.00217, significant 

tukey_result_max_spring_week <- TukeyHSD(anova_spring_week_max)
tukey_result_max_spring_week ## 2021 is different with 90% sig level

matplot( Spring2021_week$DATE,  cbind(Spring2021_week$TEMP_MAX, 
                                      Spring2022_week$TEMP_MAX, Spring2023_week$TEMP_MAX),
         col = c("red", "blue", "brown"), type = "l")

anova_spring_week_min <- aov(TEMP_MIN ~ factor(YEAR), data = Spring_week)
summary(anova_spring_week_min)  ## p= 0.0807, NOT significant 

tukey_result_min_spring_week <- TukeyHSD(anova_spring_week_min)
tukey_result_min_spring_week  ## 2023-2021 is different 

matplot( Spring2021_week$DATE,  cbind(Spring2021_week$TEMP_MIN, 
                                      Spring2022_week$TEMP_MIN, Spring2023_week$TEMP_MIN),
         col = c("red", "blue", "brown"), type = "l")


################# Fall 

Fall2021_week <- subset(data, DATE >= as.Date("2021-08-29") & DATE <= as.Date("2021-09-04"))
Fall2022_week <- subset(data, DATE >= as.Date("2022-09-27") & DATE <= as.Date("2022-10-03"))
Fall2023_week <- subset(data, DATE >= as.Date("2023-09-26") & DATE <= as.Date("2023-10-02"))

Fall_week <- rbind(Fall2021_week, Fall2022_week, Fall2023_week)
Fall_week
Fall_week$YEAR <- year(Fall_week$DATE)
Fall2021_2023_week <- Fall_week[Fall_week$YEAR == "2021"|Fall_week$YEAR == "2022"| Fall_week$YEAR == "2023" ,]
table(Fall2021_2023_week$YEAR)

anova_fall_week <- aov(TEMP_AVERAGE ~ factor(YEAR), data = Fall_week)
summary(anova_fall_week)  # p< 0.05, p= 0.000479, significant 

qqnorm(residuals(anova_fall_week))
qqline(residuals(anova_fall_week)) ## Normal 

tukey_result_fall_week <- TukeyHSD(anova_fall_week)
tukey_result_fall_week ## 2023 is different 

## max
anova_fall_week_max <- aov(TEMP_MAX ~ factor(YEAR), data = Fall_week)
summary(anova_fall_week_max) #p=0.000185 significant

tukey_result_max_week <- TukeyHSD(anova_fall_week_max)
tukey_result_max_week  # 2023 is different 

## min
anova_fall_week_min <- aov(TEMP_MIN ~ factor(YEAR), data = Fall_week)
summary(anova_fall_week_min) #p=0.177 NOT significant

tukey_result_min_week <- TukeyHSD(anova_fall_week_min)
tukey_result_min_week  # All same 

################################################################



