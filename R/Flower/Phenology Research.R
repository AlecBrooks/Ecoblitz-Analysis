install.packages("readxl")
library(readxl)
Cleaned_Flower_Data <- read_excel("Cleaned_Flower_Data.xlsx")
View(Cleaned_Flower_Data)
install.packages("dplyr")
library("dplyr")
install.packages("plotrix")
library("plotrix")

summary(Cleaned_Flower_Data)
summary(Cleaned_Flower_Data$Date)
head(Cleaned_Flower_Data)
#These statements were used to filter the data sets into there own brand new ones based upon season and year.
Cleaned_Flower_Data$Date <- as.character(Cleaned_Flower_Data$Date)
# %>% is what's called a pipe within the dplyr library and is typically used to create a new
Fall_2021 <- Cleaned_Flower_Data %>%
  filter(!is.na(Date) & Date == "Fall 2021")
Fall_2022 <- Cleaned_Flower_Data %>%
  filter(!is.na(Date) & Date == "Fall 2022")
Fall_2023 <- Cleaned_Flower_Data %>%
  filter(!is.na(Date) & Date == "Fall 2023")
Spring_2021 <- Cleaned_Flower_Data %>%
  filter(!is.na(Date) & Date == "Spring 2021")
Spring_2022 <- Cleaned_Flower_Data %>%
  filter(!is.na(Date) & Date == "Spring 2022")
Spring_2023 <- Cleaned_Flower_Data %>%
  filter(!is.na(Date) & Date == "Spring 2023")
#This is the start of the descriptive stats for the season and year.
summary(Fall_2021)
summary(Fall_2022)
summary(Fall_2023)
summary(Spring_2021)
summary(Spring_2022)
summary(Spring_2023)
#Flowering Phenology column in this data set represents Only Buds in the Data set then the numbers like 7
#Represents Early and so on and so forth this occurs up to number 10.
pie_labels <- c("False","True")
Fall_2021_Only_Buds <- as.double(Fall_2021$`Flowering Phenology`)
prop.table(table(Fall_2021_Only_Buds))
table(Fall_2021_Only_Buds)
pie3D(table(Fall_2021_Only_Buds),labels = pie_labels,main = "Only Buds Fall 2021")
sd(Fall_2021_Only_Buds)
summary(Fall_2021_Only_Buds)
#The statements above got the only buds results for Fall 2021.
#These will carry onward to get the rest of the Fall and Spring Only Buds Data.
Fall_2022_Only_Buds <- as.double(Fall_2022$`Flowering Phenology`)
prop.table(table(Fall_2022_Only_Buds))
table(Fall_2022_Only_Buds)
pie3D(table(Fall_2022_Only_Buds),labels = pie_labels,main = "Only Buds Fall 2022")
sd(Fall_2022_Only_Buds)
summary(Fall_2022_Only_Buds)

Fall_2023_Only_Buds <- as.double(Fall_2023$`Flowering Phenology`)
prop.table(table(Fall_2023_Only_Buds))
table(Fall_2023_Only_Buds)
pie3D(table(Fall_2023_Only_Buds),labels = pie_labels,main = "Only Buds Fall 2023")
sd(Fall_2023_Only_Buds)
summary(Fall_2023_Only_Buds)

Spring_2021_Only_Buds <- as.double(Spring_2021$`Flowering Phenology`)
prop.table(table(Spring_2021_Only_Buds))
table(Spring_2021_Only_Buds)
pie3D(table(Spring_2021_Only_Buds),labels = pie_labels,main = "Only Buds Spring 2021")
sd(Spring_2021_Only_Buds)
summary(Spring_2021_Only_Buds)

Spring_2022_Only_Buds <- as.double(Spring_2022$`Flowering Phenology`)
prop.table(table(Spring_2022_Only_Buds))
table(Spring_2022_Only_Buds)
pie3D(table(Spring_2022_Only_Buds),labels = pie_labels,main = "Only Buds Spring 2022")
sd(Spring_2022_Only_Buds)
summary(Spring_2022_Only_Buds)

Spring_2023_Only_Buds <- as.double(Spring_2023$`Flowering Phenology`)
prop.table(table(Spring_2023_Only_Buds))
table(Spring_2023_Only_Buds)
pie3D(table(Spring_2023_Only_Buds),labels = pie_labels,main = "Only Buds Spring 2023")
sd(Spring_2023_Only_Buds)
summary(Spring_2023_Only_Buds)

#Now this will mark the start of early blooming plants 

Fall_2021_early <- as.double(Fall_2021$...7)
prop.table(table(Fall_2021_early))
pie3D(table(Fall_2021_early),labels = pie_labels,main = "Fall 2021 early blooming plants")
sd(Fall_2021_early)
summary(Fall_2021_early)

Fall_2022_early <- as.double(Fall_2022$...7)
prop.table(table(Fall_2022_early))
pie3D(table(Fall_2022_early),labels = pie_labels,main = "Fall 2022 early blooming plants")
sd(Fall_2022_early)
summary(Fall_2022_early)

Fall_2023_early <- as.double(Fall_2023$...7)
prop.table(table(Fall_2023_early))
pie3D(table(Fall_2023_early),labels = pie_labels,main = "Fall 2023 early blooming plants")
sd(Fall_2023_early)
summary(Fall_2023_early)

Spring_2021_early <- as.double(Spring_2021$...7)
prop.table(table(Spring_2021_early))
pie3D(table(Spring_2021_early),labels = pie_labels,main = "Spring 2021 early blooming plants")
sd(Spring_2021_early)
summary(Spring_2021_early)

Spring_2022_early <- as.double(Spring_2022$...7)
prop.table(table(Spring_2022_early))
pie3D(table(Spring_2022_early),labels = pie_labels,main = "Spring 2022 early blooming plants")
sd(Spring_2022_early)
summary(Spring_2021_early)

Spring_2023_early <- as.double(Spring_2023$...7)
prop.table(table(Spring_2023_early))
pie3D(table(Spring_2023_early),labels = pie_labels,main = "Spring 2023 early blooming plants")
sd(Spring_2023_early)
summary(Spring_2023_early)
#End of the early blooming plants and the start of the peak blooming plants.

Fall_2021_peak <- as.double(Fall_2021$...8)
prop.table(table(Fall_2021_peak))
pie3D(table(Fall_2021_peak),labels = pie_labels,main = "Fall 2021 peak blooming plants")
sd(Fall_2021_peak)
summary(Fall_2021_peak)

Fall_2022_peak <- as.double(Fall_2022$...8)
prop.table(table(Fall_2022_peak))
pie3D(table(Fall_2022_peak),labels = pie_labels,main = "Fall 2022 peak blooming plants")
sd(Fall_2022_peak)
summary(Fall_2022_peak)

Fall_2023_peak <- as.double(Fall_2023$...8)
prop.table(table(Fall_2023_peak))
pie3D(table(Fall_2023_peak),labels = pie_labels,main = "Fall 2023 peak blooming plants")
sd(Fall_2023_peak)
summary(Fall_2023_peak)

Spring_2021_peak <- as.double(Spring_2021$...8)
prop.table(table(Spring_2021_peak))
pie3D(table(Spring_2021_peak),labels = pie_labels,main = "Spring 2021 peak blooming plants")
sd(Spring_2021_peak)
summary(Spring_2021_peak)

Spring_2022_peak <- as.double(Spring_2022$...8)
prop.table(table(Spring_2022_peak))
pie3D(table(Spring_2022_peak),labels = pie_labels,main = "Spring 2022 peak blooming plants")
sd(Spring_2022_peak)
summary(Spring_2022_peak)

Spring_2023_peak <- as.double(Spring_2023$...8)
prop.table(table(Spring_2023_peak))
pie3D(table(Spring_2023_peak),labels = pie_labels,main = "Spring 2023 peak blooming plants")
sd(Spring_2023_peak)
summary(Spring_2023_peak)

print(Fall_2021_Only_Buds)
print(Fall_2021_early)
print(Fall_2021_peak)
#9 is labeled for the Late blooming plants

Fall_2021_Late <-as.double(Fall_2021$...9)
print(Fall_2021_Late)
#..10 is the Only Fruits Column
Fall_2021_Only_Fruits <- as.double(Fall_2021$...10)
print(Fall_2021_Only_Fruits)
