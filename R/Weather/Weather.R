library(readr)
library(tidyr)

W <- read_csv("New Weather Data - Winter_Data_Final.csv")
W$DATE <- as.Date(W$DATE, format = "%m/%d/%Y")

#missing Dates
#2023-02-04
#2023-02-10
#2023-02-14
#2023-02-15

#yearly Time series
Year_2021 <- subset(W, W$CYCLE == 2021)
Year_2021$CYCLE_DATE <- 0
Year_2021$CYCLE_DATE[Year_2021$MONTH == "Dec"] <- as.Date(paste("2000", format(Year_2021$DATE[Year_2021$MONTH == "Dec"], "%m-%d"), sep = "-"))
Year_2021$CYCLE_DATE[Year_2021$MONTH != "Dec"] <- as.Date(paste("2001", format(Year_2021$DATE[Year_2021$MONTH != "Dec"], "%m-%d"), sep = "-"))
Year_2021$CYCLE_DATE <- as.Date(Year_2021$CYCLE_DATE, origin = "1970-01-01")

Year_2022 <- subset(W, W$CYCLE == 2022)
Year_2022$CYCLE_DATE <- 0
Year_2022$CYCLE_DATE[Year_2022$MONTH == "Dec"] <- as.Date(paste("2000", format(Year_2022$DATE[Year_2022$MONTH == "Dec"], "%m-%d"), sep = "-"))
Year_2022$CYCLE_DATE[Year_2022$MONTH != "Dec"] <- as.Date(paste("2001", format(Year_2022$DATE[Year_2022$MONTH != "Dec"], "%m-%d"), sep = "-"))
Year_2022$CYCLE_DATE <- as.Date(Year_2022$CYCLE_DATE, origin = "1970-01-01")

Year_2023 <- subset(W, W$CYCLE == 2023)
Year_2023$CYCLE_DATE <- 0
Year_2023$CYCLE_DATE[Year_2023$MONTH == "Dec"] <- as.Date(paste("2000", format(Year_2023$DATE[Year_2023$MONTH == "Dec"], "%m-%d"), sep = "-"))
Year_2023$CYCLE_DATE[Year_2023$MONTH != "Dec"] <- as.Date(paste("2001", format(Year_2023$DATE[Year_2023$MONTH != "Dec"], "%m-%d"), sep = "-"))
Year_2023$CYCLE_DATE <- as.Date(Year_2023$CYCLE_DATE, origin = "1970-01-01")

Year_2024 <- subset(W, W$CYCLE == 2024)
Year_2024$CYCLE_DATE <- 0
Year_2024$CYCLE_DATE[Year_2024$MONTH == "Dec"] <- as.Date(paste("2000", format(Year_2024$DATE[Year_2024$MONTH == "Dec"], "%m-%d"), sep = "-"))
Year_2024$CYCLE_DATE[Year_2024$MONTH != "Dec"] <- as.Date(paste("2001", format(Year_2024$DATE[Year_2024$MONTH != "Dec"], "%m-%d"), sep = "-"))
Year_2024$CYCLE_DATE <- as.Date(Year_2024$CYCLE_DATE, origin = "1970-01-01")

#############################################################################################
########### CHARTS #rm(Year_2021, Year_2022, Year_2023, Year_2024, aggregated_data, data_matrix, reshaped_data, FocusDate)
#############################################################################################

#Cycle Time series Snow
plot(Year_2021$CYCLE_DATE[-1], Year_2021$SNOW_WC[-1], type = "l", 
     xlab = "Date (Aligned)", ylab = "Snow Water Content (SNOW_WC)", 
     main = "Snow Water Content Over Time for All Cycles", col = "blue", lwd = 2,
     ylim = c(min(Year_2024$SNOW_WC), max(Year_2023$SNOW_WC)))
lines(Year_2022$CYCLE_DATE, Year_2022$SNOW_WC, col = "red", lwd = 2)
lines(Year_2023$CYCLE_DATE, Year_2023$SNOW_WC, col = "green", lwd = 2)
lines(Year_2024$CYCLE_DATE, Year_2024$SNOW_WC, col = "purple", lwd = 2)
legend("topleft", legend = c("2021", "2022", "2023", "2024"), 
       col = c("blue", "red", "green", "purple"), lwd = 2, bty = "n")
FocusDate <- as.Date("2001-02-05", format = "%Y-%m-%d")
points(FocusDate, 4, col = "red", pch = 19)
text(FocusDate, 4, labels = "No Data", pos = 4, col = "red")

# Temperature and Snow Water Content
plot(Year_2021$CYCLE_DATE[-1], scale(Year_2021$TEMP_AV[-1]), type = "l", 
     col = "orange", lty = 2, lwd = 2, 
     xlab = "Date", ylab = "Scaled Values", 
     main = "Average Temperature and Snow Water Content Over Time (2021)")
lines(Year_2021$CYCLE_DATE[-1], scale(Year_2021$SNOW_WC[-1]), col = "blue", lwd = 2)
legend("topright", legend = c("Average Temperature (TEMP_AV)", "Snow Water Content (SNOW_WC)"), 
       col = c("orange", "blue"), lty = c(2, 1), lwd = 2, bty = "n")

#Full Time series
plot(W$DATE, W$SNOW_WC, type = "l", 
     xlab = "", ylab = "Snow Water Content over range (SNOW_WC)", main = "Snow Water Content Over Time", col = "blue")
FocusDate <- as.Date("2023-02-04", format = "%Y-%m-%d")
points(FocusDate, 4, col = "red", pch = 19)
text(FocusDate, 4, labels = "No Data", pos = 4, col = "red")

# Seasonal Trends
aggregated_data <- aggregate(SNOW_WC ~ MONTH + CYCLE, data = W, FUN = mean, na.rm = TRUE)
reshaped_data <- reshape(aggregated_data, timevar = "CYCLE", idvar = "MONTH", direction = "wide")
rownames(reshaped_data) <- reshaped_data$MONTH
reshaped_data <- reshaped_data[, -1]
data_matrix <- t(reshaped_data)
barplot(as.matrix(data_matrix), beside = TRUE, col = rainbow(ncol(data_matrix)), legend.text = c("2021", "2022", "2023", "2024"), args.legend = list(title = "Cycle", x = "topleft"),
  main = "Grouped Bar Chart of SNOW_WC by Month and Cycle", xlab = "Month", ylab = "Snow Water Content (SNOW_WC)", names.arg = colnames(data_matrix))

#Temperature vs. Snow Water Content
plot(W$TEMP_AV, W$SNOW_WC)
hist(W$TEMP_AV)
hist(W$SNOW_WC)

plot(Year_2021$TEMP_AV, Year_2021$SNOW_WC)
hist(Year_2021$TEMP_AV)
hist(Year_2021$SNOW_WC)

summary(lm(W$SNOW_WC ~ W$TEMP_AV))
summary(lm(Year_2021$SNOW_WC ~ Year_2021$TEMP_AV))
summary(lm(Year_2022$SNOW_WC ~ Year_2022$TEMP_AV))
summary(lm(Year_2023$SNOW_WC ~ Year_2023$TEMP_AV))
summary(lm(Year_2024$SNOW_WC ~ Year_2024$TEMP_AV))

#rm(Year_2021, Year_2022, Year_2023, Year_2024, aggregated_data, data_matrix, reshaped_data, FocusDate)

hist(Year_2021$SNOW_WC)
hist(Year_2022$SNOW_WC)
hist(Year_2023$SNOW_WC)
hist(Year_2024$SNOW_WC)

#anova

x <- aov(TEMP_AV ~ factor(CYCLE), data = W)

summary(x)
TukeyHSD(x)

W$SNOW_WC

x <- aov(SNOW_WC ~ factor(CYCLE), data = W)

summary(x)
TukeyHSD(x)

#conf in
#compare avgs
#week av for time s of temp

#need poster done by dec 9th
#remove veg from poster
#update method to include the new weather day
#findings need to be updated 1 for flower one for weather

#fall spring weekly ave temp
#spring: march 10 - May 10
#fall: aug 10 - oct 10

#---------------------------------



#Cycle Time series Snow
plot(Year_2021$CYCLE_DATE[-1], Year_2021$TEMP_AV[-1], type = "l", 
     xlab = "Date (Aligned)", ylab = "Temp(TEMP_AV)", 
     main = "TEMP_AV", col = "blue", lwd = 2,
     ylim = c(min(Year_2024$TEMP_AV), max(Year_2023$TEMP_AV)))
lines(Year_2022$CYCLE_DATE, Year_2022$TEMP_AV, col = "red", lwd = 2)
lines(Year_2023$CYCLE_DATE, Year_2023$TEMP_AV, col = "green", lwd = 2)
lines(Year_2024$CYCLE_DATE, Year_2024$TEMP_AV, col = "purple", lwd = 2)
legend("topleft", legend = c("2021", "2022", "2023", "2024"), 
       col = c("blue", "red", "green", "purple"), lwd = 2, bty = "n")
FocusDate <- as.Date("2001-02-05", format = "%Y-%m-%d")
points(FocusDate, 4, col = "red", pch = 19)
text(FocusDate, 4, labels = "No Data", pos = 4, col = "red")
