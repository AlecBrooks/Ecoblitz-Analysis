################################################################################
                            ### Weather Analysis ###
################################################################################

                                  ### Setup ###
################################################################################
library(readr)
library(tidyr)
library(here)

---

w <- read_csv(here("Ecoblitz-Analysis", "Data", "Winter_Data_11-28-2024.csv"))
s <- read_csv(here("Ecoblitz-Analysis", "Data", "Spring_Data_11-28-2024.csv"))
f <- read_csv(here("Ecoblitz-Analysis", "Data", "Fall_Data_11-28-2024.csv"))

setwd(here("Ecoblitz-Analysis","R","Weather"))

w$DATE <- as.Date(w$DATE, format = "%m/%d/%Y")
s$DATE <- as.Date(s$DATE, format = "%m/%d/%Y")
f$DATE <- as.Date(f$DATE, format = "%m/%d/%Y")

for (year in 2021:2024) {
  f_year <- subset(f, f$YEAR == year)
  w_year <- subset(w, w$YEAR == year)
  s_year <- subset(s, s$YEAR == year)
  f_year$CYCLE_DATE <- as.Date(paste("2001", format(f_year$DATE, "%m-%d"), 
                                     sep = "-"))
  w_year$CYCLE_DATE <- 0
  s_year$CYCLE_DATE <- as.Date(paste("2001", format(s_year$DATE, "%m-%d"), 
                                     sep = "-"))
  
  w_year$CYCLE_DATE[w_year$MONTH == "Dec"] <- 
    as.Date(paste("2000", format(
      w_year$DATE[w_year$MONTH == "Dec"], "%m-%d"), sep = "-"))
  
  w_year$CYCLE_DATE[w_year$MONTH != "Dec"] <- 
    as.Date(paste("2001", format(
      w_year$DATE[w_year$MONTH != "Dec"], "%m-%d"), sep = "-"))
  
  w_year$CYCLE_DATE <- as.Date(w_year$CYCLE_DATE, origin = "1970-01-01")
  
  w_weekly <- aggregate(cbind(SNOW_WC, TEMP_AV) ~ WEEK, data = w_year, mean)
  w_weekly$CYCLE_WEEK <- c(1:nrow(w_weekly))
  
  s_weekly <- aggregate(cbind(SNOW_WC, TEMP_AV) ~ WEEK, data = s_year, mean)
  s_weekly$CYCLE_WEEK <- c(1:nrow(s_weekly))
  
  f_weekly <- aggregate(cbind(SNOW_WC, TEMP_AV) ~ WEEK, data = f_year, mean)
  f_weekly$CYCLE_WEEK <- c(1:nrow(f_weekly))
  
  assign(paste0("f_", year), f_year)
  assign(paste0("w_", year), w_year)
  assign(paste0("s_", year), s_year)
  assign(paste0("w_weekly_", year), w_weekly)
  assign(paste0("s_weekly_", year), s_weekly)
  assign(paste0("f_weekly_", year), f_weekly)
  rm(w_year,f_year,s_year,year,w_weekly,s_weekly,f_weekly)
}
colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00") 

                             #### Plots ####
################################################################################


################################# WINTER #######################################
#Time Series Overlay of Winter Snow
plot(w_2021$CYCLE_DATE, w_2021$SNOW_WC, type = "l", 
     xlab = "Date (Aligned)", ylab = "Snow Water Content (SNOW_WC)", 
     main = "Winter Snow Water Content 2021 - 2024", col = colors[1], lwd = 2,
     ylim = c(min(w_2024$SNOW_WC), max(w_2023$SNOW_WC)))
lines(w_2022$CYCLE_DATE, w_2022$SNOW_WC, col = colors[2], lwd = 2)
lines(w_2023$CYCLE_DATE, w_2023$SNOW_WC, col = colors[3], lwd = 2)
lines(w_2024$CYCLE_DATE, w_2024$SNOW_WC, col = colors[4], lwd = 2)
legend("topleft", legend = c("2021", "2022", "2023", "2024"), 
       col = colors, lwd = 2, bty = "n")

#Time Series Overlay of Temperature
plot(w_weekly_2021$CYCLE_WEEK, w_weekly_2021$TEMP_AV, type = "l", 
     xlab = "Week", ylab = "Temperature Average (TEMP_AV)", 
     main = "Average Weekly Temperature 2021 - 2024", 
     col = colors[1], lwd = 2,
     ylim = c(min(w_weekly_2023$TEMP_AV), max(w_weekly_2022$TEMP_AV)))
lines(w_weekly_2022$CYCLE_WEEK, w_weekly_2022$TEMP_AV, col = colors[2], lwd = 2)
lines(w_weekly_2023$CYCLE_WEEK, w_weekly_2023$TEMP_AV, col = colors[3], lwd = 2)
lines(w_weekly_2024$CYCLE_WEEK, w_weekly_2024$TEMP_AV, col = colors[4], lwd = 2)
legend("topleft", legend = c("2021", "2022", "2023", "2024"), 
       col = colors, lwd = 2, bty = "n")

# Temperature to Snow Water Content (scaled)
for (year in 2021:2024) {
plot(get(paste0("w_weekly_", year))$CYCLE_WEEK, 
     scale(get(paste0("w_weekly_", year))$TEMP_AV), type = "l", 
     col = colors[1], lwd = 2, 
     xlab = "Week", ylab = "Scaled Values", 
     main = paste0("Average Weekly TEMP_AV and SNOW_WC:", year))
lines(get(paste0("w_weekly_", year))$CYCLE_WEEK, 
      scale(get(paste0("w_weekly_", year))$SNOW_WC), col = colors[4],  
      lty = 2, lwd = 2)
legend("topleft", legend = c("TEMP_AV", "SNOW_WC"), 
       col = c(colors[1], colors[4]), lty = c(1, 2), lwd = 2, bty = "n", 
       cex = 0.8)
}
rm(year)

aggregated_data <- aggregate(SNOW_WC ~ MONTH + YEAR, data = w, 
                             FUN = mean, na.rm = TRUE)
reshaped_data <- reshape(aggregated_data, timevar = "YEAR", 
                         idvar = "MONTH", direction = "wide")
rownames(reshaped_data) <- reshaped_data$MONTH
reshaped_data <- reshaped_data[, -1]
data_matrix <- t(reshaped_data)
barplot(as.matrix(data_matrix), beside = TRUE, col = 
          colors, 
        legend.text = c("2021", "2022", "2023", "2024"), 
        args.legend = list(title = "", x = "topleft"),
        main = "Grouped Bar Chart of SNOW_WC by Month and Cycle", 
        xlab = "Month", ylab = "Snow Water Content (SNOW_WC)", 
        names.arg = colnames(data_matrix))
rm(aggregated_data, data_matrix, reshaped_data)

################################ SPRING ########################################

plot(s_weekly_2021$CYCLE_WEEK, s_weekly_2021$TEMP_AV, type = "l", 
     xlab = "Week", ylab = "Weekly Temperature Averages (TEMP_AV)", 
     main = "Spring Weekly Temperature Averages 2021 - 2024", 
     col = colors[1], lwd = 2,
     ylim = c(min(s_weekly_2023$TEMP_AV), max(s_weekly_2021$TEMP_AV)))
lines(s_weekly_2022$CYCLE_WEEK, s_weekly_2022$TEMP_AV, col = colors[2], lwd = 2)
lines(s_weekly_2023$CYCLE_WEEK, s_weekly_2023$TEMP_AV, col = colors[3], lwd = 2)
lines(s_weekly_2024$CYCLE_WEEK, s_weekly_2024$TEMP_AV, col = colors[4], lwd = 2)
legend("topleft", legend = c("2021", "2022", "2023", "2024"), 
       col = colors, lwd = 2, bty = "n")

################################# FALL #########################################

plot(f_weekly_2021$CYCLE_WEEK, f_weekly_2021$TEMP_AV, type = "l", 
     xlab = "Week", ylab = "Weekly Temperature Averages (TEMP_AV)", 
     main = "Fall Weekly Temperature Averages 2021 - 2024", 
     col = colors[1], lwd = 2,
     ylim = c(min(f_weekly_2021$TEMP_AV), max(f_weekly_2022$TEMP_AV)))
lines(f_weekly_2022$CYCLE_WEEK, f_weekly_2022$TEMP_AV, col = colors[2], lwd = 2)
lines(f_weekly_2023$CYCLE_WEEK, f_weekly_2023$TEMP_AV, col = colors[3], lwd = 2)
lines(f_weekly_2024$CYCLE_WEEK, f_weekly_2024$TEMP_AV, col = colors[4], lwd = 2)
legend("bottomleft", legend = c("2021", "2022", "2023", "2024"), 
       col = colors, lwd = 2, bty = "n")

################################# STATS #########################################

corTable <- data.frame(
  Year = 2021:2024,
  Fall = numeric(length(2021:2024)),
  Winter = numeric(length(2021:2024)),
  Spring = numeric(length(2021:2024)) 
)

for (year in 2021:2024) {
  
  corTable[corTable$Year == year, "Fall"] <-  cor(get(paste0("f_", year))$TEMP_AV, get(paste0("f_", year))$SNOW_WC)
  corTable[corTable$Year == year, "Winter"] <- cor(get(paste0("w_", year))$TEMP_AV, get(paste0("w_", year))$SNOW_WC)
  corTable[corTable$Year == year, "Spring"] <- cor(get(paste0("s_", year))$TEMP_AV, get(paste0("s_", year))$SNOW_WC)
    
}

knitr::kable(corTable, caption = "Correlation Matrix SNOW_WC ~ TEMP_AV")
knitr::kable(TukeyHSD(aov(TEMP_AV ~ factor(YEAR), data = f))[["factor(YEAR)"]], caption = "TukeyHSD Fall (TEMP_AV ~ factor(YEAR))")
knitr::kable(TukeyHSD(aov(TEMP_AV ~ factor(YEAR), data = w))[["factor(YEAR)"]], caption = "TukeyHSD Winter (TEMP_AV ~ factor(YEAR))")
knitr::kable(TukeyHSD(aov(TEMP_AV ~ factor(YEAR), data = s))[["factor(YEAR)"]], caption = "TukeyHSD Spring (TEMP_AV ~ factor(YEAR))")
