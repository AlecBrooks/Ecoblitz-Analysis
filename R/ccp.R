library(ggplot2)
library(dplyr)
library(readr)
library(ggpmisc) # Required for stat_poly_eq

# Load datasets
flower_data <- read_csv("FlowerData_Processed.csv")
weather_data <- read_csv("WeatherData_Processed.csv")

# Adjust Weather Data for Previous Season Comparison
weather_data <- weather_data %>%
  mutate(PREV_YEAR = ifelse(SEASON == "Winter", YEAR + 1, YEAR))

# Merge spring bloom data with previous winter data, ensuring SEASON is retained
merged_data <- flower_data %>%
  left_join(weather_data %>% filter(SEASON == "Winter") %>% select(-SEASON), by = c("YEAR" = "PREV_YEAR"))

# SPRING BLOOM ANALYSIS
# Checking correlation between nighttime lows and proportion of species blooming early
spring_data <- merged_data %>% filter(SEASON == "Spring")
spring_early_rate <- spring_data %>% group_by(YEAR) %>% 
  summarise(early_bloom_rate = mean(EARLY, na.rm = TRUE),
            avg_temp_min = mean(TEMP_MIN, na.rm = TRUE))
cor_nighttime_low <- cor(spring_early_rate$avg_temp_min, spring_early_rate$early_bloom_rate, use = "complete.obs")
print(paste("Correlation between nighttime low temp and early bloom rate: ", cor_nighttime_low))

# Visualizing the relationship
ggplot(spring_early_rate, aes(x = avg_temp_min, y = early_bloom_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE) +
  labs(title = "Effect of Nighttime Lows on Early Bloom Rate", x = "Nighttime Low Temperature", y = "Proportion of Species Blooming Early")

# FALL BLOOM CONSISTENCY
fall_data <- merged_data %>% filter(SEASON == "Fall")
fall_richness <- fall_data %>% group_by(YEAR) %>% 
  summarise(species_richness = n_distinct(SPECIES))
ggplot(fall_richness, aes(x = YEAR, y = species_richness)) +
  geom_line() +
  geom_point() +
  labs(title = "Fall Bloom Species Richness Over the Years", x = "Year", y = "Number of Species Observed Blooming")

# SPECIES-SPECIFIC ERRORS
allium_data <- merged_data %>% filter(SPECIES == "Allium anceps")
allium_seasons <- unique(allium_data$SEASON)
print(paste("Allium anceps observed bloom seasons: ", paste(allium_seasons, collapse = ", ")))

# FALL 2022 BLOOM SUPPRESSION
fall_2022 <- fall_data %>% filter(YEAR == 2022)
fall_2021 <- fall_data %>% filter(YEAR == 2021)
fall_comparison <- t.test(fall_2022$EARLY, fall_2021$EARLY, alternative = "less")
print(fall_comparison)

# SPRING SPECIES RICHNESS & WATER THRESHOLD
spring_richness <- spring_data %>% group_by(YEAR) %>% 
  summarise(species_richness = n_distinct(SPECIES),
            mean_snow = mean(SNOW_WC, na.rm = TRUE))
ggplot(spring_richness, aes(x = mean_snow, y = species_richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE) +
  labs(title = "Spring Bloom Species Richness vs. Snow Water Content", x = "Mean Snow Water Content", y = "Number of Species Blooming")