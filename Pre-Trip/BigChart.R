# Load required libraries
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)

# Load the data
df <- read.csv("Summary_No_Spring.csv")

# Convert Season into a factor to maintain order
season_order <- c("Winter", "Spring", "Summer", "Fall")
df$Season <- factor(df$Season, levels = season_order)

# Create a custom combined x-axis label (Season Year)
df <- df %>%
  arrange(Year, Season) %>%
  mutate(Season_Year = paste(Season, Year))  # Combine Season & Year into one label

# Normalize temperature and snow for better scaling
df <- df %>%
  mutate(Scaled_Temp = rescale(AVERAGE_TEMP_AVERAGE, to = c(0, 1)),
         Scaled_Snow = rescale(AVERAGE_SNOW_WC, to = c(0, 1)))

# Convert the dataframe to long format for separate plots
df_temp_snow <- df %>%
  select(Season_Year, Scaled_Temp, Scaled_Snow) %>%
  pivot_longer(cols = c(Scaled_Temp, Scaled_Snow), names_to = "Variable", values_to = "Value") %>%
  mutate(Panel = "Temperature & Snow")

df_flowering <- df %>%
  select(Season_Year, SUM_Only_Buds, SUM_Early, SUM_Peak, SUM_Late, SUM_Only_Fruits) %>%
  pivot_longer(cols = starts_with("SUM_"), names_to = "Flowering_Stage", values_to = "Count") %>%
  mutate(Panel = "Flowering Data")

# Combine both datasets with a panel identifier
df_combined <- bind_rows(
  rename(df_temp_snow, Category = Variable, Value = Value),
  rename(df_flowering, Category = Flowering_Stage, Value = Count)
)

# Convert Season_Year to a factor to maintain order
df_combined$Season_Year <- factor(df_combined$Season_Year, levels = unique(df$Season_Year))

# Order the Panel factor to ensure the correct order: Lines on Top, Bars on Bottom
df_combined$Panel <- factor(df_combined$Panel, levels = c("Temperature & Snow", "Flowering Data"))

# Define custom colors for both lines and bars to be the same
custom_colors <- c("Scaled_Temp" = "red", "Scaled_Snow" = "blue", 
                   "SUM_Only_Buds" = "purple", "SUM_Early" = "green", 
                   "SUM_Peak" = "orange", "SUM_Late" = "red", 
                   "SUM_Only_Fruits" = "brown")

# Plot with facets correctly ordered and a single clean legend
ggplot(df_combined, aes(x = Season_Year, y = Value, color = Category, fill = Category)) +
  # Line plot for temperature and snow in first panel
  geom_line(data = df_combined %>% filter(Panel == "Temperature & Snow"),
            aes(group = Category), size = 1) +
  geom_point(data = df_combined %>% filter(Panel == "Temperature & Snow"),
             size = 3) +
  
  # Grouped bar chart for flowering data in second panel
  geom_bar(data = df_combined %>% filter(Panel == "Flowering Data"),
           aes(group = Category), stat = "identity", position = "dodge", alpha = 0.7) +
  
  # Facet the plots into separate panels, forcing correct order
  facet_grid(rows = vars(Panel), scales = "free_y", switch = "y") +
  
  # Custom legend to merge color and fill together and properly show line & bar symbols
  scale_color_manual(values = custom_colors, name = "Legend") +
  scale_fill_manual(values = custom_colors, name = "Legend") +
  
  # Adjust the legend to show both lines and bars correctly
  guides(
    color = guide_legend(override.aes = list(linetype = c("solid", "solid", "blank", "blank", "blank", "blank", "blank"), 
                                             shape = c(NA, NA, 22, 22, 22, 22, 22))), 
    fill = guide_legend(override.aes = list(linetype = "blank", shape = 22))
  ) +
  
  # Labels and themes
  labs(title = "Temperature, Snow & Flowering Data Over Time",
       x = "Season & Year",
       y = "Value") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 12, face = "bold"))
