library(tidyverse)
library(lubridate)
library(patchwork)


hobos <- read_csv("all_hobos_trimmed.csv")
weather <- read_csv("all_weather.csv")
site_species <- read_csv("site_species.csv")
canopy_cover <- read_csv("canopy_cover.csv")

# Step 1: Merge Data
hobos_hourly <- hobos %>%
  mutate(
    Date_Time = as.POSIXct(Date_Time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S %Z"),
    Date_Hour = floor_date(Date_Time, unit = "hour")
  ) %>%
  # Make sure to rename *before* summarising!
  rename(Temperature_hobo = Temperature) %>%
  group_by(hobo_number, Site_Number, Date_Hour) %>%
  summarise(
    Temperature_hobo = mean(Temperature_hobo, na.rm = TRUE),
    .groups = "drop"
  )

weather <- weather %>%
  mutate(
    Date_Time = as.POSIXct(Date_Time, format = "%Y-%m-%dT%H:%M", tz = "UTC"),
    Date_Hour = floor_date(Date_Time, unit = "hour")
  ) %>%
  rename(Temperature_weather = Temperature)


# Merge
merged <- inner_join(hobos_hourly, weather,
                     by = c("hobo_number", "Site_Number", "Date_Hour"))
merged <- inner_join(merged, site_species,
                     by = c("hobo_number"))
merged <- inner_join(merged, canopy_cover,
                     by = c("hobo_number"))

# Line Graph
hobo_list <- unique(merged$hobo_number)
plot_list <- list()

for (hobo_id in hobo_list) {
  data_subset <- merged %>% filter(hobo_number == hobo_id)
  
  # Defensive: handle possible multiple site/species per hobo
  site_num <- unique(data_subset$Site_Number)
  species_name <- unique(data_subset$species)
  therm_max <- unique(data_subset$therm_max)
  
  # Handle case where therm_max might be missing or multiple
  therm_max_plot <- if(length(therm_max) == 1) therm_max else NA
  
  p <- ggplot(data_subset, aes(x = Date_Hour)) +
    geom_line(aes(y = Temperature_hobo, color = "HOBO"), size = 0.7, alpha = 0.9) +
    geom_line(aes(y = Temperature_weather, color = "Weather Station"), size = 0.7, alpha = 0.7) +
    geom_hline(yintercept = therm_max_plot, linetype = "dashed", color = "red") +
    scale_color_manual(
      name = "Source",
      values = c("HOBO" = "#2166AC", "Weather Station" = "#B2182B"),
      labels = c("HOBO logger", "Weather station")
    ) +
    labs(
      title = paste("HOBO", hobo_id, "-", paste(site_num, collapse = ", "), "-", paste(species_name, collapse = ", ")),
      x = NULL,
      y = "Temperature (°C)"
    ) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")
  
  plot_list[[as.character(hobo_id)]] <- p
}

# Combine all plots into one figure
combined_plot <- wrap_plots(plot_list, ncol = 3)
print(combined_plot)
#END PLOT

# Plot
species_shapes <- c("Cxq" = 21, "Cxt" = 22, "Unk" = 24)

scatter_plot <- ggplot(merged, aes(x = Temperature_weather, y = Temperature_hobo)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_point(aes(shape = species, fill = percent_cover), color = "black", size = 2.5, alpha = 0.85) +
  scale_shape_manual(values = species_shapes) +
  scale_fill_gradient(low = "#E6F5D0", high = "#00441B", name = "Canopy cover") +
  labs(
    x = "Weather station temperature (°C)",
    y = "HOBO logger temperature (°C)",
    shape = "Species",
    title = "Paired HOBO logger vs Weather station temperatures\nColored by Canopy Cover"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

# Step 3: Statistics
# Wilcoxon test
wilcox_results <- merged %>%
  group_by(hobo_number) %>%
  summarise(
    wilcox_statistic = wilcox.test(Temperature_hobo, Temperature_weather, paired = TRUE)$statistic,
    wilcox_p = wilcox.test(Temperature_hobo, Temperature_weather, paired = TRUE)$p.value,
    .groups = "drop"
  )

# Average temperatures
avg_temp_summary <- merged %>%
  group_by(hobo_number) %>%
  summarise(
    avg_temp_hobo = mean(Temperature_hobo, na.rm = TRUE),
    avg_temp_weather = mean(Temperature_weather, na.rm = TRUE),
    temp_difference = avg_temp_hobo - avg_temp_weather,
    .groups = "drop"
  )

# Step 4: Compare Thermal Max
# --- Count HOBO exceedances from original data ---
hobo_exceedance_summary <- hobos %>%
  left_join(site_species, by = c("hobo_number", "Site_Number")) %>%
  filter(!is.na(therm_max)) %>%
  group_by(hobo_number, Site_Number, species) %>%
  summarise(
    hobo_total_obs = sum(!is.na(Temperature_hobo)),
    hobo_exceedances = sum(Temperature_hobo > therm_max, na.rm = TRUE),
    hobo_percent_exceeding = 100 * hobo_exceedances / hobo_total_obs,
    .groups = "drop"
  )

# --- Count Weather exceedances from original weather data ---
weather_exceedance_summary <- weather %>%
  left_join(site_species, by = c("hobo_number", "Site_Number")) %>%
  filter(!is.na(therm_max)) %>%
  group_by(hobo_number, Site_Number, species) %>%
  summarise(
    weather_total_obs = sum(!is.na(Temperature_weather)),
    weather_exceedances = sum(Temperature_weather > therm_max, na.rm = TRUE),
    weather_percent_exceeding = 100 * weather_exceedances / weather_total_obs,
    .groups = "drop"
  )

# --- Combine both summaries ---
full_exceedance_summary <- hobo_exceedance_summary %>%
  left_join(weather_exceedance_summary, by = c("hobo_number", "Site_Number", "species"))


# Step 5: Save Data
# Write CSVs
ggsave("hobo_weather_scatter_canopy.png", scatter_plot, width = 8, height = 7, dpi = 300, bg = "white")

write_csv(wilcox_results, "wilcoxon_results.csv")
write_csv(avg_temp_summary, "average_temperature_differences.csv")
write_csv(full_exceedance_summary, "hobo_weather_thermal_exceedances.csv")

# Save plot as PNG
ggsave(
  filename = "hobo_comparisons_with_thermal_max.png",
  plot = combined_plot,
  width = 16, height = 20, dpi = 300
)
