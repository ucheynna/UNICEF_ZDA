#load csv of priority countries
country_regions <- read.csv("country_selection.csv")
country_regions <- country_regions %>% 
  dplyr::select(.id, region, CountryName) %>% 
  dplyr::filter(.id!="")

# combine list 
combined <- bind_rows(ir)

combined <- merge(combined, country_regions, by="CountryName")
combined <- combined %>%
  filter(youngest_1259m == 1) %>%
  select(CountryName, youngest_1259m, zd, vas6, dwormed, region, v005)

#analytical weights
counts <- combined %>%
  group_by(CountryName) %>%
  summarise(
    zd0_vas61 = sum((zd == 0 & vas6 == 1) * v005) / sum(v005) * 100,
    zd0_vas60 = sum((zd == 0 & vas6 == 0) * v005) / sum(v005) * 100,
    zd1_vas61 = sum((zd == 1 & vas6 == 1) * v005) / sum(v005) * 100,
    zd1_vas60 = sum((zd == 1 & vas6 == 0) * v005) / sum(v005) * 100
  )


# Add combined length column and sort by it
counts <- counts %>%
  mutate(combined_length = zd1_vas60 + zd1_vas61) %>%
  arrange(combined_length)

# Reshape the data from wide to long format
counts_long <- counts %>%
  select(-combined_length) %>%
  gather(key = "Scenario", value = "Percentage", -CountryName)

# Reorder CountryName factor levels based on sorted combined_length values
counts_long$CountryName <- factor(counts_long$CountryName, levels = counts$CountryName)

# Reorder the fill colors for consistent stacking
counts_long$Scenario <- factor(counts_long$Scenario, levels = c("zd0_vas61", "zd0_vas60", "zd1_vas61", "zd1_vas60"))

# Define custom colors for each scenario
custom_colors <- c("zd0_vas61" = "grey", "zd0_vas60" = "skyblue", "zd1_vas61" = "pink", "zd1_vas60" = "red")

# Plot the data
ggplot(counts_long, aes(x = CountryName, y = Percentage, fill = Scenario)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  labs(x = "Country", y = "Percentage (%)") +
  ggtitle("Zero Dose and VAS Scenarios by Country (age=12-59m)") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



write.csv(counts_long, file="ZD_VAS_stacked_longformat.csv")

##region grouping

counts <- combined %>%
  group_by(CountryName, region) %>%
  summarise(
    zd0_vas61 = sum((zd == 0 & vas6 == 1) * v005) / sum(v005) * 100,
    zd0_vas60 = sum((zd == 0 & vas6 == 0) * v005) / sum(v005) * 100,
    zd1_vas61 = sum((zd == 1 & vas6 == 1) * v005) / sum(v005) * 100,
    zd1_vas60 = sum((zd == 1 & vas6 == 0) * v005) / sum(v005) * 100
  )

# Add combined length column and sort within each region by it
counts <- counts %>%
  mutate(combined_length = zd1_vas60 + zd1_vas61) %>%
  arrange(region, combined_length)

# Reshape the data from wide to long format
counts_long <- counts %>%
  select(-combined_length) %>%
  gather(key = "Scenario", value = "Percentage", -CountryName, -region)

# Reorder CountryName factor levels based on sorted combined_length values within each region
counts_long$CountryName <- factor(counts_long$CountryName, levels = counts$CountryName)

# Reorder the fill colors for consistent stacking
counts_long$Scenario <- factor(counts_long$Scenario, levels = c("zd0_vas61", "zd0_vas60", "zd1_vas61", "zd1_vas60"))

# Define custom colors for each scenario
custom_colors <- c("zd0_vas61" = "grey", "zd0_vas60" = "skyblue", "zd1_vas61" = "pink", "zd1_vas60" = "red")

# Plot the data
ggplot(counts_long, aes(x = CountryName, y = Percentage, fill = Scenario)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Country", y = "Percentage (%)") +
  ggtitle("Zero Dose and VAS Scenarios by Country (age=12-23m)") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~ region, scales = "free_x", nrow = 1) +  
  theme(legend.position = "bottom") 