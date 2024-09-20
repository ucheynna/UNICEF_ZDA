# India IR dataframe
combined <- ir %>%
  filter(youngest_1223m == 1) %>%
  select(youngest_1223m, zd, vas6, dwormed, v005)

counts <- combined %>%
  summarise(
    zd0_vas61 = sum((zd == 0 & vas6 == 1) * v005) / sum(v005) * 100,
    zd0_vas60 = sum((zd == 0 & vas6 == 0) * v005) / sum(v005) * 100,
    zd1_vas61 = sum((zd == 1 & vas6 == 1) * v005) / sum(v005) * 100,
    zd1_vas60 = sum((zd == 1 & vas6 == 0) * v005) / sum(v005) * 100
  )

# Reshape the data from wide to long format
counts_long <- counts %>%
  select(zd0_vas61, zd0_vas60, zd1_vas61, zd1_vas60) %>%
  gather(key = "Scenario", value = "Percentage")

# Reorder the Scenario factor levels for consistent stacking
counts_long$Scenario <- factor(counts_long$Scenario, levels = c("zd0_vas61", "zd0_vas60", "zd1_vas61", "zd1_vas60"))

# Define custom colors for each scenario
custom_colors <- c("zd0_vas61" = "grey", "zd0_vas60" = "skyblue", "zd1_vas61" = "pink", "zd1_vas60" = "red")

# Calculate the total percentage and ensure it's 100%
total_percentage <- sum(counts_long$Percentage)
if (total_percentage != 100) {
  counts_long$Percentage <- counts_long$Percentage / total_percentage * 100
}

# Plot the data
ggplot(counts_long, aes(x = "", y = Percentage, fill = Scenario)) +
  geom_bar(stat = "identity", position = "fill", width = 0.8) +
  labs(x = "Scenarios", y = "Percentage (%)") +
  ggtitle("Zero Dose and VAS Scenarios") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


## Dewormed
combined <- ir %>%
  filter(youngest_1223m == 1) %>%
  select(youngest_1223m, zd, vas6, dwormed, v005)

counts <- combined %>%
  summarise(
    zd0_dwormed1 = sum((zd == 0 & dwormed == 1) * v005) / sum(v005) * 100,
    zd0_dwormed0 = sum((zd == 0 & dwormed == 0) * v005) / sum(v005) * 100,
    zd1_dwormed1 = sum((zd == 1 & dwormed == 1) * v005) / sum(v005) * 100,
    zd1_dwormed0 = sum((zd == 1 & dwormed == 0) * v005) / sum(v005) * 100
  )

# Reshape the data from wide to long format
counts_long <- counts %>%
  select(zd0_dwormed1, zd0_dwormed0, zd1_dwormed1, zd1_dwormed0) %>%
  gather(key = "Scenario", value = "Percentage")

# Reorder the Scenario factor levels for consistent stacking
counts_long$Scenario <- factor(counts_long$Scenario, levels = c("zd0_dwormed1", "zd0_dwormed0", "zd1_dwormed1", "zd1_dwormed0"))
write.csv(counts_long, file = "indiadewormed.csv")


