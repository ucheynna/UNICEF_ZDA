#ZD by MPI category
proportion_results <- map(prirhr, function(df) {
  df %>%
    filter(youngest_1223m == 1) %>%
    mutate(weight = v005) %>%
    group_by(CountryName, vulnerable) %>%
    summarise(
      proportion_zd = weighted.mean(zd == 1, w = weight, na.rm = TRUE) * 100,
      se = sqrt(wtd.var(zd == 1, weights = weight, na.rm = TRUE)) / sqrt(n()),
      ci_lower = proportion_zd - 1.96 * (se * 100),
      ci_upper = proportion_zd + 1.96 * (se * 100)
    ) %>%
    mutate(vulnerable = if_else(vulnerable == 1, "vulnerable_yes", "vulnerable_no"))
})

# Combine the results from all countries and omit NAs
combined_results <- bind_rows(proportion_results)
combined_results <- na.omit(combined_results)
# Merge with other data (e.g., country regions)
tab <- merge(combined_results, country_regions, by = "CountryName")

# Sort the data by proportion_zd
tab_sorted <- tab %>%
  arrange(proportion_zd)

# Plot using ggplot2
ggplot(tab_sorted, aes(x = proportion_zd, y = reorder(CountryName, proportion_zd), color = vulnerable)) +
  geom_point(size = 3) +  
  geom_line(aes(group = CountryName)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), width = 0.2) +  
  labs(
    x = "Percentage of Zero Dose Children (12-23m)", 
    y = "Country", 
    title = "Proportion of Zero Dose Children by MPI"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  
    plot.title = element_text(hjust = 0.5)  
  ) +
  scale_color_manual(
    name = "vulnerable",  # Label the legend
    values = c("vulnerable_no" = "orange", "vulnerable_yes" = "skyblue"),  
    labels = c("vulnerable_no" = "No", "vulnerable_yes" = "Yes")
  )
