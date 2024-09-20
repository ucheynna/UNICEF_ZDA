#ZD by stunting/wasting category
proportion_results <- map(prirhr, function(df) {
  df %>%
    filter(youngest_1223m == 1) %>%
    mutate(weight = v005) %>%
    group_by(CountryName, wfh_hfa) %>%
    summarise(
      proportion_zd = weighted.mean(zd == 1, w = weight, na.rm = TRUE) * 100,
      se = sqrt(wtd.var(zd == 1, weights = weight, na.rm = TRUE)) / sqrt(n()),
      ci_lower = proportion_zd - 1.96 * (se * 100),
      ci_upper = proportion_zd + 1.96 * (se * 100)
    ) %>%
    mutate(wfh_hfa = if_else(wfh_hfa == 1, "wfh_hfa_yes", "wfh_hfa_no"))
})

# Combine the results from all countries
combined_results <- bind_rows(proportion_results)
combined_results <- na.omit(combined_results)
# Merge with other data (e.g., country regions)
tab <- merge(combined_results, country_regions, by = "CountryName")

# Sort the data by proportion_zd
tab_sorted <- tab %>%
  arrange(proportion_zd)

# Plot using ggplot2
ggplot(tab_sorted, aes(x = proportion_zd, y = reorder(CountryName, proportion_zd), color = wfh_hfa)) +
  geom_point(size = 3) +  
  geom_line(aes(group = CountryName)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), width = 0.2) +  # Add confidence intervals
  labs(
    x = "Percentage of Zero Dose Children (12-23m)", 
    y = "Country", 
    title = "Proportion of Zero Dose Children by stunting_wasting"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  
    plot.title = element_text(hjust = 0.5)  
  ) +
  scale_color_manual(
    name = "wfh_hfa", 
    values = c("wfh_hfa_no" = "orange", "wfh_hfa_yes" = "skyblue"),  
    labels = c("wfh_hfa_no" = "No", "wfh_hfa_yes" = "Yes")
  )
