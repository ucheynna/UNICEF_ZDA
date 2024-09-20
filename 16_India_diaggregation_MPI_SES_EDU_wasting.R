# India zd disaggregated by MPI, SES, EDU AND WASTING/STUNTING
# BY MPI
proportion_results <- cleaned %>%
    filter(youngest_1223m == 1) %>%
    mutate(weight = v005) %>%
    group_by(vulnerable) %>%
    summarise(
      proportion_zd = weighted.mean(zd == 1, w = weight, na.rm = TRUE) * 100,
      se = sqrt(wtd.var(zd == 1, weights = weight, na.rm = TRUE)) / sqrt(n()),
      ci_lower = proportion_zd - 1.96 * (se * 100),
      ci_upper = proportion_zd + 1.96 * (se * 100)
    ) %>%
    mutate(vulnerable = if_else(vulnerable == 1, "vulnerable_yes", "vulnerable_no"))



##edu
proportion_results <- cleaned %>%
  filter(youngest_1223m == 1) %>%
  mutate(weight = v005) %>%
  group_by(edu) %>%
  summarise(
    proportion_zd = weighted.mean(zd == 1, w = weight, na.rm = TRUE) * 100,
    se = sqrt(wtd.var(zd == 1, weights = weight, na.rm = TRUE)) / sqrt(n()),
    ci_lower = proportion_zd - 1.96 * (se * 100),
    ci_upper = proportion_zd + 1.96 * (se * 100)
  ) %>%
  mutate(edu = if_else(edu == 1, "edu_yes", "edu_no"))



##SES
proportion_results <- cleaned %>%
  filter(youngest_1223m == 1) %>%
  mutate(weight = v005) %>%
  group_by(SES) %>%
  summarise(
    proportion_zd = weighted.mean(zd == 1, w = weight, na.rm = TRUE) * 100,
    se = sqrt(wtd.var(zd == 1, weights = weight, na.rm = TRUE)) / sqrt(n()),
    ci_lower = proportion_zd - 1.96 * (se * 100),
    ci_upper = proportion_zd + 1.96 * (se * 100)
  ) %>%
  mutate(SES = if_else(SES == 1, "SES_yes", "SES_no"))


#wasting/stunting

proportion_results <- cleaned %>%
  filter(youngest_1223m == 1) %>%
  mutate(weight = v005) %>%
  group_by(wfh_wfa) %>%
  summarise(
    proportion_zd = weighted.mean(zd == 1, w = weight, na.rm = TRUE) * 100,
    se = sqrt(wtd.var(zd == 1, weights = weight, na.rm = TRUE)) / sqrt(n()),
    ci_lower = proportion_zd - 1.96 * (se * 100),
    ci_upper = proportion_zd + 1.96 * (se * 100)
  ) %>%
  mutate(wfh_wfa = if_else(wfh_wfa == 1, "wasting_stunting_yes", "wasting_stunting_no"))



