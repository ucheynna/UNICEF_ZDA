## Recoding and indicator extraction

#----- Calculate age in months of all children
ir <- lapply(ir, function(x){ x <- x %>% 
  mutate(age_m = v008 - b3_01); return(x)})
# Keep only <60
ir <- lapply(ir, function(x){ x <- x %>% 
  filter(age_m <60); return(x)})

# Filter out the youngest child by age group

#Children 6-59m (VAS)
ir <- lapply(ir, function(x){ x <- x %>% 
  mutate(youngest_0659m = if_else(age_m >=6 , 1, 0)); return(x)})

#Children 12-59m (De-worming)
ir <- lapply(ir, function(x){ x <- x %>% 
  mutate(youngest_1259m = if_else(age_m >=12 , 1, 0)); return(x)})

#Children 12-23m (DTP)
ir <- lapply(ir, function(x){ x <- x %>%
  mutate(youngest_1223m = if_else(age_m >=12 & age_m <24, 1, 0)); return(x)})

#Extract indicators for zd, vas and deworming
#----- DTP1, assuming missing is 0 as in original code, recode to 1 if response is 0.
ir <- lapply(ir, function(x) {
  x <- x %>% mutate(
    zd = case_when(
      h3_1 == 0 ~ 1,
      TRUE ~ 0
    )
  )
  return(x)
})

#----- VAS 6
ir <- lapply(ir, function(x) {
  x <- x %>% mutate(
    vas6 = case_when(
      h34_1 == 1 ~ 1,
      TRUE ~ 0  
    )
  )
  return(x)
})

#----- Deworming
ir <- lapply(ir, function(x) {
  x <- x %>% mutate(
    dwormed = case_when(
      h43_1 == 1 ~ 1,
      TRUE ~ 0  
    )
  )
  return(x)
})

# Select needed variables
ir <- lapply(ir, function(x){ x <- x %>% 
  dplyr::select(SurveyId, CountryName, SurveyYear,caseid, v001, v005,b5_01,
                age_m, youngest_0659m,youngest_1259m,youngest_1223m, zd, vas6, dwormed); return(x)})


# Reclassify all variables as factors
ir <- lapply(ir, haven::as_factor)
## Change survey weights to numeric
ir <- lapply(ir, function(x){ x <- x %>% 
  mutate(v005 = as.numeric(v005)); return(x)})
