#India Deprivation

#Read HR file
hr <- read.csv("hrfiles.csv")
hr <- hr %>%
  # Create Electricity deprivation indicator
  mutate(
    electricity = case_when(
      hv206 == 0 ~ 1,
      TRUE ~ 0
    ),
    
    # Create Floor deprivation indicator
    floor = case_when(
      hv213 %in% c(10, 11, 20, 21) ~ 1,
      TRUE ~ 0
    ),
    
    # Create Drinking water deprivation indicator
    drinking_water = case_when(
      hv201 %in% c(11:14, 21, 31, 41, 51, 61, 62, 71) & hv204 %in% c(31:900, 998) ~ 1,
      TRUE ~ 0
    ),
    
    # Create Toilet deprivation indicator
    toilet = case_when(
      hv205 %in% c(14, 23, 42, 43, 96, 31) ~ 1,
      hv205 %in% c(11, 12, 13, 15, 21, 22, 41) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Create Fuel deprivation indicator
    fuel = case_when(
      hv226 %in% c(9:17) ~ 1,
      TRUE ~ 0
    ),
    
    # Check for asset ownership separately
    owns_bicycle = ifelse(hv210 == 1, 1, 0),
    owns_bike = ifelse(hv211 == 1, 1, 0),
    owns_refrigerator = ifelse(hv209 == 1, 1, 0),
    owns_radio = ifelse(hv207 == 1, 1, 0),
    owns_tv = ifelse(hv208 == 1, 1, 0),
    owns_phone = ifelse(hv221 == 1, 1, 0),
    owns_cart = ifelse(hv243c == 1, 1, 0),
    owns_car = ifelse(hv212 == 1, 1, 0),
    
    # Calculate the total number of specific assets owned (excluding car)
    total_assets_owned = owns_bicycle + owns_bike + owns_refrigerator + 
      owns_radio + owns_tv + owns_phone + owns_cart,
    
    # Determine deprivation status based on the correct criteria
    asset = case_when(
      total_assets_owned <= 1 & owns_car == 0 ~ 1,  
      TRUE ~ 0                                   
    )
  ) %>%
  
  # Optionally, drop the intermediate columns if not needed
  select(-owns_bicycle, -owns_bike, -owns_refrigerator, -owns_radio, 
         -owns_tv, -owns_phone, -owns_cart, -owns_car, -total_assets_owned)

# Select only specified columns
hr <- hr %>% select(floor, fuel, electricity, asset, toilet, drinking_water, hv001, hv002, hv005)


#Read IR files
ir <- read.csv("irfiles2.csv")
# Calculate age in months of all children and keep only those <60 months
ir <- ir %>%
  mutate(age_m = v008 - b3_01) %>%
  filter(age_m < 60)

# Filter out the youngest child by age group 12-23 months
ir <- ir %>%
  mutate(youngest_1223m = if_else(age_m >= 12 & age_m < 24, 1, 0))

# Recode DTP1 and assume missing is 0 as in original code
ir <- ir %>%
  mutate(
    zd = case_when(
      h3_1 == 0 ~ 1,
      TRUE ~ 0
    )
  )

# Recode education levels
ir <- ir %>%
  mutate(
    edu = case_when(
      v149 %in% c(0, 1) ~ 1,
      v149 %in% c(2, 3, 4, 5) ~ 0,
      v149 == 9 ~ NA_real_
    )
  )

# Select only specified columns
ir <- ir %>%
  select(zd, edu, v001, v002, v005, youngest_1223m)


#Read PR file
pr <- read.csv("prfiles.csv")

# Wasting
pr <- pr %>%
  mutate(
    wfh_category = case_when(
      hc72 >= -200 & hc72 <= 200 ~ 0,  # Normal category
      hc72 < -200 | (hc72 > 200 & hc72 < 9990) ~ 1,  # Abnormal category (Severely Wasted, Moderately Wasted, Overweight)
      hc72 >= 9990 ~ NA_real_  # NA for flagged/out-of-range values
    ),
    mean_wfh_zscore = if_else(hc72 < 9990, hc72 / 100, NA_real_)  # Calculate mean Z-score, return NA if flagged
  ) %>% 
  filter(!is.na(wfh_category)) 

# Stunting
pr <- pr %>%
  mutate(
      hfa_category = case_when(
        hc70 >= -200 & hc70 <= 200 ~ 0,  # Normal category
        hc70 < -200 | (hc70 > 200 & hc70 < 9990) ~ 1, 
        hc70 >= 9990 ~ NA_real_  
      ),
      mean_hfa_zscore = if_else(hc70 < 9990, hc70 / 100, NA_real_)  
    ) %>%
    filter(!is.na(hfa_category)) %>%  
    select(hfa_category,wfh_category,hv001, hv002)
  

#Merge dfs
prhr <- inner_join(pr, hr, by = c("hv001", "hv002"))
prirhr <- inner_join(prhr, ir, by = c("hv001" = "v001","hv002" = "v002"))


#deprivation weights
weights <- c(1/6,1/6, 1/3, 1/18, 1/18, 1/18, 1/18, 1/18, 1/18)

# MPI function
calculate_mpi <- function(data) {
  data <- data %>%
    mutate(
      mpi_score = wfh_category * weights[1] +
        hfa_category * weights[2] +
        edu * weights[3] +
        floor * weights[4] +
        fuel * weights[5] +
        electricity * weights[6] +
        asset * weights[7] +
        toilet * weights[8] +
        drinking_water * weights[9],
      vulnerable = if_else(mpi_score > 0.333333, 1, 0)
    )
  return(data)
}

# Apply function

prirhr <- calculate_mpi(prirhr)

# Create stunting/wasting and SES categories
prirhr <- prirhr %>%
    mutate(
      # Create wfh_hfa category
      wfh_hfa = if_else(wfh_category == 1 | hfa_category == 1, 1, 0),
      
      # Create SES category
      SES = if_else((floor + toilet + drinking_water + asset + electricity + fuel) >= 2, 1, 0)
    )



