#HR file for deprivation indicators
# we will give our permission here so that we don't have to provide a hrompt within the README.
Sys.setenv("rdhs_RENVIRON_PERMISSION"=1)

## a little nugget to return API requests as data.table rather than data.frame.
Sys.setenv(rdhs_DATA_TABLE = "TRUE")


#----- Retreive data

## Identify all DHS surveys for the following countries
countries <- dhs_countries()
cc <- c( 
  "AO",
  "ET",
  "KH", 
  "KY","MW","CM","BJ","CF","ML","NP","PK","ZA",
  "BU","BD","GM","HT","LB","NG","PG",
  "SN","SL","TJ","TZ","TL","UG","ZM","ZW",
  "BF","CI","GH","GA","KE","MZ","MD","RW",
  "GN","MM",
  "NI","TD","CG","CD",
  "BO","KM","SZ","GU","LS",
  "NM","ST","TG","YE","HN","MR")


# Use rdhs to retreive datasets, downloading them from DHS website if not already in the rdhs cache.
surveys <- dhs_surveys(countryIds = cc, surveyYearStart=2012, surveyType = "DHS")
surveys <- surveys %>% dplyr::group_by(DHS_CountryCode) %>% filter(SurveyYear == max(SurveyYear))


# Identify bhrths recode (BR) datasets corresponding to these surveys and select the most recent.
step <- dhs_datasets(fileType = "hr", fileFormat = "flat")
hrd <- step[which(step$SurveyId %in% surveys$SurveyId),]
hrd <- hrd %>% dplyr::group_by(DHS_CountryCode) %>% filter(SurveyYear == max(SurveyYear))


# Use rdhs to retreive datasets, downloading them from DHS website if not already in the rdhs cache.
Sys.setenv("rdhs_LOUD_DOWNLOAD" = TRUE)
hrd$path <- unlist(get_datasets(hrd$FileName,clear_cache = TRUE))


hr <- list()
for(survid in hrd$SurveyId){
  print(paste("HR:", survid))
  dat <- readRDS(hrd[which(hrd$SurveyId == survid),]$path)
  dat <- dat[grep("hv001|hv002|hv005|hv103|hv105|hv104|hv109|hv210|hv211|hv209|hv208|hv207|hv221|hv243|hv212|hv042|hv226|hv205|hv204|hv201|hv206|hv213", names(dat))]
  hr[[survid]] <- dat
}


##Add survey-level variables
hr <- Map(data.frame,
          SurveyId = hrd$SurveyId,
          CountryName = hrd$CountryName,
          SurveyYear = hrd$SurveyYear,
          hr)

# Convert variables to factors and scale weights
hr <- lapply(hr, function(x) {
  x$SurveyId <- factor(x$SurveyId)
  x$CountryName <- factor(x$CountryName)
  x$SurveyYear <- factor(x$SurveyYear)
  x$hv005 <- x$hv005 / 1000000
  return(x)
})


hr <- lapply(hr, function(x) {
  x <- x %>%
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
      )
    ) %>%
    # Check for asset ownership separately
    mutate(
      owns_bicycle = ifelse(hv210 == 1, 1, 0),
      owns_bike = ifelse(hv211 == 1, 1, 0),
      owns_refrigerator = ifelse(hv209 == 1, 1, 0),
      owns_radio = ifelse(hv207 == 1, 1, 0),
      owns_tv = ifelse(hv208 == 1, 1, 0),
      owns_phone = ifelse(hv221 == 1, 1, 0),
      owns_cart = ifelse(hv243c == 1, 1, 0),
      owns_car = ifelse(hv212 == 1, 1, 0)
    ) %>%
    # Calculate the total number of specific assets owned (excluding car)
    mutate(
      total_assets_owned = owns_bicycle + owns_bike + owns_refrigerator + 
        owns_radio + owns_tv + owns_phone + owns_cart
    ) %>%
    # Determine deprivation status based on the correct criteria
    mutate(
      asset = case_when(
        total_assets_owned <= 1 & owns_car == 0 ~ 1,  
        TRUE ~ 0                                    
      )
    ) %>%
    # Optionally, drop the intermediate columns if not needed
    select(-owns_bicycle, -owns_bike, -owns_refrigerator, -owns_radio, 
           -owns_tv, -owns_phone, -owns_cart, -owns_car, -total_assets_owned)
  return(x)
})

# Select only specified columns
hr <- lapply(hr, function(x) {
  x <- x %>% select(floor,fuel,electricity,asset,toilet,drinking_water,hv001,hv002, hv005, CountryName, SurveyId, SurveyYear)
  return(x)
})





#IR files FOR zd and education
Sys.setenv("rdhs_RENVIRON_PERMISSION" = 1)
Sys.setenv(rdhs_DATA_TABLE = "TRUE")


cc <- c( 
  "AO",
  "ET",
  "KH", 
  "KY","MW","CM","BJ","CF","ML","NP","PK","ZA",
  "BU","BD","GM","HT","LB","NG","PG",
  "SN","SL","TJ","TZ","TL","UG","ZM","ZW",
  "BF","CI","GH","GA","KE","MZ","MD","RW",
  "GN","MM",
  "NI","TD","CG","CD",
  "BO","KM","SZ","GU","LS",
  "NM","ST","TG","YE","HN", "MR")

surveys <- dhs_surveys(countryIds = cc, surveyYearStart = 2012, surveyType = "DHS")
surveys <- surveys %>% group_by(DHS_CountryCode) %>% filter(SurveyYear == max(SurveyYear))
step <- dhs_datasets(fileType = "IR", fileFormat = "flat")
ird <- step %>% filter(SurveyId %in% surveys$SurveyId) %>% group_by(DHS_CountryCode) %>% filter(SurveyYear == max(SurveyYear))
Sys.setenv("rdhs_LOUD_DOWNLOAD" = TRUE)
ird$path <- unlist(get_datasets(ird$FileName, clear_cache = TRUE))

# Read 
ir <- list()
for (survid in ird$SurveyId) {
  print(survid)
  dat <- readRDS(ird %>% filter(SurveyId == survid) %>% pull(path))
  dat <- dat %>% select(matches("caseid|v001|v005|b3_01|v149|mv149|v008|v002|b5|h34_1|h3_1"))
  ir[[survid]] <- dat
}

# Add survey-level variables
ir <- Map(data.frame,
          SurveyId = ird$SurveyId,
          CountryName = ird$CountryName,
          SurveyYear = ird$SurveyYear,
          ir)

# Convert variables to factors and scale weights
ir <- lapply(ir, function(x) {
  x$SurveyId <- factor(x$SurveyId)
  x$CountryName <- factor(x$CountryName)
  x$SurveyYear <- factor(x$SurveyYear)
  x$v005 <- x$v005 / 1000000
  return(x)
})


# Calculate age in months of all children and keep only those <60 months
ir <- lapply(ir, function(x) {
  x <- x %>% 
    mutate(age_m = v008 - b3_01) %>% 
    filter(age_m < 60)
  return(x)
})

# Filter out the youngest child by age group 12-23 months
ir <- lapply(ir, function(x) {
  x <- x %>%
    mutate(youngest_1223m = if_else(age_m >= 12 & age_m < 24, 1, 0))
  return(x)
})

# Recode DTP1 and assume missing is 0 as in original code
ir <- lapply(ir, function(x) {
  x <- x %>% mutate(
    zd = case_when(
      h3_1 == 0 ~ 1,
      TRUE ~ 0
    )
  )
  return(x)
})

#Recode variable for education
ir <- lapply(ir, function(x) {
  x <- x %>% mutate(
    edu = case_when(
      v149 %in% c(0, 1) ~ 1,
      v149 %in% c(2, 3, 4, 5) ~ 0,
      v149 == 9 ~ NA_real_
    )
  )
  return(x)
})

# Select only specified columns
ir <- lapply(ir, function(x) {
  x <- x %>% select(zd,edu, v001,v002, v005, CountryName, SurveyId, SurveyYear, youngest_1223m)
  return(x)
})




#PR data for anthropometery 
#Reload
# we will give our permission here so that we don't have to provide a prompt within the README.
Sys.setenv("rdhs_RENVIRON_PERMISSION"=1)

Sys.setenv(rdhs_DATA_TABLE = "TRUE")


#----- Retreive data

## Identify all DHS surveys for the following countries
countries <- dhs_countries()
cc <- c( 
  "AO",
  "ET",
  "KH", 
  "KY","MW","CM","BJ","CF","ML","NP","PK","ZA",
  "BU","BD","GM","HT","LB","NG","PG",
  "SN","SL","TJ","TZ","TL","UG","ZM","ZW",
  "BF","CI","GH","GA","KE","MZ","MD","RW",
  "GN","MM",
  "NI","TD","CG","CD",
  "BO","KM","SZ","GU","LS",
  "NM","ST","TG","YE","HN", "MR")


# Use rdhs to retreive datasets, downloading them from DHS website if not already in the rdhs cache.
surveys <- dhs_surveys(countryIds = cc, surveyYearStart=2012, surveyType = "DHS")
surveys <- surveys %>% dplyr::group_by(DHS_CountryCode) %>% filter(SurveyYear == max(SurveyYear))


step <- dhs_datasets(fileType = "pr", fileFormat = "flat")
prd <- step[which(step$SurveyId %in% surveys$SurveyId),]
prd <- prd %>% dplyr::group_by(DHS_CountryCode) %>% filter(SurveyYear == max(SurveyYear))
Sys.setenv("rdhs_LOUD_DOWNLOAD" = TRUE)
prd$path <- unlist(get_datasets(prd$FileName,clear_cache = TRUE))


pr <- list()
for(survid in prd$SurveyId){
  print(survid)
  dat <- readRDS(prd[which(prd$SurveyId == survid),]$path)
  dat <- dat[grep("hv001|hv005|hv002|hc57|hc72|hc70|hc1|hv103|hv042|hv55|hv008|
                
                  ", names(dat))]
  pr[[survid]] <- dat
}

# Add survey-level variables
pr <- Map(data.frame,
          SurveyId = prd$SurveyId,
          CountryName = prd$CountryName,
          SurveyYear = prd$SurveyYear,
          pr)

# Convert variables to factors and scale weights
pr <- lapply(pr, function(x) {
  x$SurveyId <- factor(x$SurveyId)
  x$CountryName <- factor(x$CountryName)
  x$SurveyYear <- factor(x$SurveyYear)
  x$hv005 <- x$hv005 / 1000000
  return(x)
})



# Classify WFH Z-scores and calculate mean Z-score
pr <- lapply(pr, function(x) {
  x <- x %>%
    mutate(
      wfh_category = case_when(
        hc72 >= -200 & hc72 <= 200 ~ 0,  # Normal category
        hc72 < -200 | (hc72 > 200 & hc72 < 9990) ~ 1,  # Abnormal category (Severely Wasted, Moderately Wasted, Overweight)
        hc72 >= 9990 ~ NA_real_  # NA for flagged/out-of-range values
      ),
      mean_wfh_zscore = if_else(hc72 < 9990, hc72 / 100, NA_real_)  # Calculate mean Z-score, return NA if flagged
    ) %>% 
    filter(!is.na(wfh_category)) %>%  # Exclude rows where wfh_category is NA
  return(x)
})

# Classify HFA Z-scores and calculate mean Z-score
pr <- lapply(pr, function(x) {
  x <- x %>%
    mutate(
      hfa_category = case_when(
        hc70 >= -200 & hc70 <= 200 ~ 0,  # Normal category
        hc70 < -200 | (hc70 > 200 & hc70 < 9990) ~ 1, 
        hc70 >= 9990 ~ NA_real_  
      ),
      mean_hfa_zscore = if_else(hc70 < 9990, hc70 / 100, NA_real_)  
    ) %>%
    filter(!is.na(wfa_category)) %>%  
    select(hfa_category,wfh_category,SurveyId, hv001, hv002, CountryName, SurveyYear)
  
  return(x)
})


join_data_frames <- function(hr_df, pr_df) {
  # Perform the join operation
  joined_df <- hr_df %>%
    inner_join(pr_df, by = c("hv001", "hv002", "countryname"))
  return(joined_df)
}


prhr <- map2(pr, hr, ~ inner_join(.x, .y, by = c("hv001", 
                                                 "hv002", 
                                                 "CountryName")))



prirhr <- map2(prhr, ir, ~ inner_join(.x, .y, by = c("hv001" = "v001", 
                                                     "hv002" = "v002", 
                                                     "CountryName")))




#deprivation weights, sum to 1
weights <- c(1/6,1/6, 1/3, 1/18, 1/18, 1/18, 1/18, 1/18, 1/18)

# Function y
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
prirhr <- lapply(prirhr, calculate_mpi)



# Create new stunting and wasting and SES categories
prirhr <- lapply(prirhr, function(x) {
  x <- x %>% 
    mutate(
      # Create wfh_wfa category
      wfh_hfa = if_else(wfh_category == 1 | hfa_category == 1, 1, 0),
      
      # Create SES category
      SES = if_else((floor + toilet + drinking_water + asset + electricity + fuel) >= 2, 1, 0)
    )
  return(x)
})







