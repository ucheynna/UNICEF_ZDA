

#----- Load packages

pacman::p_load(
  here,                      
  rdhs,                      
  survey,srvyr,              
  haven,
  plotly,                    
  tidyverse,
  kableExtra,
  tools,
  Hmisc
)


#---------------------------
# Retrieve data
#---------------------------

#----- Link to API

## set up your credentials
set_rdhs_config(email = "************",
                password_prompt =  TRUE,
                project = "**************", 
                config_path = "rdhs.json",
                global=FALSE)

Sys.setenv("rdhs_RENVIRON_PERMISSION"=1)
Sys.setenv(rdhs_DATA_TABLE = "TRUE")


#----- Retreive data

## Identify all DHS surveys for the following countries
countries <- dhs_countries()

cc <- c("AO","ET", "KH", "KY", "MW", "CM", "BJ", "CF", "ML", "NP", "PK", "ZA",
        "BU", "BD", "GM", "HT", "LB", "NG", "PG", "PH", "SN", "SL", "TJ", "TZ",
        "TL", "UG", "ZM", "ZW", "BF", "CI", "GH", "GA", "KE", "MZ", "MD", "RW",
        "GN", "ID", "MM", "NI", "AF", "TD", "CG", "CD", "BO", "KM", "SZ", "GU",
        "LS", "NM", "ST", "TG", "YE", "HN", "MR")

# "AO" = Angola
# "ET" = Ethiopia
# "KH" = Cambodia
# "KY" = Kyrgyzstan
# "MW" = Malawi
# "CM" = Cameroon
# "BJ" = Benin
# "CF" = Central African Republic
# "ML" = Mali
# "NP" = Nepal
# "PK" = Pakistan
# "ZA" = South Africa
# "BU" = Burkina Faso
# "BD" = Bangladesh
# "GM" = Gambia
# "HT" = Haiti
# "LB" = Lebanon
# "NG" = Nigeria
# "PG" = Papua New Guinea
# "PH" = Philippines
# "SN" = Senegal
# "SL" = Sierra Leone
# "TJ" = Tajikistan
# "TZ" = Tanzania
# "TL" = Timor-Leste
# "UG" = Uganda
# "ZM" = Zambia
# "ZW" = Zimbabwe
# "BF" = Burkina Faso
# "CI" = Côte d'Ivoire
# "GH" = Ghana
# "GA" = Gabon
# "KE" = Kenya
# "MZ" = Mozambique
# "MD" = Moldova
# "RW" = Rwanda
# "GN" = Guinea
# "ID" = Indonesia
# "MM" = Myanmar
# "NI" = Nicaragua
# "AF" = Afghanistan
# "TD" = Chad
# "CG" = Republic of Congo (Congo-Brazzaville)
# "CD" = Democratic Republic of the Congo (Congo-Kinshasa)
# "BO" = Bolivia
# "KM" = Comoros
# "SZ" = Eswatini (Swaziland)
# "GU" = Guatemala
# "LS" = Lesotho
# "NM" = Namibia
# "ST" = São Tomé and Príncipe
# "TG" = Togo
# "YE" = Yemen
# "HN" = Honduras
# "MR" = Mauritania


# Use rdhs to retreive datasets, downloading them from DHS website if not already in the rdhs cache.
# Surveys from 2012
surveys <- dhs_surveys(countryIds = cc, surveyYearStart=2012, surveyType = "DHS")
surveys <- surveys %>% dplyr::group_by(DHS_CountryCode) %>% filter(SurveyYear == max(SurveyYear))


# Identify Individual recode datasets corresponding to these surveys and select the most recent.
step <- dhs_datasets(fileType = "IR", fileFormat = "flat")
ird <- step[which(step$SurveyId %in% surveys$SurveyId),]
ird <- ird %>% dplyr::group_by(DHS_CountryCode) %>% filter(SurveyYear == max(SurveyYear))


# Use rdhs to retreive datasets, downloading them from DHS website if not already in the rdhs cache.
Sys.setenv("rdhs_LOUD_DOWNLOAD" = TRUE)
ird$path <- unlist(get_datasets(ird$FileName,clear_cache = TRUE))

#create list and select important indicators for zd, VAS and deworming
ir <- list()
for(survid in ird$SurveyId){
  print(survid)
  dat <- readRDS(ird[which(ird$SurveyId == survid),]$path)
  dat <- dat[grep("caseid|v001|v005|v002|h3_1|h34_1||h43_1", names(dat))]
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



