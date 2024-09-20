#Nat prevalence for important indicators in India

#read extracted IR file
ir <- read.csv("irfiles.csv")
#Calculate age in CMC
ir <- ir%>% 
  mutate(age_m = v008 - b3_01)

#Filter age <60 months
ir <- ir%>% 
  filter(age_m < 60)

#Create age groups
ir <- ir%>% 
  mutate(youngest_0659m = if_else(age_m >=6 , 1, 0))

ir <- ir%>% 
  mutate(youngest_1259m = if_else(age_m >=6 , 1, 0))

ir <- ir%>% 
  mutate(youngest_1223m = if_else(age_m >=6 , 1, 0))





#----- DTP1 indicator
ir <- ir %>% mutate(
    zd = case_when(
      h3_1 == 0 ~ 1,
      TRUE ~ 0
    ))

#----- VAS 6
ir <- ir %>% mutate(
    vas6 = case_when(
      h34_1 == 1 ~ 1,
      TRUE ~ 0  
    ))

#----- Deworming
ir <- ir %>% mutate(
    dwormed = case_when(
      h43_1 == 1 ~ 1,
      TRUE ~ 0  
    ))

#Scale weight
ir$v005 <- ir$v005/1000000


# weighted data
#Children 12-23m (DTP1)
w_data_1223_dtp1 <- ir %>% 
  filter(youngest_1223m == 1) %>% 
  as_survey_design(id = v001, strata =NULL, weights = v005, nest=T)


#Children 6-59m (VAS)
w_data_0659_vas6 <- ir %>% 
  filter(!is.na(vas6)) %>% 
  filter(youngest_0659m == 1) %>% 
  as_survey_design(id = v001, strata =NULL, weights = v005, nest=T)

#Children 12-59m (De-worming)
w_data_1259_dewormed <- ir %>% 
  filter(youngest_1259m == 1) %>% 
  filter(!is.na(dwormed)) %>% 
  as_survey_design(id = v001, strata =NULL, weights = v005, nest=T)


### Nat stats

#----- Zero dose
nat_zd1223 <- plyr::ldply(w_data_1223_dtp1 %>%  
    srvyr::summarise(zd1223 = (survey_mean(zd == 1, proportion = TRUE, vartype = "ci",na.rm = T)) * 100) %>% 
    dplyr::select(-zd1223_upp, -zd1223_low)
)

#----- VAS
nat_vas <- plyr::ldply(w_data_0659_vas6 %>%  
    srvyr::summarise(vas = (survey_mean(vas6 == 0, proportion = TRUE, vartype = "ci",na.rm = T)) * 100) %>% 
    dplyr::select(-vas_upp, -vas_low)
)

#----- Deworming
nat_dewormed <- plyr::ldply(w_data_1259_dewormed  %>%  
    srvyr::summarise(dworm = (survey_mean(dwormed == 0, proportion = TRUE, vartype = "ci",na.rm = T)) * 100) %>%
    select(-dworm_upp, -dworm_low)
)  

# Combine for natstats
indianat_stats <- nat_zd1223 %>%
  dplyr::left_join(., nat_vas, by ='.id') %>% 
  dplyr::left_join(., nat_dewormed, by ='.id') %>% 
  dplyr::left_join(., country_regions, by ='.id')

