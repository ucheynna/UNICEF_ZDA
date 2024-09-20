
#Create weighted lists according to age ranges


#Children 12-59m 
w_data_1259_dtp1 <- lapply(ir, function(x){ x <- x %>% 
  filter(youngest_1259m == 1) %>% 
  as_survey_design(id = v001, strata =NULL, weights = v005, nest=T); return(x)})

#Children 12-23m
w_data_1223_dtp1 <- lapply(ir, function(x){ x <- x %>% 
  filter(youngest_1223m == 1) %>% 
  as_survey_design(id = v001, strata =NULL, weights = v005, nest=T); return(x)})


#Children 6-59m 
w_data_0659_vas6 <- lapply(ir, function(x){ x <- x %>% 
  filter(!is.na(vas6)) %>% 
  filter(youngest_0659m == 1) %>% 
  as_survey_design(id = v001, strata =NULL, weights = v005, nest=T); return(x)})

#Children 12-59m 
w_data_1259_dewormed <- lapply(ir, function(x){ x <- x %>% 
  filter(youngest_1259m == 1) %>% 
  filter(!is.na(dwormed)) %>% 
  as_survey_design(id = v001, strata =NULL, weights = v005, nest=T); return(x)})

