#run new rds and then script 3 weighted.
#load country data(priority countries)

country_regions <- read.csv("country_selection.csv")
country_regions <- country_regions %>% 
  dplyr::select(.id, region, CountryName) %>% 
  dplyr::filter(.id!="")


#----- Zero dose
nat_zd1223 <- plyr::ldply(w_data_1223_dtp1, function(x){ x %>%  
    srvyr::summarise(zd1223 = (survey_mean(zd == 1, proportion = TRUE, vartype = "ci",na.rm = T)) * 100) %>% 
    dplyr::select(-zd1223_upp, -zd1223_low)
})

#----- VAS
nat_vas <- plyr::ldply(w_data_0659_vas6, function(x){ x %>%  
    srvyr::summarise(vas = (survey_mean(vas6 == 0, proportion = TRUE, vartype = "ci",na.rm = T)) * 100) %>% 
    dplyr::select(-vas_upp, -vas_low)
})

#----- Deworming
nat_dewormed <- plyr::ldply(w_data_1259_dewormed, function(x){ x %>%  
    srvyr::summarise(dworm = (survey_mean(dwormed == 0, proportion = TRUE, vartype = "ci",na.rm = T)) * 100) %>%
    select(-dworm_upp, -dworm_low)
})  

# merge figures by ".id"
nat_stats <- nat_zd1223 %>%
  dplyr::left_join(., nat_vas, by ='.id') %>% 
  dplyr::left_join(., nat_dewormed, by ='.id') %>% 
  dplyr::left_join(., country_regions, by ='.id')


