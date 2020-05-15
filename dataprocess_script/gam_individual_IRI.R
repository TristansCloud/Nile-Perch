library(tidyverse)
library(lubridate)

# STOM<- read_csv("STOM")

STOMGAM_fun<-function(){
  STOMMASS<-STOM %>% 
    select(-preynumber) %>% 
    pivot_wider(names_from = prey_cat, values_from = preymass, names_prefix = "mass_")
  
  STOMCOUNT<-STOM %>% 
    select(-preymass) %>% 
    pivot_wider(names_from = prey_cat, values_from = preynumber, names_prefix = "count_")
  
  STOMTOTAL<-STOM %>% 
    group_by(year,month,day,tag) %>% 
    summarise(total_preymass = sum(preymass),
              total_preynumber = sum(preynumber))
  
  STOMGAM<-inner_join(STOMMASS,STOMCOUNT, by = c("year","month","day","tag","Site_Type","Net","dist_shore","SL","log2SL","SL_cat"))
  STOMGAM<-inner_join(STOMGAM,STOMTOTAL,by = c("year","month","day","tag")); rm(STOMTOTAL,STOMCOUNT,STOMMASS)
  
  return(STOMGAM)} ; preSTOMGAM<-STOMGAM_fun()  ; rm(STOMGAM_fun)

#### Frequency grouped by month ####


#### Grouped by individual fish ####
STOMGAM<-preSTOMGAM %>% 
  group_by(year,month,day,tag,Site_Type,Net,dist_shore,SL,log2SL,SL_cat) %>% 
  summarise(cichlid = (sum(mass_cichlid)/sum(total_preymass)+sum(count_cichlid)/mean(total_preynumber))/2,
            invert = (sum(mass_invert)/mean(total_preymass)+sum(count_invert)/mean(total_preynumber))/2,
            nile_perch = (sum(mass_nile_perch)/mean(total_preymass)+sum(count_nile_perch)/mean(total_preynumber))/2,
            other_fish = (sum(mass_other_fish)/mean(total_preymass)+sum(count_other_fish)/mean(total_preynumber))/2
  ) %>% 
  unite(col = date_of_capture, year,month,day, sep = ".") %>% 
  mutate(cichlid = if_else(is.na(cichlid),0,cichlid),
         invert = if_else(is.na(invert),0,invert),
         nile_perch = if_else(is.na(nile_perch),0,nile_perch),
         other_fish = if_else(is.na(other_fish),0,other_fish),
         #muk_or_barb = if_else(is.na(muk_or_barb),0,muk_or_barb),
         date_of_capture = ymd(date_of_capture),
         num_date_of_capture = as.numeric(ymd(date_of_capture))) ; STOMGAM<-data.table(STOMGAM) %>% 
  mutate(Site_Type = factor(Site_Type,levels = c("1","2","3","4")),
         dist_shore = factor(dist_shore, levels = c("inshore","offshore")),
         SL_cat = factor(SL_cat))