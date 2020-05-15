library(tidyverse)
library(tidyverse)

# NP <- read_csv("NP")

STOM_fun<-function(){
  STOM<-read_csv("stomachs.csv",guess_max = 60000) %>% 
    mutate(prey_cat = case_when(
      prey == "annelid worm" ~ "invert",
      prey == "ants" ~ "invert",
      grepl("chaoborus", prey) ~ "invert",
      grepl("chironomid", prey) ~ "invert",
      prey == "corixidae adult" ~ "invert",
      grepl("diptera", prey) ~ "invert",
      prey == "ephemeroptera larvae" ~ "invert",
      prey == "hymenoptera" ~ "invert",
      prey == "leech" ~ "invert",
      prey == "odonata larvae" ~ "invert",
      prey == "uip" ~ "invert",
      grepl("cichlid", prey) ~ "cichlid",
      prey == "hap" ~ "cichlid",
      grepl("barb", prey) ~ "other_fish",
      prey == "mukene" ~ "other_fish",
      prey == "mastacembelus" ~ "other_fish",
      prey == "clarias" ~ "other_fish",
      prey == "ufp" ~ "other_fish",
      prey == "nile perch" ~ "nile_perch",
      prey == "parasitoid worm" ~ "extra",
      prey == "vegitation" ~ "extra",
      prey == "rope" ~ "extra",
      prey == "unidentified" ~ "extra",
      prey == "rocks" ~ "extra",
      TRUE ~ prey)) %>% 
    filter(prey_cat != "extra") %>% 
    mutate(tag = as.character(tag),
           preynumber = case_when(
             preynumber == "many" ~ "20", # do regression for final models
             TRUE ~ preynumber),
           preynumber = as.numeric(preynumber)
    ) %>% 
    filter(!is.na(preynumber))
  
  STOM<-inner_join(STOM,NP, by = c("year","month","day","tag")) %>% # some to fix but pretty good
    group_by(year,month,day,tag,prey_cat,Site_Type,Net,dist_shore) %>% 
    summarise(preymass = sum(preymass),
              preynumber = sum(preynumber),
              SL = mean(SL),
              log2SL = log2(SL),
              SL_cat = case_when(SL <= 14.49375 ~ "small",
                                 SL > 14.49375 ~ "big"))
  return(STOM)} ; STOM <- STOM_fun() ; rm(STOM_fun)

