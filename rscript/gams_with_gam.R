library(tidyverse)
library(lubridate)
library(data.table)
library(gam)

LTT <- read_csv("Nile-Perch/TKSTOM-ltt-master.csv", guess_max = 60000) %>%
  mutate(year = year(dmy(`Date (DD-Mon-YY)`)),
         month = month(dmy(`Date (DD-Mon-YY)`)),
         day = day(dmy(`Date (DD-Mon-YY)`)),
         tag = as.character(Tag_Num),
         Net = toupper(Net),
         Comments = toupper(Comments),
         Site = toupper(Site),
         cpue_id = paste(`Date (DD-Mon-YY)`,Net,sep = "-"), # probably change colname for publication
         np_id = paste(`Date (DD-Mon-YY)`,Tag_Num),
         SL = as.numeric(SL),
         dist_shore = case_when(Net == "5" ~ "inshore",
                                Net == "20" ~ "inshore",
                                Net == "100" ~ "offshore",
                                Net == "20 EXTRA" ~ "inshore",
                                Net == "100 EXTRA" ~ "offshore",
                                Net == "5 EXTRA" ~ "inshore",
                                Net == "MID" ~ "offshore",
                                Net == "21 EXTRA" ~ "inshore",
                                Net == "22 EXTRA" ~ "inshore",
                                Net == "101 EXTRA" ~ "offshore",
                                TRUE ~ Net),
         prey_cat = case_when(Fish_Code == 6 ~ "hap",
                              Fish_Code == 12 ~ "hap",
                              Fish_Code== 1 ~ "hap",
                              Fish_Code== 18 ~ "hap",
                              Fish_Code== 21 ~ "hap",
                              Fish_Code== 2 ~ "brycinus",
                              Fish_Code== 7 ~ "nile_perch",
                              Fish_Code== 9 ~ "tilapia",
                              Fish_Code== 10 ~ "tilapia bv")) %>%
  filter(!grepl('MUK', Comments),
         !grepl('MUK', Net),
         !grepl('BOUGHT', Net),
         !grepl('BOUGHT', Comments))

NP<-LTT %>% 
  filter(Fish_Code == 7,
         !is.na(SL),
         SL <= 50) 

STOM_fun<-function(){
  STOM<-read_csv("Nile-Perch/stomachs.csv",guess_max = 60000) %>% 
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
      prey == "ufp" ~ "unidentified_fish",
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
    group_by(year,month,day,tag,prey,Site_Type,Net,dist_shore) %>% 
    summarise(preymass = sum(preymass),
              preynumber = sum(preynumber),
              SL = mean(SL),
              log2SL = log2(SL),
              SL_cat = case_when(SL <= 14.49375 ~ "small",
                                 SL > 14.49375 ~ "big"))
  return(STOM)} ; STOM <- STOM_fun() ; rm(STOM_fun)


