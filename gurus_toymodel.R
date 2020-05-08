############################
#### Preprocessing data ####
############################
library(lme4)
library(tidyverse)
library(lubridate)

####Stomach####
NP<-read_csv("ltt-master.csv", guess_max = 60000) %>% 
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
                                TRUE ~ Net)) %>% 
  #Site_Type = factor(Site_Type,levels = c("1","2","3","4")),
  #dist_shore = factor(dist_shore, levels = c("inshore","offshore"))) %>% 
  
  filter(!grepl('MUK', Comments),
         !grepl('MUK', Net),
         !grepl('BOUGHT', Net),
         !grepl('BOUGHT', Comments),
         Fish_Code == 7,
         !is.na(SL),
         SL <= 50)
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
  return(STOM)} ; 
STOM <- STOM_fun() ; rm(STOM_fun)

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
         num_date_of_capture = as.numeric(ymd(date_of_capture)))
STOMGAM<-data.table(STOMGAM) %>% 
  mutate(Site_Type = factor(Site_Type,levels = c("1","2","3","4")),
         dist_shore = factor(dist_shore, levels = c("inshore","offshore")),
         SL_cat = factor(SL_cat))

####LTT####

NP<-read_csv("ltt-master.csv", guess_max = 60000) %>% 
  mutate(date = dmy(`Date (DD-Mon-YY)`),
         year = year(dmy(`Date (DD-Mon-YY)`)),
         month = month(dmy(`Date (DD-Mon-YY)`)),
         day = day(dmy(`Date (DD-Mon-YY)`)),
         tag = as.character(Tag_Num),
         Net = toupper(Net),
         Comments = toupper(Comments),
         Site = toupper(Site),
         cpue_id = paste(`Date (DD-Mon-YY)`,Net,sep = "-"), # probably change colname for publication
         np_id = paste(`Date (DD-Mon-YY)`,Tag_Num),
         SL = as.numeric(SL),
         Site_Type = factor(Site_Type,levels = c("1","2","3","4")),
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
        dist_shore = factor(dist_shore, levels = c("inshore","offshore"))) %>% 
  filter(!grepl('MUK', Comments),
         !grepl('MUK', Net),
         !grepl('BOUGHT', Net),
         !grepl('BOUGHT', Comments),
         Fish_Code == 7,
         !is.na(SL),
         SL <= 50) #%>% 

  

############################
#### Fitting model #########
############################

p1 <- ggplot(NP,aes(date)) +
  geom_point(aes(y=SL),shape = 1,size = 1.5) +
  geom_smooth(aes(y=SL), se = F, formula = y ~ s(x, bs = "cs"))

p1 

p1 + xlim(ymd("2017-01-01"), "2019-08-01") +
  geom_vline(xintercept = ymd("2017-09",truncated = 1), linetype = "dotted")


#### Moving regression ####
## using 1 month cycle with 3 month : 10 month short : long term regression

NP_REGRESSION <- NP %>% 
  filter(date >= ymd("2017-01-01")) %>% 
  unite(col = id, year, month, sep = ".",remove = F)

# initial ggplot objects
p <- ggplot(NP_REGRESSION, aes(date)) + 
  geom_point(aes(y=SL),shape = 1,size = 1.5)
g <- ggplot(NP_REGRESSION, aes(date)) + 
  geom_point(aes(y=SL),shape = 1,size = 1.5)
individual <- list() ; i = 1

#month <- "2018.5"

# fit linear models
for(month in unique(NP_REGRESSION$id)){

  predict_date <- ymd(month,truncated = 1)
  current_date <- ymd(month,truncated = 1)
  short_date <- ymd(month,truncated = 1)
  long_date <- ymd(month,truncated = 1)
  
  month(predict_date) <- month(predict_date) +3
  month(current_date) <- month(current_date) +1
  month(short_date) <- month(short_date) -2
  month(long_date) <- month(long_date) -9

  
  SHORT <- NP_REGRESSION %>% 
    filter(date >= short_date, date < current_date)
  LONG <- NP_REGRESSION %>% 
    filter(date >= long_date, date < current_date)
  PREDICT <- NP_REGRESSION %>% 
    filter(date > current_date, date < predict_date)
  

  short_lm <- lm(SL ~ date, data = SHORT)
  long_lm <- lm(SL ~ date, data = LONG)
  
  SHORT_PRED <- cbind(SHORT, predict(short_lm, interval = 'confidence'))
  LONG_PRED <- cbind(LONG, predict(long_lm, interval = 'confidence'))
  
  p <- p + geom_line(data = LONG_PRED,aes(date, fit), color = "blue", size = 1.25) +
    geom_line(data = SHORT_PRED, aes(date, fit), color = "lightgreen", size = 1.25)
    #geom_ribbon(data = LONG_PRED,aes(ymin=lwr,ymax=upr), alpha=0.01) +
    #geom_ribbon(data = SHORT_PRED,aes(ymin=lwr,ymax=upr), alpha=0.01)
   
  # for individual plots
  pred_target <- mean(PREDICT$SL)
  month(predict_date) <- month(predict_date) -1
  
  r <- g  + geom_line(data = LONG_PRED,aes(date, fit), color = "blue", size = 1.25) +
    geom_line(data = SHORT_PRED, aes(date, fit), color = "lightgreen", size = 1.25) 
  individual[[i]] <- r
  i = i + 1
}

# time of ban
individual[4:10]

# during no fishing
individual[11:18]

# crash
individual[19:26]

#### scratch work ####
toymodel<-lm(cichlid ~ dist_shore + SL + num_date_of_capture,data = STOMGAM)# + num_site1 + num_site2 + num_site3 + num_site4
summary(toymodel)
plot(toymodel)

toymodel1<-lm(cichlid ~ dist_shore + SL + num_date_of_capture + num_date_of_capture:dist_shore,data = STOMGAM)
summary(toymodel1)
plot(toymodel1)

STOMGAM_2016<-STOMGAM %>% 
  filter(date_of_capture >= as.Date('2016-01-01') & date_of_capture < as.Date('2017-01-01'))



toymodel_16<-lm(cichlid ~ dist_shore + SL + num_date_of_capture,data = STOMGAM_2016)
summary(toymodel_16)
plot(toymodel)


