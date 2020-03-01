library(tidyverse)
library(lubridate)
setwd("C:/Users/tk/Dropbox/Chapman lab/TK and Ronny/2nd data analysis/Stomachs/Data/")


ltt_val<-read_csv("validation ltt.csv") %>% 
  filter(!grepl('Muk', net),
         !grepl('Muk', comments)) %>% 
  mutate(month = match(month,month.abb),
         day = day(date),
         dist_shore = case_when(net == "5" ~ "inshore",
                                net == "20" ~ "inshore",
                                net == "100" ~ "offshore",
                                net == "Mid" ~ "offshore",
                                TRUE ~ net))

np_val<-ltt_val %>% 
  filter(fish_code==7,!is.na(sl),
         sl<=50) %>% 
  mutate(date_num = as.numeric(date),
         cpue_id = paste(year,month,day,site_type,net,sep = "-")) 

hap_val<-ltt_val %>% 
  filter(
    fish_code==6|
      fish_code==12|
      fish_code==1|
      fish_code==18|
      fish_code==21) %>% 
  mutate(cpue_id = paste(year,month,day,site_type,net,sep = "-")) %>%
  mutate(sl = case_when(is.na(sl)~(as.numeric(tl)/1.13051),
                        TRUE~as.numeric(sl)),
         tl = case_when(is.na(tl)~(as.numeric(sl)*1.13051),
                        TRUE~as.numeric(tl)))

cpue_val_function<-function() {
  
  np_cue_val<-np_val %>% 
    group_by(site_type,dist_shore) %>% 
    count(cpue_id)   #count() does both number of id's and groups by id, so does give the correct cpue
  
  
  np_sl<-np_val %>% 
    group_by(site_type,dist_shore,cpue_id) %>% 
    summarise(np_sl = mean(sl))
  np_sl<-data.table(np_sl) %>% 
    mutate(site_type = as.factor(site_type),
           dist_shore = as.factor(dist_shore),
           site_type = case_when(site_type == 1~"one",
                                 site_type == 2~"two",
                                 site_type == 3~"three",
                                 site_type == 4~"four"))
  
  hap_cue_val<-hap_val %>%
    group_by(site_type,dist_shore) %>% 
    count(cpue_id)    #count() does both number of id's and groups by id, so does give the correct cpue
  
  cpue_val<-full_join(hap_cue_val,np_cue_val,by=c('cpue_id','site_type','dist_shore')) %>% 
    mutate(n.y=replace_na(n.y,0),
           n.x=replace_na(n.x,0))
  colnames(cpue_val)[4] <- "hap_cpue"
  colnames(cpue_val)[5] <- "np_cpue"
  
  
  remove(hap_cue_val,np_cue_val)

  
  cpue_val_date<-str_split(cpue_val$cpue_id,"-",n = 4, simplify = TRUE) %>% 
    data.frame() %>% 
    mutate(date = paste(X1,X2,X3,sep = "-"))
  
  
  cpue_val_date<-cpue_val_date[,4:5]
  cpue_val<-cbind.data.frame(cpue_val_date,cpue_val)
  cpue_val<-cpue_val[,2:7] 
  cpue_val<-cpue_val %>% 
    mutate(date = as.Date(date,"%Y-%m-%d"))
  cpue_val<-cpue_val %>% 
    mutate(date_num = as.numeric(date))
  
  biomass_np<-aggregate(mass~cpue_id,data = np_val,FUN=sum)
  
  cpue_val<-left_join(cpue_val,biomass_np,by="cpue_id") %>% 
    mutate(mass=replace_na(mass,0))
  colnames(cpue_val)[8]<-"np_biomass"
  remove(biomass_np)
  
  cpue_val<-cpue_val %>% mutate(hap_forlog = 
                          case_when(hap_cpue == 0 ~ 0.5,TRUE~as.numeric(hap_cpue)),
                          np_forlog = 
                            case_when(np_cpue == 0 ~ 0.5,TRUE~as.numeric(np_cpue)),
                        np_massforlog =
                          case_when(np_biomass == 0 ~ 2,TRUE~as.numeric(np_biomass))) %>% 
    mutate(logtwo_npmass = log2(np_massforlog),
           logtwo_hapcpue = log2(hap_forlog),
           logtwo_npcpue = log2(np_forlog),
           site_type = case_when(site_type == 1~"one",
                                 site_type == 2~"two",
                                 site_type == 3~"three",
                                 site_type == 4~"four")) %>% 
    mutate(site_type = as.factor(site_type),
           dist_shore = as.factor(dist_shore))
 
  drop<-c("hap_forlog","np_forlog","np_massforlog")
  cpue_val<-cpue_val[,!names(cpue_val) %in% drop] 

  cpue_val<-full_join(cpue_val,np_sl,by=c("site_type","dist_shore","cpue_id"))
  
  cpue_val<cpue_val %>% 
    mutate(np_sl=replace_na(np_sl,0))
   
  return(cpue_val)
}



cpue_val<-cpue_val_function()
remove(cpue_val_function)

##fitting validation sl gam, may need to include last bit of test data to get gam to fit better.?
np_val<-np_val %>% 
  mutate(dist_shore = as.factor(dist_shore))

gam_np_sl_val <- gam(sl ~ s(date_num)+
                       s(dist_shore,bs="re"),
                 data = np_val,
                 family = quasipoisson)
plot(gam_np_sl_val, shade = TRUE)
fitval<-data.table(gam_np_sl_val$fitted.values)
colnames(fitval)[1]<-"np_sl_gam"
np_v<-bind_cols(np_val,fitval)
date_num<-data.table(cpue_val$date_num)
colnames(date_num)[1]<-"date_num"
fitval<-data.table(predict(gam_np_sl_val,cpue_val))
colnames(fitval)[1]<-"np_sl_gam"
cpue_val<-bind_cols(cpue_val,fitval)

ggplot(np_v, aes(date)) +
  geom_point(aes(y=sl,alpha = .05)) +
  geom_point(aes(y=np_sl_gam,color=dist_shore))+
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_line(colour = "grey90"),panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),axis.text = element_text(size = 10),axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "NP SL")





logmass_gam<-gam(logtwo_npmass ~ s(date_num)+
                   s(dist_shore,bs="re"),
                 data = cpue_val,
                 family = quasipoisson)
fitval<-data.table(logmass_gam$fitted.values)
colnames(fitval)[1]<-"logmass_gam"
cpue_val<-bind_cols(cpue_val,fitval)

ggplot(cpue_A, aes(date)) +
  geom_point(aes(y=logtwo_npmass,alpha = .05))+
  geom_point(aes(y=logmass_gam,alpha = .05,color=dist_shore)) +
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_line(colour = "grey90"),panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),axis.text = element_text(size = 10),axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "log_NP_mass")
