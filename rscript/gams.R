library(tidyverse)
library(lubridate)
library(data.table)
library(mgcv)
library(mgcViz)
library(tidymv)

#######################
## Errors to fix in LTT:
##    fish_code            tl              sl 
##    Max.   :53.000   Max.   :910.00   Max.   :240.000 
##     - tl and sl are wildly different max values
##     - fish_code doesn't go to 53

## ltt-with-NAs.csv had 35048 rows after filtering
## Shelby's master had 39119 rows after filtering

LTT<-read_csv("TKSTOM-ltt-master.csv", guess_max = 60000) %>% 
  mutate(Net = toupper(Net),
         Comments = toupper(Comments),
         Site = toupper(Site),
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
         prey_cat = case_when(fish_code== 6 ~ "hap",
                              fish_code== 12 ~ "hap",
                              fish_code== 1 ~ "hap",
                              fish_code== 18 ~ "hap",
                              fish_code== 21 ~ "hap",
                              fish_code== 2 ~ "brycinus",
                              fish_code== 7 ~ "nile_perch",
                              fish_code== 9 ~ "tilapia",
                              fish_code== 10 ~ "tilapia bv",
                              fish_code== ~ "",
                              fish_code== ~ "",
                              fish_code== ~ "",
                              ),
         ) %>% 
  filter(!grepl('MUK', Comments),
         !grepl('MUK', Net),
         !grepl('BOUGHT', Net),
         !grepl('BOUGHT', Comments))

NP_CPUE<-LTT %>% 
  filter(Fish_Code == 7,
         !is.na(SL))
  mutate(Site_Type = factor(Site_Type,levels = c("1","2","3","4")),
         dist_shore = factor(dist_shore, levels = c("inshore","offshore"))) #%>% 
  


## Group by distance from shore & site:
##  - NP SL
##  - NP CPUE ensure 0 catch days included
##  - NP mass and mass of daily catch
##  - Hap CPUE
##
##
## LTT gams
##  Should habitat be included initially? Probably yes, but it degrades the predictive ability, which 
##  which is an interesting result.
##
## Predictions: 
##  Descriptive gams as inputs and compare the predicted model to the
##  hap cpue and stomach gams to test goodness of fit (MSE, R^2).
##  



#### Stomach ####
## Models
##  Predition model: GAM of prey ~ s(NP SL) + s(NP abundance) + s(dist from shore)
##  Validation model: GAM of prey ~ te(NP SL, time)
##  prey = (%mass + %number)/2, for each individual
##
## Then predict each data point based on the SL, abundance, in/offshore conditions at the time
##  Compare two GAMs (R^2, MSE)
##
## I'll need a table with columns [time, prey NP, prey HAP, prey other fish, prey invert, NP SL from GAM @ time & in/offshore,
##  NP abundance GAM @ time & in/offshore, in/offshore]
##  The code should be flexible to add site bc that will likely be analyzed too
##

## Replace this with preformatted import in final script

NP<-read_csv("TKSTOM-ltt-master.csv", guess_max = 60000) %>% 
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
                                TRUE ~ Net))  %>% 
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
      group_by(year,month,day,tag,prey_cat,Site_Type,Net,dist_shore) %>% 
      summarise(preymass = sum(preymass),
                preynumber = sum(preynumber),
                SL = mean(SL),
                log2SL = log2(SL),
                SL_cat = case_when(SL <= 14.49375 ~ "small",
                                   SL > 14.49375 ~ "big"))
    return(STOM)} ; STOM <- STOM_fun() ; rm(STOM_fun)
  
  

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

return(STOMGAM)}
preSTOMGAM<-STOMGAM_fun()  ; rm(STOMGAM_fun)


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

  # if I want it binary
STOMGAM<-STOMGAM %>% 
  mutate(cichlid = case_when(cichlid > 0 ~ 1,
                             cichlid == 0 ~ 0),
         invert = case_when(invert > 0 ~ 1,
                            invert == 0 ~ 0),
         other_fish = case_when(other_fish > 0 ~ 1,
                                other_fish == 0 ~ 0),
         nile_perch = case_when(nile_perch > 0 ~ 1,
                                nile_perch == 0 ~ 0)
         )
  
#### Grouped by month, typical IRI #### IRI = (%N+%M)*%F
STOMGAM<-preSTOMGAM %>% 
  group_by(year,month,SL_cat,dist_shore) %>% 
  summarise(cichlid = ((sum(count_cichlid,na.rm = T)/sum(total_preynumber))+(sum(mass_cichlid,na.rm = T)/sum(total_preymass)))*(sum(!is.na(count_cichlid))/length(unique(tag))),
            invert = ((sum(count_invert,na.rm = T)/sum(total_preynumber))+(sum(mass_invert,na.rm = T)/sum(total_preymass)))*(sum(!is.na(count_invert))/length(unique(tag))),
            nile_perch = ((sum(count_nile_perch,na.rm = T)/sum(total_preynumber))+(sum(mass_nile_perch,na.rm = T)/sum(total_preymass)))*(sum(!is.na(count_nile_perch))/length(unique(tag))),
            other_fish = ((sum(count_other_fish,na.rm = T)/sum(total_preynumber))+(sum(mass_other_fish,na.rm = T)/sum(total_preymass)))*(sum(!is.na(count_other_fish))/length(unique(tag))),
            num_fish = length(unique(tag)),
            num_site1 = sum(Site_Type == 1),
            num_site2 = sum(Site_Type == 2),
            num_site3 = sum(Site_Type == 3),
            num_site4 = sum(Site_Type == 4)
            ) %>% 
  unite(col = date_of_capture, year,month, sep = ".",remove = F) %>% 
  mutate(date_of_capture = ymd(date_of_capture, truncated = 1),
         num_date_of_capture = as.numeric(ymd(date_of_capture)),
         weight = num_fish/mean(num_fish)) ; STOMGAM<-data.table(STOMGAM) %>% 
  mutate(# Site_Type = factor(Site_Type,levels = c("1","2","3","4")),
         dist_shore_fac = factor(dist_shore, levels = c("inshore","offshore")),
         SL_cat = factor(SL_cat))
  

## Stomach GAMs ##
# H is an interesting parameter for gam(): it supplies a
# penaly matrix to force model terms to certain values.
#
# For selecting k, use the best k from the LTT GAMs and compare to
# k auto selected by the MGCV algorithms through AIC.
#
# good tutorial on mcgvViz: https://mfasiolo.github.io/mgcViz/articles/mgcviz.html

stom_gam<-gam(cichlid ~ s(num_date_of_capture, by = interaction(SL_cat,dist_shore),bs="cr", k=15),
              #sp = array(c(10,10,10,10)),
              #family = binomial(),
              data=STOMGAM)
              

stom_gam<-gam(
  list(
    cichlid ~ s(num_date_of_capture, by = interaction(SL_cat,dist_shore), bs="cr", k=15),
    invert ~  s(num_date_of_capture, by = interaction(SL_cat,dist_shore), bs="cr", k=15)),
    #other_fish ~ s(num_date_of_capture,by = SL_cat,k=100),
    #nile_perch ~ s(num_date_of_capture,by = SL_cat,k=100)),
  family=mvn(d=2), fit = FALSE,
  data=STOMGAM)


stom_gam<-gam(list(
  cichlid ~ s(num_date_of_capture,SL,by = dist_shore,k=15),
  invert ~ s(num_date_of_capture,SL,by = dist_shore,k=15)),
  #other_fish ~ s(num_date_of_capture,SL,by = dist_shore,k=15),
  #nile_perch ~ s(num_date_of_capture,SL,by = dist_shore,k=15)),
  family=mvn(d=2),data=STOMGAM)
  
  
stom_gam
summary(stom_gam)
gam.check(stom_gam)

plot.gamViz(stom_gam,n2 = 60) +
  geom_vline(xintercept = as.numeric(ymd("2017-09",truncated = 1)), linetype = "dotted")

check1D(stom_gam,"date_of_capture")
solve(crossprod(stom_gam$family$data$R)) #cov matrix
AIC(stom_gam,stom_gam1)

# Plotting on actual data. Do this before plotting anything!
model_p <- data.table(predict.gam(stom_gam,STOMGAM))
model_p<-cbind(model_p,STOMGAM[c("date_of_capture","dist_shore","SL_cat")])

#cichlid
ggplot(STOMGAM, aes(date_of_capture)) +
  geom_point(aes(y=cichlid)) +
  geom_point(data = model_p, aes(y=V1,alpha = .5,colour = "red")) +
  theme(panel.border = element_rect(colour = "grey1",fill = NA),panel.background = element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank(),axis.text = element_text(size = 10),axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Cichlid IRI")+
  geom_vline(xintercept = ymd("2017-09",truncated = 1), linetype = "dotted") + 
  facet_grid(cols = vars(dist_shore), rows = vars(SL_cat))

#invert
stom_gam<-gam(invert ~ s(num_date_of_capture, by = interaction(SL_cat,dist_shore),bs="cr", k=15),
              sp = array(c(10,10,10,10)),
              data=STOMGAM,weights = weight)

ggplot(STOMGAM, aes(date_of_capture)) +
  geom_point(aes(y=invert,alpha = weight)) +
  geom_point(data = model_p, aes(y=V1,alpha = .5,colour = "red")) +
  theme(panel.border = element_rect(colour = "grey1",fill = NA),panel.background = element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank(),axis.text = element_text(size = 10),axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Invert IRI")+
  geom_vline(xintercept = ymd("2017-09",truncated = 1), linetype = "dotted") + 
  facet_grid(cols = vars(dist_shore), rows = vars(SL_cat))



