library(tidyverse)
library(lubridate)

#######################
## Errors to fix in LTT:
##    fish_code            tl              sl 
##    Max.   :53.000   Max.   :910.00   Max.   :240.000 
##     - tl and sl are wildly different max values
##     - fish_code doesn't go to 53

## ltt-with-NAs.csv had 35048 rows after filtering
## Shelby's master had 39119 rows after filtering

LTT<-read_csv("ltt-master.csv") %>% 
  mutate(Net = toupper(Net),
         Comments = toupper(Comments),
         Site = toupper(Site),
         cpue_id = paste(`Date (DD-Mon-YY)`,Net,sep = "-"), # probably change colname for publication
         np_id = paste(`Date (DD-Mon-YY)`,Tag_Num),
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
  filter(!grepl('MUK', Comments),
         !grepl('MUK', Net),
         !grepl('BOUGHT', Net),
         !grepl('BOUGHT', Comments))


## Group by distance from shore:
##  - NP SL
##  - NP CPUE ensure 0 catch days included
##  - NP mass and mass of daily catch
##  - Hap CPUE
##
## LTT gams
##  Should habitat be included initially? Probably yes, but it degrades the predictive ability, which 
##  which is an interesting result.
##
## Predictions: 
##  Descriptive gams as inputs and compare the predicted model to the
##  hap cpue and stomach gams to test goodness of fit (MSE, R^2).
##  


#### NP SL ####

NP<-LTT %>% 
  filter(Fish_Code==7,!is.na(SL))  #fix these missing SL values! Also missing tags

# number of NP per month
inter<-NP %>% 
  group_by(Year,Month) %>% 
  count(Tag_Num)
counts<-inter %>% 
  group_by(Year,Month) %>% 
  sum(n)
rm(inter);view(counts)

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
  filter(!grepl('MUK', Comments),
         !grepl('MUK', Net),
         !grepl('BOUGHT', Net),
         !grepl('BOUGHT', Comments),
         Fish_Code == 7)

STOM<-read_csv("stomachs.csv",guess_max = 60000) %>% 
  mutate(tag = as.character(tag),
         preynumber = case_when(
           preynumber == "many" ~ "20", # do regression for final models
           TRUE ~ preynumber),
         preynumber = as.numeric(preynumber)
         ) %>% 
  filter(!is.na(preynumber))

STOM<-inner_join(STOM,NP, by = c("year","month","day","tag")) %>% # some to fix but pretty good
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
    grepl("barb", prey) ~ "muk_or_barb",
    prey == "mukene" ~ "muk_or_barb",
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
  group_by(year,month,day,tag,prey_cat,Site_Type,Net,dist_shore) %>% 
  summarise(preymass = sum(preymass),
            preynumber = sum(preynumber),
            SL = mean(SL))


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
  
STOMGAM<-inner_join(STOMMASS,STOMCOUNT, by = c("year","month","day","tag","Site_Type","Net","dist_shore","SL"))
STOMGAM<-inner_join(STOMGAM,STOMTOTAL,by = c("year","month","day","tag")); rm(STOMTOTAL,STOMCOUNT,STOMMASS)


STOMGAM<-STOMGAM %>% 
  group_by(year,month,day,tag,Site_Type,Net,dist_shore,SL) %>% 
  summarise(cichlid = ((mass_cichlid/total_preymass)+(count_cichlid/total_preynumber))/2,
            invert = ((mass_invert/total_preymass)+(count_invert/total_preynumber))/2,
            nile_perch = ((mass_nile_perch/total_preymass)+(count_nile_perch/total_preynumber))/2,
            other_fish = ((mass_other_fish/total_preymass)+(count_other_fish/total_preynumber))/2,
            muk_or_barb = ((mass_muk_or_barb/total_preymass)+(count_muk_or_barb/total_preynumber))/2) %>% 
  unite(col = date_of_capture, year,month,day, sep = ".") %>% 
  mutate(cichlid = if_else(is.na(cichlid),0,cichlid),
         invert = if_else(is.na(invert),0,invert),
         nile_perch = if_else(is.na(nile_perch),0,nile_perch),
         other_fish = if_else(is.na(other_fish),0,other_fish),
         muk_or_barb = if_else(is.na(muk_or_barb),0,muk_or_barb),
         date_of_capture = as.numeric(ymd(date_of_capture)))
  

## Stomach GAM ##
# good tutorial on mcgvViz: https://mfasiolo.github.io/mgcViz/articles/mgcviz.html
stom_gam<-gam(cichlid ~ s(date_of_capture,SL, by = dist_shore), data=STOMGAM)
  
  gam(list(
  cichlid ~ s(date_of_capture,SL, by = dist_shore),
  invert ~ s(date_of_capture,SL, by = dist_shore),
  nile_perch ~ s(date_of_capture,SL, by = dist_shore),
  other_fish ~ s(date_of_capture,SL, by = dist_shore),
  muk_or_barb ~ s(date_of_capture,SL, by = dist_shore)
  ),
  family=mvn(d=5),data=STOMGAM)

## Example 2 response GAM
library(mgcv)
library(mgcViz)
## simulate some data...
V <- matrix(c(2,1,1,2),2,2)
f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * 
  (10 * x)^3 * (1 - x)^10
n <- 300
x0 <- runif(n);x1 <- runif(n);
x2 <- runif(n);x3 <- runif(n)
y <- matrix(0,n,2)
for (i in 1:n) {
  mu <- c(f0(x0[i])+f1(x1[i]),f2(x2[i]))
  y[i,] <- rmvn(1,mu,V)
}
dat <- data.frame(y0=y[,1],y1=y[,2],x0=x0,x1=x1,x2=x2,x3=x3) %>% 
  mutate(cat = as.factor(case_when(x0 <= 0.5 ~ "yes",
                         x0 > 0.5 ~ "no")))

## fit model...
b <- gam(y0~s(x0,x1,by = cat),data = dat)
b <- gam(list(y0~s(x0,by = cat),y1~te(x2,x3)),family=mvn(d=2),data=dat)
b
summary(b)
plot.gamViz(b,pages=1)
solve(crossprod(b$family$data$R)) ## estimated cov matrix





