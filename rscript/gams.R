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
         !grepl('BOUGHT', Comments),
         Fish_Code == 7)

NP<-NP %>% 
  mutate(year = year(dmy(`Date (DD-Mon-YY)`)),
         month = month(dmy(`Date (DD-Mon-YY)`)),
         day = day(dmy(`Date (DD-Mon-YY)`)),
         tag = as.character(Tag_Num))

STOM<-read_csv("stomachs.csv",guess_max = 60000) %>% 
  mutate(tag = as.character(tag))

JOINED_STOM<-inner_join(STOM,NP, by = c("year","month","day","tag")) # some to fix but pretty good

JOINED_STOM %>% spread(
  key = "prey",
  value = "preynumber"
)



# this group by doesnt work
  group_by(year,month,day,tag) %>% 
  summarise(np_sl = mean(SL),
            preynumber = sum(preynumber),
            preymass = sum(preymass),
            cichlid_count = if_else(first(prey == c("cichlid","hap","tilapiine cichlid"), sum(preynumber),0)))
                                          
              
              sum(preynumber[prey == "cichlid"]))
              ifelse( prey == "cichlid", preynumber, 0 ))
              preynumber[prey %in% c("cichlid","hap","tilapiine cichlid")]))
# May have to use gather & spread 

iris %>% 
  group_by(Species) %>% 
  summarise(pwz = if_else(first(Species == "setosa")
                          , sum(Petal.Width)
                          , mean(Petal.Width)))

## Example 2 response GAM
b <- gam(list(y0~s(x0)+s(x1),y1~s(x2)+s(x3)),family=mvn(d=2),data=dat)
b
summary(b)
plot(b,pages=1)
solve(crossprod(b$family$data$R))





