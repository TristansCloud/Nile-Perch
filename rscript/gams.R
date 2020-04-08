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
## Descriptive gams
##  Should habitat be included initially? I vote no because the shoreline was changing, 
##  and reconstructing that is beyond the scope of the paper. 
##
## Predictions: 
##  Descriptive gams as inputs and compare the predicted model to the
##  hap cpue and stomach gams to test goodness of fit (MSE, R^2).
##  


## NP SL

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
