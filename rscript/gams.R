
ltt<-read_csv("C:/Users/tk/Dropbox/Chapman lab/TK and Ronny/2nd data analysis/Stomachs/Data/good_cols_ltt.csv") %>% 
  mutate(net = toupper(net),
         comments = toupper(comments),
         site = capwords(site),
         cpue_id = paste(year,month,day,site_type,net,sep = "-"),
         id = paste(year,month,day,tag),
         dist_shore = case_when(net == "5" ~ "inshore",
                                net == "20" ~ "inshore",
                                net == "100" ~ "offshore",
                                net == "20 EXTRA" ~ "inshore",
                                net == "100 EXTRA" ~ "offshore",
                                net == "5 EXTRA" ~ "inshore",
                                net == "MID" ~ "offshore",
                                net == "21 EXTRA" ~ "inshore",
                                net == "22 EXTRA" ~ "inshore",
                                net == "101 EXTRA" ~ "offshore",
                                TRUE ~ net)) %>% 
  filter(!grepl('MUK', comments),
         !grepl('MUK', net),
         !grepl('BOUGHT', net),
         !grepl('BOUGHT', comments))


## Group by distance from shore:
##  - NP SL
##  - NP abundance
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

