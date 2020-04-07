capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

ltt<-read_csv("ltt-with-NAs.csv") %>% 
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

# Get number of samples for group


intermediate<-ltt %>% 
  filter(fish_code==7) %>% 
  group_by(year,month) %>% 
  count(year)
intermediate %>%
  group_by(year) %>% 
  count(year)



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

