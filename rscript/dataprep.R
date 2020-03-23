##############################################
#
# Keep track of changes in NA file, highlight changes 
# and send to Shelby.
#
# There are also a bunch of 2008, 2011, and 2012 stomachs that I haven't entered, 
# but some counted the # of prey items differently. 
#
# Issues:
#   Occurance method varies (many vs actual count vs 1 instead of many)
#
#
##############################################

library(tidyverse)

stom<-read_csv("stomachs.csv")
ltt<-read_csv("ltt-with-NAs.csv.csv")

matched<-left_join(stom,ltt,by = c("year","month","day","tag"))

NAs<-matched %>% 
  filter(is.na(fish_code) | is.na(tl) | is.na(sl)
         | is.na(stomach) | is.na(sex) | is.na(mass)
         | is.na(site_type))

write_csv(NAs, path = "fixing-NAs/entries-with-NA.csv", na = "")
