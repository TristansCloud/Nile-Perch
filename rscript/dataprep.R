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

## Which stomach data I have:
intermediate<-stom %>% 
  group_by(year, month) %>% 
  count(tag)

data_I_have<-intermediate %>%  # need the intermediate to list the unique tags per year, then count them
  group_by(year, month) %>% 
    count(year)

write_csv(data_I_have, path = "Stomachs_tristan_has.csv")

for(i in sort(unique(paste(stom$year, stom$month)))){
  print(i)
  print(count(i))
}

ltt<-read_csv("ltt-with-NAs.csv")
np<-read_csv("ltt-with-NAs.csv") %>% 
  filter(fish_code == 7)

codes<-unique(ltt$fish_code)
for(i in codes){
  str(i)
}

matched<-left_join(stom,ltt,by = c("year","month","day","tag"))

NAs<-matched %>% 
  filter(is.na(fish_code) | is.na(tl) | is.na(sl)
         | is.na(stomach) | is.na(sex) | is.na(mass)
         | is.na(site_type))

write_csv(NAs, path = "fixing-NAs/entries-with-NA.csv", na = "")

ggplot(data = np) +
  stat_bin(aes(x = date), binwidth = 30)
