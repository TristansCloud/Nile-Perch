library(tidyverse)

stom<-read_csv("all_tk_rlk_stomachs.csv") %>% 
  mutate(id = paste(year,month,day,tag))

stom<-left_join(stom,ltt, by = "id") 
stom<-stom%>% 
  filter(is.na(sl))   #somehow missing 141 fish, need to check this...
                      #however, all of them are from 


#need to join to ltt, then figure out a way to make the gam. 
#Do a gam based iri?
