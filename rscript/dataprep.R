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
#ltt<-read_csv("ltt-master.csv", guess_max = 60000)

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
#### Some old stuff, ignore ####
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
#####
#### Formatting Jennifer's stomach data ####

raw_file<-read_csv("data to add/Copy of NP Stomachs- Analyzed by Jenn.csv") 

for(i in 1:nrow(raw_file)) {
  if(is.na(raw_file[i,8])){
  } else {
    raw_file[i,7]<-paste(raw_file[i,7],raw_file[i,8], sep = ", ")
  }
}

raw_file<-raw_file[,c(1,3:7)]

newcols<-c("year","month","day","preysl","preybd","dissector")

raw_file[,newcols]<-NA
raw_file[,"dissector"]<-"Jennifer Sunahara"


dmy<-strsplit(raw_file$`Date Captured`,"/")

for(i in 1:nrow(raw_file)){
  if(is.na(raw_file[i,1])){
  } else{
    raw_file[i,"year"]<-dmy[[i]][3]
    raw_file[i,"month"]<-dmy[[i]][2]
    raw_file[i,"day"]<-dmy[[i]][1]
  }
}

raw_file$year[raw_file$year == "..."] <- "2012"

unique(raw_file$`Content (Species)`)

raw_file<-raw_file %>% 
  mutate(`Content (Species)` = case_when(
    `Content (Species)` == "CIC" ~ "cichlid",
    `Content (Species)` == "MF" ~ "ephemeroptera larvae",
    `Content (Species)` == "NP" ~ "nile perch",
    `Content (Species)` == "HAP" ~ "hap",
    `Content (Species)` == "CLA"~ "clarias",
    `Content (Species)` == "UNID" ~ "ufp",
    `Content (Species)` == "BARB" ~ "barbus",
    `Content (Species)` == "ODO" ~ "odonata larvae",
    `Content (Species)` == "RAS" ~ "mukene",
    `Content (Species)` == "VEG" ~ "vegitation",
    `Content (Species)` == "UNIDIN" ~ "uip",
    `Content (Species)` == "unknown" ~ "unidentified",
    `Content (Species)` == "HYM" ~ "hymenoptera",
    `Content (Species)` == "DIP" ~ "diptera",
    TRUE ~ `Content (Species)`
  ))

unique(raw_file$`Content (Species)`)

final_file<-raw_file[,2:12] ; rm(raw_file)

colnames(final_file)[1:5]<-c("tag","prey","preynumber","preymass","comments")
final_file[,"chaoborus"]<-NA
final_file<-final_file[,colnames(stom)]

final_file<-final_file %>% 
  filter(tag != "B")

write_csv(final_file, path = "/Users/triskos/git-directory/Nile-Perch/data to add/Jenn 2012 stomachs.csv")

#### Changing tags on Jenn 2011 stomachs ####

ltt<-read_csv("ltt-master.csv", guess_max = 60000)

all_tags<-sort(unique(ltt$Tag_Num))
print(all_tags[1000:2000])
print(all_tags[2001:length(all_tags)])

jenn_2011<-read_csv("data to add/Jenn Sunahara 2011.csv",
                    col_types = cols(
                      tag = col_character()
                    ))
    # never got to it, some tags are like "105.32"


#### Creating compiled stomach CSV ####

jenn_2011<-read_csv("data to add/Jenn Sunahara 2011.csv",
                    col_types = cols(
                      tag = col_character()
                    ))
jenn_2012<-read_csv("data to add/Jenn 2012 stomachs.csv",
                    col_types = cols(
                      tag = col_character()
                    ))
#str_count(jenn_2012$tag, " ")

stom<-read_csv("stomachs-oldcopy.csv") %>% # DO NOT OVERWRITE NEW STOM, I fixed a few duplicates that would be undone if I recreated new_stom from the stomachs-old.csv
  filter(prey != "empty") # there is still an NA 


new_stom<-rbind(stom,jenn_2011,jenn_2012, deparse.level = 0) %>% 
  mutate(month=as.numeric(month),
         prey = case_when(
           prey == "unidentifed" ~ "unidentified",
           prey == "tilapinne cichlid" ~ "tilapiine cichlid",
           TRUE ~ prey
         ))

## There are 2 NA preymass, one needs to be found, the other was an empty stomach (removed)

table(new_stom$prey,useNA = "always") # only 1 empty stom

write_csv(new_stom,path="stomachs.csv", append = FALSE)

