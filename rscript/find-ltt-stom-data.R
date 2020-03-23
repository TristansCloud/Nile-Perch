##########################################

# How to ensure best data is selected 

# Filter for fish_code
# data transfer length(unique(paste(matched$tag,matched$year,matched$month)))
#       max = 919
#
# Also compare final df with compare_df()
#
# Same number of length(unique(paste(matched(year,month,day,tag)))) & length(unique(paste(mass,sl))) or very very close


library(tidyverse)
library(diffobj)
library(compareDF)

stom<-read.csv("stomachs.csv")
ltt<-read.csv("ltt.csv")


matched<-left_join(stom,ltt, by = c("year","month","day","tag"))
print(length(unique(paste(matched$tag,matched$year,matched$month,matched$day))))
print(length(unique(paste(matched$tl,matched$sl,matched$sex,matched$mass))))
print(length(is.na(matched$tag)))



### All of them were the same!
#[1] "all_tk_rlk_stomachs extra copy.csv"   "all_tk_rlk_stomachs_2nd_analysis.csv"
#[3] "all_tk_rlk_stomachs.csv"              "stomachs.csv"                        

diffCsv("potential-stomach-files/all_tk_rlk_stomachs extra copy.csv",
        "potential-stomach-files/all_tk_rlk_stomachs_2nd_analysis.csv",context="auto")
diffCsv("potential-stomach-files/all_tk_rlk_stomachs extra copy.csv",
        "potential-stomach-files/all_tk_rlk_stomachs.csv",context="auto")
diffCsv("potential-stomach-files/stomachs.csv",
        "stomachs.csv",context="auto")
diffCsv("potential-stomach-files/stomachs.csv",
        "potential-stomach-files/all_tk_rlk_stomachs.csv",context="auto")


#[1] "good_cols_ltt copy 22Aug2019.csv" "good_cols_ltt_2nd_analysis.csv"  
#[3] "good_cols_ltt.csv"                "LTT-that-tk-changed.csv"         
#[5] "ltt.csv"                          "lttdate.csv" 

diffCsv("potential-ltt-files/good_cols_ltt copy 22Aug2019.csv",
        "potential-ltt-files/good_cols_ltt.csv",context="auto")
diffCsv("potential-ltt-files/good_cols_ltt.csv",
        "potential-ltt-files/ltt.csv",context="auto")
diffCsv("potential-ltt-files/good_cols_ltt_2nd_analysis.csv",
        "potential-ltt-files/ltt.csv",context="auto")
diffCsv("potential-ltt-files/",
        "potential-ltt-files/",context="auto")


# does datatransfer have the most recent file?
list.files("datatransfer/")

diffCsv("datatransfer/all_tk_rlk_stomachs.csv","stomachs.csv",context="auto")
diffCsv("datatransfer/all_tk_rlk_stomachs-newer.csv","stomachs.csv",context="auto")
diffCsv("datatransfer/all_tk_rlk_stomachs extra copy.csv","stomachs.csv",context="auto")

diffCsv("datatransfer/ltt/good_cols_ltt-newer.csv","ltt.csv",context="auto")
diffCsv("datatransfer/good_cols_ltt-newer.csv","datatransfer/good_cols_ltt copy 22Aug2019.csv",context="auto")
diffCsv("datatransfer/good_cols_ltt-newer.csv","datatransfer/good_cols_ltt.csv",context="auto")
diffCsv("datatransfer/good_cols_ltt-newer.csv","datatransfer/ltt.csv",context="auto")


# finding best match "potential-ltt-files/"

for (i in 1:length(stomach_files)) for (j in 1:length(ltt_files)) {}

for(i in 1:5){
  ltt_files<-list.files("potential-ltt-files/")
  print(ltt_files[i])
  ltt_test<-read.csv(paste("potential-ltt-files/",ltt_files[i],sep = ""))
  matched_a<-left_join(stom,ltt_test,by = c("year","month","day","tag"))
  #print(length(unique(paste(matched_a$tag,matched_a$year,matched_a$month,matched_a$day)))) #matched$tag,matched$mass,,matched$month,matched$day
  #print(length(unique(paste(matched_a$tl,matched_a$sl,matched_a$sex,matched_a$mass))))
  print(summary(matched_a))
}


for(i in 1:6){
  ltt_files<-list.files("datatransfer/ltt/")
  print(ltt_files[i])
  matched_a<-left_join(stom,read.csv(paste("datatransfer/ltt/",ltt_files[i],sep = "")), by = c("year","month","day","tag"))# %>% 
    #filter(fish_code==7)
  #print(length(unique(paste(matched_a$tag,matched_a$year,matched_a$month,matched_a$day))))
  #print(length(unique(paste(matched_a$tl,matched_a$sl,matched_a$sex,matched_a$mass))))
  print(summary(matched_a))
}



v<-compare_df(ltt,ltt_test,c("tag","year"))


ggplot(data = ltt)+
  geom_point()
