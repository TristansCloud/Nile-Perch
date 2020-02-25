library(tidyverse)
library(diffobj)

stom<-read.csv("stomachs.csv")
ltt<-read.csv("ltt.csv")

np<-ltt %>%
  filter(tag != "NA") %>%
  filter(tag != 7) #; rm(ltt)    # really need to fix tags

drop.cols<-c("data_inputters_comments","comments.y","date","tag",
             "comments.x","id.x","id.y")
matched<-left_join(stom,np, by = c("year","month","day","tag")) %>%
  select(-one_of(drop.cols))
colnames(matched)


### All of them were the same!
#[1] "all_tk_rlk_stomachs extra copy.csv"   "all_tk_rlk_stomachs_2nd_analysis.csv"
#[3] "all_tk_rlk_stomachs.csv"              "stomachs.csv"                        

diffCsv("potential-stomach-files/all_tk_rlk_stomachs extra copy.csv",
        "potential-stomach-files/all_tk_rlk_stomachs_2nd_analysis.csv",context="auto")
diffCsv("potential-stomach-files/all_tk_rlk_stomachs extra copy.csv",
        "potential-stomach-files/all_tk_rlk_stomachs.csv",context="auto")
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



# finding best match
stomach_files<-list.files("potential-stomach-files/")
ltt_files<-list.files("potential-ltt-files/")
for (i in stomach_files) for (j in ltt_files) {}
