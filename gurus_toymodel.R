############################
#### Preprocessing data ####
############################
library(lme4)
library(tidyverse)
library(lubridate)
library(imager)

####LTT####

#im<-load.image("~/Desktop/big_nile_perch.png")
#plot(im , xlim = c(50,1748),ylim = c(50,1347))

NP<-read_csv("TKSTOM-ltt-master.csv", guess_max = 60000) %>% 
  mutate(date = dmy(`Date (DD-Mon-YY)`),
         year = year(dmy(`Date (DD-Mon-YY)`)),
         month = month(dmy(`Date (DD-Mon-YY)`)),
         day = day(dmy(`Date (DD-Mon-YY)`)),
         tag = as.character(Tag_Num),
         Net = toupper(Net),
         Comments = toupper(Comments),
         Site = toupper(Site),
         cpue_id = paste(`Date (DD-Mon-YY)`,Net,sep = "-"), # probably change colname for publication
         np_id = paste(`Date (DD-Mon-YY)`,Tag_Num),
         SL = as.numeric(SL),
         Site_Type = factor(Site_Type,levels = c("1","2","3","4")),
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
                                TRUE ~ Net),
        dist_shore = factor(dist_shore, levels = c("inshore","offshore"))) %>% 
  filter(!grepl('MUK', Comments),
         !grepl('MUK', Net),
         !grepl('BOUGHT', Net),
         !grepl('BOUGHT', Comments),
         Fish_Code == 7,
         !is.na(SL),
         SL <= 50) #%>% 

  

############################
#### Fitting model #########
############################

p1 <- ggplot(NP,aes(date)) +
  geom_point(aes(y=SL),shape = 1,size = 1.5) +
  geom_smooth(aes(y=SL), se = F, formula = y ~ s(x, bs = "cs"))

p1 

p1 + xlim(ymd("2017-01-01"), "2019-08-01") +
  geom_vline(xintercept = ymd("2017-09",truncated = 1), linetype = "dotted")


#### Moving regression ####
## using 1 month cycle with 3 month : 10 month short : long term regression

NP_REGRESSION <- NP %>% 
  filter(date >= ymd("2017-01-01")) %>% 
  unite(col = id, year, month, sep = ".",remove = F)

# initial ggplot objects
p <- ggplot(NP_REGRESSION, aes(date)) + 
  geom_point(aes(y=SL),shape = 1,size = 1.5)
g <- ggplot(NP_REGRESSION, aes(date)) + 
  geom_point(aes(y=SL),shape = 1,size = 1.5)
individual <- list() ; i = 1

#month <- "2018.5"

# fit linear models
for(month in unique(NP_REGRESSION$id)){

  predict_date <- ymd(month,truncated = 1)
  current_date <- ymd(month,truncated = 1)
  short_date <- ymd(month,truncated = 1)
  long_date <- ymd(month,truncated = 1)
  
  month(predict_date) <- month(predict_date) +3
  month(current_date) <- month(current_date) +1
  month(short_date) <- month(short_date) -2
  month(long_date) <- month(long_date) -9

  
  SHORT <- NP_REGRESSION %>% 
    filter(date >= short_date, date < current_date)
  LONG <- NP_REGRESSION %>% 
    filter(date >= long_date, date < current_date)
  PREDICT <- NP_REGRESSION %>% 
    filter(date > current_date, date < predict_date)
  

  short_lm <- lm(SL ~ date, data = SHORT)
  long_lm <- lm(SL ~ date, data = LONG)
  
  SHORT_PRED <- cbind(SHORT, predict(short_lm, interval = 'confidence'))
  LONG_PRED <- cbind(LONG, predict(long_lm, interval = 'confidence'))
  
  p <- p + geom_line(data = LONG_PRED,aes(date, fit), color = "blue", size = 1.25) +
    geom_line(data = SHORT_PRED, aes(date, fit), color = "lightgreen", size = 1.25)
    #geom_ribbon(data = LONG_PRED,aes(ymin=lwr,ymax=upr), alpha=0.01) +
    #geom_ribbon(data = SHORT_PRED,aes(ymin=lwr,ymax=upr), alpha=0.01)
   
  # for individual plots
  pred_target <- mean(PREDICT$SL)
  month(predict_date) <- month(predict_date) -1
  
  r <- g  + geom_line(data = LONG_PRED,aes(date, fit), color = "blue", size = 1.25) +
    geom_line(data = SHORT_PRED, aes(date, fit), color = "lightgreen", size = 1.25) 
  individual[[i]] <- r
  i = i + 1
}

# time of ban
individual[4:10]

# during no fishing
individual[11:18]

# crash
individual[19:26]

p + geom_smooth(aes(y=SL), se = F,data = NP, color = "magenta") + xlim(ymd("2017-01-01"), "2019-08-01")

#### scratch work ####
toymodel<-lm(cichlid ~ dist_shore + SL + num_date_of_capture,data = STOMGAM)# + num_site1 + num_site2 + num_site3 + num_site4
summary(toymodel)
plot(toymodel)

toymodel1<-lm(cichlid ~ dist_shore + SL + num_date_of_capture + num_date_of_capture:dist_shore,data = STOMGAM)
summary(toymodel1)
plot(toymodel1)

STOMGAM_2016<-STOMGAM %>% 
  filter(date_of_capture >= as.Date('2016-01-01') & date_of_capture < as.Date('2017-01-01'))



toymodel_16<-lm(cichlid ~ dist_shore + SL + num_date_of_capture,data = STOMGAM_2016)
summary(toymodel_16)
plot(toymodel)


