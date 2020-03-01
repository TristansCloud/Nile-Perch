library(lubridate)
library(tidyverse)
library(mgcv)
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

ltt_function<-function() {

ltt<-read_csv("C:/Users/tk/Dropbox/Chapman lab/TK and Ronny/2nd data analysis/Stomachs/Data/good_cols_ltt.csv") %>% 
  mutate(net = toupper(net),
         comments = toupper(comments),
         site = capwords(site),
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


sites<- c("Before Pump","DDB",
          "DDB Far","DDB Left",
          "DDB Near","DDB Right",
          "Forest","Forest Far",
          "Forest Near","Katogoo",
          "Katoogo Far","Katoogo Near",
          "Kawuuwa","Kawuwa",
          "Kawuwa-katoogo Far","Kawuwa-Katoogo Far",
          "Kawuwa Far","Kawuwa Near",
          "Past Pump","Pump",
          "Pump Before","Pump Past")

ltt<-ltt[ltt$site %in% sites,]    #removes nontypical sites
remove(sites)
return(ltt)
}
ltt<-ltt_function()
remove(ltt_function,capwords)

np<-ltt %>% 
  filter(fish_code==7,!is.na(sl),!is.na(mass),   #fix these missing values!
         mass<=1500) %>%   #done bc two fish had extremely large sizes, "removed outliers
  mutate(cpue_id = paste(year,month,day,site_type,net,sep = "-")) 
  #age_class = case_when(sl<=15 ~ "one",sl>15&sl<=25 ~ "two",sl>25 ~ "three"),

hap<-ltt %>% 
  filter(fish_code==6|fish_code==12|fish_code==1) %>% 
  mutate(cpue_id = paste(year,month,day,site_type,net,sep = "-"))

### Combining hap and np 

bmass_np<-np %>% 
  group_by(date,cpue_id) %>% 
  summarise(biomass=sum(mass))

tester <- transform(bmass_np, ndate = as.numeric(date),
                    nyear  = as.numeric(format(date, '%Y')),
                    doy    = as.numeric(format(date, '%j')))

 model1<-gam(biomass ~ s(ndate, bs = "tp", k = 10), data = tester,
    family = quasipoisson, na.action = "na.omit")
gam.check(model1)
plot(model1,pages=1,residuals = T,all.terms=TRUE,shade=FALSE)
points(tester$biomass,tester$date)
plot(tester$biomass~tester$ndate)
lines(model1)

library(mgcv)
set.seed(0)
n<-200
sig2<-4
x0 <- rep(1:4,50)
x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1)
x3 <- runif(n, 0, 1)
y <- 2 * x0
y <- y + exp(2 * x1) - 3.75887
y <- y+0.2*x2^11*(10*(1-x2))^6+10*(10*x2)^3*(1-x2)^10-1.396
e <- rnorm(n, 0, sqrt(abs(sig2)))
y <- y + e
x0 <- factor(x0)
b<-gam(y~x0+s(x1)+s(x2)+s(x3))
plot(b,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)
# just parametric term alone
termplot(b,terms="x0",se=TRUE)
# example with 2-d plots
b1<-gam(y~x0+s(x1,x2)+s(x3))
op<-par(mfrow=c(2,2))
plot(b1,all.terms=TRUE)
par(op)



