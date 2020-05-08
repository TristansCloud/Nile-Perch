library(tidyverse)
library(lubridate)
library(data.table)
library(mgcv)
library(mgcViz)
library(tidymv)

#######################
## Errors to fix in LTT:
##    fish_code            tl              sl 
##    Max.   :53.000   Max.   :910.00   Max.   :240.000 
##     - tl and sl are wildly different max values
##     - fish_code doesn't go to 53

## ltt-with-NAs.csv had 35048 rows after filtering
## Shelby's master had 39119 rows after filtering

LTT<-read_csv("ltt-master.csv") %>% 
  mutate(Net = toupper(Net),
         Comments = toupper(Comments),
         Site = toupper(Site),
         cpue_id = paste(`Date (DD-Mon-YY)`,Net,sep = "-"), # probably change colname for publication
         np_id = paste(`Date (DD-Mon-YY)`,Tag_Num),
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
                                TRUE ~ Net)) %>% 
  filter(!grepl('MUK', Comments),
         !grepl('MUK', Net),
         !grepl('BOUGHT', Net),
         !grepl('BOUGHT', Comments))


## Group by distance from shore:
##  - NP SL
##  - NP CPUE ensure 0 catch days included
##  - NP mass and mass of daily catch
##  - Hap CPUE
##
## LTT gams
##  Should habitat be included initially? Probably yes, but it degrades the predictive ability, which 
##  which is an interesting result.
##
## Predictions: 
##  Descriptive gams as inputs and compare the predicted model to the
##  hap cpue and stomach gams to test goodness of fit (MSE, R^2).
##  



#### Stomach ####
## Models
##  Predition model: GAM of prey ~ s(NP SL) + s(NP abundance) + s(dist from shore)
##  Validation model: GAM of prey ~ te(NP SL, time)
##  prey = (%mass + %number)/2, for each individual
##
## Then predict each data point based on the SL, abundance, in/offshore conditions at the time
##  Compare two GAMs (R^2, MSE)
##
## I'll need a table with columns [time, prey NP, prey HAP, prey other fish, prey invert, NP SL from GAM @ time & in/offshore,
##  NP abundance GAM @ time & in/offshore, in/offshore]
##  The code should be flexible to add site bc that will likely be analyzed too
##

## Replace this with preformatted import in final script
gam_data_function<-function() {
  NP<-read_csv("ltt-master.csv", guess_max = 60000) %>% 
  mutate(year = year(dmy(`Date (DD-Mon-YY)`)),
         month = month(dmy(`Date (DD-Mon-YY)`)),
         day = day(dmy(`Date (DD-Mon-YY)`)),
         tag = as.character(Tag_Num),
         Net = toupper(Net),
         Comments = toupper(Comments),
         Site = toupper(Site),
         cpue_id = paste(`Date (DD-Mon-YY)`,Net,sep = "-"), # probably change colname for publication
         np_id = paste(`Date (DD-Mon-YY)`,Tag_Num),
         SL = as.numeric(SL),
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
                                TRUE ~ Net)) %>% 
         #Site_Type = factor(Site_Type,levels = c("1","2","3","4")),
         #dist_shore = factor(dist_shore, levels = c("inshore","offshore"))) %>% 
         
  filter(!grepl('MUK', Comments),
         !grepl('MUK', Net),
         !grepl('BOUGHT', Net),
         !grepl('BOUGHT', Comments),
         Fish_Code == 7,
         !is.na(SL),
         SL <= 50)
STOM_fun<-function(){
STOM<-read_csv("stomachs.csv",guess_max = 60000) %>% 
  mutate(prey_cat = case_when(
    prey == "annelid worm" ~ "invert",
    prey == "ants" ~ "invert",
    grepl("chaoborus", prey) ~ "invert",
    grepl("chironomid", prey) ~ "invert",
    prey == "corixidae adult" ~ "invert",
    grepl("diptera", prey) ~ "invert",
    prey == "ephemeroptera larvae" ~ "invert",
    prey == "hymenoptera" ~ "invert",
    prey == "leech" ~ "invert",
    prey == "odonata larvae" ~ "invert",
    prey == "uip" ~ "invert",
    grepl("cichlid", prey) ~ "cichlid",
    prey == "hap" ~ "cichlid",
    grepl("barb", prey) ~ "other_fish",
    prey == "mukene" ~ "other_fish",
    prey == "mastacembelus" ~ "other_fish",
    prey == "clarias" ~ "other_fish",
    prey == "ufp" ~ "other_fish",
    prey == "nile perch" ~ "nile_perch",
    prey == "parasitoid worm" ~ "extra",
    prey == "vegitation" ~ "extra",
    prey == "rope" ~ "extra",
    prey == "unidentified" ~ "extra",
    prey == "rocks" ~ "extra",
    TRUE ~ prey)) %>% 
  filter(prey_cat != "extra") %>% 
  mutate(tag = as.character(tag),
         preynumber = case_when(
           preynumber == "many" ~ "20", # do regression for final models
           TRUE ~ preynumber),
         preynumber = as.numeric(preynumber)
         ) %>% 
  filter(!is.na(preynumber))

STOM<-inner_join(STOM,NP, by = c("year","month","day","tag")) %>% # some to fix but pretty good
  group_by(year,month,day,tag,prey_cat,Site_Type,Net,dist_shore) %>% 
  summarise(preymass = sum(preymass),
            preynumber = sum(preynumber),
            SL = mean(SL),
            log2SL = log2(SL),
            SL_cat = case_when(SL <= 14.49375 ~ "small",
                               SL > 14.49375 ~ "big"))
return(STOM)} ; 
STOM <- STOM_fun() ; rm(STOM_fun)

STOMGAM_fun<-function(){
STOMMASS<-STOM %>% 
  select(-preynumber) %>% 
  pivot_wider(names_from = prey_cat, values_from = preymass, names_prefix = "mass_")

STOMCOUNT<-STOM %>% 
  select(-preymass) %>% 
  pivot_wider(names_from = prey_cat, values_from = preynumber, names_prefix = "count_")

STOMTOTAL<-STOM %>% 
  group_by(year,month,day,tag) %>% 
  summarise(total_preymass = sum(preymass),
            total_preynumber = sum(preynumber))
  
STOMGAM<-inner_join(STOMMASS,STOMCOUNT, by = c("year","month","day","tag","Site_Type","Net","dist_shore","SL","log2SL","SL_cat"))
STOMGAM<-inner_join(STOMGAM,STOMTOTAL,by = c("year","month","day","tag")); rm(STOMTOTAL,STOMCOUNT,STOMMASS)

return(STOMGAM)} ; preSTOMGAM<-STOMGAM_fun()  ; rm(STOMGAM_fun)

#return(NP,STOM,STOMGAM)
} ; 
#NP<-data.table() ; STOM <- data.table() ; STOMGAM <- data.table() 
#c(NP,STOM,STOMGAM) <- gam_data_function() #; rm(gam)


#### Grouped by individual fish ####
STOMGAM<-preSTOMGAM %>% 
  group_by(year,month,day,tag,Site_Type,Net,dist_shore,SL,log2SL,SL_cat) %>% 
  summarise(cichlid = (sum(mass_cichlid)/sum(total_preymass)+sum(count_cichlid)/mean(total_preynumber))/2,
            invert = (sum(mass_invert)/mean(total_preymass)+sum(count_invert)/mean(total_preynumber))/2,
            nile_perch = (sum(mass_nile_perch)/mean(total_preymass)+sum(count_nile_perch)/mean(total_preynumber))/2,
            other_fish = (sum(mass_other_fish)/mean(total_preymass)+sum(count_other_fish)/mean(total_preynumber))/2
  ) %>% 
  unite(col = date_of_capture, year,month,day, sep = ".") %>% 
  mutate(cichlid = if_else(is.na(cichlid),0,cichlid),
         invert = if_else(is.na(invert),0,invert),
         nile_perch = if_else(is.na(nile_perch),0,nile_perch),
         other_fish = if_else(is.na(other_fish),0,other_fish),
         #muk_or_barb = if_else(is.na(muk_or_barb),0,muk_or_barb),
         date_of_capture = ymd(date_of_capture),
         num_date_of_capture = as.numeric(ymd(date_of_capture)))
STOMGAM<-data.table(STOMGAM) %>% 
  mutate(Site_Type = factor(Site_Type,levels = c("1","2","3","4")),
    dist_shore = factor(dist_shore, levels = c("inshore","offshore")),
    SL_cat = factor(SL_cat))
#### Grouped by month, typical IRI #### IRI = (%N+%M)*%F
STOMGAM<-preSTOMGAM %>% 
  group_by(year,month,SL_cat,dist_shore) %>% 
  summarise(cichlid = ((sum(count_cichlid,na.rm = T)/sum(total_preynumber))+(sum(mass_cichlid,na.rm = T)/sum(total_preymass)))*(sum(!is.na(count_cichlid))/length(unique(tag))),
            invert = ((sum(count_invert,na.rm = T)/sum(total_preynumber))+(sum(mass_invert,na.rm = T)/sum(total_preymass)))*(sum(!is.na(count_invert))/length(unique(tag))),
            nile_perch = ((sum(count_nile_perch,na.rm = T)/sum(total_preynumber))+(sum(mass_nile_perch,na.rm = T)/sum(total_preymass)))*(sum(!is.na(count_nile_perch))/length(unique(tag))),
            other_fish = ((sum(count_other_fish,na.rm = T)/sum(total_preynumber))+(sum(mass_other_fish,na.rm = T)/sum(total_preymass)))*(sum(!is.na(count_other_fish))/length(unique(tag))),
            num_fish = length(unique(tag)),
            num_site1 = sum(Site_Type == 1),
            num_site2 = sum(Site_Type == 2),
            num_site3 = sum(Site_Type == 3),
            num_site4 = sum(Site_Type == 4)
            ) %>% 
  unite(col = date_of_capture, year,month, sep = ".",remove = F) %>% 
  mutate(date_of_capture = ymd(date_of_capture, truncated = 1),
         num_date_of_capture = as.numeric(ymd(date_of_capture)),
         weight = num_fish/mean(num_fish)) ; STOMGAM<-data.table(STOMGAM) %>% 
  mutate(# Site_Type = factor(Site_Type,levels = c("1","2","3","4")),
         dist_shore_fac = factor(dist_shore, levels = c("inshore","offshore")),
         SL_cat = factor(SL_cat))
  

## Stomach GAMs ##
# H is an interesting parameter for gam(): it supplies a
# penaly matrix to force model terms to certain values.
#
# For selecting k, use the best k from the LTT GAMs and compare to
# k auto selected by the MGCV algorithms through AIC.
#
# good tutorial on mcgvViz: https://mfasiolo.github.io/mgcViz/articles/mgcviz.html
stom_gam<-gam(
  list(
    cichlid ~ s(num_date_of_capture, by = interaction(SL_cat,dist_shore), k=15),
    invert ~  s(num_date_of_capture, by = interaction(SL_cat,dist_shore),k=15)),
    #other_fish ~ s(num_date_of_capture,by = SL_cat,k=100),
    #nile_perch ~ s(num_date_of_capture,by = SL_cat,k=100)),
  family=mvn(d=2),
  fx = TRUE,
  data=STOMGAM,weights = weight)


stom_gam<-gam(list(
  cichlid ~ s(num_date_of_capture,SL,by = dist_shore,k=15) + s(SL),
  invert ~ s(num_date_of_capture,SL,by = dist_shore,k=15) + s(SL)),
  #other_fish ~ s(num_date_of_capture,SL,by = dist_shore,k=15),
  #nile_perch ~ s(num_date_of_capture,SL,by = dist_shore,k=15)),
  family=mvn(d=2),data=STOMGAM)
  
  
stom_gam
summary(stom_gam)
gam.check(stom_gam)

plot.gamViz(stom_gam,n2 = 60)

check1D(stom_gam,"date_of_capture")
solve(crossprod(stom_gam$family$data$R)) #cov matrix
AIC(stom_gam,stom_gam1)

# Plotting on actual data. Do this before plotting anything!
model_p <- data.table(predict.gam(stom_gam,STOMGAM))
model_p<-cbind(model_p,STOMGAM[c("date_of_capture","dist_shore","SL_cat")])

#cichlid
ggplot(STOMGAM, aes(date_of_capture)) +
  geom_point(aes(y=cichlid,alpha = weight)) +
  geom_point(data = model_p, aes(y=V1,alpha = .5,colour = "red")) +
  theme(panel.border = element_rect(colour = "grey1",fill = NA),panel.background = element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank(),axis.text = element_text(size = 10),axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Cichlid IRI")+
  geom_vline(xintercept = ymd("2017-09",truncated = 1), linetype = "dotted") + 
  facet_grid(cols = vars(dist_shore), rows = vars(SL_cat))

#invert
ggplot(STOMGAM, aes(date_of_capture)) +
  geom_point(aes(y=invert,alpha = weight)) +
  geom_point(data = model_p, aes(y=V2,alpha = .5,colour = "red")) +
  theme(panel.border = element_rect(colour = "grey1",fill = NA),panel.background = element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank(),axis.text = element_text(size = 10),axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Invert IRI")+
  geom_vline(xintercept = ymd("2017-09",truncated = 1), linetype = "dotted") + 
  facet_grid(cols = vars(dist_shore), rows = vars(SL_cat))

## Example 2 response GAM
library(mgcv)
library(mgcViz)
## simulate some data...
V <- matrix(c(2,1,1,2),2,2)
f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * 
  (10 * x)^3 * (1 - x)^10
n <- 300
x0 <- runif(n);x1 <- runif(n);
x2 <- runif(n);x3 <- runif(n)
y <- matrix(0,n,2)
for (i in 1:n) {
  mu <- c(f0(x0[i])+f1(x1[i]),f2(x2[i]))
  y[i,] <- rmvn(1,mu,V)
}
dat <- data.frame(y0=y[,1],y1=y[,2],x0=x0,x1=x1,x2=x2,x3=x3) %>% 
  mutate(cat = as.factor(case_when(x0 <= 0.5 ~ "yes",
                         x0 > 0.5 ~ "no")))

## fit model...
b <- gam(y0~s(x0,x1,by = cat),data = dat)
b <- gam(list(y0~s(x0,by = cat),y1~te(x2,x3)),family=mvn(d=2),data=dat)
b
summary(b)
plot.gamViz(b,pages=1)
solve(crossprod(b$family$data$R)) ## estimated cov matrix

#example plotting gam
set.seed(10)
data <- gamSim(4, 400)
#> Factor `by' variable example

model <- gam(list(
  y ~ fac + s(x2, by = fac),
  x0 ~ fac + s(x2, by = fac)
),
family=mvn(d=2),data = data
)

summary(model)
model_p <- predict_gam(model)
model_p

model_p %>%
  ggplot(aes(x2, fit.1)) +
  geom_smooth_ci(fac)



