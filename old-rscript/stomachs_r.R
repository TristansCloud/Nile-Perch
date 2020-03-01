library(lubridate)
library(tidyverse)
library(data.table)
library(mgcViz)
library(DHARMa)
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

ltt<-read_csv("C:/Users/tk/Dropbox/Chapman lab/TK and Ronny/2nd data analysis/Stomachs/Data/good_cols_ltt.csv") %>% 
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
remove(sites,capwords)

np<-ltt %>% 
  filter(fish_code==7,!is.na(sl),
         sl<=50) %>%   #fix these missing values!
  mutate(age_class = case_when(sl<=15 ~ "one",
                               sl>15&sl<=25 ~ "two",
                               sl>25 ~ "three"),
         cpue_id = paste(year,month,day,site_type,net,sep = "-"),
         date_num = as.numeric(date))

hap<-ltt %>% 
  filter(
      fish_code==6|
      fish_code==12|
      fish_code==1|
      fish_code==18|
      fish_code==21) %>% 
  mutate(cpue_id = paste(year,month,day,site_type,net,sep = "-")) %>% 
  mutate(sl = case_when(is.na(sl)~(as.numeric(tl)/1.13051),
                                        TRUE~as.numeric(sl)),
                         tl = case_when(is.na(tl)~(as.numeric(sl)*1.13051),
                                        TRUE~as.numeric(tl)))

####getting CPUE####

cpue_function<-function() {
  
np_cue<-np %>% 
  group_by(site_type,dist_shore) %>% 
  count(cpue_id)    #counts does both number of id's and groups by id, so does give the correct cpue
  
np_sl<-np %>% 
  group_by(site_type,dist_shore,cpue_id) %>% 
  summarise(np_sl = mean(sl))
np_sl<-data.table(np_sl) %>% 
  mutate(site_type = as.factor(site_type),
       dist_shore = as.factor(dist_shore),
       site_type = case_when(site_type == 1~"one",
                             site_type == 2~"two",
                             site_type == 3~"three",
                             site_type == 4~"four"))

hap_cue<-hap %>%
  group_by(site_type,dist_shore) %>% 
  count(cpue_id)    #counts does both number of id's and groups by id, so does give the correct cpue

cpue<-full_join(hap_cue,np_cue,by=c('cpue_id','site_type','dist_shore')) %>% 
  mutate(n.y=replace_na(n.y,0),
         n.x=replace_na(n.x,0))
colnames(cpue)[4] <- "hap_cpue"
colnames(cpue)[5] <- "np_cpue"

remove(hap_cue,np_cue)


cpue_date<-str_split(cpue$cpue_id,"-",n = 4, simplify = TRUE) %>% 
  data.frame() %>% 
  mutate(date = paste(X1,X2,X3,sep = "-"))
  

cpue_date<-cpue_date[,4:5]
cpue<-cbind.data.frame(cpue_date,cpue)
cpue<-cpue[,2:7] 
cpue<-cpue %>% 
  mutate(date = as.Date(date,"%Y-%m-%d"))
cpue<-cpue %>% 
  mutate(date_num = as.numeric(date))

#adding biomass

biomass_np<-aggregate(mass~cpue_id,data = np,FUN=sum)

cpue<-left_join(cpue,biomass_np,by="cpue_id") %>% 
  mutate(mass=replace_na(mass,0))
colnames(cpue)[8]<-"np_biomass"
remove(biomass_np)

cpue<-cpue %>% mutate(hap_forlog = 
                        case_when(hap_cpue == 0 ~ 0.5,TRUE~as.numeric(hap_cpue)),
                      np_forlog = 
                        case_when(np_cpue == 0 ~ 0.5,TRUE~as.numeric(np_cpue)),
                      np_massforlog =
                        case_when(np_biomass == 0 ~ 2,TRUE~as.numeric(np_biomass))) %>% 
  mutate(logtwo_npmass = log2(np_massforlog),
         logtwo_hapcpue = log2(hap_forlog),
         logtwo_npcpue = log2(np_forlog),
         site_type = case_when(site_type == 1~"one",
                               site_type == 2~"two",
                               site_type == 3~"three",
                               site_type == 4~"four")) %>% 
  mutate(site_type = as.character(site_type),
         dist_shore = as.character(dist_shore))

drop<-c("hap_forlog","np_forlog","np_massforlog")
cpue<-cpue[,!names(cpue) %in% drop]

cpue<-full_join(cpue,np_sl,by=c("site_type","dist_shore","cpue_id"))

cpue<-cpue %>% 
  mutate(np_sl=replace_na(np_sl,0))

return(cpue)
}



cpue<-cpue_function()
remove(cpue_function)

moon<-read_csv("C:/Users/tk/Dropbox/Chapman lab/TK and Ronny/2nd data analysis/Stomachs/Data/moon.csv")
moon<-moon[,c(1,3)] %>% 
  mutate(date = as.Date(date))
cpue<-left_join(cpue,moon, by=c("date"))

#predictor gams
np<-np %>%
  mutate(dist_shore = as.factor(dist_shore))

gam_np_sl <- gam(sl ~ s(date_num)+
                   s(dist_shore,bs="re"),
                 data = np,
                 family = quasipoisson)
fitval<-data.table(gam_np_sl$fitted.values)
colnames(fitval)[1]<-"np_sl_gam"
np<-bind_cols(np,fitval)
fitval<-data.table(predict.gam(gam_np_sl,cpue))
colnames(fitval)[1]<-"np_sl_gam"
cpue<-bind_cols(cpue,fitval)

#np sl gam
ggplot(np, aes(date)) +
  geom_point(aes(y=sl,alpha = .05)) +
  geom_point(aes(y=np_sl_gam,color=dist_shore))+
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_line(colour = "grey90"),panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),axis.text = element_text(size = 10),axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "NP SL")

#plot(gam_np_sl, shade = TRUE)
#plot(np$sl~np$date_num)



#This function is interesting because it shows how 
#although after the bas, whenever we catch fish we catch more biomass
#than we did before the ban. However, there are more days with 0 catch,
#and on average we catch a similar amount, if not less total biomass.

cpue<-cpue %>% 
  mutate(dist_shore = as.factor(dist_shore),
         site_type = as.factor(site_type))

logmass_gam<-gam(logtwo_npmass ~ s(date_num)+
                   s(dist_shore,bs="re"),
                 data = cpue,
                 family = quasipoisson)
fitval<-data.table(logmass_gam$fitted.values)
colnames(fitval)[1]<-"logmass_gam"
cpue<-bind_cols(cpue,fitval)



ggplot(cpue, aes(date)) +
  geom_point(aes(y=logtwo_npmass,alpha = .05))+
  geom_point(aes(y=logmass_gam,alpha = .05,color=dist_shore)) +
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_line(colour = "grey90"),panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),axis.text = element_text(size = 10),axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "log_NP_mass")


##GAMs section
gamma <- log(nrow(cpue))/2 ## BIC like penalization, reduces wigglyness
# then use gamma=gamma in gam()
cp_test<-cpue

#example gam
gam_hap_np<-gam(hap_cpue~s(dist_shore,bs="re")+s(date_num,bs="re")+
                  s(np_sl_gam)+s(logmass_gam),data=cpue,family = quasipoisson,
                  na.action = na.omit)
#plot.gam(gam_hap_np)             


#Play around with the s(date_num) parameter. This specifies if time is 
#included in the model. Putting it to random effect (,bs="re")
#we see it explains very little variation if 

gam_hap_np<-gam(hap_cpue~
                  s(date_num,bs="re")+  #as s(date_num), R-sq = .0986
                  s(dist_shore,bs="re")+    #R-sq = .071  #Both = .181
                  #s(site_type,bs="re")+
                  #te(dist_shore,site_type,bs="re")+
                  #s(moon_age)+
                  #s(logtwo_hap)+
                  s(np_sl_gam)+
                  #s(np_sl,k=5)+
                  #s(logtwo_npmass),
                  s(logmass_gam),
                  #s(np_sl_gam),
                  #s(logtwo_npmass,np_sl),
                  #s(logtwo_npmass),
                  #s(random),
                data = cpue,
                family = quasipoisson,
                na.action = na.omit,
                gamma = gamma)
plot.gam(gam_hap_np)
plot(gam_hap_np,residuals = TRUE,all.terms = TRUE)

fitval<-data.table(gam_hap_np$fitted.values)
colnames(fitval)[1]<-"gam_hap_np"
cpue_a<-bind_cols(cpue,fitval)

ggplot(cpue_a, aes(date)) +
  geom_point(aes(y=hap_cpue,alpha = .05))+
  geom_point(aes(y=gam_hap_np,alpha = 0.05,color=dist_shore)) +
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_line(colour = "grey90"),panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),axis.text = element_text(size = 10),axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Hap_cpue")

gam.check(gam_hap_np)#,old.style = TRUE)
summary(gam_hap_np)
vis.gam(gam_hap_np)


#simulateResiduals(fittedModel = gam_hap_np, n = 500)

#using mgcViz package
a<-getViz(gam_hap_np)

plot.gamViz(a)
plotRGL(sm(a, 1), fix = c("z" = 0), residuals = TRUE,
        maxpo = 10000, too.far = 40)


#predicting gam on validation ltt

a<-data.table(predict.gam(gam_hap_np,newdata = cpue_val,type = "response",
                          se=FALSE,unconditional = FALSE))
colnames(a)[1]<-"hap_predict"
cpue_predict <- cbind(cpue_val,a)

library(ez) #this package can be used for bootstrap, check
            #out exBoot and ezPlot2 functions

#unconditional = TRUE

#for log
ggplot(cpue_predict,aes(x=date_num))+
  geom_point(aes(y=logtwo_hapcpue,alpha = .05))+
  geom_point(aes(y=hap_predict,alpha = .05,color = "red"))


ggplot(cpue_predict,aes(x=date))+
  geom_point(aes(y=hap_cpue,alpha = .05),position = "jitter")+
  geom_point(aes(y=hap_predict,alpha = .05,color = "red"))#+
  #ylim(0,100)


##useful plots

ggplot(cpue,aes(date_num,hap_cpue))+
  geom_point(aes(alpha = .05))+
  geom_smooth()


ggplot(cpue,aes(date,np_sl))+
  geom_point(aes(alpha = .05))+
  stat_smooth(n=20,span = .001)



cpue_test<-cpue %>% 
  filter(date_num>=17200,
         date_num<=17900)

ggplot(cpue_test,aes(date,hap_cpue))+
  geom_point(aes(alpha = .05))

cpue_test<-cpue %>% 
  filter(date_num >=15000,
         date_num<=16000)
cpue_b<-cpue %>% 
  filter(date_num >=16000,
         date_num<=17000)

x<-cpue_predict$hap_cpue
y<-cpue_predict$hap_predict
z<-cpue_test$hap_cpue
d<-cpue_b$hap_cpue
t.test(x,y = y)


#checking for moon influence on cpue
moon<-read_csv("C:/Users/tk/Dropbox/Chapman lab/TK and Ronny/2nd data analysis/Stomachs/Data/moon.csv") %>% 
  mutate(date=as.Date(date))


#playing with biomass

'''terms for gam:
  s(site_type,k=4) 
knots <- data.frame(x=seq(1,3,length=9))
range(cpue$logtwo_npmass)'''

#fitting gams
gam_hapcpue <- gam(hap_cpue ~ s(date_num, bs = "cr", k = 12),
               data = cpue,
               family = quasipoisson)
  # plot(gam_hap, shade = TRUE) 
fitval<-data.table(gam_hap$fitted.values)
colnames(fitval)[1]<-"hap_cpue_gam"
cpue<-bind_cols(cpue,fitval)


gam_np <- gam(np_cpue ~ s(date_num, bs = "cr", k = 12),
              data = cpue,
              family = quasipoisson)
  # plot(gam_np, shade = TRUE)
fitval<-data.table(gam_np$fitted.values)
colnames(fitval)[1]<-"np_cpue_gam"
cpue<-bind_cols(cpue,fitval);remove(gam_np)

np<-np %>% 
  mutate(dist_shore = as.factor(dist_shore))

gam_np_sl <- gam(sl ~ s(date_num)+
                 s(dist_shore,bs="re"),
                 data = np,
                 family = quasipoisson)
  plot(gam_np_sl, shade = TRUE)
  plot(np$sl~np$date_num)
  fitval<-data.table(predict.gam(gam_np_sl,cpue))
  colnames(fitval)[1]<-"np_sl_gam"
  cpue<-bind_cols(cpue,fitval)
  

 #hap gam
ggplot(cpue, aes(date_num)) +
  geom_point(aes(y=hap_cpue,alpha = .05)) +
  geom_line(aes(y=hap_cpue_gam,color='blue'))+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Hap CPUE")



    #np gam
ggplot(cpue, aes(date_num)) +
  geom_point(aes(y=np_cpue,alpha = .05)) +
  geom_line(aes(y=np_cpue_gam,color='blue'))+
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_line(colour = "grey90"),panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),axis.text = element_text(size = 10),axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "NP CPUE")




#fitval<-data.table(gam_np_sl$fitted.values)
#colnames(fitval)[1]<-"np_sl_gam"
#np_test<-bind_cols(np,fitval)
#fitting gam for hap cpue ~np_sl & cpue

random<-data.table(runif(2104, 0.0, 1.0))
colnames(random)[1]<-"random"
cpue<-bind_cols(cpue,random)

crash<-cpue %>% filter(date_num>=1650)

#good gam
gam_hap_np<-gam(logtwo_hapcpue~
                  te(logtwo_npmass,np_sl_gam),
                data = cpue)

gam_hap_np<-gam(hap_cpue~
                #s(np)+
                #s(dist_shore,bs="re")+
                #s(site_type,bs="re")+
                #te(dist_shore,site_type,bs="re")+
                #s(date_num)+
                #s(np_sl_gam,k=5)+
                #s(logtwo_npmass),
                #s(logtwo_hap),
                s(np_sl_gam)+
                s(np_cpue),
                #s(logtwo_npmass),
                #ti(logtwo_npmass,np_sl_gam),
                #s(random),
                data = cpue,
                family = quasipoisson,
                na.action = na.omit)
plot(gam_hap_np,residuals = TRUE,all.terms = TRUE)
plot.gam(gam_hap_np)
gam.check(gam_hap_np)#,old.style = TRUE)
summary(gam_hap_np)
vis.gam(gam_hap_np)

simulateResiduals(fittedModel = gam_hap_np, n = 500)

#using mgcViz package
a<-getViz(gam_hap_np)

plot.gamViz(a)
plotRGL(sm(a, 1), fix = c("z" = 0), residuals = TRUE,
        maxpo = 10000, too.far = 40)


#predicting gam on validation ltt

a<-data.table(predict.gam(gam_hap_np,newdata = cpue_val,type = "response",se=FALSE))
colnames(a)[1]<-"hap_predict"
cpue_predict <- cbind(cpue_val,a)

#for log
ggplot(cpue_predict,aes(x=date_num))+
  geom_point(aes(y=logtwo_hapcpue,alpha = .05))+
  geom_point(aes(y=hap_predict,alpha = .05,color = "red"))


ggplot(cpue_predict,aes(x=date_num))+
  geom_point(aes(y=hap_cpue,alpha = .05))+
  geom_point(aes(y=hap_predict,alpha = .05,color = "red"))




#more gams
haplsdate_gam<-gam()


summary(glm(hap~date,data = cpue,
    family = quasipoisson()))

 #diagnosing fit of gam
rsd <- residuals(bp)
gam(rsd~s(x0,k=40,bs="cs"),gamma=1.4,data=dat) ## fine
gam(rsd~s(x1,k=40,bs="cs"),gamma=1.4,data=dat) ## fine
gam(rsd~s(x2,k=40,bs="cs"),gamma=1.4,data=dat) ##`k'too low
gam(rsd~s(x3,k=40,bs="cs"),gamma=1.4,data=dat) ## fine






 ggplot()+
  geom_line(data = gam_hap, aes(y=fitted.values,x=date_vector))+
  geom_point(data = cpue,aes(y=hap,x=date_num))


ggplot(data = cpue, aes(x=date,y=hap))+
  geom_point()+geom_smooth(span=0.35)+
  facet_wrap(~site_type)
ggplot(data = cpue, aes(x=date,y=np))+
  geom_point()+geom_smooth(span=0.35)+
  facet_wrap(~site_type)

ggplot(data = np, aes(x=date,y=sl))+
  geom_point()+geom_smooth(span=0.35)+
  facet_wrap(~site_type)
ggplot(data = cpue, aes(x=date,y=np_biomass))+
  geom_point()+geom_smooth(span=0.35)+
  facet_wrap(~site_type)
####grouping cpue####

hap_grp<-cpue %>% 
  group_by(site_type,month=floor_date(date,"month")) %>% 
  summarise(sum(hap),length(unique(cpue_id))) 
colnames(hap_grp)[2:3]<-c("hap","effort")
hap_grp<-hap_grp %>% 
  mutate(cpue=(hap/effort))

np_grp<-cpue %>% 
  group_by(
    age_class,month=floor_date(date,"month")) %>% 
  summarise(
    sum(np),length(unique(cpue_id))) 
colnames(np_grp)[3:4]<-c("np","effort")

np_one<-np_grp %>% 
  filter(age_class=="one"|age_class=="none") %>%   #adds 0 catch days to avoid upwards count bias
  group_by(month) %>% 
  summarise(sum(np),sum(effort))
colnames(np_one)[2:3]<-c("np","effort")
np_one<-np_one %>% mutate(cpue=(np/effort))
  
np_two<-np_grp %>% 
  filter(age_class=="two"|age_class=="none") %>% 
  group_by(month) %>% 
  summarise(sum(np),sum(effort))
colnames(np_two)[2:3]<-c("np","effort")
np_two<-np_two %>% mutate(cpue=(np/effort))

np_three<-np_grp %>% 
  filter(age_class=="three"|age_class=="none") %>% 
  group_by(month) %>% 
  summarise(sum(np),sum(effort))
colnames(np_three)[2:3]<-c("np","effort")
np_three<-np_three %>% mutate(cpue=(np/effort))


#####


##Working with biomass

biomass_np<-aggregate(mass~cpue_id,data = np,FUN=sum)

cpue<-full_join(cpue,biomass_np,by="cpue_id") %>% 
  mutate(mass=replace_na(mass,0))
 colnames(cpue)[7]<-"biomass"









#np vs hap cpue
cpue_test<-full_join(hap_cue,np_cue,by=c('cpue_id','site_type','dist_shore')) %>% 
  mutate(n.y=replace_na(n.y,0))

filler<-cpue_test %>% 
  filter(is.na(age_class))

one<-filler %>% 
  mutate(age_class=replace_na(age_class,"one"))
two<-filler %>% 
  mutate(age_class=replace_na(age_class,"two"))
three<-filler %>% 
  mutate(age_class=replace_na(age_class,"three"))

cpue<-cpue_test %>% 
  filter(!is.na(age_class))
cpue<-rbind(cpue,one,two,three) %>%
  mutate(n.x=replace_na(n.x,0))

#split by habitat
cpue_misc<-cpue %>% 
  filter(site_type==1)
cpue_kawuwa<-cpue %>% 
  filter(site_type==2)
cpue_ddb<-cpue %>% 
  filter(site_type==3)
cpue_forest<-cpue %>% 
  filter(site_type==4)

x=cpue
ggplot(data = x,aes(x=n.y,y=n.x))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = glm, family = poisson)+
  facet_grid(age_class~dist_shore)


#checking for moon influence on cpue
moon<-read_csv("C:/Users/tk/Dropbox/Chapman lab/TK and Ronny/2nd data analysis/Stomachs/Data/moon.csv") %>% 
  mutate(date=as.Date(date))

hap_cue<-left_join(hap_cue,moon,by="date")

hap_early<-hap_cue %>% 
  filter(unix_date<1306933261)
hap_mid<-hap_cue %>% 
  filter(unix_date>1306933261,
         unix_date<1388581261)
hap_late<-hap_cue %>% 
  filter(unix_date>1388581261,
         unix_date<1504270861)

np<-np %>% 
  mutate(date_numeric=as.numeric(np$date)) %>% 
  mutate(date_numeric=date_numeric-13952)


abline(gam(mass~s(date),family = "quasipoisson", data = np),col="blue",lwd=c(2))