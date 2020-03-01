library(feather)
library(data.table)
library(mgcv)
library(car)
library(ggplot2)
library(animation)
library(nlme)

setwd("C:/Users/tk/Dropbox/Chapman lab/TK and Ronny/2nd data analysis/Stomachs/Data")

DT <- as.data.table(read_feather("DT_4_ind"))

#renaming days of the week as integer
DT[, week_num := as.integer(car::recode(week,
                                        "'Monday'='1';'Tuesday'='2';'Wednesday'='3';'Thursday'='4';
    'Friday'='5';'Saturday'='6';'Sunday'='7'"))]

#creating lists of predictor variables
n_type <- unique(DT[, type])
n_date <- unique(DT[, date])
n_weekdays <- unique(DT[, week])
period <- 48



#isolating some of the variables
data_r <- DT[(type == n_type[1] & date %in% n_date[57:70])]

ggplot(data_r, aes(date_time, value)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")


#setting up the first gam
N <- nrow(data_r) # number of observations in the train set
window <- N / period # number of days in the train set
matrix_gam <- data.table(Load = data_r[, value],
                         Daily = rep(1:period, window),
                         Weekly = data_r[, week_num])
#make gam
gam_1 <- gam(Load ~ s(Daily, bs = "cr", k = period) +
               s(Weekly, bs = "ps", k = 7),
             data = matrix_gam,
             family = gaussian)
#plot gam
layout(matrix(1:2, nrow = 1))
plot(gam_1, shade = TRUE)


#extracting fitted values and seeing how well the model performs
datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_1$fitted.values,
                                   data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.1")

#second gam with interaction
gam_2 <- gam(Load ~ s(Daily, Weekly),
             data = matrix_gam,
             family = gaussian)

summary(gam_2)$r.sq
summary(gam_2)$s.table

datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_2$fitted.values,
                                   data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.2")

#third gam, with tensorproduct interaction
gam_3 <- gam(Load ~ te(Daily, Weekly,
                       bs = c("cr", "ps")),
             data = matrix_gam,
             family = gaussian)

summary(gam_3)$r.sq

#fourth gam, increased possible number of knots
gam_4 <- gam(Load ~ te(Daily, Weekly,
                      k = c(period, 7),
                      bs = c("cr", "ps")),
            data = matrix_gam,
            family = gaussian)

summary(gam_4)$r.sq

datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_4$fitted.values,
                                   data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.4")


#Example from https://r.789695.n4.nabble.com/Use-pcls-in-quot-mgcv-quot-package-to-achieve-constrained-cubic-spline-td4660966.html
library(mgcv)
set.seed(0)
n <- 100
x <- runif(n)*4-1;x <- sort(x);
f <- exp(4*x)/(1+exp(4*x));y <- f+rnorm(100)*0.1;plot(x,y)
dat <- data.frame(x=x,y=y) 

## Create a spline basis and penalty, making sure there is a knot
## at the constraint point, (0 here, but could be anywhere)
knots <- data.frame(x=seq(-1,3,length=9)) ## create knots
## set up smoother...
sm <- smoothCon(s(x,k=9,bs="cr"),dat,knots=knots)[[1]] 

## 3rd parameter is value of spline at knot location 0,
## set it to 0 by dropping...
X <- sm$X[,-3]        ## spline basis
S <- sm$S[[1]][-3,-3] ## spline penalty
off <- y*0 + .6       ## offset term to force curve through (0, .6) 
