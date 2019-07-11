##########################################################################################
##                                                                                      ##
#############   Investigatation of energy consumption patterns  ##########################
##              Author: Gabor Dunai                                                     ##
##              Version: 1.0                                                            ##
##              Date: 06.2019                                                           ##
##                                                                                      ##
##########################################################################################

####################                Part 0: directory, libraries, dataset ################

#################### set directory, call libraries ###########################

setwd("A:/B/Ubiqum/module3/Analyses")
library(RMySQL)
library(lubridate)
library(dplyr)
library(ggplot)
library(ggplot2)
library(factoextra)
library(NbClust)
library(mclust)
library(cluster)
library(HSAUR)
library(fpc)
library(fmsb)
library(export)

#################### load basic environment (from a file already saved) ####################

load("A:/B/Ubiqum/module3/Analyses/power_consumtption_basic.RData")

#################### load database (from a csv file)  #############

PowerCons<-read.table('A:/B/Ubiqum/module3/Research/household_power_consumption.txt',
                      header=TRUE, sep=";",fill=TRUE)

#################### create database (from the web with SQL) ##################

con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
dbListTables(con)
dbListFields(con,'yr_2006')

#yearly queries

PowerConsSQL2006<-dbGetQuery(con,"SELECT* FROM yr_2006")
PowerConsSQL2007<-dbGetQuery(con,"SELECT* FROM yr_2007")
PowerConsSQL2008<-dbGetQuery(con,"SELECT* FROM yr_2008")
PowerConsSQL2009<-dbGetQuery(con,"SELECT* FROM yr_2009")
PowerConsSQL2010<-dbGetQuery(con,"SELECT* FROM yr_2010")

PowerConsSQL2006<-PowerConsSQL2006[2:10]
PowerConsSQL2007<-PowerConsSQL2007[2:10]
PowerConsSQL2008<-PowerConsSQL2008[2:10]
PowerConsSQL2009<-PowerConsSQL2009[2:10]
PowerConsSQL2010<-PowerConsSQL2010[2:10]

# binding individual years

WholeDB<-rbind(PowerConsSQL2006,PowerConsSQL2007,PowerConsSQL2008,PowerConsSQL2009,PowerConsSQL2010)
WholeYears<-rbind(PowerConsSQL2007,PowerConsSQL2008,PowerConsSQL2009)

#add a time span as column

WholeDBDate<-cbind(WholeDB, paste(WholeDB$Date,WholeDB$Time))
colnames(WholeDBDate)[10]<-"DateTime"

WholeDBDate<-WholeDBDate[,c(ncol(WholeDBDate), 1:(ncol(WholeDBDate)-1))]

# Convert DateTime from POSIXlt to POSIXct and add time zone

WholeDBDate$DateTime <- as.POSIXct(WholeDBDate$DateTime, "%Y/%m/%d %H:%M:%S")
attr(WholeDBDate$DateTime, "tzone")<-"Europe/Paris"

# create timestamp attributes

WholeDBDate$year<-year(WholeDBDate$DateTime)
WholeDBDate$quarter<-quarter(WholeDBDate$DateTime)
WholeDBDate$month<-month(WholeDBDate$DateTime)
WholeDBDate$week<-week(WholeDBDate$DateTime)
WholeDBDate$weekdays<-weekdays(WholeDBDate$DateTime)
WholeDBDate$day<-day(WholeDBDate$DateTime)
WholeDBDate$hour<-hour(WholeDBDate$DateTime)
WholeDBDate$minute<-minute(WholeDBDate$DateTime)


####################                Part I: descriptive analysis     #####################

#################### calculate consumption from performance metrics #################

WholeDB<-mutate(WholeDB,Sub_metering_1=Sub_metering_1/1000/(1/60),
                Sub_metering_2=Sub_metering_2/1000/(1/60),
                Sub_metering_3=Sub_metering_3/1000/(1/60))

WholeDBDate<-mutate(WholeDBDate,Sub_metering_1=Sub_metering_1/1000/(1/60),
                Sub_metering_2=Sub_metering_2/1000/(1/60),
                Sub_metering_3=Sub_metering_3/1000/(1/60))

WholeYears<-mutate(WholeYears,Sub_metering_1=Sub_metering_1/1000/(1/60),
                Sub_metering_2=Sub_metering_2/1000/(1/60),
                Sub_metering_3=Sub_metering_3/1000/(1/60))

#################### calculate residual consumption #################

WholeDB<-mutate(WholeDB,
                other_kW=Global_active_power-Sub_metering_1-Sub_metering_2-Sub_metering_3)

WholeDBDate<-mutate(WholeDBDate,
                other_kW=Global_active_power-Sub_metering_1-Sub_metering_2-Sub_metering_3)

WholeYears<-mutate(WholeYears,
                other_kW=Global_active_power-Sub_metering_1-Sub_metering_2-Sub_metering_3)

#################### max, min, mean on different frequencies ##########################
  
# max, min, mean values by weekdays

power_MAX<-WholeDBDate %>% group_by(weekdays) %>% 
    summarize(Sub_metering_1_MAX=max(Sub_metering_1),Sub_metering_2_MAX=max(Sub_metering_2),
    Sub_metering_3_MAX=max(Sub_metering_3), others_MAX=max(other_kW),
      amper_max=max(Global_intensity),
    kW_max=max(Global_active_power), unused_kW_max=max(Global_reactive_power))
 
power_MIN<-WholeDBDate %>% group_by(weekdays) %>% 
  summarize(Sub_metering_1_MIN=min(Sub_metering_1),Sub_metering_2_MIN=min(Sub_metering_2),
            Sub_metering_3_MIN=min(Sub_metering_3), others_MIN=min(other_kW),
            amper_min=min(Global_intensity),
            kW_min=min(Global_active_power), unused_kW_min=min(Global_reactive_power))

power_MEAN<-WholeDBDate %>% group_by(weekdays) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1),Sub_metering_2_MEAN=mean(Sub_metering_2),
            Sub_metering_3_MEAN=mean(Sub_metering_3),others_MEAN=mean(other_kW),
            amper_mean=mean(Global_intensity),
            kW_mean=mean(Global_active_power), unused_kW_mean=mean(Global_reactive_power))

# max, min, mean values by months

power_MAX_m<-WholeDBDate %>% group_by(month) %>% 
  summarize(Sub_metering_1_MAX=max(Sub_metering_1),Sub_metering_2_MAX=max(Sub_metering_2),
            Sub_metering_3_MAX=max(Sub_metering_3),others_MAX=max(other_kW),
            amper_max=max(Global_intensity),
            kW_max=max(Global_active_power), unused_kW_max=max(Global_reactive_power))

power_MIN_m<-WholeDBDate %>% group_by(month) %>% 
  summarize(Sub_metering_1_MIN=min(Sub_metering_1),Sub_metering_2_MIN=min(Sub_metering_2),
            Sub_metering_3_MIN=min(Sub_metering_3),others_MIN=min(other_kW),
            amper_min=min(Global_intensity),
            kW_min=min(Global_active_power), unused_kW_min=min(Global_reactive_power))

power_MEAN_m<-WholeDBDate %>% group_by(month) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1),Sub_metering_2_MEAN=mean(Sub_metering_2),
            Sub_metering_3_MEAN=mean(Sub_metering_3),others_MEAN=mean(other_kW),
            amper_mean=mean(Global_intensity),
            kW_mean=mean(Global_active_power), unused_kW_mean=mean(Global_reactive_power))

# max, min, mean values by years

power_MAX_y<-WholeDBDate %>% group_by(year) %>% 
  summarize(Sub_metering_1_MAX=max(Sub_metering_1),Sub_metering_2_MAX=max(Sub_metering_2),
            Sub_metering_3_MAX=max(Sub_metering_3),others_MAX=max(other_kW),
            amper_max=max(Global_intensity),
            kW_max=max(Global_active_power), unused_kW_max=max(Global_reactive_power))

power_MIN_y<-WholeDBDate %>% group_by(year) %>% 
  summarize(Sub_metering_1_MIN=min(Sub_metering_1),Sub_metering_2_MIN=min(Sub_metering_2),
            Sub_metering_3_MIN=min(Sub_metering_3),others_MIN=min(other_kW),
            amper_min=min(Global_intensity),
            kW_min=min(Global_active_power), unused_kW_min=min(Global_reactive_power))

power_MEAN_y<-WholeDBDate %>% group_by(year) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1),Sub_metering_2_MEAN=mean(Sub_metering_2),
            Sub_metering_3_MEAN=mean(Sub_metering_3),others_MEAN=mean(other_kW),
            amper_mean=mean(Global_intensity),
            kW_mean=mean(Global_active_power), unused_kW_mean=mean(Global_reactive_power))

#################### compute total energy usage ###################################

power_MAX_y<-mutate(power_MAX_y,Total_kW=unused_kW_max+kW_max)
power_MAX_m<-mutate(power_MAX_m,Total_kW=unused_kW_max+kW_max)
power_MAX<-mutate(power_MAX,Total_kW=unused_kW_max+kW_max)

power_MEAN_y<-mutate(power_MEAN_y,Total_kW=unused_kW_mean+kW_mean)
power_MEAN_m<-mutate(power_MEAN_m,Total_kW=unused_kW_mean+kW_mean)
power_MEAN<-mutate(power_MEAN,Total_kW=unused_kW_mean+kW_mean)

power_MIN_y<-mutate(power_MIN_y,Total_kW=unused_kW_min+kW_min)
power_MIN_m<-mutate(power_MIN_m,Total_kW=unused_kW_min+kW_min)
power_MIN<-mutate(power_MIN,Total_kW=unused_kW_min+kW_min)

#################### create hourly, daily, monthly, yearly averages #########################

wholeDB_hourly<-WholeDBDate %>% group_by(year,month,day,hour) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1),
            Sub_metering_2_MEAN=mean(Sub_metering_2),
            Sub_metering_3_MEAN=mean(Sub_metering_3),
            others_MEAN=mean(other_kW),
            amper_mean=mean(Global_intensity),
            kW_mean=mean(Global_active_power), 
            unused_kW_mean=mean(Global_reactive_power)) %>% 
  arrange(year,month,day,hour)

wholeDB_daily<-WholeDBDate %>% group_by(year,month,day) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1),
            Sub_metering_2_MEAN=mean(Sub_metering_2),
            Sub_metering_3_MEAN=mean(Sub_metering_3),
            others_MEAN=mean(other_kW),
            amper_mean=mean(Global_intensity),
            kW_mean=mean(Global_active_power), 
            unused_kW_mean=mean(Global_reactive_power)) %>% 
  arrange(year,month,day)

wholeDB_monthly<-WholeDBDate %>% group_by(year,month) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1),
            Sub_metering_2_MEAN=mean(Sub_metering_2),
            Sub_metering_3_MEAN=mean(Sub_metering_3),
            others_MEAN=mean(other_kW),
            amper_mean=mean(Global_intensity),
            kW_mean=mean(Global_active_power), 
            unused_kW_mean=mean(Global_reactive_power)) %>% 
            arrange(year,month)

wholeDB_yearly<-WholeDBDate %>% group_by(year) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1),
            Sub_metering_2_MEAN=mean(Sub_metering_2),
            Sub_metering_3_MEAN=mean(Sub_metering_3),
            others_MEAN=mean(other_kW),
            amper_mean=mean(Global_intensity),
            kW_mean=mean(Global_active_power), 
            unused_kW_mean=mean(Global_reactive_power)) %>% 
  arrange(year)

#################### create ranges of usage ########################################

#amper

amper_range_y<-data.frame(
  cbind(power_MAX_y$amper_max,power_MEAN_y$amper_mean,power_MIN_y$amper_min),
  c(2006,2007,2008,2009,2010))
colnames(amper_range_y)<-c('max','mean','min','date')

amper_range_m<-data.frame(
  cbind(power_MAX_m$amper_max,power_MEAN_m$amper_mean,power_MIN_m$amper_min),
  c('Jan','Feb','Mar','Apr','May','Jun','July','Aug','Sep','Oct','Nov','Dec'))
colnames(amper_range_m)<-c('max','mean','min','date')

amper_range<-data.frame(
  cbind(power_MAX$amper_max,power_MEAN$amper_mean,power_MIN$amper_min),
  c('Thr','Mon','Tus','Fri','Wed','Sat','Sun'))
colnames(amper_range)<-c('max','mean','min','date')

#used kW

kW_range_y<-data.frame(
  cbind(power_MAX_y$kW_max,power_MEAN_y$kW_mean,power_MIN_y$kW_min),
  c(2006,2007,2008,2009,2010))
colnames(kW_range_y)<-c('max','mean','min','date')

kW_range_m<-data.frame(
  cbind(power_MAX_m$kW_max,power_MEAN_m$kW_mean,power_MIN_m$kW_min),
  c('Jan','Feb','Mar','Apr','May','Jun','July','Aug','Sep','Oct','Nov','Dec'))
colnames(kW_range_m)<-c('max','mean','min','date')

kW_range<-data.frame(
  cbind(power_MAX$kW_max,power_MEAN$kW_mean,power_MIN$kW_min),
  c('Thr','Mon','Tus','Fri','Wed','Sat','Sun'))
colnames(kW_range)<-c('max','mean','min','date')

#total kW

TotalkW_range_y<-data.frame(
  cbind(power_MAX_y$Total_kW,power_MEAN_y$Total_kW,power_MIN_y$Total_kW),
  c(2006,2007,2008,2009,2010))
colnames(TotalkW_range_y)<-c('max','mean','min','date')

TotalkW_range_m<-data.frame(
  cbind(power_MAX_m$Total_kW,power_MEAN_m$Total_kW,power_MIN_m$Total_kW),
  c('Jan','Feb','Mar','Apr','May','Jun','July','Aug','Sep','Oct','Nov','Dec'))
colnames(TotalkW_range_m)<-c('max','mean','min','date')

TotalkW_range<-data.frame(
  cbind(power_MAX$Total_kW,power_MEAN$Total_kW,power_MIN$Total_kW),
  c('Thr','Mon','Tus','Fri','Wed','Sat','Sun'))
colnames(TotalkW_range)<-c('max','mean','min','date')

#################### some descriptive plots #######################################

# date axises

date_h<-seq(as.Date("2006-12-16 18:00"),to=as.Date("2010-11-26 22:00"), length.out = 34164)
date_d<-seq(as.Date("2006-12-16"),to=as.Date("2010-11-26"), length.out = 1433)
date_m<-seq(as.Date("2006-12-01"),to=as.Date("2010-11-01"), length.out = 48)

yearly_axis<-c(2006,2007,2008,2009,2010)
monthly_axis<-c('Jan','Feb','Mar','Apr','May','Jun','July','Aug','Sep','Oct','Nov','Dec')
weekdays_axis<-c('Thr','Mon','Tus','Fri','Wed','Sat','Sun')

# line charts for submeters' kW values

ggplot(wholeDB_hourly)+
  geom_line(aes(x=date_h,y=Sub_metering_1_MEAN),color="blue")+
  geom_line(aes(x=date_h,y=Sub_metering_2_MEAN),color="red")+
  geom_line(aes(x=date_h,y=Sub_metering_3_MEAN),color="magenta")+
  geom_line(aes(x=date_h,y=others_MEAN),color="orange")+
  ylab("average kW")+xlab("")

ggplot(wholeDB_daily)+
  geom_line(aes(x=date_d,y=Sub_metering_1_MEAN),colour="blue")+
  geom_line(aes(x=date_d,y=Sub_metering_2_MEAN),colour="red")+
  geom_line(aes(x=date_d,y=Sub_metering_3_MEAN),colour="magenta")+
  geom_line(aes(x=date_d,y=others_MEAN),color="orange")+
  ylab("average kW")+xlab("")

ggplot(wholeDB_monthly)+
  geom_line(aes(x=date_m,y=Sub_metering_1_MEAN),colour="blue")+
  geom_line(aes(x=date_m,y=Sub_metering_2_MEAN),colour="red")+
  geom_line(aes(x=date_m,y=Sub_metering_3_MEAN),colour="magenta")+
  geom_line(aes(x=date_m,y=others_MEAN),color="orange")+
  ylab("average kW")+xlab("")
  
#################### boxplots for ranges #####################################

# amper

boxplot_input_amper_range_y<-
  data.frame(c(amper_range_y$min,amper_range_y$mean,amper_range_y$max),rep(yearly_axis,3))
colnames(boxplot_input_amper_range_y)<-c('values','date')
boxplot(values~date,data=boxplot_input_amper_range_y,main="Amper range for years",
        col="blue",border="magenta")

boxplot_input_amper_range_m<-
  data.frame(c(amper_range_m$min,amper_range_m$mean,amper_range_m$max),rep(monthly_axis,3))
colnames(boxplot_input_amper_range_m)<-c('values','date')
boxplot(values~date,data=boxplot_input_amper_range_m,main="Amper range for months",
        col="blue",border="magenta")

boxplot_input_amper_range<-
  data.frame(c(amper_range$min,amper_range$mean,amper_range$max),rep(weekdays_axis,3))
colnames(boxplot_input_amper_range)<-c('values','date')
boxplot(values~date,data=boxplot_input_amper_range,main="Amper range for weekdays",
        col="blue",border="magenta")

# used kW

boxplot_input_kW_range_y<-
  data.frame(c(kW_range_y$min,kW_range_y$mean,kW_range_y$max),rep(yearly_axis,3))
colnames(boxplot_input_kW_range_y)<-c('values','date')
boxplot(values~date,data=boxplot_input_kW_range_y,main="kW range for years",
        col="darkgrey",border="grey")

boxplot_input_kW_range_m<-
  data.frame(c(kW_range_m$min,kW_range_m$mean,kW_range_m$max),rep(monthly_axis,3))
colnames(boxplot_input_kW_range_m)<-c('values','date')
boxplot(values~date,data=boxplot_input_kW_range_m,main="kW range for months",
        col="darkgrey",border="grey")

boxplot_input_kW_range<-
  data.frame(c(kW_range$min,kW_range$mean,kW_range$max),rep(weekdays_axis,3))
colnames(boxplot_input_kW_range)<-c('values','date')
boxplot(values~date,data=boxplot_input_kW_range,main="kW range for weekdays",
        col="darkgrey",border="grey")

# total kW

boxplot_input_TotalkW_range_y<-
  data.frame(c(TotalkW_range_y$min,TotalkW_range_y$mean,TotalkW_range_y$max),rep(yearly_axis,3))
colnames(boxplot_input_TotalkW_range_y)<-c('values','date')
boxplot(values~date,data=boxplot_input_TotalkW_range_y,main="Used & wasted kW range for years",
        col="orange",border="brown")

boxplot_input_TotalkW_range_m<-
  data.frame(c(TotalkW_range_m$min,TotalkW_range_m$mean,TotalkW_range_m$max),rep(monthly_axis,3))
colnames(boxplot_input_TotalkW_range_m)<-c('values','date')
boxplot(values~date,data=boxplot_input_TotalkW_range_m,main="Used & wasted kW range for months",
        col="orange",border="brown")

boxplot_input_TotalkW_range<-
  data.frame(c(TotalkW_range$min,TotalkW_range$mean,TotalkW_range$max),rep(weekdays_axis,3))
colnames(boxplot_input_TotalkW_range)<-c('values','date')
boxplot(values~date,data=boxplot_input_TotalkW_range,main="Used & wasted kW range for months",
        col="orange",border="brown")

#################### save then delete charts (1) #########################

graph2ppt(file="plots1.pptx",upscale=TRUE, append= TRUE)
graphics.off()


####################                Part II: cluster analysis      #######################

#################### cluster analyses ########################################

# prepare the data

WholeDB_clust<-na.omit(WholeDBDate)
WholeDB_clust<-WholeDB_clust[c(-2,-3)]

# create stamps for aggregating

helpline<-seq(0,to=2049280/15,length.out = 2049280)
helpline<-floor(helpline)
WholeDB_clust<-cbind(helpline,WholeDB_clust)

# 15 min agregation

WholeDB_clust_15min<-
  WholeDB_clust %>% group_by(helpline) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1),
            Sub_metering_2_MEAN=mean(Sub_metering_2),
            Sub_metering_3_MEAN=mean(Sub_metering_3),
            other_kW_mean=mean(other_kW),
            amper_mean=mean(Global_intensity),
            voltage=mean(Voltage),
            unused_kW_mean=mean(Global_reactive_power),
            weekdays=min(weekdays)) %>% 
            arrange(helpline)

# create weekdays averages

## create stamps for aggregating

helpline2<-c(1:(4*24))
helpline3<-rep(helpline2,length.out =2049280/15+1)
WholeDB_clust_15min<-cbind(helpline3,WholeDB_clust_15min)

# weekdays' averages

Monday<-WholeDB_clust_15min %>% group_by(weekdays,helpline3) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean)) %>% 
            filter(weekdays=="hétfõ") %>% 
            arrange(helpline3) %>% 
            mutate(helpline4=sin((helpline3/96)*(2*pi)), helpline3=NULL)

Tuesday<-WholeDB_clust_15min %>% group_by(weekdays,helpline3) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean)) %>% 
            filter(weekdays=="kedd") %>% 
            arrange(helpline3) %>% 
            mutate(helpline4=sin((helpline3/96)*(2*pi)), helpline3=NULL)

Wednesday<-WholeDB_clust_15min %>% group_by(weekdays,helpline3) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean)) %>% 
            filter(weekdays=="szerda") %>% 
            arrange(helpline3)  %>% 
            mutate(helpline4=sin((helpline3/96)*(2*pi)), helpline3=NULL)

Thursday<-WholeDB_clust_15min %>% group_by(weekdays,helpline3) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean)) %>% 
            filter(weekdays=="csütörtök") %>% 
            arrange(helpline3)  %>% 
            mutate(helpline4=sin((helpline3/96)*(2*pi)), helpline3=NULL)

Friday<-WholeDB_clust_15min %>% group_by(weekdays,helpline3) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean)) %>% 
            filter(weekdays=="péntek") %>% 
            arrange(helpline3) %>% 
            mutate(helpline4=sin((helpline3/96)*(2*pi)), helpline3=NULL)

Saturday<-WholeDB_clust_15min %>% group_by(weekdays,helpline3) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean)) %>% 
            filter(weekdays=="szombat") %>% 
            arrange(helpline3) %>% 
            mutate(helpline4=sin((helpline3/96)*(2*pi)), helpline3=NULL)

Sunday<-WholeDB_clust_15min %>% group_by(weekdays,helpline3) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean)) %>% 
            filter(weekdays=="vasárnap") %>% 
            arrange(helpline3) %>% 
            mutate(helpline4=sin((helpline3/96)*(2*pi)), helpline3=NULL)

# normalize by scaling

Monday<-Monday[-1]
Monday_Scale<-scale(Monday)

Tuesday<-Tuesday[-1]
Tuesday_Scale<-scale(Tuesday)

Wednesday<-Wednesday[-1]
Wednesday_Scale<-scale(Wednesday)

Thursday<-Thursday[-1]
Thursday_Scale<-scale(Thursday)

Friday<-Friday[-1]
Friday_Scale<-scale(Friday)

Saturday<-Saturday[-1]
Saturday_Scale<-scale(Saturday)

Sunday<-Sunday[-1]
Sunday_Scale<-scale(Sunday)

#################### k-means clustering, Monday  ###########################

# investigate the number of optimal clusters

## simple rule of thumb

wss <- (nrow(Monday_Scale)-1)*sum(apply(Monday_Scale,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(Monday_Scale, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Elbow method

fviz_nbclust(Monday_Scale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

## Silhouette method

fviz_nbclust(Monday_Scale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

## Gap statistic
          # recommended value: nboot= 500 for your analysis.
          # Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(Monday_Scale, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

##delete plots

graphics.off()

# k-means clustering

kmeansResultsMonday <- kmeans(Monday_Scale, 4)
aggregate(Monday_Scale,by=list(kmeansResultsMonday$cluster),FUN=mean)
Monday_Scale <- data.frame(Monday_Scale, kmeansResultsMonday$cluster)

# visualize results

plot(kmeansResultsMonday$cluster)
clusplot(Monday_Scale, kmeansResultsMonday$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
ggplot(Monday_Scale, aes(x=helpline4, y=Sub_metering_3_MEAN, fill=Sub_metering_1_MEAN, 
                   color=Sub_metering_2_MEAN, size=other_kW_mean^2)) +
                  geom_point(shape=21) +
                  scale_color_gradient(low="red", high="green") +
                  scale_size_continuous(range=c(1,30))

# visualize by clusters, Monday

##1

Monday_Scale_clus1<-Monday_Scale %>% filter(kmeansResultsMonday.cluster==1) %>% 
                summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
                          Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
                          Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
                          other_kW_mean=mean(other_kW_mean),
                          amper_mean=mean(amper_mean),
                          unused_kW_mean=mean(unused_kW_mean))

Monday_Scale_clus1<-rbind(rep(-2,6),rep(2,6),as.data.frame(Monday_Scale_clus1))
radarchart(Monday_Scale_clus1, axistype = 1, pcol= "red", seg = 5, 
           axislabcol = "darkgrey", title = "Monday, cluster 1") #, caxislabels=seq(-2,2,8)

##2

Monday_Scale_clus2<-Monday_Scale %>% filter(kmeansResultsMonday.cluster==2) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Monday_Scale_clus2<-rbind(rep(-2,6),rep(2,6),as.data.frame(Monday_Scale_clus2))
radarchart(Monday_Scale_clus2, axistype = 1, pcol= "red", seg = 5,
           axislabcol = "darkgrey", title = "Monday, cluster 2")

##3

Monday_Scale_clus3<-Monday_Scale %>% filter(kmeansResultsMonday.cluster==3) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Monday_Scale_clus3<-rbind(rep(-2,6),rep(2,6),as.data.frame(Monday_Scale_clus3))
radarchart(Monday_Scale_clus3, axistype = 1, pcol= "red", seg = 5,
           axislabcol = "darkgrey", title = "Monday, cluster 3")

##4

Monday_Scale_clus4<-Monday_Scale %>% filter(kmeansResultsMonday.cluster==4) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Monday_Scale_clus4<-rbind(rep(-2,6),rep(2,6),as.data.frame(Monday_Scale_clus4))
radarchart(Monday_Scale_clus4, axistype = 1, pcol= "red", seg = 5,
           axislabcol = "darkgrey", title = "Monday, cluster 4")

# save then delete charts, Monday 

graph2ppt(file="plots_Monday.pptx",upscale=TRUE, append= TRUE)
graphics.off()

#################### k-means clustering, Tuesday  ###########################

# investigate the number of optimal clusters

## simple rule of thumb

wss <- (nrow(Tuesday_Scale)-1)*sum(apply(Tuesday_Scale,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(Tuesday_Scale, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Elbow method

fviz_nbclust(Tuesday_Scale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

## Silhouette method

fviz_nbclust(Tuesday_Scale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

## Gap statistic

set.seed(123)
fviz_nbclust(Tuesday_Scale, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

## delete plots

graphics.off()

# k-means clustering

kmeansResultsTuesday <- kmeans(Tuesday_Scale, 3)
aggregate(Tuesday_Scale,by=list(kmeansResultsTuesday$cluster),FUN=mean)
Tuesday_Scale <- data.frame(Tuesday_Scale, kmeansResultsTuesday$cluster)

# visualize results

plot(kmeansResultsTuesday$cluster)
clusplot(Tuesday_Scale, kmeansResultsTuesday$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
ggplot(Tuesday_Scale, aes(x=helpline4, y=Sub_metering_3_MEAN, fill=Sub_metering_1_MEAN, 
                          color=Sub_metering_2_MEAN, size=other_kW_mean^2)) +
  geom_point(shape=21) +
  scale_color_gradient(low="red", high="green") +
  scale_size_continuous(range=c(1,30))

# visualize by clusters, Tuesday

##1

Tuesday_Scale_clus1<-Tuesday_Scale %>% filter(kmeansResultsTuesday.cluster==1) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Tuesday_Scale_clus1<-rbind(rep(-2,6),rep(2,6),as.data.frame(Tuesday_Scale_clus1))
radarchart(Tuesday_Scale_clus1, axistype = 1, pcol= "orange", seg = 5, 
           axislabcol = "darkgrey", title = "Tuesday, cluster 1")

##2

Tuesday_Scale_clus2<-Tuesday_Scale %>% filter(kmeansResultsTuesday.cluster==2) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Tuesday_Scale_clus2<-rbind(rep(-2,6),rep(2,6),as.data.frame(Tuesday_Scale_clus2))
radarchart(Tuesday_Scale_clus2, axistype = 1, pcol= "orange", seg = 5, 
           axislabcol = "darkgrey", title = "Tuesday, cluster 2")

##3

Tuesday_Scale_clus3<-Tuesday_Scale %>% filter(kmeansResultsTuesday.cluster==3) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Tuesday_Scale_clus3<-rbind(rep(-2,6),rep(2,6),as.data.frame(Tuesday_Scale_clus3))
radarchart(Tuesday_Scale_clus3, axistype = 1, pcol= "orange", seg = 5, 
           axislabcol = "darkgrey", title = "Tuesday, cluster 3")

# save then delete charts, Tuesday 

graph2ppt(file="plots_Tuesday.pptx",upscale=TRUE, append= TRUE)
graphics.off()

#################### k-means clustering, Wednesday  ###########################

# investigate the number of optimal clusters

## simple rule of thumb

wss <- (nrow(Wednesday_Scale)-1)*sum(apply(Wednesday_Scale,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(Wednesday_Scale, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Elbow method

fviz_nbclust(Wednesday_Scale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

## Silhouette method

fviz_nbclust(Wednesday_Scale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

## Gap statistic

set.seed(123)
fviz_nbclust(Wednesday_Scale, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

## delete plots

graphics.off()

# k-means clustering

kmeansResultsWednesday <- kmeans(Wednesday_Scale, 4)
aggregate(Wednesday_Scale,by=list(kmeansResultsWednesday$cluster),FUN=mean)
Wednesday_Scale <- data.frame(Wednesday_Scale, kmeansResultsWednesday$cluster)

# visualize results

plot(kmeansResultsWednesday$cluster)
clusplot(Wednesday_Scale, kmeansResultsWednesday$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
ggplot(Wednesday_Scale, aes(x=helpline4, y=Sub_metering_3_MEAN, fill=Sub_metering_1_MEAN, 
                          color=Sub_metering_2_MEAN, size=other_kW_mean^2)) +
  geom_point(shape=21) +
  scale_color_gradient(low="red", high="green") +
  scale_size_continuous(range=c(1,30))

# visualize by clusters, Wednesday

##1

Wednesday_Scale_clus1<-Wednesday_Scale %>% filter(kmeansResultsWednesday.cluster==1) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Wednesday_Scale_clus1<-rbind(rep(-2,6),rep(2,6),as.data.frame(Wednesday_Scale_clus1))
radarchart(Wednesday_Scale_clus1, axistype = 1, pcol= "purple", seg = 5, 
           axislabcol = "darkgrey", title = "Wednesday, cluster 1")

##2

Wednesday_Scale_clus2<-Wednesday_Scale %>% filter(kmeansResultsWednesday.cluster==2) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Wednesday_Scale_clus2<-rbind(rep(-2,6),rep(2,6),as.data.frame(Wednesday_Scale_clus2))
radarchart(Wednesday_Scale_clus2, axistype = 1, pcol= "purple", seg = 5, 
           axislabcol = "darkgrey", title = "Wednesday, cluster 2")

##3

Wednesday_Scale_clus3<-Wednesday_Scale %>% filter(kmeansResultsWednesday.cluster==3) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Wednesday_Scale_clus3<-rbind(rep(-2,6),rep(2,6),as.data.frame(Wednesday_Scale_clus3))
radarchart(Wednesday_Scale_clus3, axistype = 1, pcol= "purple", seg = 5, 
           axislabcol = "darkgrey", title = "Wednesday, cluster 3")

##4

Wednesday_Scale_clus4<-Wednesday_Scale %>% filter(kmeansResultsWednesday.cluster==4) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Wednesday_Scale_clus4<-rbind(rep(-2,6),rep(2,6),as.data.frame(Wednesday_Scale_clus4))
radarchart(Wednesday_Scale_clus4, axistype = 1, pcol= "purple", seg = 5, 
           axislabcol = "darkgrey", title = "Wednesday, cluster 4")

# save then delete charts, Wednesday 

graph2ppt(file="plots_Wednesday.pptx",upscale=TRUE, append= TRUE)
graphics.off()

#################### k-means clustering, Thursday  ###########################

# investigate the number of optimal clusters

## simple rule of thumb

wss <- (nrow(Thursday_Scale)-1)*sum(apply(Thursday_Scale,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(Thursday_Scale, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Elbow method

fviz_nbclust(Thursday_Scale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

## Silhouette method

fviz_nbclust(Thursday_Scale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

## Gap statistic

set.seed(123)
fviz_nbclust(Thursday_Scale, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

## delete plots

graphics.off()

# k-means clustering

kmeansResultsThursday <- kmeans(Thursday_Scale, 4)
aggregate(Thursday_Scale,by=list(kmeansResultsThursday$cluster),FUN=mean)
Thursday_Scale <- data.frame(Thursday_Scale, kmeansResultsThursday$cluster)

# visualize results

plot(kmeansResultsThursday$cluster)
clusplot(Thursday_Scale, kmeansResultsThursday$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
ggplot(Thursday_Scale, aes(x=helpline4, y=Sub_metering_3_MEAN, fill=Sub_metering_1_MEAN, 
                          color=Sub_metering_2_MEAN, size=other_kW_mean^2)) +
  geom_point(shape=21) +
  scale_color_gradient(low="red", high="green") +
  scale_size_continuous(range=c(1,30))

# visualize by clusters, Thursday

##1

Thursday_Scale_clus1<-Thursday_Scale %>% filter(kmeansResultsThursday.cluster==1) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Thursday_Scale_clus1<-rbind(rep(-2,6),rep(2,6),as.data.frame(Thursday_Scale_clus1))
radarchart(Thursday_Scale_clus1, axistype = 1, pcol= "darkgreen", seg = 5, 
           axislabcol = "darkgrey", title = "Thursday, cluster 1")

##2

Thursday_Scale_clus2<-Thursday_Scale %>% filter(kmeansResultsThursday.cluster==2) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Thursday_Scale_clus2<-rbind(rep(-2,6),rep(2,6),as.data.frame(Thursday_Scale_clus2))
radarchart(Thursday_Scale_clus2, axistype = 1, pcol= "darkgreen", seg = 5, 
           axislabcol = "darkgrey", title = "Thursday, cluster 2")

##3

Thursday_Scale_clus3<-Thursday_Scale %>% filter(kmeansResultsThursday.cluster==3) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Thursday_Scale_clus3<-rbind(rep(-2,6),rep(2,6),as.data.frame(Thursday_Scale_clus3))
radarchart(Thursday_Scale_clus3, axistype = 1, pcol= "darkgreen", seg = 5, 
           axislabcol = "darkgrey", title = "Thursday, cluster 3")

##4

Thursday_Scale_clus4<-Thursday_Scale %>% filter(kmeansResultsThursday.cluster==4) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Thursday_Scale_clus4<-rbind(rep(-2,6),rep(2,6),as.data.frame(Thursday_Scale_clus4))
radarchart(Thursday_Scale_clus4, axistype = 1, pcol= "darkgreen", seg = 5, 
           axislabcol = "darkgrey", title = "Thursday, cluster 4")

# save then delete charts, Thursday 

graph2ppt(file="plots_Thursday.pptx",upscale=TRUE, append= TRUE)
graphics.off()

#################### k-means clustering, Friday  ###########################

# investigate the number of optimal clusters

## simple rule of thumb

wss <- (nrow(Friday_Scale)-1)*sum(apply(Friday_Scale,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(Friday_Scale, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Elbow method

fviz_nbclust(Friday_Scale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

## Silhouette method

fviz_nbclust(Friday_Scale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

## Gap statistic

set.seed(123)
fviz_nbclust(Friday_Scale, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

## delete plots

graphics.off()

# k-means clustering

kmeansResultsFriday <- kmeans(Friday_Scale, 4)
aggregate(Friday_Scale,by=list(kmeansResultsFriday$cluster),FUN=mean)
Friday_Scale <- data.frame(Friday_Scale, kmeansResultsFriday$cluster)

# visualize results

plot(kmeansResultsFriday$cluster)
clusplot(Friday_Scale, kmeansResultsFriday$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
ggplot(Friday_Scale, aes(x=helpline4, y=Sub_metering_3_MEAN, fill=Sub_metering_1_MEAN, 
                           color=Sub_metering_2_MEAN, size=other_kW_mean^2)) +
  geom_point(shape=21) +
  scale_color_gradient(low="red", high="green") +
  scale_size_continuous(range=c(1,30))

# visualize by clusters, Friday

##1

Friday_Scale_clus1<-Friday_Scale %>% filter(kmeansResultsFriday.cluster==1) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Friday_Scale_clus1<-rbind(rep(-2,6),rep(2,6),as.data.frame(Friday_Scale_clus1))
radarchart(Friday_Scale_clus1, axistype = 1, pcol= "lightblue", seg = 5, 
           axislabcol = "darkgrey", title = "Friday, cluster 1")

##2

Friday_Scale_clus2<-Friday_Scale %>% filter(kmeansResultsFriday.cluster==2) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Friday_Scale_clus2<-rbind(rep(-2,6),rep(2,6),as.data.frame(Friday_Scale_clus2))
radarchart(Friday_Scale_clus2, axistype = 1, pcol= "lightblue", seg = 5, 
           axislabcol = "darkgrey", title = "Friday, cluster 2")

##3

Friday_Scale_clus3<-Friday_Scale %>% filter(kmeansResultsFriday.cluster==3) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Friday_Scale_clus3<-rbind(rep(-2,6),rep(2,6),as.data.frame(Friday_Scale_clus3))
radarchart(Friday_Scale_clus3, axistype = 1, pcol= "lightblue", seg = 5, 
           axislabcol = "darkgrey", title = "Friday, cluster 3")

##4

Friday_Scale_clus4<-Friday_Scale %>% filter(kmeansResultsFriday.cluster==4) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Friday_Scale_clus4<-rbind(rep(-2,6),rep(2,6),as.data.frame(Friday_Scale_clus4))
radarchart(Friday_Scale_clus4, axistype = 1, pcol= "lightblue", seg = 5, 
           axislabcol = "darkgrey", title = "Friday, cluster 4")

# save then delete charts, Friday 

graph2ppt(file="plots_Friday.pptx",upscale=TRUE, append= TRUE)
graphics.off()

#################### k-means clustering, Saturday  ###########################

# investigate the number of optimal clusters

## simple rule of thumb

wss <- (nrow(Saturday_Scale)-1)*sum(apply(Saturday_Scale,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(Saturday_Scale, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Elbow method

fviz_nbclust(Saturday_Scale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

## Silhouette method

fviz_nbclust(Saturday_Scale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

## Gap statistic

set.seed(123)
fviz_nbclust(Saturday_Scale, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

## delete plots

graphics.off()

# k-means clustering

kmeansResultsSaturday <- kmeans(Saturday_Scale, 2)
aggregate(Saturday_Scale,by=list(kmeansResultsSaturday$cluster),FUN=mean)
Saturday_Scale <- data.frame(Saturday_Scale, kmeansResultsSaturday$cluster)

# visualize results

plot(kmeansResultsSaturday$cluster)
clusplot(Saturday_Scale, kmeansResultsSaturday$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
ggplot(Saturday_Scale, aes(x=helpline4, y=Sub_metering_3_MEAN, fill=Sub_metering_1_MEAN, 
                         color=Sub_metering_2_MEAN, size=other_kW_mean^2)) +
  geom_point(shape=21) +
  scale_color_gradient(low="red", high="green") +
  scale_size_continuous(range=c(1,30))

# visualize by clusters, Saturday

##1

Saturday_Scale_clus1<-Saturday_Scale %>% filter(kmeansResultsSaturday.cluster==1) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Saturday_Scale_clus1<-rbind(rep(-2,6),rep(2,6),as.data.frame(Saturday_Scale_clus1))
radarchart(Saturday_Scale_clus1, axistype = 1, pcol= "magenta", seg = 5, 
           axislabcol = "darkgrey", title = "Saturday, cluster 1")

##2

Saturday_Scale_clus2<-Saturday_Scale %>% filter(kmeansResultsSaturday.cluster==2) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Saturday_Scale_clus2<-rbind(rep(-2,6),rep(2,6),as.data.frame(Saturday_Scale_clus2))
radarchart(Saturday_Scale_clus2, axistype = 1, pcol= "magenta", seg = 5, 
           axislabcol = "darkgrey", title = "Saturday, cluster 2")

# save then delete charts, Saturday 

graph2ppt(file="plots_Saturday.pptx",upscale=TRUE, append= TRUE)
graphics.off()

#################### k-means clustering, Sunday  ###########################

# investigate the number of optimal clusters

## simple rule of thumb

wss <- (nrow(Sunday_Scale)-1)*sum(apply(Sunday_Scale,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(Sunday_Scale, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Elbow method

fviz_nbclust(Sunday_Scale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

## Silhouette method

fviz_nbclust(Sunday_Scale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

## Gap statistic

set.seed(123)
fviz_nbclust(Sunday_Scale, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

## delete plots

graphics.off()

# k-means clustering

kmeansResultsSunday <- kmeans(Sunday_Scale, 2)
aggregate(Sunday_Scale,by=list(kmeansResultsSunday$cluster),FUN=mean)
Sunday_Scale <- data.frame(Sunday_Scale, kmeansResultsSunday$cluster)

# visualize results

plot(kmeansResultsSunday$cluster)
clusplot(Sunday_Scale, kmeansResultsSunday$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
ggplot(Sunday_Scale, aes(x=helpline4, y=Sub_metering_3_MEAN, fill=Sub_metering_1_MEAN, 
                           color=Sub_metering_2_MEAN, size=other_kW_mean^2)) +
  geom_point(shape=21) +
  scale_color_gradient(low="red", high="green") +
  scale_size_continuous(range=c(1,30))

# visualize by clusters, Sunday

##1

Sunday_Scale_clus1<-Sunday_Scale %>% filter(kmeansResultsSunday.cluster==1) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Sunday_Scale_clus1<-rbind(rep(-2,6),rep(2,6),as.data.frame(Sunday_Scale_clus1))
radarchart(Sunday_Scale_clus1, axistype = 1, pcol= "brown", seg = 5, 
           axislabcol = "darkgrey", title = "Sunday, cluster 1")

##2

Sunday_Scale_clus2<-Sunday_Scale %>% filter(kmeansResultsSunday.cluster==2) %>% 
  summarize(Sub_metering_1_MEAN=mean(Sub_metering_1_MEAN),
            Sub_metering_2_MEAN=mean(Sub_metering_2_MEAN),
            Sub_metering_3_MEAN=mean(Sub_metering_3_MEAN),
            other_kW_mean=mean(other_kW_mean),
            amper_mean=mean(amper_mean),
            unused_kW_mean=mean(unused_kW_mean))

Sunday_Scale_clus2<-rbind(rep(-2,6),rep(2,6),as.data.frame(Sunday_Scale_clus2))
radarchart(Sunday_Scale_clus2, axistype = 1, pcol= "brown", seg = 5, 
           axislabcol = "darkgrey", title = "Sunday, cluster 2")

# save then delete charts, Sunday 

graph2ppt(file="plots_Sunday.pptx",upscale=TRUE, append= TRUE)
graphics.off()

####################                Part III: appendix     ###############################

#################### hierarchical clustering ######################################

# Ward Hierarchical Clustering

## Monday

distMon <- dist(Monday_Scale, method = "euclidean") # distance matrix
wardMon <- hclust(distMon, method="ward") 
plot(wardMon) # display dendogram
groups <- cutree(wardMon, k=4) # cut tree into 4 clusters
rect.hclust(wardMon, k=4, border="red") # draw dendogram with red borders around the 5 clusters 

## Tuesday

distTues <- dist(Tuesday_Scale, method = "euclidean")
wardTues <- hclust(distTues, method="ward") 
plot(wardTues)
groups <- cutree(wardMon, k=3)
rect.hclust(wardTues, k=3, border="red")

## Wednesday

distWed <- dist(Wednesday_Scale, method = "euclidean")
wardWed <- hclust(distWed, method="ward") 
plot(wardWed)
groups <- cutree(wardWed, k=4)
rect.hclust(wardWed, k=4, border="red")

## Thursday

distThurs <- dist(Thursday_Scale, method = "euclidean")
wardThurs <- hclust(distThurs, method="ward") 
plot(wardThurs)
groups <- cutree(wardThurs, k=4)
rect.hclust(wardThurs, k=4, border="red")

## Friday

distFri <- dist(Friday_Scale, method = "euclidean")
wardFri <- hclust(distFri, method="ward") 
plot(wardFri)
groups <- cutree(wardFri, k=4)
rect.hclust(wardFri, k=4, border="red")

## Saturday

distSat <- dist(Saturday_Scale, method = "euclidean")
wardSat <- hclust(distSat, method="ward") 
plot(wardSat)
groups <- cutree(wardSat, k=2)
rect.hclust(wardSat, k=2, border="red")

## Sunday

distSun <- dist(Sunday_Scale, method = "euclidean")
wardSun <- hclust(distSun, method="ward") 
plot(wardSun)
groups <- cutree(wardSun, k=2)
rect.hclust(wardSun, k=2, border="red")

# save then delete charts, hierarchical clustering

graph2ppt(file="plots_hierch_cluster.pptx",upscale=TRUE, append= TRUE)
graphics.off()

#################### model based clustering by mclust package ##############################

Mclust_Monday<-Mclust(Monday_Scale)
plot(Mclust_Monday)
summary(Mclust_Monday)

Mclust_Tuesday<-Mclust(Tuesday_Scale)
plot(Mclust_Tuesday)
summary(Mclust_Tuesday)

Mclust_Wednesday<-Mclust(Wednesday_Scale)
plot(Mclust_Wednesday)
summary(Mclust_Wednesday)

Mclust_Thursday<-Mclust(Thursday_Scale)
plot(Mclust_Thursday)
summary(Mclust_Thursday)

Mclust_Friday<-Mclust(Friday_Scale)
plot(Mclust_Friday)
summary(Mclust_Friday)

Mclust_Saturday<-Mclust(Saturday_Scale)
plot(Mclust_Saturday)
summary(Mclust_Saturday)

Mclust_Sunday<-Mclust(Sunday_Scale)
plot(Mclust_Sunday)
summary(Mclust_Sunday)

#################### more plots #############################
# max barplots

ggplot(power_MAX_y, aes(x=year,y=amper_max))+geom_bar(fill=heat.colors(5), stat="identity")
ggplot(power_MAX_m, aes(x=month,y=amper_max))+geom_bar(fill=heat.colors(12), stat="identity")
ggplot(power_MAX, aes(x=weekdays,y=amper_max))+geom_bar(fill=heat.colors(7), stat="identity")

ggplot(power_MAX_y, aes(x=year,y=kW_max))+geom_bar(fill=heat.colors(5), stat="identity")
ggplot(power_MAX_m, aes(x=month,y=kW_max))+geom_bar(fill=heat.colors(12), stat="identity")
ggplot(power_MAX, aes(x=weekdays,y=kW_max))+geom_bar(fill=heat.colors(7), stat="identity")

ggplot(power_MAX_y, aes(x=year,y=Total_kW))+geom_bar(fill=heat.colors(5), stat="identity")
ggplot(power_MAX_m, aes(x=month,y=Total_kW))+geom_bar(fill=heat.colors(12), stat="identity")
ggplot(power_MAX, aes(x=weekdays,y=Total_kW))+geom_bar(fill=heat.colors(7), stat="identity")

# max, mean, minimum line charts

ggplot(amper_range_y)+
  geom_line(aes(x=date,y=min),colour="blue")+
  geom_line(aes(x=date,y=mean),colour="red")+
  geom_line(aes(x=date,y=max),colour="magenta")+
  ylab("average ampere")+xlab("")

ggplot(kW_range_y)+
  geom_line(aes(x=date,y=min),colour="blue")+
  geom_line(aes(x=date,y=mean),colour="red")+
  geom_line(aes(x=date,y=max),colour="magenta")+
  ylab("average used kW")+xlab("")

ggplot(TotalkW_range_y)+
  geom_line(aes(x=date,y=min),colour="blue")+
  geom_line(aes(x=date,y=mean),colour="red")+
  geom_line(aes(x=date,y=max),colour="magenta")+
  ylab("average total kW")+xlab("")

# delete charts

graphics.off()

