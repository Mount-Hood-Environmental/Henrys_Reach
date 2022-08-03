# Author: Bryce Oldemeyer
# Purpose: This script pulls USGS stream data using USGS-dataRetrieval package and outputs figures and summary statistics for the selected stations
# Created: 4/2019
# Last Modified: 6/28/2022

# 
library(tidyverse)
library(zoo)
library(here)
library(lubridate)
# Install devtools to retrieve latest version of USGS-dataRetrieval via github
# install.packages(devtools)
# Use devtools to install the latest version of USGS-dataRetrieval via github
# library(devtools)
# install_github("USGS-R/dataRetrieval")
# Update all packages via "tools>Check for package updates>update all" if you are having issues installing dataRetrieval
# using the "install_github() function

library(dataRetrieval)

rm(list=ls()) #Clear global environment

# Lemhi River sites - I suspect 13305000 is the sites we will want to use for the Henry's Reach app but it doesn't hurt to have both.
# Lemhi River near Lemhi 13305000 
# Lemhi below L5 Diversion 13305310

# Parameter codes for dataRetrieval
# Discharge = "00060"
# Gage height = "00065"
# Temperature = "00010"
# Precipitation = "00045"
# pH = "00400"

# Stat codes for dataRetrieval
# Max = "00001"
# Min = "00002"
# Mean = "00003"
# Median = "00008"

flow.sites<-c("13305000","13305310")

site.table <- as.data.frame(seq(from = as.Date("1993-01-01", format="%Y-%m-%d"),
                                  to = Sys.Date(),
                                  by = "days"))  
names(site.table)[1] <- "Date"

for(siteNumber in flow.sites){
  
  data<-readNWISdv(siteNumber, parameterCd="00060", startDate = "1993-01-01")
  data<-data[,3:4]
  
  site.table<-merge.data.frame(site.table, data, by="Date", all = T)
}

site.list<-c("LemUP","LemL5")

names(site.table)[2:3] <- site.list


site.table$Date<-as.POSIXlt(as.character(site.table$Date), format="%Y-%m-%d")#Format date
####### Start loop here if you want water flow figure for each

for(siteName in site.list){
  
  input<-site.table[,c("Date",siteName)]
  input = filter(input, !(month(Date) == 2 & day(Date) == 29))
  
  ndays<-dim(input)[1]
  
  firstday<-as.POSIXlt(as.character(input$Date[1]))$yday+1
  lastday<-as.POSIXlt(as.character(input$Date[ndays]))$yday+1
  lastyear<-as.POSIXlt(as.character(input$Date[ndays]))$year+1900
  firstyear<-as.POSIXlt(as.character(input$Date[1]))$year+1900
  
  leap.years<-seq(firstyear+(4-firstyear%%4)%%4,lastyear,4)
  
  if((lastday>274)|((lastday==274)&!(lastyear %in% leap.years))){lastwy<-lastyear+1}else{
    lastwy<-lastyear}
  
  if((firstday<274)|((firstday==274)&(firstyear %in% leap.years))){firstwy<-firstyear}else{
    firstwy<-firstyear+1}
  
  nwy<-lastwy-firstwy+1
  nwy
  
  out<-data.frame(array(NA,c(365+6,nwy+6)))
  rownames(out)[1:365]<-format(seq(as.Date("2014-01-01"),as.Date("2014-12-31"),1),"%b-%d")
  rownames(out)[366:371]<-c("Min","Q1","Median","Mean","Q3","Max")
  colnames(out)[1:nwy]<-as.character(firstwy:lastwy)
  colnames(out)[(nwy+1):(nwy+6)]<-c("Min","Q1","Median","Mean","Q3","Max")
  
  dates_fill <- as.data.frame(seq(from = min(as.Date(input$Date, format="%Y-%m-%d")),
                                  to = max(as.Date(input$Date, format="%Y-%m-%d")),
                                  by = "days"))
  
  names(dates_fill) <- c("Date")
  input$Date<-as.Date(input$Date)
  
  input = merge(input, dates_fill, by="Date", all= TRUE)
  
  find_leap = function(x){
       day(x) == 29 & month(x) == 2 
     }
  
  for(j in 1:(nwy-1)){
                 dates<-seq(as.Date(paste(as.character(firstwy+(j-1)),"-01-01",sep="")),as.Date(paste(as.character(firstwy+(j-1)),"-12-31",sep="")),1)
                 dates<-dates[!find_leap(dates)]
               
    out[1:365,j]<-input[input$Date %in% dates,2]
  }

  #
  # Now for the last water year
  #
  
  dates<-seq(as.Date(paste(as.character(lastwy),"-01-01",sep="")),
                          max(input$Date),1)
             
  
  out[1:length(dates),nwy]<-input[input$Date %in% dates,2]
  
  #
  #
  #Now the summaries
  #
  #
  
  for(k in 1:365){
    out[k,(nwy+1):(nwy+6)]<-apply(out[k,1:nwy],1,summary)[1:6]}
  
  for(k in 1:nwy){
    if((is.na(out[1,k])==FALSE)&(is.na(out[365,k])==FALSE)){
      out[(365+1):(365+6),k]<-apply(as.matrix(out[1:365,k]),2,summary)[1:6]
    }
  }
  
  write.csv(out, paste(here("Data"),"/",siteName,"_Flow_JY.csv", sep = ""))
}

# EXAMPLE PLOT FOR SUMMARY

first<-as.Date("2018-10-01")
last<-as.Date("2019-09-30")
pcol<-ncol(out)

plot.data<-out[1:365,(pcol-7):pcol]
plot.data$Day <- as.Date(rownames(plot.data), format="%b-%d")
plot.data$Day<-seq(first,last,1)
names(plot.data)[2] <- "Y2022"
names(plot.data)[1] <- "Y2021"

dates.plot<-plot.data$Day

#png(filename = paste(siteName," Discharge.png", sep = ""),
#    width = 7.5, height = 5.5,units = "in", pointsize = 12,
#    bg = "white", res = 600, family = "", restoreConsole = TRUE,
#    type = c("windows", "cairo", "cairo-png"), antialias="cleartype")

plot(plot.data$Day, plot.data$Mean, type="n",
     ylim=c(0,max(plot.data$Max)), ylab="Discharge (cfs)",
     xlab="Date", main=siteName, cex.lab=1.2, cex.main=1.4, xaxt='n')

axis.Date(1,dates.plot,seq(first,last+1,"month"),format="%b-%d")

polygon(c(plot.data$Day, rev(plot.data$Day)),
        c(plot.data$Max,rev(plot.data$Min)),
        col = "grey90", border = NA)

abline(h=seq(0,max(plot.data$Max),(max(plot.data$Max)/8)),lty=2, col="gray85")
abline(v=seq(first,last+1,"month"),col="gray85",lty=2)
abline(h=0)

lines(plot.data$Day,plot.data$Y2021,lwd=1.5,lty=2,col="green4")
lines(plot.data$Day,plot.data$Mean,lwd=1.5,lty=3,col="red")
lines(plot.data$Day,plot.data$Y2022,lwd=2.5,col="blue")

legend("topleft", col = c("blue","green4","red",NA), lty = c(1,2,3,NA),bg="white",box.col=1, 
       lwd = c(2.5,1.5,1.5,NA),fill=c(NA,NA,NA,"gray90"),border=c(NA,NA,NA,"gray90"), 
       legend = c( paste("2022 through", Sys.Date()),"2021 actual","1978-2021 average","1978-2021 range"))

#dev.off() 




  
  
