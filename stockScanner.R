#!/usr/bin/env Rscript
# This script scans all the stocks listed under HKEX and returns the results based on the filters.
# Run this once in the morning of the trade day to update all the daily stock quotes and scan for any noteworthy stocks
# to be further scanned by the intraday scanner.
# Console syntax e.g.: ./stockScanner.R > dailyScanReport.txt

require(XML)
require(quantmod)
source("quoteFetcher.R")
options("getSymbols.warning4.0"=FALSE)

# Check if the stock s has been rising for certain number of days
# Return average rate of change if true. Return 0 otherwise.
isRising<-function(s, days=3){
  q<-readHistory(s)
  if(nrow(q)<days+1)
    return(0)
  else {
    # Note: The dailyReturn in the first day is calculated by close price - open price rather than difference with previous closing.
    # Therefore stocks that are rising in 2 days will not be counted as rising in 1 day if open<=close (but still closing price > previous closing price) for that day.
    # The solution is to calculate daily return first before taking the last.
    # Same for isFalling below.
    change<-last(dailyReturn(q),paste0(days,' days'))
    return(ifelse(all(change>0),mean(change),0))
  }
}

isAnyRising <- function(days=3){
  for(s in gsub(".dat","",list.files(path="~/Documents/stockTrader.Data/HKEX"))) {
    #cat("Checking",s,"...\n")
    r1<-isRising(s,days)
    r2<-last(Vo(readHistory(s)))
    if(r1>0 && r2>50000000) cat(s,"has been rising for",days,"day(s) with average ",r1*100,"% change per day and latest volume of",r2/1000000,"M!\n")
  }
}

# Check if the stock s has been falling for certain number of days
# Return average rate of change if true. Return 0 otherwise.
isFalling<-function(s, days=3){
  q<-readHistory(s)
  if(nrow(q)<days+1)
    return(0)
  else {
    change<-last(dailyReturn(q),paste0(days,' days'))
    return(ifelse(all(change<0),mean(change),0))
  }
}

isAnyFalling <- function(days=5){
  for(s in gsub(".dat","",list.files(path="~/Documents/stockTrader.Data/HKEX"))) {
    #cat("Checking",s,"...\n")
    r1<-isFalling(s,days)
    if(r1<0) cat(s,"has been falling for",days,"day(s) with average ",r1*100,"% change per day!\n")
  }
}

# Is the last trade volume exceeds the [days]-day moving average by at least ([change]*100)%,
# AND the volume is larger than [vmin]-M?
# Return the volume if true. Return 0 otherwise.
isAbnormalVolume <- function(s,days=9,change=2.,vmin=50){
  q<-readHistory(s)
  lastVolAverage<-mean(last(Vo(q),days))
  if(lastVolAverage==0) return(0)
  r<-last(Vo(q))/lastVolAverage -1
  return(ifelse(r>change && last(Vo(q))>vmin*1000000,last(Vo(q)),0))
}

isAnyAbnormalVolume <- function(days=9,change=2.,vmin=50){
  for(s in gsub(".dat","",list.files(path="~/Documents/stockTrader.Data/HKEX"))) {
    #cat("Checking",s,"...\n")
    r<-isAbnormalVolume(s,days,change,vmin)
    if(r>0) cat(s,"has", r*100, "% increase in trade volume w.r.t. previous", days,"-day moving average!\n")
  }
}

# Return the stocks with top-n volume
topVolume<-function(v=getLatestHistory(),n=20){
  return(head(v[order(-v$volume),],n))
}

# Return the stocks with top-n daily increase
topRise<-function(v=getLatestHistory(),n=20){
  v$change<-v$close-v$open
  v$pctchange<-v$change/v$open*100
  return(head(v[order(-v$pctchange),],n))
}

# Return the stocks with top-n daily decrease
topFall<-function(v=getLatestHistory(),n=20){
  v$change<-v$close-v$open
  v$pctchange<-v$change/v$open*100
  return(head(v[order(v$pctchange),],n))
}


plot<-function(s){
  chartSeries(last(readHistory(s),'30 days'))
  addBBands()
  addEMA(9)
}


# How many bars has the stock price for s been rising / falling continuously?
# e.g. Continuous rise for 2 bars: nChange=+2; Contnuous fall for 3 bars: nChange=0; No change: nChange=0;
# Return the information as one row data frame.
findChange <- function(s){
  #cat(s,"...")
  q<-readHistory(s)
  #qSMA<-rollmean(Cl(q),min(3,nrow(q)),align="right") # Use SMA for trend detection
  #r<-rle(drop(coredata(sign(ClCl(qSMA)))))
  r<-rle(drop(coredata(sign(ClCl(q)))))
  avePctChange<-mean(last(ClCl(q),last(r$lengths)))*100 # Average change rate in those last continuous changes
  return(data.frame(row.names=s, date = last(index(q)), last=drop(coredata(last(q[,4]))), nChange=last(r$lengths)*last(r$values), avePctChange=avePctChange, volumeM=drop(coredata(last(q[,5])))/1000000))
}

# Return the findChange.intraday(s) for all stocks as one single data frame.
# Sort the results by nChange then average rate of change then volume. Omit all rows with NA.
findAllChanges <- function(){
  m<-NULL
  for(s in gsub(".dat","",list.files(path="~/Documents/stockTrader.Data/HKEX"))) {
    m<-rbind(m,findChange(s))
  }
  m<-m[complete.cases(m),] # Remove all rows with NAs
  return(m[order(-m$nChange, -m$avePctChange, -m$volumeM),]) # Sort the results by nChange then average rate of change then volume
}

changePercentiles <- function(g=getLatestHistory()){
  return(quantile((g$close/g$open-1)*100,c(.5,.75,.9, .95, .99)))
}

topIntradayChange<-function(g=getLatestHistory()){
  g$pctChange<-(g$close/g$open-1)*100
  return(g[order(-g$pctChange),])
}

#stop("Stop for debugging\n")

# Main program starts here

cat("===================================\n")
cat("=== DAILY STOCK SCANNING REPORT ===\n")
cat("===================================\n")
cat(paste("Date:",Sys.Date(),"\n"))

cat("\n# Update all history...\n")
updateAllHistory()

cat("\n# Scan for noteworthy stocks...\n")

cat("\n# Momentum Trade Opportunities\n")
m<-findAllChanges()
cat("\nStocks with uptrend over 3-5 days (w.r.t. closing prices) and at least 10M volume in last day?\n")
print(m[m$nChange>=3 & m$nChange<=5 & Sys.Date()-m$date<=5 & Sys.Date()-m$date>=0 & m$volumeM>=10,  ])
cat("\nTop stocks with uptrend over 6 or more days (w.r.t. closing prices) and at least 10M volume in last day?\n")
print(m[m$nChange>5 & Sys.Date()-m$date<=5 & Sys.Date()-m$date>=0 & m$volumeM>=10,  ])
cat("\nHow much did the stocks rise (in terms of percetiles w.r.t close/open-1 within the last trade day?\n")
g<-getLatestHistory()
changePercentiles(g)

cat("\n# Reversal Trade Opportunities\n")
n<-m[order(m$nChange),]
cat("\nAny stocks continuously falling for 5 or more days (w.r.t. closing prices 3SMA)?\n")
print(n[n$nChange<=-5 & Sys.Date()-n$date<=5 & Sys.Date()-n$date>=0,  ])

cat("\nEnd of report\n")


