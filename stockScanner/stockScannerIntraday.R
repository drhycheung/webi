#!/usr/bin/env Rscript
# This script loops over the day to collect the 1-min stock quotes for all the stocks listed under HKEX.
# Start it in the beginning of the trade day and let it loop till the end.

options(warn=-1)

source("quoteFetcherIntraday.R")
source("quoteFetcher.R")

# Check if the stock s has been rising for certain number of bars
# Return average rate of change if true. Return 0 otherwise.
isRising.intraday<-function(s, bars=3){
  q<-readHistory.intraday(s)
  if(nrow(q)<bars+1)
    return(0)
  else {
    change<-tail( (q$Last-Lag(q$Last))/Lag(q$Last), bars )
    return(ifelse(all(change>0),mean(change),0))
  }
}

# Return any stock rising for [bars] bars and with at least 1M volume in last trade
isAnyRising.intraday <- function(bars=3,vo=1000000){
  for(s in gsub(".dat","",list.files(path="~/Documents/stockTrader.Data/HKEX.Intraday"))) {
    #cat("Checking",s,"...\n")
    r1<-isRising.intraday(s,bars)
    q<-Vo(readHistory.intraday(s))
    r2<-last(Vo(q)-Lag(Vo(q))) # volume is cumulative, so subtract the previous one to get the volume for that bar
    if(r1>0 && r2>vo) cat(s,"has been rising for",bars,"bar(s) with average ",r1*100,"% change per bar and latest volume of",r2/1000000,"M!\n")
  }
}

# How many bars has the stock price for s been rising / falling continuously?
# e.g. Continuous rise for 2 bars: nChange=+2; Contnuous fall for 3 bars: nChange=0; No change: nChange=0;
# Return the information as one row data frame.
findChange.intraday <- function(s){
  q<-readHistory.intraday(s)
  qSMA<-rollmean(q$Last,min(9,nrow(q)),align="right") # Use SMA for trend detection
  r<-rle(drop(coredata(sign(qSMA$Last-Lag(qSMA$Last))))) 
  #change<-mean(last(q$Last/Lag(q$Last)-1,last(r$lengths))) # Average change rate in those last continuous changes
  pctRange<-(drop(coredata(last(q$Last)))/first(last(q$Last,last(r$lengths)))-1)*100 # % change over the last continuous changes
  pctFromOpen<-(last(q$Last)/last(q$Open)-1)*100 # % change w.r.t. today's open
  volumeM<-mean(last(Vo(q)-Lag(Vo(q)),last(r$lengths)))/1000000 # Average volume in those last continuous changes
  #m<-data.frame(row.names=s, time=last(index(q)), last=last(q$Last), nChange=last(r$lengths)*last(r$values), change=round(change,2), fromOpen<-round(fromOpen,2), range=round(range,2), volume=volume)
  m<-data.frame(row.names=s, time=last(index(q)), last=last(q$Last), nChange=last(r$lengths)*last(r$values), pctFromOpen<-round(pctFromOpen,4), pctRange=round(pctRange,2), volumeM=round(volumeM,2))
  colnames(m)<-c("time","last","nChange","pctFromOpen","pctRange","volumeM")
  return(m)
}

# Return the findChange.intraday(s) for all stocks as one single data frame.
# Sort the results before return. Omit all rows with NA.
findAllChanges.intraday <- function(){
  m<-NULL
  for(s in gsub(".dat","",list.files(path="~/Documents/stockTrader.Data/HKEX.Intraday"))) {
    m<-rbind(m,findChange.intraday(s))
  }
  m<-m[complete.cases(m),] # Remove all rows with NAs
  return(m[order(-m$nChange, -m$pctRange, -m$volumeM),]) # Sort the results
}

# Find all the gap open (def as today open - last trade day close)
# Warning: Ensure that the daily data belongs to the last trade day, and getQuote data belongs to the trade day after the last trade day
findAllGaps<-function(){
  
  Q1<-getLatestQuotes() #quotes from intraday
  Q2<-getLatestHistory() #quotes from daily
  
  QQ<-transform(merge(Q1,Q2,by="row.names"), row.names=Row.names, Row.names=NULL)[, c("date", "Open", "close")]
  colnames(QQ) <- c("PreviousCloseDate", "Open", "PreviousClose")
  QQ$pctChange <- (QQ$Open / QQ$PreviousClose-1)*100
  
  return(QQ[rev(order(QQ$PreviousCloseDate, QQ$pctChange)),c(1,3,2,4)]) 
}

# Check if stock s is rising with last volume larger than the volume SMA
# Return a one-line data frame containing the information if yes.
# Otherwise return nothing.
# Author: Dalimeow
isAnyVolIncSMAnStep.intraday<-function(s="0002.HK", nRiseBar = 1, nStepSMA=5){
  r<-isRising.intraday(s, bars=nRiseBar)
  q<-readHistory.intraday(s)
  
  if (r>0 & nrow(q) >= nStepSMA) {
    q$netVol = ifelse(is.na(q[,7]-lag(q[,7])), q[,7] , q[,7]-lag(q[,7]) )
    q$VolSMA = rollmean(q[, 8], k = nStepSMA, fill = NA, align="right")
    
    q<-data.frame(index(q), q$Last, q$netVol, q$VolSMA, lag(q$VolSMA))
    #q<-data.frame(q$Last, q$netVol, rollapply(q[,8], width=nStepSMA, FUN=mean, fill=NA, align="right"))
    colnames(q)<- c("Date", "Last", "VolM", "VolMSMA", "VolMSMA1Lag")
    
    return(data.frame(row.names = s, Date=last(q$Date), 
                      PricePctChg=round(r*100, digits=1), PriceLast=last(q$Last), 
                      VolM = round(last(q$VolM)/1000000,2), VolMSMA1Lag = round(last(q$VolMSMA1Lag)/1000000,2), 
                      VolIncTimes=round(last(q$VolM) / last(q$VolMSMA1Lag), digits = 0)))
    
  }
}


# Finds the rising stocks with last volume larger than the volume SMA
# Returns a data frame containing the information.
# Author: Dalimeow
# Got the following error in the morning of 20170202:
# Error in -m$VolIncTimes : invalid argument to unary operator
# Calls: AllVolIncSMAnStep.intraday -> order
# Execution halted
AllVolIncSMAnStep.intraday<-function(){
  m<-NULL
  for(s in gsub(".dat","",list.files(path="~/Documents/stockTrader.Data/HKEX.Intraday"))) {
    m<-rbind(m,isAnyVolIncSMAnStep.intraday(s))
  }
  
  return(m[order(-m$VolIncTimes, -m$PricePctChg),])
}


# Main program starts here

#stop("Stop for debugging\n")

cat("======================================\n")
cat("=== INTRADAY STOCK SCANNING REPORT ===\n")
cat("======================================\n")
cat(paste("Date:",Sys.Date(),"\n"))

# Ensure that it is trading time
if(Sys.time()<strptime("09:45","%H:%M") | Sys.time()>strptime("16:15","%H:%M")){
  stop("Market not yet open or trading data not yet available.\n")
  #cat("No trading data available at this moment. Intraday Scanner terminated.\n")
}

i<-1

# Get the full stock list (do this only once per day)
sList<-getSymbolList()

# First find the gaps
if(Sys.time()<strptime("09:50","%H:%M")){
  cat("\nScanning all stocks for opening gaps...\n")
  cat(paste0("[",Sys.time(),"]"), " What are the top 10 opening gaps?\n")
  g<-findAllGaps() # The function will fetch latest quotes. No need to update history above.
  print(head(g,10))
}

reportedLunchBreak<-FALSE
repeat {
  
  # Take a break during lunch
  if(Sys.time()>strptime("12:15","%H:%M") && Sys.time()<strptime("13:15","%H:%M")) {
    if(!reportedLunchBreak){ 
      cat("\n--- Lunch break ---\n") 
      reportedLunchBreak<-TRUE
    }
    next
  }
  # Terminate after market close
  if(Sys.time()>strptime("16:15","%H:%M")){
    break
  }
  
  cat("\nScanning all stocks... ( loop",i,")\n")
  i<-i+1
  updateAllHistory.intraday(getLatestQuotes(sList))
  
  cat("Apply filters...\n")
  m<-findAllChanges.intraday()
  #g<-AllVolIncSMAnStep.intraday()
  
  cat("===================================================================================\n")
  cat(paste0("[",Sys.time(),"]"), " What are the top rising stocks in terms of (approx) 1-min 9SMA?\n")
  #isAnyRising.intraday(bars=1,vo=500000)
  
  cat("Top 1st to 5th:\n")
  print(head(m,5))
  cat("Top 6th-10th:\n")
  print(last(head(m,10),5))
  
  cat(paste0("[",Sys.time(),"]"), " What are the top falling stocks in terms of (approx) 1-min 9SMA?\n")
  print(head(m[order(m$nChange, m$pctRange, -m$volumeM),],5))
  
  #cat(paste0("\n[",Sys.time(),"]"), " Any rising stock with abnormally large volume?\n")
  #print(head(g,10))
  cat("===================================================================================\n")
  cat("... DONE. Sleep for 20 seconds.\n")
  Sys.sleep(20)
}

cat("\nEnd of intraday report\n")

