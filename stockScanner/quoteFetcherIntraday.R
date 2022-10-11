# This script fetches and updates the daily data of all the stocks lised under HKEX
# Run writeAllHistory once to fetch all the historical data.
# Then run updateAllHistory daily after market close to update the latest data.

require(XML)
require(quantmod)

source("quoteFetcher.R")
#options(warn=-1)

# Get the latest quote of all stocks (but with 15 minutes delay due to data source limitations)
# Took about 10 seconds.
# Sample output
#                  Trade Time  Last Change % Change  Open  High   Low   Volume
# 0001.HK 2017-01-13 16:08:00 92.05   0.45   0.0049 91.50 92.15 91.20  4235011
# 0002.HK 2017-01-13 16:08:00  75.3   0.45   0.0060 74.95 75.45 74.90  1708300
# 0003.HK 2017-01-13 16:08:00  14.5   0.10   0.0069 14.44 14.50 14.40 11908725
getLatestQuotes <- function(ss=getSymbolList()){
  #ss<-data.frame(symbol=c("0001")) # For debugging
  q<-getQuote(paste0(ss[,1],".HK")) # Using the function from quantmod;
  
  q[,1]<-fixtime(q[,1]) # fix the bug of the Yahoo data in which the time is always supplied as am time
  q[,4]<-as.numeric(gsub("%","",q[,4]))/100 # turn the % change into number percents (in decimal numbers)
  # Turn all other fake chars into numbers. 
  # Will warn about "NAs introduced by coercion" for non-numeric values (e.g. N/A) but those can be safely ignored.
  q<-transform(q,Last=as.numeric(Last),Open=as.numeric(Open),High=as.numeric(High),Low=as.numeric(Low))
  # Return only the records without any NA
  return(q[complete.cases(q),]) 
}

# ------ The time format issue ------
# For unknown reason, the time stamps from Yahoo are always returned as am.
# This means, 09:00 is 09:00, but 13:00 is 01:00.
# To fix this, I need to convert the time to string using strftime, attach am/pm to it, and convert it back to time using strptime.
# Hour part from getQuote: 09-11 -> AM; 01-04 or 12 -> PM 
# t is the column in the data frame created by getQuote of the list of symbols, e.g., q[,1].
fixtime<-function(t){
  tString <- strftime(t,"%Y-%m-%d %H:%M:%S") # covert time to string 
  t09 <- strptime(paste(strftime(t,"%Y-%m-%d"), "9"),"%Y-%m-%d %H") # time object for 0900 on that day
  t12 <- strptime(paste(strftime(t,"%Y-%m-%d"), "12"),"%Y-%m-%d %H") # time object for 1200 on that day
  unlist(as.POSIXct(strptime(paste(tString, ifelse(t>=t09 & t<t12,"am","pm")),"%Y-%m-%d %I:%M:%S %p")))
}
# -----------------------------------

# Update the latest quote of one stock (represented by one record of getQuote output) to the history file
# e.g. of input parameter: 
#                  Trade Time  Last Change % Change  Open  High   Low   Volume
# 0001.HK 2017-01-13 04:08:00 92.05   0.45   +0.49% 91.50 92.15 91.20  4235011
updateHistory.intraday <- function(oneQuote){
  filename<-paste0("~/Documents/stockTrader.Data/HKEX.Intraday/",rownames(oneQuote),".dat")
  # Write to file only if current quote is not already in history file
  if(file.exists(filename)){
    #cat(rownames(oneQuote),"...")
    if(difftime(end(readHistory.intraday(rownames(oneQuote))),oneQuote[,1])<0){
      try(write.table(oneQuote, file=filename,row.names=F,append=T,col.names=F,sep=",",quote=F))
    }
  } else {
    try(write.table(oneQuote, file=filename,row.names=F,append=T,col.names=T,sep=",",quote=F))
  }
  #tmp  <- readHistory.intraday(rownames(quote))
  #tmp  <- tmp[ ! duplicated( index(tmp), fromLast = TRUE ),  ]
  #tmp2 <- data.frame(Trade.Time=index(tmp), coredata(tmp))
  #try(write.table(tmp2, file=filename,row.names=F,col.names=T,sep=","))
}

# Update the latest quote to the history file.
updateAllHistory.intraday <- function(allQuotes=getLatestQuotes(ss=getSymbolList())){ 
  try(invisible(by(allQuotes,1:nrow(allQuotes), updateHistory.intraday))) 
}

# Read the intraday quote history for s
readHistory.intraday <- function(s){
  as.xts(read.zoo(paste0("~/Documents/stockTrader.Data/HKEX.Intraday/",s,".dat"),sep=",",format="%Y-%m-%d %H:%M:%S",FUN=as.POSIXct,header=T))
}
