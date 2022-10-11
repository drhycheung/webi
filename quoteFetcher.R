# This script fetches and updates the daily data of all the stocks lised under HKEX
# Run writeAllHistory once to fetch all the historical data.
# Then run updateAllHistory daily after market close to update the latest data.

require(XML)
require(quantmod)

# Get all symbols listed under HKEX
getSymbolList <- function(){
  # First fetch the symbols from the main board
  #url1<-"http://www.hkex.com.hk/chi/market/sec_tradinfo/stockcode/eisdeqty_pf.htm" # Main board
  url1<-"http://www.hkex.com.hk/eng/market/sec_tradinfo/stockcode/eisdeqty_pf.htm"
  ss1<-readHTMLTable(url1,header=T,which=3,stringsAsFactors=F) # Get the table from web
  ss1<-tail(ss1,-4) # First 4 rows are useless
  rownames(ss1)<-NULL # Recount row numbers after row removal
  colnames(ss1)<-c("symbol", "companyName", "contractSize", "C", "H", "O", "F") # Assign col names
  # Next fetch the symbols from GEM
  #url2<-"http://www.hkex.com.hk/chi/market/sec_tradinfo/stockcode/eisdgems_pf.htm" # GEM
  url2<-"http://www.hkex.com.hk/eng/market/sec_tradinfo/stockcode/eisdgems_pf.htm"
  ss2<-readHTMLTable(url2,header=T,which=3,stringsAsFactors=F) # Get the table from web
  ss2<-tail(ss2,-4) # First 4 rows are useless
  rownames(ss2)<-NULL # Recount row numbers after row removal
  colnames(ss2)<-c("symbol", "companyName", "contractSize", "C", "H", "O", "F") # Assign col names  
  # Combine the lists
  ss<-rbind(ss1,ss2)
  # Remove first zero in the stock symbol
  ss$symbol<-gsub('^0','',ss$symbol)
  return(ss)
}

# Return all the latest entries in the history files 
getLatestHistory<-function(){
  v<-NULL
  #colnames(v)<-c("stock","volume")
  for(s in gsub(".dat","",list.files(path="~/Documents/stockTrader.Data/HKEX"))) {
    #cat("Checking",s,"...\n")
    #vv<-merge(data.frame(symbol=s),last(readHistory(s)))
    vv<-data.frame(last(readHistory(s)))
    vv<-transform(vv, date=as.Date(row.names(vv), format="%Y-%m-%d"), row.names= s )
    colnames(vv)<-c("open","high","low","close","volume","adjusted","date")
    v<-rbind(v, vv)
  }
  return(v[, c(7,1:6)])
}

# Write the symbol list to file
writeSymbolList <-function(ss=getSymbolList()){
  write.table(ss,file="HKEXlist.dat",row.names=F,col.names=T,sep=",",quote=T)
}

# Use this to read symbol list from file if cannot access the web version above
readSymbolList <- function(){
  return(read.table(file="HKEXlist.dat",sep=",",header=T,colClasses=c(rep("character",7))))
}

# Write the history file for stock symbol s to file but only if the data file does not already exist.
# This script is meant to be run only once for each stock.
# e.g. writeHistory("0001.HK")
writeHistory <- function(s,startDate="2016-01-01"){
  filename<-paste0("~/Documents/stockTrader.Data/HKEX/",s,".dat")
  if(file.exists(filename)){
    #cat(filename,"already exists. Skipping...\n")
  } else {
    #cat("Saving",s,"to file...\n")
    #try(write.zoo(getSymbols(s, from=startDate,auto.assign=F),file=filename,sep=","),silent=T)
    try({
      h<-getSymbols(s, from=startDate,auto.assign=F)
      h<-h[h[,5]!=0,] # Yahoo writes new record with zero volume during holidays in Mon-Fri. Need to remove these.
      if(nrow(h)>0) write.zoo(h, file=filename,sep=",") # Write only if h is not empty
    }, silent=T)
  }
}

# Write the history to data files for all the stocks listed under HKEX
writeAllHistory <- function(){ try(invisible(lapply(paste0(getSymbolList()[,1],".HK"), writeHistory))) }

# Update today's quote to the history file.
# If there are a few days missing then also add them back.
updateHistory <- function(s){
  filename<-paste0("~/Documents/stockTrader.Data/HKEX/",s,".dat")
  if(!file.exists(filename)){
    #cat(filename,"does not yet exist. Fetching all available history...\n")
    try(writeHistory(s),silent=T)
  } else {
    #cat("Updating all the missing quote for",s,"to file...\n")
    try({
      h<-getSymbols(s, from=end(readHistory(s))+1,auto.assign=F)
      h<-h[h[,5]!=0,] # Yahoo writes new record with zero volume during holidays in Mon-Fri. Need to remove these.
      write.zoo(h, file=filename,row.names=F,append=T,col.names=F,sep=",")
    }, silent=T)
  }
}

# Update today's quote to the history file for all the stocks listed under HKEX
updateAllHistory <- function(){ try(invisible(lapply(paste0(getSymbolList()[,1],".HK"), updateHistory))) }

# Read the history file for s and return as xts object
readHistory <- function(s){
  as.xts(read.zoo(paste0("~/Documents/stockTrader.Data/HKEX/",s,".dat"),sep=",",format="%Y-%m-%d",header=TRUE))
}


