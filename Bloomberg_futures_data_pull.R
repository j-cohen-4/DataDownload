##install packages and call 
install.packages("Rblpapi")
library(Rblpapi)

##connect to Bloomy
con = blpConnect()

##create all tickers from Jan 2000 to Dec 2019
months <- c("F","G","H","J","K","M","N","Q","U","V","X","Z")
years <- c("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","8","9")
tickers <- rep(NA,length(years)*length(months))
counter <- 0

for(i in 1:length(years)){
  for(j in 1:length(months)){
    month.year <- paste(months[j],years[i],sep="")
    counter <- counter + 1
    tickers[counter] <-  sprintf("CL%s Comdty", month.year)
  }
}
##tickers that defy the rule
tickers[214:216] <- c("CLV7 Comdty" ,"CLX7 Comdty", "CLZ7 Comdty")

##find last trade date; only pull last 250 days of data
lastdate <- rep(NA, length(tickers))
for(i in 1:length(tickers)){
  lastdate[i] <- bdp(tickers[i],"LAST_TRADEABLE_DT") 
}

save(tickers, file = "tickers.Rdata")
save(lastdate, file = "lastdate.Rdata")

fut_data <- vector("list")
opt <- c("periodicitySelection" = "DAILY")

for(i in 1:length(lastdate)){
  data <- bdh(tickers[i], c("PX_LAST"),start.date = as.Date(lastdate[[i]], "%Y-%m-%d")-250,end.date = as.Date(lastdate[[i]], "%Y-%m-%d"), options = opt )
  fut_data[[i]] <- data
}

save(fut_data, file = "fut_data.Rdata")

##get option prices
##CL option prices only go back to CLQ7. There is nothing further

data_test <- bdh(c("SPX Index", "KRP US Equity"), c("PX_LAST"),start.date = as.Date("2018-05-15"),end.date = as.Date("2018-06-25"), options = opt )

##option tickers

months <- c("F","G","H","J","K","M","N","Q","U","V","X","Z")
years <- c("7","8","9")
counter <- 1 
ticker_w_strike <- NA
master_ticker_list <- vector("list")

for(i in 1:length(years)){
  for(j in 1:length(months)){
    month.year <- paste(months[j],years[i],sep="")
    month.index <- 204 + counter
    last_price <- fut_data[[month.index]][dim(fut_data[[month.index]])[1],2]
    strikes <- round(seq(last_price - 15, last_price+15, by = 1))
    for(z in 1:length(strikes)){
      if(strikes[z] > last_price){
        type = paste(sprintf("C %s",strikes[z]),".00",sep="")
      } else {
        type = paste(sprintf("P %s",strikes[z]),".00",sep="")
      }
      exp.type <- paste(month.year,type, sep="")
      ticker_w_strike[z] <- sprintf("CL%s",exp.type)
      
    }
    master_ticker_list[[counter]] <- ticker_w_strike
    counter <- counter + 1 
  }
}


