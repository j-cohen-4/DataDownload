pullOptionData <- function(tickers, 
                           futures,
                           lb=125,
                           decimals=0,
                           updn=15,
                           stp=1,
                           upperbnd=1000,
                           lowerbnd=10){
  
    # Sanity check
    nTickers = length(tickers);
    nFutures = length(futures);
    if(nTickers != nFutures){
        message("pullOptionData: Failed! 'tickers' and 'futures' have different sizes!")
    }
    
    for(i in 1:nTickers){
      
        sym = strsplit(tickers[i], " ")[[1]][1];
        opt = getOptionTicker(tickers[i], futures[[i]], lb=lb, decimals=decimals, updn=updn, stp=stp, upperbnd=upperbnd, lowerbnd=lowerbnd);
        message(sprintf("Loading: %s...%d of %d", sym, i, nTickers));
        dat = bdh(opt$tick, c("PX_LAST"), start.date=opt$st, end.date=opt$nd);
        
        # Save
        sFile = sprintf("%s.RData", sym);
        save(dat, file=sFile);
    }
    return(TRUE);
}

pullOptionData.basic <- function(tick.req, dtStart, dtEnd, sFile="dat.req"){
  
    dat   = bdh(tick.req, c("PX_LASt"), start.date=dtStart, end.date=dtEnd);
    sName = sprintf("%s.RData", sFile);
    save(dat, file=sName);
}