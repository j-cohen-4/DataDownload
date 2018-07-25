getOptionTicker <- function(fut.ticker, 
                            futures, 
                            lb=125,                # days prior to futures expiration for midprc 
                            decimals=0,            # # of decimals to round futures price to
                            updn=15,               # # of strikes up/down from mid price
                            stp=1,                 # step of strike price
                            upperbnd=1000,         # highest strike allowed
                            lowerbnd=10){          # lowest strike allowed
  
    # Get price
    tgt = tail(futures, 1)[,1] - lb;
    prc = head(futures[futures$date >= tgt, 2], 1);
    mod = prc %% stp;
    if( (stp - mod) < (0.5 * stp) ){
        mid = prc + stp - mod;
    } else {
        mid = prc - mod;
    }
    
    # Anchor
    sym = strsplit(fut.ticker, " ")[[1]][1];
    fin = ifelse(nchar(sym) == 5, 5, 4);
    sym = sprintf("%s%s", substr(sym, 1, 3), substr(sym, fin, fin));
    rng = seq(mid - updn, mid + updn, stp);
    rng = rng[rng >= lowerbnd & rnd <= upperbnd];
    
    # Build ticker
    len  = length(rng);
    opt.t = array(data=NA, dim=len);
    for(i in 1:len){
      
        stk      = rng[i];
        opt      = ifelse(stk > prc, "C", "P");
        opt.t[i] = sprintf("%s%s %d.0 Comdty", sym, opt, stk);

    }
    return(list(tick=opt.t, st=futures[1,1], nd=tail(futures, 1)[,1]));
}
