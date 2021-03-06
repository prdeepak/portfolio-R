# Building on Adam Duncan's http://www.r-bloggers.com/monitoring-an-etf-portfolio-in-r/

library(quantmod, warn.conflicts = FALSE, quietly = TRUE)
library(PerformanceAnalytics, warn.conflicts = FALSE, quietly = TRUE)
# library(knitr, warn.conflicts = FALSE, quietly = TRUE)

# from Dominik and GSee at http://stackoverflow.com/questions/12028671/merging-a-large-list-of-xts-objects
do.call.rbind <- function(lst) {
  while(length(lst) > 1) {
    idxlst <- seq(from=1, to=length(lst), by=2)
    lst <- lapply(idxlst, function(i) {
      if(i==length(lst)) { return(lst[[i]]) }
      return(rbind(lst[[i]], lst[[i+1]]))
    })
  }
  lst[[1]]
} # do.call.rbind

do.call.cbind <- function(lst) {
  while(length(lst) > 1) {
    idxlst <- seq(from=1, to=length(lst), by=2)
    lst <- lapply(idxlst, function(i) {
      if(i==length(lst)) { return(lst[[i]]) }
      return(cbind(lst[[i]], lst[[i+1]]))
    })
  }
  lst[[1]]
} # do.call.cbind


pf.components <- function(etf.Symbols ="SPY", data.source="yahoo", start.date="2006-12-29", end.date=Sys.Date()) {
  etfs <- mapply(function(x,y){getSymbols(env = NULL, Symbols = x, src = y, from = start.date)}, x = etf.Symbols, y = data.source, SIMPLIFY = FALSE)
  # names(etfs) <- etf.Symbols
  
  date.range <- paste(start.date, end.date, sep="::")
  etfs <- lapply(etfs, function(x){x <- x[date.range]})
  
  return(etfs)
}  # pf.components


pf.toIndex <- function(etfs) {  # normalize each ETF to firstrow.Adj or Close = 100
  lapply(etfs, function(x){adjustOHLC(x, use.Adjusted=TRUE)})

  indexTo <- lapply(etfs, function(x){
    if(has.Cl(x)) {as.double(Cl(x[1]))}
    else 1
  })

  voCol <- lapply(etfs, function(x){as.integer(has.Vo(x, which=TRUE))})
  adCol <- lapply(etfs, function(x){as.integer(has.Ad(x, which=TRUE))})
  clCol <- lapply(etfs, function(x){as.integer(has.Cl(x, which=TRUE))})

  etfM <- lapply(etfs, as.matrix) 
  for(aa in 1:length(etfM)){
    for(bb in 1:nrow(etfM[[aa]])){
      etfM[[aa]][bb,] <- 100 * etfM[[aa]][bb,] / indexTo[[aa]]
    }
    etfM[[aa]][,voCol[[aa]]] <- etfs[[aa]][,voCol[[aa]]]  # don't normalize volume
    etfM[[aa]][,adCol[[aa]]] <- etfM[[aa]][,clCol[[aa]]]  # Adj should be Cl (since we did AdjustOHLC)
  }

  etfs <- lapply(etfM, as.xts)  
  return(etfs)
} # pf.toIndex


pf.createIndex <- function(etfs, weights) { # requires OHLCV data
  if (!is.OHLCV(etfs[[1]])) {stop("only works with OHLCV data for now")}
    
  x <- etfs[[1]]
  for(bb in 1:ncol(x)) { # number of OHLCV columns
   x[,bb] <- 0
   for(aa in 1:length(etfs))
     {x[,bb] <- x[,bb] + etfs[[aa]][,bb] * weights[aa]}
  }
  x[,6] <- x[,4]  # add Adjusted column
  
  names(x) <- c("Index.Open", "Index.High", "Index.Low", "Index.Close", "Index.Volume", "Index.Adjusted")
        
  etfs$Index <- x
  return(etfs) 
} # pf.createIndex


pf.index <- function(etf.Symbols ="SPY", data.source="yahoo", weights=1, start.date="2006-12-29", end.date=Sys.Date()) {
  etfs <- pf.components(etf.Symbols = etf.Symbols, data.source = data.source, start.date = start.date, end.date = end.date)

  etfs <- pf.toIndex(etfs) # how to make this faster?

  weights <- rep(weights, length.out = length(etf.Symbols))
  weights <- weights / sum(weights)  # force weights to add to 1
  
  etfs <- pf.createIndex(etfs, weights)

  #  Do I need to change these indices into returns-data?
  #  returnsDF <- data.frame(lapply(etfs, function(s){ROC(Ad(s), type="discrete")})) # daily returns -- later let user set periodicity!

  return(etfs)
} # pf.index


pf.plot <- function(etfs) {  # Thanks to http://blog.revolutionanalytics.com/2014/01/quantitative-finance-applications-in-r-plotting-xts-time-series.html
  y <- do.call.cbind(lapply(etfs, Cl))
  zoo.y <- as.zoo(y)
  
  tsRainbow <- rainbow(ncol(zoo.y))  # Set a color scheme

  plot(x = zoo.y, ylab = "Cumulative Return", main = "Cumulative Returns",
       col = tsRainbow, screens = 1, lwd = 2)  # Plot the overlayed series

  legend(x = "topleft", legend = names(zoo.y), 
         lty = 1,col = tsRainbow)    # Set a legend in the upper left hand corner to match color to return series
} # pf.plot

# Next steps:  add 200-day SMA; plot last year, and last 90d.  Plot SMA as thin line, same color
# Note:  lwd sets line width; last12mo <- lapply(etfs, function(x){last(x, 250)}); 
# apply last() only *After* you've created 200d SMA data (or there won't be enough)

# quick and dirty stuff to help me import info for Google Sheet quicker:
dpr.Symbols <- c("GLD","GSG","DJP","jjc","dba","spy","qqq","iwm","eusa","vgk","dxj","eem","fxi","ewc","xiu.to","ewa","acwi","iyr","rwo","igf","iei","ief","tlt","pttrx","bwx","emb","lqd","jnk","ibnd","tip","xrb.to","wip","shv")

dpr.Ad <- function(etfs) {
  etfs <- lapply(etfs, to.monthly)

  m <- do.call.cbind(lapply(etfs, Ad)) # matrix of Adjusted only
  m[is.na(m)] <- " "
  names(m) <- names(etfs) # update names
  write.zoo(m, file="data/Adjusted.csv", sep=",")
}

dpr.Cl <- function(etfs) {
  etfs <- lapply(etfs, to.monthly)

  m <- do.call.cbind(lapply(etfs, Cl))
  m[is.na(m)] <- " "
  names(m) <- names(etfs)
  write.zoo(m, file="data/Raw.csv", sep=",")
}

dpr.Update <- function() {
  etfs <- pf.components(dpr.Symbols, start.date="2003-12-01")

  dpr.Ad(etfs)
  dpr.Cl(etfs)
}


# Quandl
library(Quandl)
Quandl.api_key("aysDdkH-cpncQfux26A3")

fut.Symbols = c("CHRIS/CME_SP1","CHRIS/CME_TY1","CHRIS/CME_GC1","CHRIS/CME_CL1","BOE/XUDLCDD")
fut.Names = c("SP1","TY1","GC1","CL1","USDCAD")
fut.Convert = c(FALSE, FALSE, TRUE, TRUE, FALSE)

q.components <- function(fut.Symbols ="CHRIS/CME_SP1", fut.Names = fut.Symbols, start.date="1982-12-01", end.date=Sys.Date()) {
  fut <- lapply(fut.Symbols, function(x){Quandl(x, type="xts")})
  names(fut) <- fut.Names
  
  date.range <- paste(start.date, end.date, sep="::")
  fut <- lapply(fut, function(x){x <- x[date.range]})
  
  return(fut)
}  # q.components


q.CADdata <- function(){
  tsx <- getSymbols("^GSPTSE", env=NULL, from="1983-12-01")

  f <- q.components(fut.Symbols, fut.Names=fut.Names, start.date="1983-12-01")
  usdcad <- f[[5]]

  cad.Fut <- list(Cl(tsx), f[[2]][,6], f[[3]][,6] * usdcad, f[[4]][,6] * usdcad)
  names(cad.Fut) <- c("TSX", "TY1", "GC1:CAD", "CL1:CAD")
  
  cfm <- do.call.cbind(cad.Fut)
  names(cfm) <- names(cad.Fut)

  for(cc in 1:ncol(cfm)){  # Thanks to Jonathan Ulrich!  http://stackoverflow.com/questions/37082673/very-slow-xts-dividing-a-column-by-first-row
    cfm[,cc] <- cfm[,cc] / drop(coredata(cfm[1,cc]))
  }

  return(100 * cfm)
}
