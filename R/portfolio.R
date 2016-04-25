# Building on Adam Duncan's http://www.r-bloggers.com/monitoring-an-etf-portfolio-in-r/

library(quantmod, warn.conflicts = FALSE, quietly = TRUE)
# library(PerformanceAnalytics, warn.conflicts = FALSE, quietly = TRUE)
# library(knitr, warn.conflicts = FALSE, quietly = TRUE)

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
}


pf.createIndex <- function(etfs, weights) { # requires OHLCV data
  if (!is.OHLCV(etfs[[1]])) {stop("only works with OHLCV data for now")}
    
  # opCol <- lapply(etfs, function(x){as.integer(has.Op(x, which=TRUE))})
  # hiCol <- lapply(etfs, function(x){as.integer(has.Hi(x, which=TRUE))})
  # loCol <- lapply(etfs, function(x){as.integer(has.Lo(x, which=TRUE))})
  # clCol <- lapply(etfs, function(x){as.integer(has.Cl(x, which=TRUE))})
  # voCol <- lapply(etfs, function(x){as.integer(has.Vo(x, which=TRUE))})

  x <- etfs[[1]]
  for(bb in 1:5) { # number of OHLCV columns
   x[,bb] <- 0
   for(aa in 1:length(etfs))
     {x[,bb] <- x[,bb] + etfs[[aa]][,bb] * weights[aa]}
  }
  x[,6] <- x[,4]  # add Adjusted column
  
  names(x) <- c("Index.Open", "Index.High", "Index.Low", "Index.Close", "Index.Volume", "Index.Adjusted")
        
  etfs$Index <- x
  return(etfs) 
}


pf.index <- function(etf.Symbols ="SPY", data.source="yahoo", weights=1, start.date="2006-12-29", end.date=Sys.Date()) {
  etfs <- pf.components(etf.Symbols = etf.Symbols, data.source = data.source, start.date = start.date, end.date = end.date)

  etfs <- pf.toIndex(etfs) # how to make this faster?

  weights <- rep(weights, length.out = length(etf.Symbols))
  weights <- weights / sum(weights)  # force weights to add to 1
  
#  etfs <- pf.createIndex(etfs, weights)

  #  Do I need to change these indices into returns-data?
  #  returnsDF <- data.frame(lapply(etfs, function(s){ROC(Ad(s), type="discrete")})) # daily returns -- later let user set periodicity!

  return(etfs)
} # pf.index
