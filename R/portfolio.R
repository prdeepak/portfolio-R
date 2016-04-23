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
  indexTo <- lapply(etfs, function(x){
    adjustOHLC(x, use.Adjusted=TRUE)
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
    etfM[[aa]][,voCol[[aa]]] <- etfs[[aa]][,voCol[[aa]]]
    etfM[[aa]][,adCol[[aa]]] <- etfM[[aa]][,clCol[[aa]]]
  }

  etfs <- lapply(etfM, as.xts)  
  return(etfs)
}

pf.index <- function(etf.Symbols ="SPY", data.source="yahoo", weights=1, start.date="2006-12-29", end.date=Sys.Date()) {
  etfs <- pf.components(etf.Symbols = etf.Symbols, data.source = data.source, start.date = start.date, end.date = end.date)

  etfs <- pf.toIndex(etfs) # how to make this faster?

#  etfs$Index <- etfs[[1]] # replicate structure of one component for Index?
#  returnsDF <- data.frame(lapply(etfs, function(s){ROC(Ad(s), type="discrete")})) # daily returns -- later let user set periodicity!
    
#  weights <- rep(weights, length.out = length(etf.Symbols))
#  weights <- weights / sum(weights)  # force weights to add to 1

#  returnsIndex <- as.data.frame(rowSums(returnsDF * weights), drop = FALSE)
#  names(returnsIndex) <- "Change"

#  returnsIndex$Close <- returnsIndex$Change
#  returnsIndex$Close[1] <- 100
#  for(ii in 2:length(returnsIndex$Close)) 
#    { returnsIndex$Close[ii] <- returnsIndex$Close[ii-1] * (1 + returnsIndex$Close[ii]) }
  
#  return(returnsIndex)
  
  return(etfs)
  
    # more to come
} # pf.index
