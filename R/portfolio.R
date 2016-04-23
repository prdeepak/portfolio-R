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

pf.index <- function(etf.Symbols ="SPY", data.source="yahoo", weights=1, start.date="2006-12-29", end.date=Sys.Date()) {
  etfs <- pf.components(etf.Symbols = etf.Symbols, data.source = data.source, start.date = start.date, end.date = end.date)

  etfM <- lapply(etfs, as.matrix) # normalize each ETF to start = 100
#  etfM <- lapply(etfM, 
#    function(x) {
#      for(ii in 2:length(x)-1) {x[ii,] <- 100 * x[ii,] / x[1,]}
#      x[1,] <- 100
#      }
#    )

  returnsDF <- data.frame(lapply(etfs, function(s){ROC(Ad(s), type="discrete")})) # daily returns -- later let user set periodicity!
    
  weights <- rep(weights, length.out = length(etf.Symbols))
  weights <- weights / sum(weights)  # force weights to add to 1

  returnsIndex <- as.data.frame(rowSums(returnsDF * weights), drop = FALSE)
  names(returnsIndex) <- "Change"

  returnsIndex$Close <- returnsIndex$Change
  returnsIndex$Close[1] <- 100
  for(ii in 2:length(returnsIndex$Close)) 
    { returnsIndex$Close[ii] <- returnsIndex$Close[ii-1] * (1 + returnsIndex$Close[ii]) }
  
  return(returnsIndex)
  # more to come
} # pf.index
