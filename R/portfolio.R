# Building on Adam Duncan's http://www.r-bloggers.com/monitoring-an-etf-portfolio-in-r/

library(quantmod, warn.conflicts = FALSE, quietly = TRUE)
# library(PerformanceAnalytics, warn.conflicts = FALSE, quietly = TRUE)
# library(knitr, warn.conflicts = FALSE, quietly = TRUE)

pf.components <- function(etf.Symbols ="SPY", data.source="yahoo", start.date="2000-01-01", end.date=Sys.Date()) {
  getSymbols(Symbols = etf.Symbols, src = data.source, warnings = FALSE)
  etfs <- lapply(etf.Symbols, get)
  names(etfs) <- etf.Symbols
  
  date.range <- paste(start.date, end.date, sep="::")
  etfs <- lapply(etfs, function(x){x <- x[date.range]})
  
  return(etfs)
}  # pf.components

pf.index <- function(etf.Symbols ="SPY", data.source="yahoo", weights=1, start.date="2000-01-01", end.date=Sys.Date()) {
  etfs <- pf.components(etf.Symbols = etf.Symbols, data.source = data.source, start.date = start.date, end.date = end.date)
  
  weights <- weights / sum(weights)  # force weights to add to 1
  
  # more to come
} # pf.index
