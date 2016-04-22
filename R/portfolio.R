# Building on Adam Duncan's http://www.r-bloggers.com/monitoring-an-etf-portfolio-in-r/

library(quantmod, warn.conflicts = FALSE, quietly = TRUE)
# library(PerformanceAnalytics, warn.conflicts = FALSE, quietly = TRUE)
# library(knitr, warn.conflicts = FALSE, quietly = TRUE)

pf.index <- function(etf.Symbols ="SPY", data.source="yahoo", weights=1, start.date="2000-01-01", end.date=Sys.Date()) {
  etfs <- getSymbols(Symbols = etf.Symbols, src = data.source, warnings = FALSE)
  date.range <- paste(start.date, end.date, sep="::")
  # etfs <- etfs[date.range]
  
  weights <- weights / sum(weights)  # force weights to add to 1
  
  return(etfs)
  # more to come
}
