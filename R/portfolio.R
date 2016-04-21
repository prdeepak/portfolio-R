# Building on Adam Duncan's http://www.r-bloggers.com/monitoring-an-etf-portfolio-in-r/

library(quantmod, warn.conflicts = FALSE, quietly = TRUE)
library(PerformanceAnalytics, warn.conflicts = FALSE, quietly = TRUE)
library(knitr, warn.conflicts = FALSE, quietly = TRUE)

pf.portfolio.index <- function(etfs=SPY, data.source="yahoo", weights=1, start.date="2000-01-01", end.date=today()) {
  suppressWarnings(getData(etfs, data.source))
  # more to come
}
