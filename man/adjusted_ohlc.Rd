% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/market_utils.R
\name{adjusted_ohlc}
\alias{adjusted_ohlc}
\title{Adjusted closing prices}
\usage{
adjusted_ohlc(values, factors, adjust = c("qfq", "hfq"))
}
\arguments{
\item{values}{a data.table, keyed by date/time column. Can be obtained from eod market functions.}

\item{factors}{a data.table, keyed by date/time column. Can be obtained from adj_factor().}

\item{adjust}{following Tushare Pro convention, qfq adjusts previous closing
prices w.r.t last closing prices, hfq adjusts closing prices w.r.t first traded
price.}
}
\value{
data.table
}
\description{
Adjusted closing prices
}
\examples{
\dontrun{
#query prices and adjust factors
pab <- daily(ts_code = "000001.sz")
pab_af <- adj_factor(ts_code = "000001.sz")
#post-adjusted
pab_adjusted <- adjusted_ohlc(pab, pab_af, "hfq")
#pre-adjusted
pab_adjusted <- adjusted_ohlc(pab, pab_af, "qfq")
}
}
