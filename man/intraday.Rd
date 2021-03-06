% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eod.R
\name{intraday}
\alias{intraday}
\title{Get end-of-day market intraday data.}
\usage{
intraday(ts_code = "", start_time = "", end_time = "",
  freq = c("1", "5", "15", "30", "60"), time_format = c("POSIXct",
  "char"), ...)
}
\arguments{
\item{ts_code}{Tushare equity code}

\item{start_time}{Start date/datetime}

\item{end_time}{End date/datetime}

\item{freq}{frequency of intraday data}

\item{time_format}{How to cast datetime columns?}

\item{...}{futher arguments passed}
}
\value{
data.table
}
\description{
Get end-of-day market intraday data.
}
\examples{
\dontrun{
dt <- intraday("000001.sz", "20190101", "20190131", "15")
}

}
