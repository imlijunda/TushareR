market_eod <- function(..., api, timeout = 5) {

  x <- GetAPI()
  dt <- `$.tushare_api`(x, func = force(api))(..., timeout = timeout)

  if (nrow(dt) && api == "adj_factor" && all(dt$ts_code == dt$ts_code[1])) {
    #remove duplicated rows of adj_factor
    dt[, fgrp := data.table::rleid(adj_factor)]
    dt <- dt[, .SD[1L], by = fgrp][, fgrp := NULL]
    data.table::setkeyv(dt, cols = "trade_date")
  }

  dt
}

#' Get end-of-day market intraday data.
#'
#' @param ts_code Tushare equity code
#' @param start_time Start date/datetime
#' @param end_time End date/datetime
#' @param freq frequency of intraday data
#' @param time_format How to cast datetime columns?
#' @param ... futher arguments passed
#'
#' @return data.table
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- intraday("000001.sz", "20190101", "20190131", "15")
#' }
#'
intraday <- function(ts_code = "", start_time = "", end_time = "",
                     freq = c("1", "5", "15", "30", "60"),
                     time_format = c("POSIXct", "char"), ...) {


  freq <- paste0(match.arg(as.character(freq),
                           choices = c("1", "5", "15", "30", "60")),
                 "min")

  args <- list(ts_code = ts_code,
               start_time = start_time,
               end_time = end_time,
               freq = freq,
               time_format = time_format,
               api = "mins")
  dots <- list(...)

  dt <- do.call(market_eod, c(args, dots))
  #fix possible duplicated data points
  unique(dt, by = "trade_time")
}

#' Suspend information
#'
#' @param ts_code OPTIONAL, Tushare equity code.
#' @param suspend_date OPTIONAL, suspend date in YYYYmmdd format or Date/POSIXct object.
#' @param resume_date OPTIONAL, resume date in YYYYmmdd format or Date/POSIXct object.
#' @param date_format How to cast return datetime columns? Default casts values to POSIXct objects.
#' @param ... futher arguments passed
#'
#' @return a data.frame/data.table
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- suspend(ts_code = "000001.sz")
#' }
suspend <- function(ts_code = "", suspend_date = "", resume_date="",
                    date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               suspend_date = suspend_date,
               resume_date = resume_date,
               date_format = date_format,
               api = "suspend")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' End-of-day market data.
#'
#' Please refer to online document for details.
#'
#' @param ts_code OPTIONAL, Tushare equity code.
#' @param trade_date OPTIONAL, trading date.
#' @param start_date OPTIONAL, start date of returned data.
#' @param end_date OPTIONAL, end date of returned date.
#' @param date_format How to cast return datetime columns? Default casts values to POSIXct objects.
#' @param ... futher arguments passed
#'
#' @return a data.frame/data.table
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- daily(ts_code = "000001.sz")
#' dt <- fut_daily(trade_date = "20190301")
#' }
daily <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                  date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "daily")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname daily
#' @export
#'
weekly <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                   date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "weekly")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname daily
#' @export
#'
monthly <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                    date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "monthly")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname daily
#' @export
#'
daily_basic <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                        date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "daily_basic")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname daily
#' @export
#'
adj_factor <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                       date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "adj_factor")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname daily
#' @export
#'
moneyflow <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                      date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "moneyflow")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname daily
#' @export
#'
stk_limit <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                      date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "stk_limit")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname daily
#' @export
#'
fund_daily <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                       date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "fund_daily")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname daily
#' @export
#'
opt_daily <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                      date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "opt_daily")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname daily
#' @export
#'
index_daily <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                        date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "index_daily")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname daily
#' @export
#'
index_weekly <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                         date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "index_weekly")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname daily
#' @export
#'
index_monthly <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                          date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "index_monthly")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname daily
#' @export
#'
index_weight <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                         date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "index_weight")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname daily
#' @export
#'
index_dailybasic <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                             date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "index_dailybasic")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}
