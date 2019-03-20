#' Get end-of-day market data.
#'
#' @param ts_code Tushare equity code
#' @param trade_date Trading date
#' @param start_date Start date
#' @param end_date End date
#' @param date_format How to cast datetime columns
#' @param api actual API function
#'
#' @return data.table
#'
market_eod <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                       date_format = c("POSIXct", "Date", "char"),
                       api = c("daily", "weekly", "monthly", "daily_basic", "adj_factor",
                               "moneyflow", "fund_daily", "fut_daily", "opt_daily",
                               "index_daily")) {

  ts_code <- fix_code(ts_code)
  trade_date <- fix_date(trade_date)
  start_date <- fix_date(start_date)
  end_date <- fix_date(end_date)
  date_format <- match.arg(date_format)
  api <- match.arg(api)

  dt <- TusRequest(api, ts_code = ts_code, trade_date = trade_date,
                   start_date = start_date, end_date = end_date)

  if (nrow(dt)) {
    colfunc <- cast_date(date_format)
    dt[, trade_date := colfunc(trade_date)]
    if (trade_date == "") {
      setkey(dt, trade_date)
    } else {
      setkey(dt, ts_code)
    }
    #remove duplicated rows of adj_factor
    if (api == "adj_factor") {
      dt <- unique(dt, by = "adj_factor")
    }
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
                     time_format = c("POSIXct", "char")) {

  ts_code <- fix_code(ts_code)
  start_time <- fix_time(start_time)
  end_time <- fix_time(end_time)
  freq <- match.arg(freq)
  time_format <- match.arg(time_format)

  dt <- TusRequest("mins", ts_code = ts_code, start_time = start_time, end_time = end_time,
                   freq = paste0(freq, "min"))

  if (nrow(dt)) {
    colfunc <- cast_time(time_format)
    dt[, trade_time := colfunc(trade_time)]
    setkey(dt, trade_time)
  }

  dt
}

#' Suspend information
#'
#' @param ts_code OPTIONAL, Tushare equity code.
#' @param suspend_date OPTIONAL, suspend date in YYYYmmdd format or Date/POSIXct object.
#' @param resume_date OPTIONAL, resume date in YYYYmmdd format or Date/POSIXct object.
#' @param date_format How to cast return datetime columns? Default casts values to POSIXct objects.
#'
#' @return a data.frame/data.table
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- suspend(ts_code = "000001.sz")
#' }
suspend <- function(ts_code = "", suspend_date = "", resume_date="",
                    date_format = c("POSIXct", "Date", "char")) {

  ts_code <- fix_code(ts_code)
  suspend_date <- fix_date(suspend_date)
  resume_date <- fix_date(resume_date)
  date_format <- match.arg(date_format)

  dt <- TusRequest("suspend", ts_code = ts_code, suspend_date = suspend_date,
                   resume_date = resume_date)

  if (nrow(dt)) {
    colfunc <- cast_date(date_format)
    dt[, suspend_date := colfunc(suspend_date)]
    dt[, resume_date := resume_date]
    if (suspend_date == "") {
      setkey(dt, ts_code)
    } else {
      setkey(dt, suspend_date)
    }
  }

  dt
}

#' Daily stock market data.
#'
#' Please refer to online document for details.
#'
#' @param ts_code OPTIONAL, Tushare equity code.
#' @param trade_date OPTIONAL, trading date.
#' @param start_date OPTIONAL, start date of returned data.
#' @param end_date OPTIONAL, end date of returned date.
#' @param date_format How to cast return datetime columns? Default casts values to POSIXct objects.
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
                  date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "daily")

  do.call(market_eod, args)
}

#' @rdname daily
#' @export
#'
weekly <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                   date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "weekly")

  do.call(market_eod, args)
}

#' @rdname daily
#' @export
#'
monthly <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                    date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "monthly")

  do.call(market_eod, args)
}

#' @rdname daily
#' @export
#'
daily_basic <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                        date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "daily_basic")

  do.call(market_eod, args)
}

#' @rdname daily
#' @export
#'
adj_factor <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                       date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "adj_factor")

  do.call(market_eod, args)
}

#' @rdname daily
#' @export
#'
moneyflow <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                      date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "moneyflow")

  do.call(market_eod, args)
}

#' @rdname daily
#' @export
#'
fund_daily <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                      date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "fund_daily")

  do.call(market_eod, args)
}

#' @rdname daily
#' @export
#'
fut_daily <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                       date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "fut_daily")

  do.call(market_eod, args)
}

#' @rdname daily
#' @export
#'
opt_daily <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                      date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "opt_daily")

  do.call(market_eod, args)
}

#' @rdname daily
#' @export
#'
index_daily <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                      date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "index_daily")

  do.call(market_eod, args)
}
