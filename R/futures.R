#' Get basic info of futures contracts.
#'
#' @param exchange Future exchagne centre.
#' @param fut_type Type of futures.
#' @param date_format How to cast datetime format.
#'
#' @return data.frame/data.table
#' @export
#'
#' @examples
#' \dontrun{
#' fut_basic(exchange="DCE", fut_type="1")
#' }
fut_basic <- function(exchange, fut_type = "",
                      date_format = c("POSIXct", "Date", "char")) {

  date_format <- match.arg(date_format)

  ans <- TusRequest("fut_basic", exchange = exchange, fut_type = fut_type)
  if (nrow(ans)) {
    colfunc <- cast_date(date_format)
    ans[, list_date := colfunc(list_date)]
    ans[, delist_date := colfunc(delist_date)]
    ans[, last_ddate := colfunc(last_ddate)]
  }

  ans
}

futures_eod <- function(trade_date = "", symbol = "", start_date = "", end_date = "",
                        exchange = "", date_format = c("POSIXct", "Date", "char"),
                        api_name = c("fut_holding", "fut_wsr")) {

  trade_date <- fix_date(trade_date)
  start_date <- fix_date(start_date)
  end_date <- fix_date(end_date)
  date_format <- match.arg(date_format)
  api_name <- match.arg(api_name)

  ans <- TusRequest(api_name, trade_date = trade_date, symbol = symbol,
                    start_date = start_date, end_date = end_date,
                    exchange = exchange)
  if (nrow(ans)) {
    colfunc <- cast_date(date_format)
    ans[, trade_date := colfunc(trade_date)]
  }

  ans
}

futures_eod2 <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                         exchange = "", date_format = c("POSIXct", "Date", "char"),
                         api_name = c("fut_daily", "fut_settle")) {

  ts_code <- fix_code(ts_code)
  trade_date <- fix_date(trade_date)
  start_date <- fix_date(start_date)
  end_date <- fix_date(end_date)
  date_format <- match.arg(date_format)
  api_name <- match.arg(api_name)

  ans <- TusRequest(api_name, ts_code = ts_code, trade_date = trade_date,
                    start_date = start_date, end_date = end_date, exchange = exchange)
  if (nrow(ans)) {
    colfunc <- cast_date(date_format)
    ans[, trade_date := colfunc(trade_date)]
  }

  ans
}

#' Futures market data.
#'
#' @param trade_date OPTIONAL, trading date.
#' @param symbol OPTIONAL, futures symbol.
#' @param ts_code OPTIONAL, Tushare equity code.
#' @param start_date OPTIONAL, start date of returned data.
#' @param end_date OPTIONAL, end date of returned data.
#' @param exchange OPTIONAL, Future exchange centre.
#' @param date_format How to cast datetime format.
#'
#' @return data.frame/data.table
#' @export
#'
#' @examples
#' \dontrun{
#' fut_holding(trade_date='20181113', symbol='C', exchange='DCE')
#' fut_wsr(trade_date='20181113', symbol='ZN')
#' }
fut_holding <- function(trade_date = "", symbol = "", start_date = "", end_date = "",
                        exchange = "", date_format = c("POSIXct", "Date", "char")) {

  futures_eod(trade_date = trade_date, symbol = symbol,
              start_date = start_date, end_date = end_date,
              exchange = exchange, date_format = date_format,
              api_name = "fut_holding")
}

#' @rdname fut_holding
#' @export
#'
fut_wsr <- function(trade_date = "", symbol = "", start_date = "", end_date = "",
                    exchange = "", date_format = c("POSIXct", "Date", "char")) {

  futures_eod(trade_date = trade_date, symbol = symbol,
              start_date = start_date, end_date = end_date,
              exchange = exchange, date_format = date_format,
              api_name = "fut_wsr")
}

#' @rdname fut_holding
#' @export
#'
fut_daily <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                      exchange = "", date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               exchange = exchange,
               date_format = date_format,
               api = "fut_daily")

  do.call(futures_eod2, args)
}

#' @rdname fut_holding
#' @export
#'
fut_settle <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                       exchange = "", date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               exchange = exchange,
               date_format = date_format,
               api = "fut_settle")

  do.call(futures_eod2, args)
}
