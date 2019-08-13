#' Get basic info of futures contracts.
#'
#' @param exchange Future exchagne centre.
#' @param fut_type Type of futures.
#' @param date_format How to cast datetime format.
#' @param ... futher arguments passed
#'
#' @return data.frame/data.table
#' @export
#'
#' @examples
#' \dontrun{
#' fut_basic(exchange="DCE", fut_type="1")
#' }
fut_basic <- function(exchange, fut_type = "",
                      date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(exchange = exchange,
               fut_type = fut_type,
               date_format = date_format,
               api = "fut_basic")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' Futures market data.
#'
#' @param ts_code OPTIONAL, Tushare equity code.
#' @param trade_date OPTIONAL, trading date.
#' @param symbol OPTIONAL, futures symbol.
#' @param start_date OPTIONAL, start date of returned data.
#' @param end_date OPTIONAL, end date of returned data.
#' @param exchange OPTIONAL, Future exchange centre.
#' @param date_format How to cast datetime format.
#' @param ... futher argument passed
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
                        exchange = "", date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(trade_date = trade_date,
               symbol = symbol,
               start_date = start_date,
               end_date = end_date,
               exchange = exchange,
               date_format = date_format,
               api = "fut_holding")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname fut_holding
#' @export
#'
fut_wsr <- function(trade_date = "", symbol = "", start_date = "", end_date = "",
                    exchange = "", date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(trade_date = trade_date,
               symbol = symbol,
               start_date = start_date,
               end_date = end_date,
               exchange = exchange,
               date_format = date_format,
               api = "fut_wsr")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname fut_holding
#' @export
#'
fut_daily <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                      exchange = "", date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               exchange = exchange,
               date_format = date_format,
               api = "fut_daily")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname fut_holding
#' @export
#'
fut_settle <- function(ts_code = "", trade_date = "", start_date = "", end_date = "",
                       exchange = "", date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               trade_date = trade_date,
               start_date = start_date,
               end_date = end_date,
               exchange = exchange,
               date_format = date_format,
               api = "fut_settle")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}
