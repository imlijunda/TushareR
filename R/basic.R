#' Get basic stock market information
#'
#' @param ts_code Tushare equity code
#' @param start_date Start date
#' @param end_date End date
#' @param is_open Whether the market is open
#' @param is_new Whether the stock is new
#' @param is_hs Flag for Shenzhen/Shanghai-Hong Kong Stock Connect#'
#' @param hs_type Type of Shenzhen/Shanghai-Hong Kong Stock Connect
#' @param list_status List status
#' @param exchange Exchange market
#' @param date_format How to cast datetime format.
#' @param ... futher arguments passed
#'
#' @return data.frame/data.table
#' @export
#'
#' @examples
#' \dontrun{
#' stock_basic()
#' }
stock_basic <- function(is_hs = c("", "N", "H", "S"),
                        list_status = c("", "L", "D", "P"),
                        exchange = c("", "SSE", "SZSE", "HKEX"),
                        date_format = c("POSIXct", "Date", "char"), ...) {

  is_hs <- match.arg(is_hs)
  list_status <- match.arg(list_status)
  exchange <- match.arg(exchange)

  args <- list(is_hs = is_hs,
               list_status = list_status,
               exchange = exchange,
               date_format = date_format,
               api = "stock_basic")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname stock_basic
#' @export
#'
trade_cal <- function(exchange = c("", "SSE", "SZSE"), start_date = "", end_date = "", is_open,
                      date_format = c("POSIXct", "Date", "char"), ...){

  exchange <- match.arg(exchange)
  start_date <- fix_date(start_date)
  end_date <- fix_date(end_date)
  date_format <- match.arg(date_format)

  args <- list(api = "trade_cal",
               exchange = exchange,
               start_date = start_date,
               end_date = end_date)
  if (!missing(is_open)) {
    args$is_open <- is_open
  }
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname stock_basic
#' @export
#'
hs_const <- function(hs_type = c("SH", "SZ"), is_new = c("1", "0"),
                     date_format = c("POSIXct", "Date", "char"), ...) {

  hs_type <- match.arg(hs_type)
  is_new <- match.arg(is_new)

  args <- list(hs_type = hs_type,
               is_new = is_new,
               date_format = date_format,
               api = "hs_const")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname stock_basic
#' @export
#'
namechange <- function(ts_code = "", start_date = "", end_date = "",
                       date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "stock_basic")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname stock_basic
#' @export
#'
stock_company <- function(exchange = c("SSE", "SZSE"),
                          date_format = c("POSIXct", "Date", "char"), ...) {

  exchange <- match.arg(exchange)

  args <- list(exchange = exchange,
               date_format = date_format,
               api = "stock_company")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}

#' @rdname stock_basic
#' @export
#'
new_share <- function(start_date = "", end_date = "",
                      date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "new_share")
  dots <- list(...)

  do.call(market_eod, c(args, dots))
}
