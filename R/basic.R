#' Get basic stock market information
#'
#' @param is_hs Flag for Shenzhen/Shanghai-Hong Kong Stock Connect
#' @param list_status List status
#' @param exchange Exchange market
#' @param date_format How to cast datetime format.
#' @param is_open Wheter the market is open
#' @param ts_code Tushare equity code
#' @param hs_type Type of Shenzhen/Shanghai-Hong Kong Stock Connect
#' @param start_date Start date
#' @param end_date End date
#' @param is_new Whether the stock is new
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
                        date_format = c("POSIXct", "Date", "char")) {

  is_hs <- match.arg(is_hs)
  list_status <- match.arg(list_status)
  exchange <- match.arg(exchange)
  date_format <- match.arg(date_format)

  dt <- TusRequest(api_name = "stock_basic", is_hs = is_hs, list_status = list_status,
                   exchange = exchange)

  colfunc <- cast_date(date_format)
  dt[, list_date := colfunc(list_date)]

  dt
}

#' @rdname stock_basic
#' @export
#'
trade_cal <- function(exchange = c("", "SSE", "SZSE"), start_date = "", end_date = "", is_open,
                      date_format = c("POSIXct", "Date", "char")){

  exchange <- match.arg(exchange)
  start_date <- fix_date(start_date)
  end_date <- fix_date(end_date)
  date_format <- match.arg(date_format)

  args <- list("trade_cal",
               exchange = exchange,
               start_date = start_date,
               end_date = end_date)
  if (!missing(is_open)) {
    args$is_open <- is_open
  }
  dt <- do.call(TusRequest, args)

  colfunc <- cast_date(date_format)
  dt[, cal_date := colfunc(cal_date)]

  dt
}

#' @rdname stock_basic
#' @export
#'
hs_const <- function(hs_type = c("SH", "SZ"), is_new = c("1", "0"),
                     date_format = c("POSIXct", "Date", "char")) {

  hs_type <- match.arg(hs_type)
  is_new <- match.arg(is_new)
  date_format <- match.arg(date_format)

  dt <- TusRequest("hs_const", hs_type = hs_type, is_new = is_new)

  colfunc <- cast_date(date_format)
  dt[, in_date := colfunc(in_date)]
  dt[, out_date := colfunc(out_date)]

  dt
}

#' @rdname stock_basic
#' @export
#'
namechange <- function(ts_code = "", start_date = "", end_date = "",
                       date_format = c("POSIXct", "Date", "char")) {

  ts_code <- fix_code(ts_code)
  start_date <- fix_date(start_date)
  end_date <- fix_date(end_date)
  date_format <- match.arg(date_format)

  dt <- TusRequest("namechange", ts_code = ts_code, start_date = start_date, end_date = end_date)

  colfunc <- cast_date(date_format)
  dt[, start_date := colfunc(start_date)]
  dt[, end_date := colfunc(end_date)]
  dt[, ann_date := colfunc(ann_date)]

  dt
}

#' @rdname stock_basic
#' @export
#'
stock_company <- function(exchange = c("SSE", "SZSE"),
                          date_format = c("POSIXct", "Date", "char")) {

  exchange <- match.arg(exchange)
  date_format <- match.arg(date_format)

  dt <- TusRequest("stock_company", exchange = exchange)

  colfunc <- cast_date(date_format)
  dt[, setup_date := colfunc(setup_date)]

  dt
}

#' @rdname stock_basic
#' @export
#'
new_share <- function(start_date = "", end_date = "",
                      date_format = c("POSIXct", "Date", "char")) {

  start_date <- fix_date(start_date)
  end_date <- fix_date(end_date)
  date_format <- match.arg(date_format)

  dt <- TusRequest("new_share", start_date = start_date, end_date = end_date)

  colfunc <- cast_date(date_format)
  dt[, ipo_date := colfunc(ipo_date)]
  dt[, issue_date := colfunc(issue_date)]

  dt
}
