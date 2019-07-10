market_finance <- function(ts_code = "", ann_date = "", start_date = "", end_date = "",
                           period = "", report_type = "", comp_type = "",
                           date_format = c("POSIXct", "Date", "char"),
                           api = c("income", "income_vip",
                                   "balancesheet", "balancesheet_vip",
                                   "cashflow", "cashflow_vip")) {

  ts_code <- fix_code(ts_code)
  ann_date <- fix_date(ann_date)
  start_date <- fix_date(start_date)
  end_date <- fix_date(end_date)
  period <- fix_date(period)

  date_format <- match.arg(date_format)
  api <- match.arg(api)

  dt <- TusRequest(api, ts_code = ts_code, ann_date = ann_date,
                   start_date = start_date, end_date = end_date, period = period,
                   report_type = report_type, comp_type = comp_type)

  if (nrow(dt)) {
    colfunc <- cast_date(date_format)
    dt[, ann_date := colfunc(ann_date)]
    dt[, f_ann_date := colfunc(f_ann_date)]
    dt[, end_date := colfunc(end_date)]
    dt[, report_type := as.integer(report_type)]
    dt[, comp_type := as.integer(comp_type)]
    if (ts_code == "") {
      data.table::setkeyv(dt, cols = "end_date")
    } else {
      data.table::setkeyv(dt, cols = "ts_code")
    }
  }

  dt
}

#' Consolidated financial reports.
#'
#' @param ts_code OPTIONAL, Tushare equity code.
#' @param ann_date OPTIONAL, annoucement date.
#' @param start_date OPTIONAL, report start date.
#' @param end_date OPTIONAL, report end date.
#' @param period OPTIONAL, report period.
#' @param report_type OPTIONAL, report type. Please refer to online API document.
#' @param comp_type OPTIONAL, report company type. Please refer to online API document.
#' @param date_format How to cast return datetime columns? Default casts values to POSIXct objects.
#'
#' @return a data.frame/data.table
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- income(ts_code='600000.SH', start_date='20180101', end_date='20180730')
#' }
income <- function(ts_code = "", ann_date = "", start_date = "", end_date = "",
                   period = "", report_type = "", comp_type = "",
                   date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               report_type = report_type,
               comp_type = comp_type,
               date_format = date_format,
               api = "income")

  do.call(market_finance, args)
}

#' @rdname income
#' @export
#'
income_vip <- function(ts_code = "", ann_date = "", start_date = "", end_date = "",
                       period = "", report_type = "", comp_type = "",
                       date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               report_type = report_type,
               comp_type = comp_type,
               date_format = date_format,
               api = "income_vip")

  do.call(market_finance, args)
}

#' @rdname income
#' @export
#'
balancesheet <- function(ts_code = "", ann_date = "", start_date = "", end_date = "",
                       period = "", report_type = "", comp_type = "",
                       date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               report_type = report_type,
               comp_type = comp_type,
               date_format = date_format,
               api = "balancesheet")

  do.call(market_finance, args)
}

#' @rdname income
#' @export
#'
balancesheet_vip <- function(ts_code = "", ann_date = "", start_date = "", end_date = "",
                         period = "", report_type = "", comp_type = "",
                         date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               report_type = report_type,
               comp_type = comp_type,
               date_format = date_format,
               api = "balancesheet_vip")

  do.call(market_finance, args)
}

#' @rdname income
#' @export
#'
cashflow <- function(ts_code = "", ann_date = "", start_date = "", end_date = "",
                     period = "", report_type = "", comp_type = "",
                     date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               report_type = report_type,
               comp_type = comp_type,
               date_format = date_format,
               api = "cashflow")

  do.call(market_finance, args)
}

#' @rdname income
#' @export
#'
cashflow_vip <- function(ts_code = "", ann_date = "", start_date = "", end_date = "",
                         period = "", report_type = "", comp_type = "",
                         date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               report_type = report_type,
               comp_type = comp_type,
               date_format = date_format,
               api = "cashflow_vip")

  do.call(market_finance, args)
}

market_finance_extra <- function(ts_code = "", ann_date = "",
                                 start_date = "", end_date = "", period = "",
                                 date_format = c("POSIXct", "Date", "char"),
                                 api = c("forecast", "forecast_vip",
                                         "express", "express_vip",
                                         "fina_indicator", "fina_indicator_vip",
                                         "fina_audit")) {

  ts_code <- fix_code(ts_code)
  ann_date <- fix_date(ann_date)
  start_date <- fix_date(start_date)
  end_date <- fix_date(end_date)
  period <- fix_date(period)

  date_format <- match.arg(date_format)
  api <- match.arg(api)

  dt <- TusRequest(api, ts_code = ts_code, ann_date = ann_date,
                   start_date = start_date, end_date = end_date, period = period)

  if (nrow(dt)) {
    colfunc <- cast_date(date_format)
    dt[, ann_date := colfunc(ann_date)]
    dt[, end_date := colfunc(end_date)]
    if (ts_code == "") {
      data.table::setkeyv(dt, cols = "end_date")
    } else {
      data.table::setkeyv(dt, cols = "ts_code")
    }
  }

  dt
}

#' @rdname income
#' @export
#'
forecast <- function(ts_code = "", ann_date = "",
                     start_date = "", end_date = "", period = "",
                     date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               date_format = date_format,
               api = "forecast")

  do.call(market_finance_extra, args)
}

#' @rdname income
#' @export
#'
forecast_vip <- function(ts_code = "", ann_date = "",
                         start_date = "", end_date = "", period = "",
                         date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               date_format = date_format,
               api = "forecast_vip")

  do.call(market_finance_extra, args)
}

#' @rdname income
#' @export
#'
express <- function(ts_code = "", ann_date = "",
                    start_date = "", end_date = "", period = "",
                    date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               date_format = date_format,
               api = "express")

  do.call(market_finance_extra, args)
}

#' @rdname income
#' @export
#'
express_vip <- function(ts_code = "", ann_date = "",
                        start_date = "", end_date = "", period = "",
                        date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               date_format = date_format,
               api = "express_vip")

  do.call(market_finance_extra, args)
}

#' @rdname income
#' @export
#'
fina_indicator <- function(ts_code = "", ann_date = "",
                           start_date = "", end_date = "", period = "",
                           date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               date_format = date_format,
               api = "fina_indicator")

  do.call(market_finance_extra, args)
}

#' @rdname income
#' @export
#'
fina_indicator_vip <- function(ts_code = "", ann_date = "",
                               start_date = "", end_date = "", period = "",
                               date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               date_format = date_format,
               api = "fina_indicator_vip")

  do.call(market_finance_extra, args)
}

#' @rdname income
#' @export
#'
fina_audit <- function(ts_code = "", ann_date = "",
                           start_date = "", end_date = "", period = "",
                           date_format = c("POSIXct", "Date", "char")) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               date_format = date_format,
               api = "fina_audit")

  do.call(market_finance_extra, args)
}

#' @rdname income
#' @export
#'
fina_mainbz <- function(ts_code = "", period = "", type = c("", "P", "D"),
                        start_date = "", end_date = "",
                        date_format = c("POSIXct", "Date", "char")) {

  ts_code <- fix_code(ts_code)
  period <- fix_date(period)
  type = match.arg(type)
  start_date <- fix_date(start_date)
  end_date <- fix_date(end_date)

  date_format <- match.arg(date_format)

  dt <- TusRequest("fina_mainbz", period = period, type = type,
                   start_date = start_date, end_date = end_date)

  if (nrow(dt)) {
    colfunc <- cast_date(date_format)
    dt[, end_date := colfunc(end_date)]
  }

  dt
}

#' @rdname income
#' @export
#'
fina_mainbz_vip <- function(ts_code = "", period = "", type = c("", "P", "D"),
                        start_date = "", end_date = "",
                        date_format = c("POSIXct", "Date", "char")) {

  ts_code <- fix_code(ts_code)
  period <- fix_date(period)
  type = match.arg(type)
  start_date <- fix_date(start_date)
  end_date <- fix_date(end_date)

  date_format <- match.arg(date_format)

  dt <- TusRequest("fina_mainbz_vip", period = period, type = type,
                   start_date = start_date, end_date = end_date)

  if (nrow(dt)) {
    colfunc <- cast_date(date_format)
    dt[, end_date := colfunc(end_date)]
  }

  dt
}
