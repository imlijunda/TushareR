get_finance <- function(..., api, timeout = 15) {

  x <- GetAPI()
  dt <- `$.tushare_api`(x, func = force(api))(..., timeout = timeout)

  if (nrow(dt)) {
    cols <- names(dt)
    if ("report_type" %in% cols) {
      dt[, report_type := as.integer(report_type)]
    }
    if ("comp_type" %in% cols) {
      dt[, comp_type := as.integer(comp_type)]
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
#' @param type OPTIONAL, business type.
#' @param date_format How to cast return datetime columns? Default casts values to POSIXct objects.
#' @param ... further arguments passed
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
                   date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               report_type = report_type,
               comp_type = comp_type,
               date_format = date_format,
               api = "income")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}

#' @rdname income
#' @export
#'
income_vip <- function(ts_code = "", ann_date = "", start_date = "", end_date = "",
                       period = "", report_type = "", comp_type = "",
                       date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               report_type = report_type,
               comp_type = comp_type,
               date_format = date_format,
               api = "income_vip")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}

#' @rdname income
#' @export
#'
balancesheet <- function(ts_code = "", ann_date = "", start_date = "", end_date = "",
                       period = "", report_type = "", comp_type = "",
                       date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               report_type = report_type,
               comp_type = comp_type,
               date_format = date_format,
               api = "balancesheet")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}

#' @rdname income
#' @export
#'
balancesheet_vip <- function(ts_code = "", ann_date = "", start_date = "", end_date = "",
                         period = "", report_type = "", comp_type = "",
                         date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               report_type = report_type,
               comp_type = comp_type,
               date_format = date_format,
               api = "balancesheet_vip")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}

#' @rdname income
#' @export
#'
cashflow <- function(ts_code = "", ann_date = "", start_date = "", end_date = "",
                     period = "", report_type = "", comp_type = "",
                     date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               report_type = report_type,
               comp_type = comp_type,
               date_format = date_format,
               api = "cashflow")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}

#' @rdname income
#' @export
#'
cashflow_vip <- function(ts_code = "", ann_date = "", start_date = "", end_date = "",
                         period = "", report_type = "", comp_type = "",
                         date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               report_type = report_type,
               comp_type = comp_type,
               date_format = date_format,
               api = "cashflow_vip")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}

#' @rdname income
#' @export
#'
forecast <- function(ts_code = "", ann_date = "",
                     start_date = "", end_date = "", period = "",
                     date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               date_format = date_format,
               api = "forecast")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}

#' @rdname income
#' @export
#'
forecast_vip <- function(ts_code = "", ann_date = "",
                         start_date = "", end_date = "", period = "",
                         date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               date_format = date_format,
               api = "forecast_vip")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}

#' @rdname income
#' @export
#'
express <- function(ts_code = "", ann_date = "",
                    start_date = "", end_date = "", period = "",
                    date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               date_format = date_format,
               api = "express")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}

#' @rdname income
#' @export
#'
express_vip <- function(ts_code = "", ann_date = "",
                        start_date = "", end_date = "", period = "",
                        date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               date_format = date_format,
               api = "express_vip")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}

#' @rdname income
#' @export
#'
fina_indicator <- function(ts_code = "", ann_date = "",
                           start_date = "", end_date = "", period = "",
                           date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               date_format = date_format,
               api = "fina_indicator")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}

#' @rdname income
#' @export
#'
fina_indicator_vip <- function(ts_code = "", ann_date = "",
                               start_date = "", end_date = "", period = "",
                               date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               date_format = date_format,
               api = "fina_indicator_vip")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}

#' @rdname income
#' @export
#'
fina_audit <- function(ts_code = "", ann_date = "",
                           start_date = "", end_date = "", period = "",
                           date_format = c("POSIXct", "Date", "char"), ...) {

  args <- list(ts_code = ts_code,
               ann_date = ann_date,
               start_date = start_date,
               end_date = end_date,
               period = period,
               date_format = date_format,
               api = "fina_audit")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}

#' @rdname income
#' @export
#'
fina_mainbz <- function(ts_code = "", period = "", type = c("", "P", "D"),
                        start_date = "", end_date = "",
                        date_format = c("POSIXct", "Date", "char"), ...) {

  type = match.arg(type)

  args <- list(ts_code = ts_code,
               period = period,
               type = type,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "fina_mainbz")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}

#' @rdname income
#' @export
#'
fina_mainbz_vip <- function(ts_code = "", period = "", type = c("", "P", "D"),
                            start_date = "", end_date = "",
                            date_format = c("POSIXct", "Date", "char"), ...) {

  type = match.arg(type)

  args <- list(ts_code = ts_code,
               period = period,
               type = type,
               start_date = start_date,
               end_date = end_date,
               date_format = date_format,
               api = "fina_mainbz")
  dots <- list(...)

  do.call(get_finance, c(args, dots))
}
