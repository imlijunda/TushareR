#' Adjusted closing prices
#'
#' @param values a data.table, keyed by date/time column. Can be obtained from eod market functions.
#' @param factors a data.table, keyed by date/time column. Can be obtained from adj_factor().
#' @param adjust following Tushare Pro convention, qfq adjusts previous closing
#' prices w.r.t last closing prices, hfq adjusts closing prices w.r.t first traded
#' price.
#'
#' @return data.table
#' @export
#'
#' @examples
#' \dontrun{
#' #query prices and adjust factors
#' pab <- daily(ts_code = "000001.sz")
#' pab_af <- adj_factor(ts_code = "000001.sz")
#' #post-adjusted
#' pab_adjusted <- adjusted_ohlc(pab, pab_af, "hfq")
#' #pre-adjusted
#' pab_adjusted <- adjusted_ohlc(pab, pab_af, "qfq")
#' }
adjusted_ohlc <- function(values, factors, adjust = c("qfq", "hfq")) {

  f <- data.table::copy(factors)
  adjust <- match.arg(adjust)
  if (adjust == "qfq") {
    f[, adj_factor := adj_factor / adj_factor[.N]]
  }

  dt <- f[values, , roll = TRUE]
  dt[, open := open * adj_factor]
  dt[, high := high * adj_factor]
  dt[, low := low * adj_factor]
  dt[, close := close * adj_factor]
  dt[, adj_factor := NULL]
  dt[, i.ts_code := NULL]

  dt
}
