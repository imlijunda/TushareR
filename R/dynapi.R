#' Get a Tushare API object.
#'
#' @param token API token.
#'
#' @return a tushare_api object.
#' @export
#'
GetAPI <- function(token = GetToken()) {

  return(
    structure(token, class = "tushare_api")
  )
}

#' Print Values
#'
#' @param x A tushare_api object
#' @param ... not used
#'
#' @return x, invisibly
#' @export
#'
'print.tushare_api' <- function(x, ...) {

  val <- sprintf("Tushare API object token %s", x)
  print(val)

  invisible(x)
}

arg_date <- c("start_date", "end_date", "trade_date", "suspend_date", "resume_date",
              "ann_date", "period", "record_date", "ex_date", "imp_ann_date",
              "pre_date", "actual_date", "float_date", "enddate",
              "pay_date", "date", "report_date",
              "list_date", "delist_date", "cal_date", "pretrade_date",
              "in_date", "out_date", "setup_date", "ipo_date", "issue_date",
              "first_ann_date", "base_date", "div_listdate", "modify_date",
              "release_date", "exp_date", "begin_date", "close_date", "found_date",
              "due_date", "purc_startdate", "redm_startdate", "imp_anndate",
              "earpay_date", "net_ex_date", "account_date", "last_ddate",
              "maturity_date", "last_edate")

arg_time <- c("start_time", "end_time", "trade_time", "datetime", "pub_time")

arg_logical01 <- c("is_open", "is_new", "is_audit", "is_release", "is_buyback",
                    "is_ct", "update_flag")

#' Dynamic Tushare API functions
#'
#' @param x A tushare_api object
#' @param func Tushare API function to call
#'
#' @return a data.frame/data.table
#' @export
#'
#' @examples
#' \dontrun{
#'
#' api <- GetAPI()
#'
#' api$stock_basic(exchange='', list_status='L')
#' api$trade_cal(exchange='', start_date='20180101', end_date='20181231')
#' api$namechange(ts_code='600848.SH')
#' api$hs_const(hs_type='SH')
#' api$stock_company(exchange='SZSE')
#' api$new_share(start_date='20180901', end_date='20181018')
#' api$daily(ts_code='000001.SZ', start_date='20180701', end_date='20180718')
#' api$daily(trade_date='20180810')
#' api$weekly(ts_code='000001.SZ', start_date='20180101', end_date='20181101')
#' api$weekly(trade_date='20181123')
#' api$monthly(ts_code='000001.SZ', start_date='20180101', end_date='20181101')
#' api$monthly(trade_date='20181031')
#' api$adj_factor(ts_code='000001.SZ', trade_date='')
#' api$adj_factor(ts_code='', trade_date='20180718')
#' api$suspend(ts_code='600848.SH', suspend_date='', resume_date='')
#' api$daily_basic(ts_code='', trade_date='20180726')
#' api$moneyflow(trade_date='20190315')
#' api$moneyflow(ts_code='002149.SZ', start_date='20190115', end_date='20190315')
#' api$api$stk_limit(ts_code='002149.SZ', start_date='20190115', end_date='20190615')
#' api$income(ts_code='600000.SH', start_date='20180101', end_date='20180730')
#' api$income_vip(period='20181231')
#' api$balancesheet(ts_code='600000.SH', start_date='20180101', end_date='20180730')
#' api$balancesheet_vip(period='20181231')
#' api$cashflow(ts_code='600000.SH', start_date='20180101', end_date='20180730')
#' api$cashflow_vip(period='20181231')
#' api$forecast(ann_date='20190131')
#' api$forecast_vip(period='20181231')
#' api$express(ts_code='600000.SH', start_date='20180101', end_date='20180701')
#' api$express_vip(period='20181231')
#' api$dividend(ts_code='600848.SH', fields='ts_code,div_proc,stk_div,record_date,ex_date')
#' api$fina_indicator(ts_code='600000.SH')
#' api$fina_audit(ts_code='600000.SH', start_date='20100101', end_date='20180808')
#' api$fina_mainbz(ts_code='000627.SZ', type='P')
#' api$disclosure_date(end_date='20181231')
#' api$moneyflow_hsgt(start_date='20180125', end_date='20180808')
#' api$hsgt_top10(trade_date='20180725', market_type='1')
#' api$ggt_top10(trade_date='20180727')
#' api$margin(trade_date='20180802')
#' api$margin_detail(trade_date='20180802')
#' api$top10_holders(ts_code='600000.SH', start_date='20170101', end_date='20171231')
#' api$top10_floatholders(ts_code='600000.SH', start_date='20170101', end_date='20171231')
#' api$top_list(trade_date='20180928')
#' api$top_inst(trade_date='20180928')
#' api$pledge_stat(ts_code='000014.SZ')
#' api$pledge_detail(ts_code='000014.SZ')
#' api$repurchase(ann_date='20181010')
#' api$concept(src='ts')
#' api$concept_detail(id='TS2', fields='ts_code,name')
#' api$concept_detail(ts_code = '600848.SH')
#' api$share_float(ann_date='20181220')
#' api$block_trade(trade_date='20181227')
#' api$stk_account(start_date='20180101', end_date='20181231')
#' api$stk_holdernumber(ts_code='300199.SZ', start_date='20160101', end_date='20181231')
#' api$stk_holdertrade(ann_date='20190426')
#' api$stk_holdertrade(ts_code='002149.SZ')
#' api$stk_holdertrade(ann_date='20190426', trade_type='IN')
#' api$index_daily(ts_code='399300.SZ')
#' api$index_daily(ts_code='399300.SZ', start_date='20180101', end_date='20181010')
#' api$index_weight(index_code='399300.SZ', start_date='20180901', end_date='20180930')
#' api$index_dailybasic(trade_date='20181018', fields='ts_code,trade_date,turnover_rate,pe')
#' api$index_classify(level='L3', src='SW')
#' api$index_member(ts_code='000001.SZ')
#' api$index_member(index_code='850531.SI')
#'
#' }
'$.tushare_api' <- function(x, func) {

  f <- function(...,
                time_format = c("POSIXct", "char"),
                date_format = c("POSIXct", "Date", "char"),
                logi_format = c("logical", "char"),
                timeout = 5.0) {

    args <- list(...)
    args <- c(args, list(
      token = x,
      timeout = timeout
    ))

    #special treatment due to year-month date format
    if (func == "teleplay_record") {
      fix_date <- fix_date_teleplay
      time_format <- "char"
      date_format <- "char"
    }

    argn <- names(args)
    #convert date to proper str
    idx <- argn %in% arg_date
    if (any(idx)) {
      args[idx] <- lapply(args[idx], fix_date)
    }
    #convert time to proper str
    idx <- argn %in% arg_time
    if (any(idx)) {
      args[idx] <- lapply(args[idx], fix_time)
    }

    args$api_name <- func
    args$token <- as.character(x)
    args$timeout <- timeout
    dt <- do.call(TusRequest, args)

    if (nrow(dt)) {
      date_format <- match.arg(date_format)
      date_col_cast <- cast_date(date_format)
      time_format <- match.arg(time_format)
      time_col_cast <- cast_time(time_format)
      logi_format <- match.arg(logi_format)
      logi_col_cast <- cast_logical(logi_format)

      cols <- colnames(dt)
      col <- which(cols %in% arg_date)
      if (length(col)) {
        #convert date string to proper date type
        set(dt, j = col, value = lapply(dt[, ..col], date_col_cast))
      }
      col <- which(cols %in% arg_time)
      if (length(col)) {
        #convert time string to proper time type
        set(dt, j = col, value = lapply(dt[, ..col], time_col_cast))
      }
      col <- which(cols %in% arg_logical01)
      if (length(col)) {
        #convert logi string to logical type
        set(dt, j = col, value = lapply(dt[, ..col], logi_col_cast))
      }

      if ("ts_code" %in% cols) {
        if (all(dt$ts_code == dt$ts_code[1])) {
          #single ts_code returned, sort by date
          if ("trade_date" %in% cols) {
            #trade_date is present, sort by trade_date
            setkeyv(dt, "trade_date")
          } else if ("trade_time" %in% cols) {
            setkeyv(dt, "trade_time")
          } else {
            #this is a bit complitated
            if (func == "namechange" && "start_date" %in% cols) {
              #sort by start_date
              setkeyv(dt, "start_date")
            } else if (func == "share_float" && "float_date" %in% cols) {
              #sort by float_date
              setkeyv(dt, "float_date")
            } else if (func == "stk_holdertrade" && "ann_date" %in% cols) {
              #sort by ann_date
              setkeyv(dt, "ann_date")
            } else if (func == "fund_div" && "imp_anndate" %in% cols) {
              #sort by imp_anndate
              setkeyv(dt, "imp_anndate")
            } else if (func == "anns" && "ann_date" %in% cols) {
              #sort by ann_date
              setkeyv(dt, "ann_date")
            } else {
              if ("end_date" %in% cols) {
                setkeyv(dt, "end_date")
              }
            }
          }
        } else {
          #multiple ts_code returned, sort by ts_code
          setkeyv(dt, "ts_code")
        }
      } else {
        if ("trade_date" %in% cols) {
          if (all(dt$trade_date == dt$trade_date[1])) {
            if ("symbol" %in% cols) {
              setkeyv(dt, "symbol")
            } else if ("trade_time" %in% cols) {
              setkeyv(dt, "trade_time")
            } else if ("cal_date" %in% cols) {
              setkeyv(dt, "cal_date")
            } else if ("code" %in% cols) {
              setkeyv(dt, "code")
            } else if ("date" %in% cols) {
              setkeyv(dt, "date")
            } else if ("index_code" %in% cols) {
              setkeyv(dt, "index_code")
            } else if ("rec_no" %in% cols) {
              setkeyv(dt, "rec_no")
            } else if ("name" %in% cols) {
              setkeyv(dt, "name")
            } else if ("datetime" %in% cols) {
              setkeyv(dt, "datetime")
            }
          } else {
            setkeyv(dt, "trade_date")
          }
        }
      }
    }

    dt
  }

  f
}
