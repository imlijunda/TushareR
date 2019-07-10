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
                    "is_ct")

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
#'
#' }
'$.tushare_api' <- function(x, func) {

  f <- function(...,
                time_format = c("POSIXct", "char"),
                date_format = c("POSIXct", "Date", "char"),
                logi_format = c("logical", "char"),
                timeout = 5.0) {

    args <- list(...)

    if (func == "teleplay_record") {
      fix_date <- fix_date_teleplay
      time_format <- "char"
      date_format <- "char"
    }

    for (name in names(args)) {
      #convert date to proper str
      if (name %in% arg_date) {
        args[[name]] <- fix_date(args[[name]])
      }
      #convert time to proper str
      if (name %in% arg_time) {
        args[[name]] <- fix_time(args[[name]])
      }
    }

    args$api_name <- func
    args$token <- as.character(x)
    args$timeout <- timeout
    dt <- do.call(TusRequest, args)

    date_format <- match.arg(date_format)
    date_col_cast <- cast_date(date_format)
    time_format <- match.arg(time_format)
    time_col_cast <- cast_time(time_format)
    logi_format <- match.arg(logi_format)
    logi_col_cast <- cast_logical(logi_format)

    cnames <- colnames(dt)
    for (col in cnames) {
      #convert date to proper datetime type
      if (col %in% arg_date) {
        set(dt, j = col, value = date_col_cast(dt[[col]]))
      }
      #convert time to proper datetime type
      if (col %in% arg_time) {
        set(dt, j = col, value = time_col_cast(dt[[col]]))
      }
      #convert 0/1 to proper logical value
      if (col %in% arg_logical01) {
        set(dt, j = col, value = logi_col_cast(dt[[col]]))
      }
    }
    if ("ts_code" %in% cnames) {
      if (all(dt$ts_code == dt$ts_code[1])) {
        #single ts_code returned, sort by date
        if ("trade_date" %in% cnames) {
          #trade_date is present, sort by trade_date
          setkeyv(dt, "trade_date")
        } else {
          #this is a bit complitated
          if (func == "namechange" && "start_date" %in% cnames) {
            #sort by start_date
            setkeyv(dt, "start_date")
          } else if (func == "share_float" && "float_date" %in% cnames) {
            #sort by float_date
            setkeyv(dt, "float_date")
          } else if (func == "stk_holdertrade" && "ann_date" %in% cnames) {
            #sort by ann_date
            setkeyv(dt, "ann_date")
          } else if (func == "fund_div" && "imp_anndate" %in% cnames) {
            #sort by imp_anndate
            setkeyv(dt, "imp_anndate")
          } else if (func == "anns" && "ann_date" %in% cnames) {
            #sort by ann_date
            setkeyv(dt, "ann_date")
          } else {
            if ("end_date" %in% cnames) {
              setkeyv(dt, "end_date")
            }
          }
        }
      } else {
        #multiple ts_code returned, sort by ts_code
        setkeyv(dt, "ts_code")
      }
    } else {
      if ("trade_date" %in% cnames) {
        setkeyv(dt, "trade_date")
      } else if ("cal_date" %in% cnames) {
        setkeyv(dt, "cal_date")
      } else if ("code" %in% cnames) {
        setkeyv(dt, "code")
      } else if ("date" %in% cnames) {
        setkeyv(dt, "date")
      } else if ("index_code" %in% cnames) {
        setkeyv(dt, "index_code")
      } else if ("rec_no" %in% cnames) {
        setkeyv(dt, "rec_no")
      } else if ("name" %in% cnames) {
        setkeyv(dt, "name")
      } else if ("datetime" %in% cnames) {
        setkeyv(dt, "datetime")
      }
    }

    dt
  }

  f
}
