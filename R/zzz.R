tus.globals <- new.env()
tus.globals$api_token <- NULL
tus.globals$timezone <- "Asia/Shanghai"
tus.globals$date_fmt <- "%Y%m%d"
tus.globals$date_teleplay_fmt <- "%Y%m"
tus.globals$datetime_fmt <- "%Y-%m-%d %H:%M:%S"

#fixes for R CMD check
utils::globalVariables(c(
  "f_ann_date",
  "ann_date",
  "cal_date",
  "in_date",
  "ipo_date",
  "issue_date",
  "list_date",
  "out_date",
  "setup_date",
  "trade_time",
  "fgrp",
  "list_date",
  "delist_date",
  "last_ddate",
  "trade_time",
  "i.ts_code",
  "high",
  "low",
  "trade_time",
  "..col",
  "adj_factor",
  "open",
  "close",
  "comp_type",
  "report_type"
))
