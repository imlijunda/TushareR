% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dynapi.R
\name{$.tushare_api}
\alias{$.tushare_api}
\title{Dynamic Tushare API functions}
\usage{
\method{$}{tushare_api}(x, func)
}
\arguments{
\item{x}{A tushare_api object}

\item{func}{Tushare API function to call}
}
\value{
a data.frame/data.table
}
\description{
Dynamic Tushare API functions
}
\examples{
\dontrun{

api <- GetAPI()

api$stock_basic(exchange='', list_status='L')
api$trade_cal(exchange='', start_date='20180101', end_date='20181231')
api$namechange(ts_code='600848.SH')
api$hs_const(hs_type='SH')
api$stock_company(exchange='SZSE')
api$new_share(start_date='20180901', end_date='20181018')
api$daily(ts_code='000001.SZ', start_date='20180701', end_date='20180718')
api$daily(trade_date='20180810')
api$weekly(ts_code='000001.SZ', start_date='20180101', end_date='20181101')
api$weekly(trade_date='20181123')
api$monthly(ts_code='000001.SZ', start_date='20180101', end_date='20181101')
api$monthly(trade_date='20181031')
api$adj_factor(ts_code='000001.SZ', trade_date='')
api$adj_factor(ts_code='', trade_date='20180718')
api$suspend(ts_code='600848.SH', suspend_date='', resume_date='')
api$daily_basic(ts_code='', trade_date='20180726')
api$moneyflow(trade_date='20190315')
api$moneyflow(ts_code='002149.SZ', start_date='20190115', end_date='20190315')
api$api$stk_limit(ts_code='002149.SZ', start_date='20190115', end_date='20190615')
api$income(ts_code='600000.SH', start_date='20180101', end_date='20180730')
api$income_vip(period='20181231')
api$balancesheet(ts_code='600000.SH', start_date='20180101', end_date='20180730')
api$balancesheet_vip(period='20181231')
api$cashflow(ts_code='600000.SH', start_date='20180101', end_date='20180730')
api$cashflow_vip(period='20181231')
api$forecast(ann_date='20190131')
api$forecast_vip(period='20181231')
api$express(ts_code='600000.SH', start_date='20180101', end_date='20180701')
api$express_vip(period='20181231')
api$dividend(ts_code='600848.SH', fields='ts_code,div_proc,stk_div,record_date,ex_date')
api$fina_indicator(ts_code='600000.SH')
api$fina_audit(ts_code='600000.SH', start_date='20100101', end_date='20180808')
api$fina_mainbz(ts_code='000627.SZ', type='P')
api$disclosure_date(end_date='20181231')
api$moneyflow_hsgt(start_date='20180125', end_date='20180808')
api$hsgt_top10(trade_date='20180725', market_type='1')
api$ggt_top10(trade_date='20180727')
api$margin(trade_date='20180802')
api$margin_detail(trade_date='20180802')
api$top10_holders(ts_code='600000.SH', start_date='20170101', end_date='20171231')
api$top10_floatholders(ts_code='600000.SH', start_date='20170101', end_date='20171231')
api$top_list(trade_date='20180928')
api$top_inst(trade_date='20180928')
api$pledge_stat(ts_code='000014.SZ')
api$pledge_detail(ts_code='000014.SZ')
api$repurchase(ann_date='20181010')
api$concept(src='ts')
api$concept_detail(id='TS2', fields='ts_code,name')
api$concept_detail(ts_code = '600848.SH')
api$share_float(ann_date='20181220')
api$block_trade(trade_date='20181227')
api$stk_account(start_date='20180101', end_date='20181231')
api$stk_holdernumber(ts_code='300199.SZ', start_date='20160101', end_date='20181231')
api$stk_holdertrade(ann_date='20190426')
api$stk_holdertrade(ts_code='002149.SZ')
api$stk_holdertrade(ann_date='20190426', trade_type='IN')
api$index_daily(ts_code='399300.SZ')
api$index_daily(ts_code='399300.SZ', start_date='20180101', end_date='20181010')
api$index_weight(index_code='399300.SZ', start_date='20180901', end_date='20180930')
api$index_dailybasic(trade_date='20181018', fields='ts_code,trade_date,turnover_rate,pe')
api$index_classify(level='L3', src='SW')
api$index_member(ts_code='000001.SZ')
api$index_member(index_code='850531.SI')

}
}
