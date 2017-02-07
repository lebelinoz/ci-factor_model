######################################
# A stock.returns object is an xts time series of returns of one or more stocks, coupled with a timeframe.
# Each column's name corresponds to the ticker as it appears in PCI_CORE.dbo.t_Ref_Sec, with the '.ASX' exchange omitted where applicable.
# The frequency, start date and end date of the timeframe ought to correspond to the xts's index.
#
# Initialize functions will be supplied so users can create stock.returns objects directly from benchmark codes, portfolio codes and sec_id lists
# Where the user supplies more than one parameter (i.e. a benchmark code AND a portfolio code), the lists of securities will be combined.
# "all" is a valid portfolio code which means "all stocks currently held by sec_id".
######################################

require(lubridate)
source('./lib/Sql_Wrapper.R')
source('./class/timeframe.R')

# This is what I call the base constructor
stock.returns <- setClass(
    Class = "stock.returns",
    slots = c(xts_returns = "xts", timeframe = "timeframe"),
    prototype = prototype(xts_returns = xts(, order.by = c(today())), timeframe = timeframe()),
)

stock.returns <- function(timeframe, benchmark_code, portfolio_code, sec_id_list) {

}

test1 = new("stock.returns", start_date = dmy("1/1/2015"), end_date = dmy("1/1/2016"), frequency = "D") 