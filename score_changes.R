#################################################
## Quickly throw together some relative charts ##
#################################################
source('StockPlot.R')
source('Sql_Wrapper.R')

# options
bmark_code = 'MSCIWORLDG'
currency = "Local"
ticker_list = c("ANZ.ASX", "BHP.ASX")
min_date = as.Date("7/12/2015", "%d/%m/%Y")
date_range_index = paste(format(min_date, "%Y-%m-%d"), "/", sep = "")

# chart title
title = "stocks"
if (length(ticker_list) == 1) { title = ticker_list[1] }
title = paste(title,"vs",bmark_code)

x = get_ticker_xts_return_index(ticker_list, currency, min_date)  # head(x)
stock_return = CalculateReturns(x)[date_range_index] # head(stock_return)
bmark_return = get_benchmark_xts_returns(bmark_code, currency)[date_range_index] # head(bmark_return)
xts_relative = Return.relative(stock_return, bmark_return)

chart = rel_plot(stock_return, bmark_return, title)
chart