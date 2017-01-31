###################################
###                             ###
### COMPUTE A FACTOR MODEL      ###
###                             ###
###################################

### PREAMBLE
source('./lib/Sql_Wrapper.R') # <-- for grabbing data straight from our SQL database
source('./lib/StockPlot.R')   # <-- for plotting relative return charts

# Parameters:
benchmark_code = 'XJO'
frequency = "Monthly"
currency = "AUD"
min_date = as_date("2011-11-28") # as.Date("2011-12-30") # as.Date("2015-09-29") # 
max_date = as_date("2016-11-30")
test_date = as_date("2016-12-30")

###########################
###   EQUITY RETURNS    ###

### STEP 1: GET A LIST OF sec_id's (this is the part which will change every time)
sql_sec_id = paste("EXEC PCI_CORE.dbo.get_index_constituents @index_code  = '",
                   benchmark_code,
                   "', @at_date = '",
                   format(max_date, "%Y-%m-%d"),
                   "'",
                   sep = "")
secIdList <- get_table_from_sql_CISMPRDSVR(sql_sec_id)$sec_id

# secIdList = c(1572, 1676, 2790) # ANZ, BHP, TLS


### STEP 2: GET RETURN DATA FROM THE DATABASE AT THE DESIRED FREQUENCY
# Make a sql statement which looks like this:  EXEC PCI_REPORTING.dbo.get_performance_chart @sec_id_list = '1676,2604' , @currency='AUD', @frequency='Weekly'frequency <- "Weekly"
secIdList_AsString <- paste(secIdList, collapse = ",")
sql_returns = paste("EXEC PCI_REPORTING.dbo.get_performance_chart @sec_id_list = '",
                    secIdList_AsString,
                    "', @currency='",
                    currency,
                    "', @frequency='",
                    frequency,
                    "', @minimum_date_override = '",
                    format(min_date, "%Y-%m-%d"),
                    "'",
                    sep = "")
all_stock_returns_df <- get_table_from_sql_CISMPRDSVR(sql_returns)

# There is a "missing" column in all_stock_returns_df which we can ignore
all_stock_returns_df[, "missing"] <- list(NULL)

# Rename the columns to be the ticker, not the sec_id:
sql_column_names = paste("SELECT sec_id, REPLACE(sec_ticker,'.ASX','') AS [ticker] from PCI_CORE.dbo.t_Ref_Sec WHERE sec_id in (", secIdList_AsString, ")", sep = "")
column_names <- get_table_from_sql_CISMPRDSVR(sql_column_names)
for (i in 1:length(column_names[, 1])) {
    pair = column_names[i,]
    colnames(all_stock_returns_df)[which(colnames(all_stock_returns_df) == pair[, 1])] = toString(pair[, 2])
}

# Drop all columns where there is only data for less than 50% of the data 
# (eg. for 52-week correlations, stocks which IPO'd six months ago will look off-the-charts)
all_stock_returns_df = all_stock_returns_df[, colSums(is.na(all_stock_returns_df)) < length(all_stock_returns_df[, 1]) / 2]

# Drop T-1 row (remember that SQL's get_performance_chart always tacks on a T-1 data point which may not reflect a full period)
all_stock_returns_df = all_stock_returns_df[1:(length(all_stock_returns_df[, 1]) - 1),]

# Convert to xts
all_stock_returns = xts(all_stock_returns_df[, -1], all_stock_returns_df[, 1])

# Keep only returns between max_date and min_date
stock_returns = all_stock_returns[paste(min_date, max_date, sep = "/")]
###   EQUITY RETURNS    ###
###########################


###########################
###  BENCHMARK RETURNS  ###
all_bmark_returns = get_benchmark_xts_returns(benchmark_code, currency, frequency)
bmark_returns = all_bmark_returns[paste(min_date, max_date, sep = "/")]
###  BENCHMARK RETURNS  ###
###########################


###########################
###  FACTOR RETURNS     ###

## TO DO:  Get the bond index returns for factor_sec_id.
#factor_ticker = 'SPBDAGVT.ASX' # S&P/ASX Government Bond index ticker is SPBDAGVT.ASX  ...For the U.S. Treasury Bond index, use SPBDUSB0.US
#ticker_list = c(factor_ticker)
#factor_index = get_ticker_xts_return_index(ticker_list, currency)
#all_factor_returns = periodReturn(factor_index, tolower(frequency))
#factor_returns = all_factor_returns[paste(min_date, max_date, sep = "/")]

# Change of plans:  let's use the Citigroup bond index because it has more history, and it has a corresponding yield which we may find useful.
# I've exported the Aussie one (ticker SBABIG) into a csv file:

# How to format upon importing according to http://stackoverflow.com/questions/13022299/specify-custom-date-format-for-colclasses-argument-in-read-table-read-csv
setClass('myDate')
setAs("character", "myDate", function(from) as.Date(from, format = "%m/%d/%y"))
bond_index = read.csv("C://Temp//SBABIG.csv", colClasses = c('myDate', 'numeric', 'numeric'))
all_factor_returns = periodReturn(xts(bond_index[, 2], order.by = bond_index[, 1]), period = tolower(frequency))
factor_returns = all_factor_returns[paste(min_date, max_date, sep = "/")]

###  FACTOR RETURNS     ###
###########################


###########################
### FACTOR MODEL STEP 1 ###

# TO DO:  Regression to compute beta and the error term (which will be alpha)

### FACTOR MODEL STEP 1 ###
###########################


###########################
### FACTOR MODEL STEP 2 ###

# TO DO:  Regression to compute second beta and the leftover error term, thus building a 2-factor model

### FACTOR MODEL STEP 2 ###
###########################


###########################
###   CALCULATE RISKS   ###

# TO DO:  Compute risk metrics.  Calculate the shock of the bond index to the portfolio.

###   CALCULATE RISKS   ###
###########################

test_result = data.frame(
    ticker = character(), 
    frequency = character(), 
    min_date = as_date(character()), 
    max_date = as_date(character()),
    test_date = as_date(character()),
    beta = numeric(), 
    alpha = numeric(), 
    r_squared = numeric(), 
    estimated_next_return = numeric(), 
    actual_next_return = numeric(),
    return_error = numeric()
)

for (ticker in colnames(stock_returns)) {
    asset_returns = stock_returns[, ticker]
    asset_and_bmark = data.frame(index(asset_returns), asset_returns[, 1], bmark_returns[, 1], row.names = NULL)
    colnames(asset_and_bmark) = c("date", "asset", "bmark")
    asset.lm = lm(asset ~ bmark, data = asset_and_bmark)
    asset.beta = asset.lm$coefficients[2]
    asset.alpha = asset.lm$coefficients[1]
    asset.r.squared = summary(asset.lm)$r.squared

    #x = ggplot(asset_and_bmark, aes(x = bmark, y = asset)) 
    #x = x + geom_point()
    #x = x + geom_abline(intercept = asset.alpha, slope = asset.beta)
    #x + ggtitle(paste(ticker, "R-squared =", format(asset.r.squared, digits = 2)))

    next_bmark_move = as.numeric(all_bmark_returns[test_date, 1])
    estimated_next_asset_move = asset.beta * next_bmark_move + asset.alpha
    actual_next_asset_move = as.numeric(all_stock_returns[test_date, ticker])

    df = data.frame(
        ticker = ticker,
        frequency = frequency,
        min_date = min_date,
        max_date = max_date,
        test_date = test_date,
        beta = asset.beta, 
        alpha = asset.alpha, 
        r_squared = asset.r.squared, 
        estimated_next_return = estimated_next_asset_move, 
        actual_next_return = actual_next_asset_move,
        return_error = estimated_next_asset_move - actual_next_asset_move
    )
    test_result = rbind(test_result, df)
}

rownames(test_result) = NULL

test_result2 = CAPM_Backtester("XJO", "Monthly", "AUD", as_date("2011-11-28"), as_date("2016-11-30"), as_date("2016-12-30"))

#write.zoo(asset1_returns, " C: / / Temp / / asset1_returns.csv ")
#write.zoo(bmark_returns, "C://Temp//bmark_returns.csv")
#write.zoo(factor_returns, "C://Temp//factor_returns.csv")