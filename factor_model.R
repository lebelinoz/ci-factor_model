###################################
###                             ###
### COMPUTE A FACTOR MODEL      ###
###                             ###
###################################

### PREAMBLE
source('Sql_Wrapper.R') # <-- for grabbing data straight from our SQL database
source('StockPlot.R')   # <-- for plotting relative return charts

# Parameters:
benchark_code = 'XJO'
frequency = "Weekly"
currency = "AUD"
min_date = as.Date("2016-01-20") # as.Date("2011-12-30") # as.Date("2015-09-29") # 
max_date = today()

###########################
###   EQUITY RETURNS    ###

### STEP 1: GET A LIST OF sec_id's (this is the part which will change every time)
sql_sec_id = paste("SELECT DISTINCT sec_id FROM dbo.t_Data_Index_L WHERE index_code  = '",
                   benchark_code,
                   "'",
                   sep = "")
secIdList <- get_table_from_sql_CISMPRDSVR(sql_sec_id)$sec_id
# secIdList <- c(secIdList, 123456) # For fun, let's add the secId of the stock we're thinking of buying


### STEP 2: GET RETURN DATA FROM THE DATABASE AT THE DESIRED FREQUENCY
# Make a sql statement which looks like this:  EXEC PCI_REPORTING.dbo.get_performance_chart @sec_id_list = '1676,2604' , @currency='AUD', @frequency='Weekly'frequency <- "Weekly"
# TO DO:  Build a wrapper which allows for latest or historic index constituents snapshots (just ASX200 & MSCIWORLD ok for now)
secIdList_AsString <- paste(secIdList, collapse = ",")
sql_returns = paste("EXEC PCI_REPORTING.dbo.get_performance_chart @sec_id_list = '",
                    secIdList_AsString,
                    "', @currency='",
                    currency,
                    "', @frequency='",
                    frequency,
                    "'",
                    sep = "")
stock_returns_df <- get_table_from_sql_CISMPRDSVR(sql_returns)

# There is a "missing" column in stock_returns_df which we can ignore
stock_returns_df[, "missing"] <- list(NULL)

# Keep only dates between max_date and min_date
stock_returns_df = stock_returns_df[which(as_date(stock_returns_df$Date) >= as_date(min_date) & as_date(stock_returns_df$Date) <= as_date(max_date)),]

# Rename the columns to be the ticker, not the sec_id:
sql_column_names = paste("SELECT sec_id, REPLACE(sec_ticker,'.ASX','') AS [ticker] from PCI_CORE.dbo.t_Ref_Sec WHERE sec_id in (", secIdList_AsString, ")", sep = "")
column_names <- get_table_from_sql_CISMPRDSVR(sql_column_names)
for (i in 1:length(column_names[, 1])) {
    pair = column_names[i,]
    colnames(stock_returns_df)[which(colnames(stock_returns_df) == pair[, 1])] = toString(pair[, 2])
}

# Drop all columns where there is only data for less than 50% of the data 
# (eg. for 52-week correlations, stocks which IPO'd six months ago will look off-the-charts)
stock_returns_df = stock_returns_df[, colSums(is.na(stock_returns_df)) < length(stock_returns_df[, 1]) / 2]

# Drop T-1 row (remember that SQL's get_performance_chart always tacks on a T-1 data point which may not reflect a full period)
stock_returns_df = stock_returns_df[1:(length(stock_returns_df[, 1]) - 1),]
max_date = as_date(max(stock_returns_df$Date))

# Convert to xts
stock_returns = xts(stock_returns_df[, -1], stock_returns_df[, 1])

###   EQUITY RETURNS    ###
###########################


###########################
###  BENCHMARK RETURNS  ###

all_bmark_returns = get_benchmark_xts_returns(benchark_code, currency, frequency)
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

# TO DO:  Regression (or whatever) to compute beta and the error term (which will be alpha)

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

ticker = "Bond Index"
asset_returns = factor_returns # stock_returns[, ticker]
asset_and_bmark = data.frame(index(asset_returns), asset_returns[, 1], bmark_returns[, 1], row.names = NULL)
colnames(asset_and_bmark) = c("date", "asset", "bmark")

asset.lm = lm(asset ~ bmark, data = asset_and_bmark)
x = ggplot(asset_and_bmark, aes(x = bmark, y = asset)) 
x = x + geom_point()
x = x + geom_abline(intercept = asset.lm$coefficients[1], slope = asset.lm$coefficients[2])
x + ggtitle(paste(ticker, "R-squared =", format(summary(asset.lm)$r.squared, digits = 2)))

#write.zoo(asset1_returns, " C: / / Temp / / asset1_returns.csv ")
#write.zoo(bmark_returns, "C://Temp//bmark_returns.csv")
#write.zoo(factor_returns, "C://Temp//factor_returns.csv")


