###########################################################################
## MSCI AC World volatility in different currencies
###########################################################################

# Preamble
library(ggplot2)
library(xts)                    # <-- time series class used in our favourite quant packages
# library(quantmod)             # <-- for converting daily prices to weekly/monthly returns
library(RODBC)                  # <-- to grab SQL data from our database
library(lubridate)              # <-- because  R gets confused with dates, treating some as POSIXct and others not, and can't compare them.
library(roll)                   # <-- contains some rolling correlation, mean and standard deviation functions
library(PerformanceAnalytics)   # <-- has a potentially useful Return.cumulative function
library(zoo)                    # <-- I like giraffes
# source('Functions.R')         # <-- put all your functions in one place

index_code = 'MSCIWORLDG'
currencies = c("USD", "EUR", "JPY", "GBP", "AUD")
max_date = as.Date('2016-10-31')
min_date = as.Date('2011-10-31')  # <-- both years, 31 October fell on a Monday.

#####################################
## SQL SQL SQL SQL SQL SQL SQL SQL ##
# Retrieve the benchmark returns for the given currency
get_benchmark_xts_returns = function(benchmark_code, ccy) {
    conn <- odbcConnect(dsn="CISMPRDSVR")

    # Benchmark returns in different currencies
    sqlReturns = paste("EXEC PCI_REPORTING.dbo.get_benchmark_chart @benchmark = '", benchmark_code, "', @ccy = '", ccy, "', @months_horizon = 72, @frequency = 'Daily'", sep = "")
    raw_returns_data = sqlQuery(conn, sqlReturns)
    raw_returns_data[, "Date"] = as_date(raw_returns_data[, "Date"]) # <-- R gets confused by SQL dates.  Fix this.
    xts_returns = as.xts(raw_returns_data[, "Total Return"], order.by = raw_returns_data[, "Date"])
    colnames(xts_returns) = ccy

    odbcClose(conn)
    return(xts_returns)
}
## SQL SQL SQL SQL SQL SQL SQL SQL ##
#####################################

# Create a big xts crosstab of the benchmark returns in different currencies:
index_return = get_benchmark_xts_returns(index_code, "Local")
for (ccy in currencies) {
    index_return = merge(index_return, get_benchmark_xts_returns(index_code, ccy))
}

# Only keep the returns over the requested period (as determined by min_date and max_date, above)
index_return = index_return[which(index(index_return) > min_date & index(index_return) <= max_date),]

# Recreate normalised prices in an xts
index_price = xts(100, min_date)
for (i in 2:dim(index_return)[2]) {
    index_price = merge(index_price, xts(100, min_date))
}
colnames(index_price) = colnames(index_return)
for (i in 1:dim(index_return)[1]) {
    d = index(index_return)[i]
    singleton = zoo(100 * (1 + Return.cumulative(index_return[which(index(index_return) <= d),])), d)
    index_price = rbind(index_price, as.xts(singleton))
}

# Plot the prices just given
# ...first by creating a dataframe
####df = data.frame(date = index(index_price), price = coredata(index_price[, "Local"]), currency = rep("Local", dim(index_price)[1]))
####names(df) = c("date", "price", "currency")
####rownames(df) = NULL
####for (ccy in currencies) {
    ####df2 = data.frame(date = index(index_price), price = coredata(index_price[, ccy]), currency = rep(ccy, dim(index_price)[1]))
    ####names(df2) = c("date", "price", "currency")
    ####df = rbind(df, df2)
####}
df = flatten_xts(index_price, "price", "currency")

# ...next by ggplot on the dataframe
ggplot(df, aes(x = date, y = price, colour = currency)) + geom_line()

# Let's compute rolling standard deviations:
vol1y = roll_sd(index_return, width = 252)
vol1y = vol1y[which(!is.na(vol1y[, "Local"]))]
vol1y_df = flatten_xts(vol1y, "vol", "currency")
ggplot(vol1y_df, aes(x = date, y = vol, colour = currency)) + geom_line()

vol3y = roll_sd(index_return, width = 3 * 252)
vol3y = vol3y[which(!is.na(vol3y[, "Local"]))]
vol3y_df = flatten_xts(vol3y, "vol", "currency")
ggplot(vol3y_df, aes(x = date, y = vol, colour = currency)) + geom_line()

vol5y = roll_sd(index_return, width = 5 * 252)
vol5y = vol5y[which(!is.na(vol5y[, "Local"]))]
vol5y_df = flatten_xts(vol5y, "vol", "currency")
ggplot(vol5y_df, aes(x = date, y = vol, colour = currency)) + geom_line()


write.zoo(index_price, "C:/Temp/index_price.csv", sep = ",")
write.zoo(vol1y, "C:/Temp/vol1y.csv", sep = ",")
write.zoo(vol3y, "C:/Temp/vol3y.csv", sep = ",")
write.zoo(vol5y, "C:/Temp/vol5y.csv", sep = ",")