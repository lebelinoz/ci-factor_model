###########################################################################
## spc returns
###########################################################################

# Preamble
library(ggplot2)
library(xts) # <-- time series class used in our favourite quant packages
# library(quantmod)             # <-- for converting daily prices to weekly/monthly returns
library(RODBC) # <-- to grab SQL data from our database
library(lubridate) # <-- because  R gets confused with dates, treating some as POSIXct and others not, and can't compare them.
library(roll) # <-- contains some rolling correlation, mean and standard deviation functions
library(PerformanceAnalytics) # <-- has a potentially useful Return.cumulative function
library(zoo) # <-- I like giraffes
library(directlabels) # <-- for adding labels to the chart
source('Functions.R') # <-- put all your functions in one place

max_date = as.Date('2016-09-30')
min_date = as.Date('2013-09-30')
spc_id_list = c(82, 83, 84, 85, 86, 87, 90)
pfolio_names = c("Stalwarts", "Turnarounds", "Growth", "Cyclicals", "BLE", "Assets", "All")

#####################################
## SQL SQL SQL SQL SQL SQL SQL SQL ##
# Retrieve the benchmark returns for the given currency
get_spc_xts_returns = function(spc_id) {
    conn <- odbcConnect(dsn = "CISMPRDSVR")

    # Benchmark returns in different currencies
    sqlReturns = paste("SELECT period_end AS [Date], port_return AS [Total Return] FROM PCI_REPORTING.dbo.t_spc_data_performance WHERE spc_id = ", spc_id, sep = "")
    raw_returns_data = sqlQuery(conn, sqlReturns)
    raw_returns_data[, "Date"] = as_date(raw_returns_data[, "Date"]) # <-- R gets confused by SQL dates.  Fix this.
    xts_returns = as.xts(raw_returns_data[, "Total Return"], order.by = raw_returns_data[, "Date"])
    colnames(xts_returns) = spc_id

    odbcClose(conn)
    return(xts_returns)
}
## SQL SQL SQL SQL SQL SQL SQL SQL ##
#####################################

# Create a big xts crosstab of the benchmark returns in different currencies:
pfolio_return = get_spc_xts_returns(spc_id_list[1])
for (spc_id in spc_id_list[-1]) {
    pfolio_return = merge(pfolio_return, get_spc_xts_returns(spc_id))
}

# Only keep the returns over the requested period (as determined by min_date and max_date, above)
pfolio_return = pfolio_return[which(index(pfolio_return) > min_date & index(pfolio_return) <= max_date),]

# Recreate normalised prices in an xts
pfolio_price = xts(100, min_date)
for (i in 2:dim(pfolio_return)[2]) {
    pfolio_price = merge(pfolio_price, xts(100, min_date))
}
colnames(pfolio_price) = colnames(pfolio_return)
for (i in 1:dim(pfolio_return)[1]) {
    d = index(pfolio_return)[i]
    singleton = zoo(100 * (1 + Return.cumulative(pfolio_return[which(index(pfolio_return) <= d),])), d)
    pfolio_price = rbind(pfolio_price, as.xts(singleton))
}

colnames(pfolio_price) = pfolio_names

# Plot the prices just given
# ...first by creating a dataframe
df = flatten_xts(pfolio_price, "price", "spc_id")

# ...next by ggplot on the dataframe
ggplot(df, aes(x = date, y = price, colour = spc_id)) + geom_line() + geom_dl(aes(label = spc_id), method = list(dl.combine("last.points"), cex = 0.8))

tail(pfolio_price, 1)


