###########################################################################
## spc returns
###########################################################################

# Preamble
library(ggplot2)
library(xts) # <-- time series class used in our favourite quant packages
# library(quantmod)             # <-- for converting daily prices to weekly/monthly returns
library(lubridate) # <-- because  R gets confused with dates, treating some as POSIXct and others not, and can't compare them.
library(roll) # <-- contains some rolling correlation, mean and standard deviation functions
library(PerformanceAnalytics) # <-- has a potentially useful Return.cumulative function
library(zoo) # <-- I like giraffes
library(directlabels) # <-- for adding labels to the chart
source('Functions.R') # <-- put all your functions in one place
source('Sql_Wrapper.R')

max_date = as.Date('2016-09-30')
min_date = as.Date('2008-08-31')
spc_id_list = c(71, 79, 67, 80, 74, 81, 90)
pfolio_names = c("FinQ", "FinQ_Div", "QV_EvEbit", "QV_Ev_DIV", "QV_PB", "QV_PB_Div", "All")

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
