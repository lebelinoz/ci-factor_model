###########################################################################
## MSCI AC World volatility in different currencies
###########################################################################
# Preamble
library(ggplot2)
library(lubridate) # <-- because  R gets confused with dates, treating some as POSIXct and others not, and can't compare them.
library(roll) # <-- contains some rolling correlation, mean and standard deviation functions
library(scales) # <-- a ggplot2 thing for formatting axis
library(readxl) # <-- for getting data out of Excel
library(PerformanceAnalytics) # <-- has a potentially useful Return.cumulative function
source('Functions.R') # <-- put all your functions in one place
source('Sql_Wrapper.R') # <-- put all your functions in one place

index_code = 'MSCIWORLDG'
currencies = c("USD", "EUR", "JPY", "GBP", "AUD")
max_date = as.Date('2016-10-31')
min_date = as.Date('2000-12-31') # <-- both years, 31 October fell on a Monday.

#############################################################
## SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL ##
# Create a big xts crosstab of the benchmark returns in different currencies:
index_return = get_benchmark_xts_returns(index_code, "Local")
index_return_monthly = get_benchmark_xts_returns(index_code, "Local", "Monthly")
for (ccy in currencies) {
    index_return = merge(index_return, get_benchmark_xts_returns(index_code, ccy))
    index_return_monthly = merge(index_return_monthly, get_benchmark_xts_returns(index_code, ccy, "Monthly"))
}
## SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL ##
#############################################################

# Only keep the returns over the requested period (as determined by min_date and max_date, above)
index_return = index_return[which(index(index_return) > min_date & index(index_return) <= max_date),]
index_return_monthly = index_return_monthly[which(index(index_return_monthly) > min_date & index(index_return_monthly) <= max_date),]

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
df = flatten_xts(index_price, "price", "currency")

# ...next by ggplot on the dataframe
price_plot = ggplot(df, aes(x = date, y = price, colour = currency)) 
price_plot = price_plot + geom_line(size = 1) 
price_plot = price_plot + ggtitle("MSCI AC World - Total Returns by currency")
ggsave("C:/Temp/index_price.png")
write.zoo(index_price, "C:/Temp/index_price.csv")
write.zoo(index_return, "C:/Temp/index_return.csv")


# Create the volatility charts and output:
do_vol_outputs = function(return_xts, periods_per_year, years_to_roll, entity_name, title) {
    vol = roll_sd(return_xts, width = periods_per_year * years_to_roll)
    vol = vol[which(!is.na(vol[, 1]))]
    vol = vol * sqrt(periods_per_year) # Annualise
    vol_df = flatten_xts(vol, "vol", "currency")
    dev.set(which = 3)
    x = ggplot(vol_df, aes(x = date, y = vol, colour = currency))
    x = x + geom_line(size = 1)
    x = x + ggtitle(title)
    x = x + scale_y_continuous(breaks = c(NA, 0.1, 0.2, 0.3, 0.4, NA), labels = percent, limits = c(0, 0.5))
    ggsave(paste("C:/Temp/", entity_name, ".png", sep = ""))
    write.zoo(vol, paste("C:/Temp/", entity_name, ".csv", sep = ""))
    return(x)
}

# Create vol plots and save them as png files.  Also save raw data in CSV files:
vol1_chart = do_vol_outputs(index_return, 252, 1, "vol1y", "MSCI AC World - Rolling 1y standard deviation of returns by currency")
vol3_chart = do_vol_outputs(index_return, 252, 3, "vol3y", "MSCI AC World - Rolling 3y standard deviation of returns by currency")
vol5_chart = do_vol_outputs(index_return, 252, 5, "vol5y", "MSCI AC World - Rolling 5y standard deviation of returns by currency")
vol5m_chart = do_vol_outputs(index_return_monthly, 12, 5, "vol5y_monthly", "MSCI AC World - Rolling 5y standard deviation of (monthly) returns by currency")

# Output to the ide
dev.set(3) # Set the output environment to be the ide
price_plot
vol1_chart
vol3_chart
vol5_chart
vol5m_chart

##############################################################
## PCGLOB vs PCGLUF ## PCGLOB vs PCGLUF ## PCGLOB vs PCGLUF ##
excel_path = "M:/Staff Folders/Alain LeBel/GLUF  GLOB  - performance in AUD 20161121.xlsx" 

options(warn = -1) # <-- warnings off:  I don't care if there are some blank rows in the data
pc_daily = read_excel(excel_path, sheet = "Daily")
options(warn = 0) # <- warnings back on
pc_daily = pc_daily[which(!is.na(pc_daily[, "date"])),] # <-- because a few empty rows are cluttering up the data

pc_monthly = read_excel(excel_path, sheet = "Monthly")
pc_monthly = pc_monthly[which(!is.na(pc_monthly[, "PCGLUF"])),] # <-- let's only keep the rows where we have data for both PCGLUF and PCGLOB


pc_daily[, "date"] = as_date(pc_daily[, "date"]) # <-- R gets confused by .csv dates.  Fix this.
pc_monthly[, "date"] = as_date(pc_monthly[, "date"])

pc_daily_returns = as.xts(pc_daily[, "PCGLOB"], order.by = pc_daily[, "date"])
pc_daily_returns = merge(pc_daily_returns, as.xts(pc_daily[, "PCGLUF"], order.by = pc_daily[, "date"]))
colnames(pc_daily_returns) = c("GLOB", "GLUF")

pc_monthly_returns = as.xts(pc_monthly[, "PCGLOB"], order.by = pc_monthly[, "date"])
pc_monthly_returns = merge(pc_monthly_returns, as.xts(pc_monthly[, "PCGLUF"], order.by = pc_monthly[, "date"]))
colnames(pc_monthly_returns) = c("GLOB", "GLUF")

# Output chart (similar to above but with slightly different axis scale
vol = roll_sd(pc_daily_returns, width = 252)
vol = vol[which(!is.na(vol[, "GLOB"]))]
vol = vol * sqrt(252) # Annualise
vol_df = flatten_xts(vol, "vol", "portfolio")
pc_daily_chart = ggplot(vol_df, aes(x = date, y = vol, colour = portfolio))
pc_daily_chart = pc_daily_chart + geom_line(size = 1)
pc_daily_chart = pc_daily_chart + ggtitle("GLOB & GLUF - Rolling 1y standard deviation of daily returns")
pc_daily_chart = pc_daily_chart + scale_y_continuous(labels = percent, limits = c(0.08, 0.16))
dev.set(3)
print(pc_daily_chart)
ggsave(paste("C:/Temp/pc_daily.png", sep = ""))
write.zoo(vol, paste("C:/Temp/pc_daily.csv", sep = ""))


volm = roll_sd(pc_monthly_returns, width = 12 * 5)
volm = volm[which(!is.na(volm[, "GLOB"]))]
volm = volm * sqrt(12) # Annualise
vol_df = flatten_xts(volm, "volm", "portfolio")
pc_monthly_chart = ggplot(vol_df, aes(x = date, y = volm, colour = portfolio))
pc_monthly_chart = pc_monthly_chart + geom_line(size = 1)
pc_monthly_chart = pc_monthly_chart + ggtitle("GLOB & GLUF - Rolling 5y standard deviation of monthly returns")
pc_monthly_chart = pc_monthly_chart + scale_y_continuous(labels = percent, limits = c(0.08, 0.16))
dev.set(3)
print(pc_monthly_chart)
ggsave(paste("C:/Temp/pc_monthly.png", sep = ""))
write.zoo(volm, paste("C:/Temp/pc_monthly.csv", sep = ""))
