###########################################################################
##                                                                       ##
##  The goal is to study the correlations between the returns of stocks  ##
##  in the Global fund vs bond indices (AU & US).                        ##
##                                                                       ##
##  Use S&P indices for the government bonds.                            ##
##  FactSet codes are SPBDAGVT-CME for Aus and SPBDUSB0 for US.          ##
##                                                                       ##
##  AL, 17 January 2017                                                  ##
###########################################################################

# Preamble
library(xts) # <-- time series class used in our favourite quant packages
library(quantmod) # <-- for converting daily prices to weekly/monthly returns
library(reshape) # <-- handy tools for reshaping raw data into crosstabs
library(PerformanceAnalytics) # <-- has a lovely rolling correlation chart
source('Sql_Wrapper.R')
fund_code = 'PCGLUF' # <-- the code for our chosen portfolio

# Collect AU bond returns:
#raw_data = get_table_from_sql_CISMPRDSVR("SELECT metric_date, ")
#raw_data_frame = data.frame(Date = as.Date(raw_data[, 1], format("%d/%m/%y")), Index = raw_data[, 2])
bond_au_index = get_ticker_xts_return_index('SPBDAGVT.ASX') # as.xts(raw_data_frame[, 2], order.by = raw_data_frame[, 1])
bond_au_weekly_return = weeklyReturn(bond_au_index, leading = FALSE)
bond_au_monthly_return = monthlyReturn(bond_au_index, leading = FALSE)

# Collect US bond returns
#raw_data = read.csv("BOND-US-Total Return.csv")
#raw_data_frame = data.frame(Date = as.Date(raw_data[, 1], format("%d/%m/%Y")), Index = raw_data[, 2])
bond_us_index = get_ticker_xts_return_index('SPBDUSB0.US') # as.xts(raw_data_frame[, 2], order.by = raw_data_frame[, 1])
bond_us_weekly_return = weeklyReturn(bond_us_index, leading = FALSE)
bond_us_monthly_return = monthlyReturn(bond_us_index, leading = FALSE)

# Portfolio stocks' daily total return indices in AUD.  Only go back to Feb 2012 because that's when AUD Bond prices go back.
sqlStocks = paste(
    "SELECT 'ASX 200' AS [ticker], price_date AS [date], closeprice AS [total_return] FROM PCI_CORE.dbo.t_Data_Price WHERE sec_id = dbo.GetSecId('XJOAI.ASX') AND price_date >= '2012-02-27'",
    " UNION ",
    "SELECT sec.sec_ticker AS [ticker], caps.metric_date AS [date], caps.metric_value AS [total_return] ",
    "FROM PCI_CORE.dbo.t_Ref_Portfolio ref ",
    "INNER JOIN PCI_CORE.dbo.t_Ref_Positions_L pos ON ref.id = pos.portfolio_id ",
    "INNER JOIN PCI_CORE.dbo.t_Ref_Sec sec ON pos.sec_id = sec.sec_id ",
    "INNER JOIN PCI_REPORTING.dbo.t_data_fs_eps caps ON sec.sec_id = caps.sec_id ",
    "WHERE caps.fs_code = 'FG_RETURN_ICS_AUD' AND caps.metric_date >= '2012-02-27' ",
    "AND ref.fund_code = ",
    "'", fund_code, "'",
    sep = ""
)
raw_stock_data = get_table_from_sql_CISMPRDSVR(sqlStocks) # sqlQuery(conn, sqlStocks)
raw_stock_data$date = as_date(raw_stock_data$date)

# AUDUSD conversion rate
sqlFX = "SELECT fx_date AS [date], 1/fx_value AS [AUDUSD] FROM t_data_fx WHERE fx_code = 'USDAUD'"
raw_FX = get_table_from_sql_CISMPRDSVR(sqlFX)
raw_FX$date = as_date(raw_FX$date)

#####################################
## RESHAPE RESHAPE RESHAPE RESHAPE ##

# Convert the flat data into some time series objects
# (use cast from the reshape package...  xts column names will need to be explicitly renamed)
crosstab_stock_data = cast(raw_stock_data, date ~ ticker, value = "total_return")
xts_stock_data_aud = as.xts(crosstab_stock_data[, -1], order.by = crosstab_stock_data[, 1])
colnames(xts_stock_data_aud) = names(crosstab_stock_data[, -1]) # <-- make the column names tickers
xts_stock_data_aud = xts_stock_data_aud[1:(nrow(xts_stock_data_aud) - 1),] # <-- drop last row because data might be incomplete


# Convert raw FX to an xts format:
fx_rates_all = as.xts(raw_FX[, -1], order.by = raw_FX[, 1])
fx_rates = fx_rates_all[which(index(fx_rates_all) %in% index(xts_stock_data_aud))]

# Now create a USD equivalent time series:
xts_stock_data_usd = xts(apply(xts_stock_data_aud, 2, function(col) col * fx_rates), index(xts_stock_data_aud))

# Now we can compute weekly and monthly returns (I need to use a function, I think)
xts_returns = function(frequency, my_xts) {
    temp = xts(order.by = index(my_xts))

    for (i in 1:ncol(my_xts)) {
        if (frequency == "weekly") {
            temp2 = weeklyReturn(my_xts[, i], leading = FALSE)
        } else {
            temp2 = monthlyReturn(my_xts[, i], leading = FALSE)
        }
        names(temp2) = names(my_xts[, i])
        temp = merge(temp, temp2)
    }
    temp = temp[rowSums(is.na(temp)) != ncol(temp)] # <-- remove rows which are all NA
    return(temp)
}
weekly_returns_aud = xts_returns("weekly", xts_stock_data_aud)
weekly_returns_usd = xts_returns("weekly", xts_stock_data_usd)
monthly_returns_aud = xts_returns("monthly", xts_stock_data_aud)
monthly_returns_usd = xts_returns("monthly", xts_stock_data_usd)

# Now grab the minimum dates, and lop off bond return data from before that date
weekly_min_date = min(index(weekly_returns_aud))
monthly_min_date = min(index(monthly_returns_aud))

bond_au_weekly_return = bond_au_weekly_return[which(difftime(index(bond_au_weekly_return), weekly_min_date) >= 0),]
bond_us_weekly_return = bond_us_weekly_return[which(difftime(index(bond_us_weekly_return), weekly_min_date) >= 0),]
bond_au_monthly_return = bond_au_monthly_return[which(difftime(index(bond_au_monthly_return), monthly_min_date) >= 0),]
bond_us_monthly_return = bond_us_monthly_return[which(difftime(index(bond_us_monthly_return), monthly_min_date) >= 0),]

## RESHAPE RESHAPE RESHAPE RESHAPE ##
#####################################

##################
## CALCULATIONS ##
##################

#cor_aud_monthly = cor(monthly_returns_aud, bond_au_monthly_return)
#colnames(cor_aud_monthly) = c("AU.monthly")

cor_usd_monthly = cor(monthly_returns_usd, bond_us_monthly_return)
colnames(cor_usd_monthly) = c("US.monthly")
write.zoo(cor_usd_monthly, paste("C://Temp//", "USD monthly correlations to US Bond Index - ", fund_code, ".csv", sep = ""), row.names = TRUE)

#cor_aud_weekly = cor(weekly_returns_aud, bond_au_weekly_return)
#colnames(cor_aud_weekly) = c("AU.weekly")
#cor_usd_weekly = cor(weekly_returns_usd, bond_us_weekly_return)
#colnames(cor_usd_weekly) = c("US.weekly")

#df = merge(cor_aud_monthly, cor_aud_weekly, by = "row.names")
#rownames(df) = df[, 1] # there must be a better way...
#df = df[, -1]

#df = merge(df, cor_usd_monthly, by = "row.names")
#rownames(df) = df[, 1]
#df = df[, -1]

#df = merge(df, cor_usd_weekly, by = "row.names")
#rownames(df) = df[, 1]
#df = df[, -1]

#write.csv(df, paste("C://Temp//", "Bond-like equities in ", fund_code, ".csv", sep = ""))

#bond_us_monthly_return

###########################################################################
### How are global portfolios (GLOB & GLUF) correlated to US Bond Index? ##
###########################################################################

## How to format upon importing according to http://stackoverflow.com/questions/13022299/specify-custom-date-format-for-colclasses-argument-in-read-table-read-csv
#setClass('myDate')
#setAs("character", "myDate", function(from) as_date(as.Date(from, format = "%d/%m/%Y")))
#raw_data = read.csv("C://Temp//PCGLOB.csv", colClasses = c('myDate', 'numeric'))
## raw_data[["Business_Date"]] = lapply(raw_data$Date, previous_business_date_if_weekend) # Note that I can't figure out how to add a "previous business day" column, so I've fudged the data.

#pcglob_index = as.xts(raw_data[, 2], order.by = raw_data[, 1])
#colnames(pcglob_index) = c("PCGLOB")

#pcglob_index = pcglob_index["2012-02-27/"]

#pcglob_index_in_USD = pcglob_index * fx_rates
#head(pcglob_index)
#head(pcglob_index_in_USD)

#pcglob_index_in_USD_returns = monthlyReturn(pcglob_index_in_USD)

#cor(pcglob_index_in_USD_returns, bond_us_monthly_return)

## Check using Excel
#write.zoo(pcglob_index_in_USD_returns, "C://Temp//pcglob_index_in_USD_returns.csv")
#write.zoo(bond_us_monthly_return, "C://Temp//bond_us_monthly_return.csv")

## Conclusion:  the 5-year monthly returns of PCGLOB (in USD) have a -6% (-0.06) correlation 
##              with the US Treasury Bond Index as of 31 Dec 2017.

## Now try PCGLUF (in local currency) vs the same index
#raw_data = read.csv("C://Temp//PCGLUF.csv", colClasses = c('myDate', 'numeric'))
#pcgluf_index = as.xts(raw_data[, 2], order.by = raw_data[, 1])
#colnames(pcgluf_index) = c("PCGLUF")

#pcgluf_index = pcgluf_index["2012-02-27/"]

#head(pcgluf_index)

#pcgluf_index_returns = monthlyReturn(pcgluf_index)

#x_pcgluf_index_returns = pcgluf_index_returns["2012-03-30:2017-12-30"]
#x_bond_us_monthly_return = bond_us_monthly_return["2012-03-30:2017-12-30"]

#cor(x_pcgluf_index_returns, x_bond_us_monthly_return, use = "complete.obs", method = "pearson")

## Check using Excel
#write.zoo(x_pcgluf_index_returns, "C://Temp//pcgluf_index_returns.csv")
#write.zoo(x_bond_us_monthly_return, "C://Temp//bond_us_monthly_return.csv")

#?cor