###########################################################################
## BOND-LIKE EQUITIES IN BRUNSWICK                                       ##
##  The goal is to study the correlations between the returns of stocks  ##
##  in the Brunswick fund vs bond indices (AU & US), in the hopes of     ##
##  confirming or changing the current value subset classification.      ##
##                                                                       ##
##  Use S&P indices for the government bonds.                            ##
##  FactSet codes are SPBDAGVT-CME for Aus and SPBDUSB0 for US.          ##
##                                                                       ##
##  AL, 19 October 2016                                                  ##
###########################################################################

# Preamble
library(xts) # <-- time series class used in our favourite quant packages
library(quantmod) # <-- for converting daily prices to weekly/monthly returns
library(RODBC) # <-- to grab SQL data from our database
library(reshape) # <-- handy tools for reshaping raw data into crosstabs
library(PerformanceAnalytics) # <-- has a lovely rolling correlation chart
fund_code = 'PCBRAE' # <-- the code for our chosen portfolio

# Collect AU bond returns:
raw_data = read.csv("BOND-AU-Total Return.csv")
raw_data_frame = data.frame(Date = as.Date(raw_data[, 1], format("%d/%m/%y")), Index = raw_data[, 2])
bond_au_index = as.xts(raw_data_frame[, 2], order.by = raw_data_frame[, 1])
bond_au_weekly_return = weeklyReturn(bond_au_index, leading = FALSE)
bond_au_monthly_return = monthlyReturn(bond_au_index, leading = FALSE)

# Collect US bond returns
raw_data = read.csv("BOND-US-Total Return.csv")
raw_data_frame = data.frame(Date = as.Date(raw_data[, 1], format("%d/%m/%Y")), Index = raw_data[, 2])
bond_us_index = as.xts(raw_data_frame[, 2], order.by = raw_data_frame[, 1])
bond_us_weekly_return = weeklyReturn(bond_us_index, leading = FALSE)
bond_us_monthly_return = monthlyReturn(bond_us_index, leading = FALSE)


#####################################
## SQL SQL SQL SQL SQL SQL SQL SQL ##
conn <- odbcConnect(dsn = "CISMPRDSVR")

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
raw_stock_data = sqlQuery(conn, sqlStocks)

# AUDUSD conversion rate
sqlFX = "SELECT fx_date AS [Date], 1/fx_value AS [AUDUSD] FROM t_data_fx WHERE fx_code = 'USDAUD'"
raw_FX = sqlQuery(conn, sqlFX)

odbcClose(conn)
## SQL SQL SQL SQL SQL SQL SQL SQL ##
#####################################

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

# Now create a USD equivalent time series
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

# Cleanup:
remove(bond_au_index)
remove(bond_us_index)
remove(crosstab_stock_data)
remove(fx_rates)
remove(fx_rates_all)
remove(raw_data)
remove(raw_data_frame)
remove(xts_stock_data_usd)
remove(xts_stock_data_aud)
remove(conn)
remove(sqlFX)
remove(sqlStocks)

##################
## CALCULATIONS ##
##################

cor_aud_monthly = cor(monthly_returns_aud, bond_au_monthly_return)
colnames(cor_aud_monthly) = c("AU.monthly")
cor_usd_monthly = cor(monthly_returns_usd, bond_us_monthly_return)
colnames(cor_usd_monthly) = c("US.monthly")
cor_aud_weekly = cor(weekly_returns_aud, bond_au_weekly_return)
colnames(cor_aud_weekly) = c("AU.weekly")
cor_usd_weekly = cor(weekly_returns_usd, bond_us_weekly_return)
colnames(cor_usd_weekly) = c("US.weekly")

df = merge(cor_aud_monthly, cor_aud_weekly, by = "row.names")
rownames(df) = df[, 1] # there must be a better way...
df = df[, -1]

df = merge(df, cor_usd_monthly, by = "row.names")
rownames(df) = df[, 1]
df = df[, -1]

df = merge(df, cor_usd_weekly, by = "row.names")
rownames(df) = df[, 1]
df = df[, -1]

write.csv(df, paste("Bond-like equities in ", fund_code, ".csv", sep = ""))
