library(RODBC) # <-- to grab SQL data from our database
library(lubridate) # <-- because R gets confused with dates, treating some as POSIXct and others not, and can't compare them.
source('Functions.R')

get_table_from_sql_CISMPRDSVR = function(sql) {
    conn <- odbcConnect(dsn = "CISMPRDSVR")
    sql_data = sqlQuery(conn, sql)
    odbcClose(conn)
    return(sql_data)
}

date_to_sql_string = function(date, endpoints = "'") {
    return(paste(endpoints, format(date, "%Y-%m-%d"), endpoints, sep=""))
}
# date_to_sql_string(as.Date("01/01/1980", "%d/%m/%Y"))

# Retrieve the monthly returns for the given spc portfolio
get_spc_xts_returns = function(spc_id) {
    sqlReturns = paste("SELECT period_end AS [Date], port_return AS [Total Return] FROM PCI_REPORTING.dbo.t_spc_data_performance WHERE spc_id = ", spc_id, sep = "")
    raw_returns_data = get_table_from_sql_CISMPRDSVR(sqlReturns)
    raw_returns_data[, "Date"] = as_date(raw_returns_data[, "Date"]) # <-- R gets confused by SQL dates.  Fix this.
    xts_returns = as.xts(raw_returns_data[, "Total Return"], order.by = raw_returns_data[, "Date"])
    colnames(xts_returns) = spc_id
    return(xts_returns)
}

# Retrieve the daily returns for the given spc portfolio
get_spc_xts_raw_total_returns = function(spc_id, currency = "AUD", frequency = "daily") {
    frequency = tolower(frequency)
    sqlReturns = paste("EXEC PCI_REPORTING.dbo.calc_spc_", frequency, "_returns @spc_id = ", spc_id, ", @currency = '", currency, "'", sep = "")
    raw_returns_data = get_table_from_sql_CISMPRDSVR(sqlReturns)
    raw_returns_data[, "date"] = as_date(raw_returns_data[, "date"]) # <-- R gets confused by SQL dates.  Fix this.
    xts_returns = as.xts(raw_returns_data[, "daily_return"], order.by = raw_returns_data[, "date"])
    colnames(xts_returns) = spc_id
    return(xts_returns)
}

# Retrieve the benchmark returns for the given currency
get_benchmark_xts_returns = function(benchmark_code, ccy = "Local", frequency = "Daily") {
    sqlReturns = paste("EXEC PCI_REPORTING.dbo.get_benchmark_chart @benchmark = '", benchmark_code, "', @ccy = '", ccy, "', @frequency = '", frequency, "'", sep = "")
    raw_returns_data = get_table_from_sql_CISMPRDSVR(sqlReturns)
    raw_returns_data[, "Date"] = as_date(raw_returns_data[, "Date"]) # <-- R gets confused by SQL dates.  Fix this.
    xts_returns = as.xts(raw_returns_data[, "Total Return"], order.by = raw_returns_data[, "Date"])
    colnames(xts_returns) = ccy
    xts_returns = xts_returns[which(!is.na(xts_returns)),] # <-- Drop all the NA entries
    return(xts_returns)
}

get_ticker_xts_return_index = function(ticker_list, currency = "Local", min_date = NULL, debug = 0) {
    if (is.null(min_date)) {
        min_date = as.Date("01/01/1970", "%d/%m/%Y")
    } else {
        min_date = as_date(min_date)
    }
    
    string_ticker_list = paste("'", paste(ticker_list, collapse = "','"), "'", sep = "")
    sql = "SELECT sec.sec_ticker AS [Ticker], eps.metric_date AS [Date], eps.metric_value AS [Total_Return] FROM PCI_REPORTING.dbo.t_data_fs_eps eps INNER JOIN PCI_CORE.dbo.t_Ref_Sec sec ON eps.sec_id = sec.sec_id WHERE sec.sec_ticker IN ("
    sql = paste(sql, string_ticker_list, ") AND fs_code = 'FG_RETURN_ICS_", toupper(currency), "'", sep = "")
    sql = paste(sql, "AND eps.metric_date >=", date_to_sql_string(min_date))
    if (debug == 1) { print(sql) }

    raw_data = get_table_from_sql_CISMPRDSVR(sql)
    raw_data[, "Date"] = as_date(raw_data[, "Date"]) # <-- R gets confused by SQL dates.  Fix this
    if (debug == 1) { print(paste("raw_data number of rows = ", nrow(raw_data))) }
    return(df_to_xts(raw_data, metric_name = "Total_Return", debug = debug))
}
