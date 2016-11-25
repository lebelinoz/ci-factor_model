library(RODBC) # <-- to grab SQL data from our database
library(lubridate) # <-- because R gets confused with dates, treating some as POSIXct and others not, and can't compare them.

get_table_from_sql_CISMPRDSVR = function(sql) {
    conn <- odbcConnect(dsn = "CISMPRDSVR")
    sql_data = sqlQuery(conn, sql)
    odbcClose(conn)
    return(sql_data)
}

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
get_spc_xts_returns_daily = function(spc_id) {
    sqlReturns = paste("EXEC PCI_REPORTING.dbo.calc_spc_daily_returns @spc_id = ", spc_id, sep = "")
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