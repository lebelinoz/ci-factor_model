library(RODBC) # <-- to grab SQL data from our database

# Retrieve the monthly returns for the given spc portfolio
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

# Retrieve the benchmark returns for the given currency
get_benchmark_xts_returns = function(benchmark_code, ccy = "Local", frequency = "Daily") {
    conn <- odbcConnect(dsn = "CISMPRDSVR")

    # Benchmark returns in different currencies
    sqlReturns = paste("EXEC PCI_REPORTING.dbo.get_benchmark_chart @benchmark = '", benchmark_code, "', @ccy = '", ccy, "', @frequency = '", frequency, "'", sep = "")
    raw_returns_data = sqlQuery(conn, sqlReturns)
    raw_returns_data[, "Date"] = as_date(raw_returns_data[, "Date"]) # <-- R gets confused by SQL dates.  Fix this.
    xts_returns = as.xts(raw_returns_data[, "Total Return"], order.by = raw_returns_data[, "Date"])
    colnames(xts_returns) = ccy

    # Drop all the NA entries
    xts_returns = xts_returns[which(!is.na(xts_returns)),]

    odbcClose(conn)
    return(xts_returns)
}