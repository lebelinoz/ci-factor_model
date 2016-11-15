library(RODBC) # <-- to grab SQL data from our database

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