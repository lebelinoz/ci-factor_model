library(RODBC) # <-- to grab SQL data from our database
library(lubridate) # <-- because R gets confused with dates, treating some as POSIXct and others not, and can't compare them.
source('./lib/Functions.R')

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

get_ticker_xts_t_data_fs_eps = function(ticker, metric) {
    sql = "SELECT metric_date AS [date], metric_value FROM PCI_REPORTING.dbo.t_data_fs_eps WHERE sec_id = dbo.GetSecId('"
    sql = paste(sql, ticker, "') AND fs_code = '", metric, "' ORDER BY metric_date", sep = "")

    df = get_table_from_sql_CISMPRDSVR(sql)
    return_xts = xts(df[, 2], order.by = as_date(df[, 1]))
    return(return_xts)
}


get_watchlist = function(watchlist_name, snapshot_date, frequency = 'M') {
    if (missing(snapshot_date)) snapshot_date = today()

    sql_watchlist = paste(
          "SELECT wh.sec_id, sec.sec_ticker AS [ticker], sec.sec_name AS [name] FROM PCI_CORE.dbo.watchlist_history_cleansed('M') wh INNER JOIN PCI_CORE.dbo.t_Ref_Sec sec ON wh.sec_id = sec.sec_id WHERE wh.period_start <= DATEFROMPARTS(YEAR('"
        , snapshot_date
        , "'), MONTH('"
        , snapshot_date
        , "'), 1) AND wh.period_end > '"
        , snapshot_date
        , "' AND wh.Watchlist = '"
        , watchlist_name
        , "' AND wh.research_level = 1"
        , sep = "")
    watchlist = get_table_from_sql_CISMPRDSVR(sql_watchlist)

    # Assume equal weighting:
    if (nrow(watchlist) > 0) watchlist = mutate(watchlist, weight = 1 / nrow(watchlist))

    return(watchlist)
}

# The portfolio will be a dataframe with columns sec_id, ticker, name and weight.
# Note that, due to Charles River glitchiness, sum(portfolio$weight) sometimes add to something like 1.9 (instead of 0.95).  Remember to always
# divide final weighted-sum answers by the total weights.
get_portfolio = function(portfolio_code, snapshot_date) {
    if (missing(snapshot_date)) snapshot_date = today()

    sql_pfolio = paste(
         "SELECT sec.sec_id, REPLACE(sec.sec_ticker,'.ASX','') AS [ticker], sec.sec_name as [name], pfolio.[weight] FROM PCI_REPORTING.dbo.t_data_historic_port_weights pfolio INNER JOIN PCI_CORE.dbo.t_Ref_Sec sec ON pfolio.sec_id = sec.sec_id WHERE pfolio.acct_cd = '"
        , portfolio_code
        , "' AND pfolio.metric_date IN (SELECT MAX(metric_date) FROM PCI_REPORTING.dbo.t_data_historic_port_weights WHERE acct_cd = '"
        , portfolio_code
        , "' AND sec_id IS NOT NULL AND metric_date <= '"
        , snapshot_date
        , "')"
        , sep = "")
    portfolio = get_table_from_sql_CISMPRDSVR(sql_pfolio)

    return(portfolio)
}

get_index_snapshot = function(bmark_code) {
    sql_bmark = paste(
    "SELECT sec.sec_ticker, sec.sec_id, sec.sec_exchange, vs.value_subset, sec.sec_gics_code, gics.sector_name AS [sector], "
    , " gics.ind_group_name AS[industry_group], msci.index_weight FROM dbo.t_Data_Index_L msci "
    , "INNER JOIN dbo.t_Ref_Sec sec ON msci.sec_id = sec.sec_id "
    , "LEFT OUTER JOIN dbo.vValueSubsets vs ON sec.sec_id = vs.sec_id "
    , "LEFT OUTER JOIN dbo.t_Ref_GICS_Map gics ON sec.sec_gics_code = gics.sub_ind_code "
    , "WHERE msci.index_code = '"
    , bmark_code
    , "' ORDER BY vs.value_subset"
    , sep = "")

    bmark = get_table_from_sql_CISMPRDSVR(sql_bmark)

    return(bmark)
}