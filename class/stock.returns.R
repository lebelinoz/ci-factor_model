######################################
# A stock.returns object is an xts time series of returns of one or more stocks, coupled with a timeframe.
# Each column's name corresponds to the ticker as it appears in PCI_CORE.dbo.t_Ref_Sec, with the '.ASX' exchange omitted where applicable.
# The frequency, start date and end date of the timeframe ought to correspond to the xts's index.
#
# Initialize functions will be supplied so users can create stock.returns objects directly from benchmark codes, portfolio codes and sec_id lists
# Where the user supplies more than one parameter (i.e. a benchmark code AND a portfolio code), the lists of securities will be combined.
# "all" is a valid portfolio code which means "all stocks currently held by sec_id".
######################################

require(lubridate)
source('./lib/Sql_Wrapper.R')
source('./class/timeframe.R')

# This is what I call the base constructor
stock.returns <- setClass(
    Class = "stock.returns",
    slots = c(xts_returns = "xts", timeframe = "timeframe", currency = "character"),
    prototype = prototype(xts_returns = xts(, order.by = c(today())), timeframe = timeframe(), currency = "Local")
)


stock.returns <- function(timeframe, benchmark_code, portfolio_code, sec_id_list, currency = "AUD") {
    
    # Ensure the function has been called correctly:
    if (!is(timeframe, "timeframe")) stop("The timeframe parameter must be a timeframe object")
    if (missing(benchmark_code) & missing(portfolio_code) & missing(sec_id_list)) stop("User must supply at least one of benchmark_code, portfolio_code or sec_id_list")
    if (!is.character(currency)) stop("currency must be a string")

    # Get the parameters out of the timeframe object:
    min_date = get_start_date(timeframe)
    max_date = get_end_date(timeframe)
    frequency = get_frequency(timeframe)

    ### STEP 1: GET A LIST OF sec_id's
    # There could potentially be three sources for securities:  a benchmark, a portfolio, and a customised list of securities.
    # Check the validity of every parameter which appears.  If there are more than one, combine the results from sql (without duplicates)
    if (missing(sec_id_list)) {
        sec_id_list = vector()
    }

    # Get the list of securities for the given benchmark
    if (!missing(benchmark_code)) {
        if (!is.character(benchmark_code)) stop("If used, benchmark_code must a string")
        sql1 = paste("EXEC PCI_CORE.dbo.get_index_constituents @index_code  = '",
                    benchmark_code,
                    "', @at_date = '",
                    format(max_date, "%Y-%m-%d"),
                    "'",
                    sep = "")
        bmark_sec_id_list <- get_table_from_sql_CISMPRDSVR(sql1)$sec_id
        sec_id_list = union(sec_id_list, bmark_sec_id_list)
    }

    if (!missing(portfolio_code)) {
        if (!is.character(portfolio_code)) stop("If used, portfolio_code must a string")

        sql2 = paste("select pos.sec_id from t_Ref_Positions_L pos inner join t_Ref_Portfolio pfolio on pos.portfolio_id = pfolio.id where pfolio.fund_code = '",
                    portfolio_code,
                    "' and pos.sec_id is not null",
                    sep = "")
        pfolio_sec_id_list <- get_table_from_sql_CISMPRDSVR(sql2)$sec_id
        sec_id_list = union(sec_id_list, pfolio_sec_id_list)
    }

    ### STEP 2: GET RETURN DATA FROM THE DATABASE AT THE DESIRED FREQUENCY
    # Make a sql statement which looks like this:  EXEC PCI_REPORTING.dbo.get_performance_chart @sec_id_list = '1676,2604' , @currency='AUD', @frequency='Weekly'frequency <- "Weekly"
    secIdList_AsString <- paste(sec_id_list, collapse = ",")
    sql_returns = paste("EXEC PCI_REPORTING.dbo.get_performance_chart @sec_id_list = '",
                    secIdList_AsString,
                    "', @currency='",
                    currency,
                    "', @frequency='",
                    frequency,
                    "', @minimum_date_override = '",
                    format(min_date, "%Y-%m-%d"),
                    "'",
                    sep = "")
    all_stock_returns_df <- get_table_from_sql_CISMPRDSVR(sql_returns)

    # There is a "missing" column in all_stock_returns_df which we can ignore
    all_stock_returns_df[, "missing"] <- list(NULL)

    # Rename the columns to be the ticker, not the sec_id:
    sql_column_names = paste("SELECT sec_id, REPLACE(sec_ticker,'.ASX','') AS [ticker] from PCI_CORE.dbo.t_Ref_Sec WHERE sec_id in (", secIdList_AsString, ")", sep = "")
    column_names <- get_table_from_sql_CISMPRDSVR(sql_column_names)
    for (i in 1:length(column_names[, 1])) {
        pair = column_names[i,]
        colnames(all_stock_returns_df)[which(colnames(all_stock_returns_df) == pair[, 1])] = toString(pair[, 2])
    }

    # Drop T-1 row (remember that SQL's get_performance_chart always tacks on a T-1 data point which may not reflect a full period if daily routines are still in progress)
    all_stock_returns_df = all_stock_returns_df[1:(length(all_stock_returns_df[, 1]) - 1),]

    # Convert to xts
    all_stock_returns = xts(all_stock_returns_df[, -1], all_stock_returns_df[, 1])

    # Keep only returns between max_date and min_date
    stock_returns = all_stock_returns[paste(min_date, max_date, sep = "/")]

    return_object = new("stock.returns", xts_returns = stock_returns, timeframe = timeframe, currency = currency)
    return(return_object)
}