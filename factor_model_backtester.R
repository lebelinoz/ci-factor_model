### PREAMBLE
source('Sql_Wrapper.R') # <-- for grabbing data straight from our SQL database
source('StockPlot.R') # <-- for plotting relative return charts

CAPM_Backtester = function(benchmark_code, frequency, currency, min_date, max_date, test_date, sec_id_override = 0) {

    ###########################
    ###   EQUITY RETURNS    ###
    # Get the universe of sec_id's.  This will be constituents AT THE TIME (unless we only want to focus on a single stock)
    if (sec_id_override == 0) {
        sql_sec_id = paste("EXEC PCI_CORE.dbo.get_index_constituents @index_code  = '", benchark_code, "', @at_date = '", format(max_date, "%Y-%m-%d"), "'", sep = "")
        secIdList <- get_table_from_sql_CISMPRDSVR(sql_sec_id)$sec_id
    } else {
        secIdList = c(sec_id_override)
    }

    # Get the equity returns (remember that we might need to go far back in time, so use the new @minimum_date_override feature in get_performance_chart
    secIdList_AsString <- paste(secIdList, collapse = ",")
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

    # Drop all columns where there is only data for less than 50% of the data 
    # (eg. for 52-week correlations, stocks which IPO'd six months ago will look off-the-charts)
    all_stock_returns_df = all_stock_returns_df[, colSums(is.na(all_stock_returns_df)) < length(all_stock_returns_df[, 1]) / 2]

    # Drop T-1 row (remember that SQL's get_performance_chart always tacks on a T-1 data point which may not reflect a full period)
    all_stock_returns_df = all_stock_returns_df[1:(length(all_stock_returns_df[, 1]) - 1),]

    # Convert to xts
    all_stock_returns = xts(all_stock_returns_df[, -1], all_stock_returns_df[, 1])

    # Keep only returns between max_date and min_date
    stock_returns = all_stock_returns[paste(min_date, max_date, sep = "/")]

    ###   EQUITY RETURNS    ###
    ###########################

    ###########################
    ###  BENCHMARK RETURNS  ###
    all_bmark_returns = get_benchmark_xts_returns(benchark_code, currency, frequency)
    bmark_returns = all_bmark_returns[paste(min_date, max_date, sep = "/")]
    ###  BENCHMARK RETURNS  ###
    ###########################

    # TODO:  Ensure the dates coincide perfectly between the two 

    test_result = data.frame(
        ticker = character(),
        frequency = character(),
        min_date = as_date(character()),
        max_date = as_date(character()),
        test_date = as_date(character()),
        beta = numeric(),
        alpha = numeric(),
        r_squared = numeric(),
        estimated_next_return = numeric(),
        actual_next_return = numeric(),
        return_error = numeric()
    )

    for (ticker in colnames(stock_returns)) {
        # Isolate the stock and benchmark returns:
        asset_returns = stock_returns[, ticker]
        asset_and_bmark = data.frame(index(asset_returns), asset_returns[, 1], bmark_returns[, 1], row.names = NULL)
        colnames(asset_and_bmark) = c("date", "asset", "bmark")

        # Compute the slope, intercept and r-squared 
        asset.lm = lm(asset ~ bmark, data = asset_and_bmark)
        asset.beta = asset.lm$coefficients[2]
        asset.alpha = asset.lm$coefficients[1]
        asset.r.squared = summary(asset.lm)$r.squared
        next_bmark_move = as.numeric(all_bmark_returns[test_date, 1])
        estimated_next_asset_move = asset.beta * next_bmark_move + asset.alpha
        actual_next_asset_move = as.numeric(all_stock_returns[test_date, ticker])

        # Bind it to our result dataset
        df = data.frame(
            ticker = ticker,
            frequency = frequency,
            min_date = min_date,
            max_date = max_date,
            test_date = test_date,
            beta = asset.beta,
            alpha = asset.alpha,
            r_squared = asset.r.squared,
            estimated_next_return = estimated_next_asset_move,
            actual_next_return = actual_next_asset_move,
            return_error = estimated_next_asset_move - actual_next_asset_move
        )
        test_result = rbind(test_result, df)
    }

    rownames(test_result) = NULL
    return(test_result)
}


benchark_code = 'XJO'
frequency = "Monthly"
currency = "AUD"
number_of_months = 60
max_date = as_date("2016-11-30")
min_date = EOMonth(max_date, - number_of_months, TRUE) # as_date("2011-11-28") # as.Date("2011-12-30") # as.Date("2015-09-29") # 
test_date = EOMonth(max_date, 1, TRUE)

test_result = CAPM_Backtester(benchmark_code, frequency, currency, min_date, max_date, test_date)