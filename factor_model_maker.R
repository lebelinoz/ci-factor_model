source('./class/stock.returns.R')
source('./single_experiment_summary.R')

# Create a universe_factor_model
factor_model_maker = function(tf, sec_id_list, currency = "AUD", bmark_index, yield_index) {

    # Step 1:  translate the first five parameters (timeframe, benchmark_code, portfolio_code and sec_id_list) into an xts structure of returns.
    stock_returns = stock.returns(tf, sec_id_list = sec_id_list, currency = currency)@xts_returns

    # (drop all columns from the xts structure) which don't have 50% data)
    drop_count = sum(sapply(stock_returns, function(x) sum(is.na(x)) > nrow(stock_returns) / 2))
    stock_returns = stock_returns[, - which(sapply(stock_returns, function(x) sum(is.na(x)) > nrow(stock_returns) / 2))]    
    if (drop_count > 0) warning(paste(drop_count, "stocks(s) were dropped from the analysis due to insufficient data:  less than 50% of returns were available"))


    # Step 2:  calculate and restrict the given factor returns
    all_bmark_returns = periodReturn(bmark_index, period = get_frequency(tf, long.form = TRUE))
    bmark_returns = all_bmark_returns[paste(get_start_date(tf), get_end_date(tf), sep = "/")]
    
    all_yield_returns = periodReturn(yield_index, period = get_frequency(tf, long.form = TRUE), type = 'log')
    yield_returns = all_yield_returns[paste(get_start_date(tf), get_end_date(tf), sep = "/")]


    # Step 3:  for each stock, compute its factor model.
    all_stock_factor_models = data.frame(
        ticker = character(),
        universe = character(),
        bmark = logical(),
        bond = logical(),
        yield = logical(),
        r.squared = numeric(),
        adj.r.squared = numeric(),
        F_value = numeric(),
        p_value = numeric(),
        intercept = numeric(),
        bmark_beta = numeric(),
        bond_beta = numeric(),
        yield_beta = numeric(),
        residual_error = numeric(),
        bmark_F_value = numeric(),
        bond_F_value = numeric(),
        yield_F_value = numeric(),
        bmark_p_value = numeric(),
        bond_p_value = numeric(),
        yield_p_value = numeric()
    )

    for (ticker in colnames(stock_returns)) {
        asset_returns = stock_returns[, ticker]
        asset_and_bmark_and_factors = data.frame(index(asset_returns), asset_returns[, 1], bmark_returns[, 1], yield_returns[, 1], row.names = NULL)
        colnames(asset_and_bmark_and_factors) = c("date", "asset", "bmark", "yield")
        all_stock_factor_models = rbind(all_stock_factor_models, single_experiment_summary(ticker, "junk", include_bmark = TRUE, include_yield = TRUE, lm_object = lm(asset ~ bmark + yield, data = asset_and_bmark_and_factors)))
        cat("ticker =", ticker, "\n")
    }

    # We need to return a separate factor model which shows the change of the benchmark relative the yield:
    bmark_yield_df = data.frame(date = index(bmark_returns), bmark = bmark_returns[,1], yield = yield_returns[,1])
    colnames(bmark_yield_df) = c("date", "bmark", "yield")
    bmark_yield.lm = lm(bmark ~ yield, bmark_yield_df)

    # We also the final yield in the series to shock:
    last_date = max(index(yield_index[paste(get_start_date(tf), get_end_date(tf), sep = "/")]))
    last_yield = as.numeric(yield_index[last_date, 1])

    return_list <- list("stock_factor_models" = all_stock_factor_models, "bmark_yield.lm" = bmark_yield.lm, "last_yield" = last_yield)
    return(return_list)
}