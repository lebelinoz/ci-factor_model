source('./single_experiment_summary.R')

# Create a universe_factor_model
factor_model_maker = function(tf, sec_id_list, currency = "AUD", bmark_index, factor_index, factor_return_type = 'arithmetic', stock_returns) {

    # Step 1:  if stock_returns has not been supplied, translate the first five parameters (timeframe, benchmark_code, portfolio_code and sec_id_list) into an xts structure of returns.
    if (missing(stock_returns)) {
        if (missing(currency) | missing(sec_id_list)) stop("if stock_returns is not supplied, currency and sec_id_list must be supplied")        
        stock_returns = stock.returns(tf, sec_id_list = sec_id_list, currency = currency)@xts_returns
    }
    # if (!is(stock_returns, "stock.returns")) stop("if stock_returns is supplied, it must be of class stock.returns")


    # (drop all columns from the xts structure) which don't have 50% data)
    drop_count = sum(sapply(stock_returns, function(x) sum(is.na(x)) > nrow(stock_returns) / 2))
    if (any(which(sapply(stock_returns, function(x) sum(is.na(x)) > nrow(stock_returns) / 2)))) {
        stock_returns = stock_returns[, - which(sapply(stock_returns, function(x) sum(is.na(x)) > nrow(stock_returns) / 2))]
        # if (drop_count > 0) warning(paste(drop_count, "stocks(s) were dropped from the analysis due to insufficient data:  less than 50% of returns were available"))
    }


    # Step 2:  calculate and restrict the given factor returns
    all_bmark_returns = periodReturn(bmark_index, period = get_frequency(tf, long.form = TRUE))
    bmark_returns = all_bmark_returns[paste(get_start_date(tf), get_end_date(tf), sep = "/")]
    
    all_factor_returns = periodReturn(factor_index, period = get_frequency(tf, long.form = TRUE), type = factor_return_type)
    factor_returns = all_factor_returns[paste(get_start_date(tf), get_end_date(tf), sep = "/")]

    # Drop any dates which might be in one but not the other:
    bmark_returns = bmark_returns[which(index(bmark_returns) %in% index(factor_returns)),]
    factor_returns = factor_returns[which(index(factor_returns) %in% index(bmark_returns)),]


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
        index(asset_returns) = as_date(index(asset_returns))

        # Make sure all dates coincide:
        this_bmark_returns = bmark_returns[which(index(bmark_returns) %in% index(asset_returns)),]
        this_factor_returns = factor_returns[which(index(factor_returns) %in% index(asset_returns)),]
        asset_returns = asset_returns[which(index(asset_returns) %in% index(bmark_returns)),]

        asset_and_bmark_and_factors = data.frame(index(asset_returns), asset_returns[, 1], this_bmark_returns[, 1], this_factor_returns[, 1], row.names = NULL)
        colnames(asset_and_bmark_and_factors) = c("date", "asset", "bmark", "bond")
        all_stock_factor_models = rbind(all_stock_factor_models, single_experiment_summary(ticker, "junk", include_bmark = TRUE, include_bond = TRUE, lm_object = lm(asset ~ bmark + bond, data = asset_and_bmark_and_factors)))
        # cat("ticker =", ticker, "\n")
    }

    # We need to return a separate factor model which shows the change of the benchmark relative the yield:
    bmark_factor_df = setNames(data.frame(index(bmark_returns), bmark_returns[, 1], factor_returns[, 1]), c("date", "bmark", "factor"))
    bmark_factor.lm = lm(bmark ~ factor, bmark_factor_df)

    return_list <- list("stock_factor_models" = all_stock_factor_models, "bmark_factor.lm" = bmark_factor.lm, "bmark_factor_df" = bmark_factor_df, "bmark_returns" = bmark_returns, "factor_returns" = factor_returns, "stock_returns" = stock_returns)
    return(return_list)
}