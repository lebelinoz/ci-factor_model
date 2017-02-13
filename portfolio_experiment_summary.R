# For the given timeframe, yield_shock, portfolio, currency, bmark_index and yield_index,
# calculate and return a single-row dataframe which contains some of the information we need:
#  ... (see below)
portfolio_experiment_summary = function(tf, yield_shock, portfolio, currency, bond_index, bmark_index, yield_index) {

    #####################
    ## CREATE THE UNIVERSE FACTOR MODEL (ufm):

    ufm = factor_model_maker(tf, portfolio$sec_id, currency, bmark_index, bond_index)

    #####################
    ## DO THE SHOCK
    # compute change in yield log return:

    # We also the final yield in the series to shock:
    last_date = max(index(yield_index[paste(get_start_date(tf), get_end_date(tf), sep = "/")]))
    last_yield = as.numeric(yield_index[last_date, 1])
    shocked_yield_log_return = log(last_yield + yield_shock) - log(last_yield)
    unshocked_yield_log_return = 0

    # Compute how the shock changes the bond.
    # Unfortunately, we only have monthly yields vs monthly bonds.  But these seem to have high correlation even when the timespan is small.
    all_monthly_bond_index_returns = monthlyReturn(bond_index)
    all_monthly_yield_index_returns = monthlyReturn(yield_index, type = 'log')
    monthly_bond_index_returns = all_monthly_bond_index_returns[paste(get_start_date(tf), get_end_date(tf), sep = "/")]
    monthly_yield_index_returns = all_monthly_yield_index_returns[paste(get_start_date(tf), get_end_date(tf), sep = "/")]
    bond_yield_df = merge(setNames(data.frame(index(monthly_bond_index_returns), monthly_bond_index_returns[, 1]), c("date", "bond")),
                            setNames(data.frame(index(monthly_yield_index_returns), monthly_yield_index_returns[, 1]), c("date", "yield")),
                            by = "date", all = FALSE)
    # show_regression(bond_yield_df, "bond", "yield")
    bond_yield.lm = lm(bond ~ yield, bond_yield_df)
    intercept_bond_yield = bond_yield.lm$coefficients[1]
    beta_bond_yield = bond_yield.lm$coefficients[2]

    shocked_bond_return = intercept_bond_yield + beta_bond_yield * shocked_yield_log_return
    unshocked_bond_return = intercept_bond_yield + beta_bond_yield * unshocked_yield_log_return

    # Now use ufm$bmark_factor.lm to compute how the bond change affects the benchmark return:
    bmark_intercept_rel_factor = ufm$bmark_factor.lm$coefficients[1]
    bmark_beta_rel_factor = ufm$bmark_factor.lm$coefficients[2]
    shocked_bmark_return = bmark_intercept_rel_factor + bmark_beta_rel_factor * shocked_bond_return
    unshocked_bmark_return = bmark_intercept_rel_factor + bmark_beta_rel_factor * unshocked_bond_return # should be very close to zero.

    # pass the above two variables through ufm$stock_factor_models to get individual shocked returns
    stock_forecast_returns_with_details = mutate(ufm$stock_factor_models,
                                                shocked_return = intercept + bmark_beta * shocked_bmark_return + bond_beta * shocked_bond_return,
                                                unshocked_return = intercept + bmark_beta * unshocked_bmark_return + bond_beta * unshocked_bond_return,
                                                delta_shock = shocked_return - unshocked_return,
                                                delta_shock_caused_by_bmark = bmark_beta * (shocked_bmark_return - unshocked_bmark_return),
                                                delta_shock_caused_by_bond = bond_beta * (shocked_bond_return - unshocked_bond_return),
                                                benchmark_delta_shock = shocked_bmark_return - unshocked_bmark_return,
                                                delta_shock_relative_benchmark = delta_shock - benchmark_delta_shock)

    stock_forecast_returns = select(stock_forecast_returns_with_details, ticker, delta_shock)

    # Add the individual shocks back to the original portfolio:
    portfolio_with_shocks = merge(portfolio, stock_forecast_returns, by = "ticker", all.x = TRUE)

    # ... and add a weighted_delta_shock column, turning missings into zeroes
    portfolio_with_shocks = mutate(portfolio_with_shocks, weighted_delta_shock = weight * delta_shock)
    portfolio_with_shocks$weighted_delta_shock[which(is.na(portfolio_with_shocks$weighted_delta_shock))] <- 0

    # take the weighted sum of the shocked returns to get a portfolio return:
    delta_shocked_portfolio_return = sum(portfolio_with_shocks$weighted_delta_shock) / sum(portfolio_with_shocks$weight)

    df = data.frame(
    # inputs:
          start_date = get_start_date(tf)
        , end_date = get_end_date(tf)
        , frequency = get_frequency(tf, long.form = TRUE)
        , months = interval(get_start_date(tf), get_end_date(tf)) %/% months(1)
        , yield_shock = yield_shock
    # outputs:
        , pfolio_return_delta_shock_unadjusted_by_weighted = sum(portfolio_with_shocks$weighted_delta_shock)
        , pfolio_noncash_weight = sum(portfolio_with_shocks$weight)
        , pfolio_return_delta_shock = sum(portfolio_with_shocks$weighted_delta_shock) / sum(portfolio_with_shocks$weight)
        , count_of_stocks = sum(!is.na(portfolio_with_shocks$delta_shock))
        , bmark_return_delta_shock = shocked_bmark_return - unshocked_bmark_return
    )

    return_list <- list("portfolio_experiment_summary" = df, "stock_factor_models" = stock_forecast_returns_with_details, "bmark_factor.lm" = ufm$bmark_factor.lm)
    return(return_list)
}