# For the given timeframe, yield_shock, portfolio, currency, bmark_index and yield_index,
# calculate and return a single-row dataframe which contains some of the information we need:
#  ... (see below)
portfolio_experiment_summary = function(tf, yield_shock, portfolio, currency, bmark_index, yield_index) {

    #####################
    ## CREATE THE UNIVERSE FACTOR MODEL (ufm):

    ufm = factor_model_maker(tf, portfolio$sec_id, currency, bmark_index, yield_index)

    # We now have a model which predicts benchmark moves relative yield moves:

    bmark_intercept_rel_yield = ufm$bmark_yield.lm$coefficients[1]
    bmark_beta_rel_yield = ufm$bmark_yield.lm$coefficients[2]

    ## If you're interested, here are some details of that other model:
    #summary(ufm$bmark_yield.lm)
    #anova(ufm$bmark_yield.lm)

    #####################
    ## DO THE SHOCK
    # compute change in yield log return:
    shocked_yield_log_return = log(ufm$last_yield + yield_shock) - log(ufm$last_yield)
    unshocked_yield_log_return = 0

    # use ufm$bmark_yield.lm to compute change in benchmark return:
    shocked_bmark_return = bmark_intercept_rel_yield + bmark_beta_rel_yield * shocked_yield_log_return
    unshocked_bmark_return = bmark_intercept_rel_yield # should be very close to zero.

    # pass the above two variables through ufm$stock_factor_models to get individual shocked returns
    stock_forecast_returns_with_details = mutate(ufm$stock_factor_models,
                                                shocked_return = intercept + bmark_beta * shocked_bmark_return + yield_beta * shocked_yield_log_return,
                                                unshocked_return = intercept + bmark_beta * unshocked_bmark_return + yield_beta * unshocked_yield_log_return,
                                                delta_shock = shocked_return - unshocked_return)

    stock_forecast_returns = select(stock_forecast_returns_with_details, ticker, delta_shock)

    # Add the individual shocks back to the original portfolio:
    portfolio_with_shocks = merge(portfolio, stock_forecast_returns, by = "ticker", all.x = TRUE)

    # ... and add a weighted_delta_shock column, turning missings into zeroes
    portfolio_with_shocks = mutate(portfolio_with_shocks, weighted_delta_shock = weight * delta_shock)
    portfolio_with_shocks$weighted_delta_shock[which(is.na(portfolio_with_shocks$weighted_delta_shock))] <- 0

    # take the weighted sum of the shocked returns to get a portfolio return:
    shocked_portfolio_return = sum(portfolio_with_shocks$weighted_delta_shock) / sum(portfolio_with_shocks$weight)

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

    return(df)
}