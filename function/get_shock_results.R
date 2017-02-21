# Add shock details to the portfolio table.  Return it in a list which includes shocked_bmark_return and bmark_beta_rel_factor.
get_shock_results = function(tf, portfolio, bmark_index, factor_index, factor_return_shock, stock_returns, shock_name) {

    # Compute the shock model for each stock, then add the important parts back to the original portfolio dataframe:
    ufm = factor_model_maker(tf = tf, bmark_index = bmark_index, factor_index = factor_index, stock_returns = stock_returns)
    stock_factor_models = merge(portfolio, select(ufm$stock_factor_models, ticker, intercept, bmark_beta, factor_beta = bond_beta), by = "ticker")
    
    # Calculate the benchmark shocks:
    bmark_beta_rel_factor = ufm$bmark_factor.lm$coefficients[2]
    delta_shock_bmark_return = bmark_beta_rel_factor * factor_return_shock

    # Yes, I know:  once you return the betas, you can let the user calculate the shocks.  The purpose is readability
    # (one call to 'get_shock_results' gives you your shock results) and transparency (give the betas for audit purposes)
    stock_shocks = mutate(stock_factor_models,
        delta_shock_from_bmark = bmark_beta * delta_shock_bmark_return,
        delta_shock_factor_return = factor_beta * factor_return_shock,
        delta_shock = delta_shock_from_bmark + delta_shock_factor_return)

    # Makes it look nicer when printed to the screen
    stock_shocks = as_tibble(stock_shocks)

    if (missing(shock_name)) {
        v = list("shocked_portfolio" = stock_shocks, "shocked_bmark_return" = delta_shock_bmark_return, "bmark_beta_rel_factor" = bmark_beta_rel_factor)
    } else {
        stock_shocks = mutate(stock_shocks, shock_name = shock_name)
        v = list("shocked_portfolio" = stock_shocks, "shocked_bmark_return" = delta_shock_bmark_return, "bmark_beta_rel_factor" = bmark_beta_rel_factor, "shock_name" = shock_name)
    }
    return(v)
}