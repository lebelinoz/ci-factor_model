# Add shock details to the portfolio table.  Return it in a list which includes shocked_bmark_return and bmark_beta_rel_factor.
# (special case where we are given yield & bond indices and a yield shock)
get_shock_results.yield_version = function(tf, portfolio, bmark_index, bond_index, yield_index, yield_shock, stock_returns, shock_name) {

    # Step 1:  convert the yield_shock to a log return.
    last_date = max(index(yield_index[paste(get_start_date(tf), get_end_date(tf), sep = "/")]))
    last_yield = as.numeric(yield_index[last_date, 1])
    yield_return_shock = log(last_yield + yield_shock) - log(last_yield)

    # Step 2:  regress bonds vs yields, and calculate the shock to the bonds.
    #  Convert the indices to returns at the desired frequency:
    all_bond_index_returns = periodReturn(bond_index, period = get_frequency(tf, long.form = TRUE))
    all_yield_index_returns = periodReturn(yield_index, period = get_frequency(tf, long.form = TRUE), type = 'log')
    #  Restrict ourselves to the desired timeframe:
    bond_index_returns = all_bond_index_returns[paste(get_start_date(tf), get_end_date(tf), sep = "/")]
    yield_index_returns = all_yield_index_returns[paste(get_start_date(tf), get_end_date(tf), sep = "/")]
    #  Combine into a dataframe for the regression analysis:
    bond_yield_df = merge(setNames(data.frame(index(bond_index_returns), bond_index_returns[, 1]), c("date", "bond")),
                            setNames(data.frame(index(yield_index_returns), yield_index_returns[, 1]), c("date", "yield")),
                            by = "date", all = FALSE)
    bond_yield.lm = lm(bond ~ yield, bond_yield_df)
    beta_bond_yield = bond_yield.lm$coefficients[2]
    #  And the shocked bond return is:
    bond_return_shock = beta_bond_yield * yield_return_shock

    # Step 3:  return the usual get_shock_results, but with bond_index and our specially calculated bond_return_shock
    if (missing(shock_name)) {
        v = get_shock_results(tf, portfolio, bmark_index, bond_index, bond_return_shock, stock_returns)
    } else {
        v = get_shock_results(tf, portfolio, bmark_index, bond_index, bond_return_shock, stock_returns, shock_name)
    }
    return(v)
}