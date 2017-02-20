#######################
## PREAMBLE:
##  WARNING! The source + library parts must be run separately for some reason (?).
source('./preamble.R')
source('./factor_model_maker.R')
source('./portfolio_experiment_summary.R')

# Raw Parameters for all experiment
bmark_code = "MSCIWORLDG"
pfolio_code = "PCGLUF"
currency = "AUD"

# The benchmark factor, benchmark and timeframe
bmark_index = get_benchmark_index(bmark_code, currency)
portfolio = get_portfolio(pfolio_code)

# Use 3-year weekly timeframe ending at the end of the latest month.
freq = "W"
end_date = previous_business_date_if_weekend(EOMonth(today(), -1)) # previous_business_date_if_weekend(ymd("2016-10-31")) # previous_business_date_if_weekend(EOMonth(today(), -1)) # 
start_date = previous_business_date_if_weekend(EOMonth(end_date, -36))
tf = timeframe(start_date = start_date, end_date = end_date, frequency = freq)

get_shock_vector = function(tf, portfolio, currency, bmark_index, ccy_index, factor_return_shock) {
    # Compute the shock model for each stock, then compute the effect of the currency shock on the benchmark:
    ufm = factor_model_maker(tf, portfolio$sec_id, currency, bmark_index, ccy_index)
    stock_factor_models = merge(portfolio, select(ufm$stock_factor_models, ticker, intercept, bmark_beta, factor_beta = bond_beta), by = "ticker")
    bmark_intercept_rel_factor = ufm$bmark_factor.lm$coefficients[1]
    bmark_beta_rel_factor = ufm$bmark_factor.lm$coefficients[2]

    # Factor shock
    shocked_bmark_return = bmark_intercept_rel_factor + bmark_beta_rel_factor * factor_return_shock
    unshocked_bmark_return = bmark_intercept_rel_factor # should be very close to zero.
    delta_shock_bmark_return = shocked_bmark_return - unshocked_bmark_return

    show_regression(ufm$bmark_factor_df, "bmark", "factor")

    stock_shock = mutate(stock_factor_models, 
        delta_shock_from_bmark = bmark_beta * delta_shock_bmark_return, 
        delta_shock_factor_return = factor_beta * factor_return_shock, 
        delta_shock = delta_shock_from_bmark + delta_shock_factor_return,
        weighted_delta_shock_from_bmark = weight * delta_shock_from_bmark,
        weighted_delta_shock_factor_return = weight * delta_shock_factor_return,
        weighted_delta_shock = weight * delta_shock)

    v = c(sum(stock_shock$delta_shock * stock_shock$weight), delta_shock_bmark_return, sum(stock_shock$delta_shock * stock_shock$weight) - delta_shock_bmark_return)
    names(v) = c("P'folio", "B'mark", "rel")
    return(v)
}

v1u = get_shock_vector(tf, portfolio, currency, bmark_index, get_fx_cross("AUD", "USD"), 0.1)
v1d = get_shock_vector(tf, portfolio, currency, bmark_index, get_fx_cross("AUD", "USD"), -0.1)
v2u = get_shock_vector(tf, portfolio, currency, bmark_index, get_fx_cross("AUD", "EUR"), 0.2)
v2d = get_shock_vector(tf, portfolio, currency, bmark_index, get_fx_cross("AUD", "EUR"), -0.2)
v3u = get_shock_vector(tf, portfolio, currency, bmark_index, get_ticker_xts_return_index("OILB.US"), 0.5)
v3d = get_shock_vector(tf, portfolio, currency, bmark_index, get_ticker_xts_return_index("OILB.US"), -0.5)

df = data.frame(Portfolio = names(v1u), v1u = v1u, v1d = v1d, v2u = v2u, v2d = v2d, v3u = v3u, v3d = v3d)
rownames(df) = NULL
colnames(df) = c("Portfolio", "AUD USD +10%", "AUD USD -10%", "AUD EUR +20%", "AUD EUR -20%", "Crude +50%", "Crude -50%")


df_formatted = df
df_formatted[, -1] = sapply(df_formatted[, -1], function(x) paste(sprintf("%1.2f", x * 100), "%", sep = ""))
df_formatted[, 1] = as.character(df_formatted[, 1])
#df_formatted = rbind(colnames(df_formatted), df_formatted)
#colnames(df_formatted) = NULL
#df_formatted[1,1] = as.character("Portfolio")


# TO DO:  
#   1. Get the true USD Treasury Shocks in here
#   2. Figure out how to display tables nicely using knitr::kable
#   3. Compute the latest forecasts, and also compute the "to Oct 2016 forecasts with Barra numbers for comparison.