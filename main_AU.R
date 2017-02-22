# Source every file in "function", "lib" and "class" subfolders.
source('./preamble.R')

# Raw Parameters for all experiment
bmark_code = "XJO"
pfolio_code = "PCHEST"
watchlist_name = 'Global'
currency = "AUD"

# Use 3-year weekly timeframe ending at the end of the latest month.
freq = "W"
end_date = previous_business_date_if_weekend(EOMonth(today(), -1)) # previous_business_date_if_weekend(ymd("2016-10-31")) # previous_business_date_if_weekend(EOMonth(today(), -1)) # 
start_date = previous_business_date_if_weekend(EOMonth(end_date, -36))
tf = timeframe(start_date = start_date, end_date = end_date, frequency = freq)
freq_long_form = get_frequency(tf, long.form = TRUE)

# The benchmark factor, bond, yield and portfolio
bmark_index = get_benchmark_index(bmark_code, currency)
bond_index = get_bond_index("SBABIG.ASX")
yield_index = get_yield_index("AU10Y")
portfolio = get_portfolio(pfolio_code) # get_index_snapshot(bmark_code) # Let's look at the entire ASX 200.  We can focus on portfolios later.

# Hack:  something strange going on in our database where ASX 200 weights don't add to 100%
#        even though all stocks are present and the weights are very close to weights in Iress:
if (pfolio_code == "XJO") portfolio$weight = portfolio$weight / sum(portfolio$weight)

# Graph stuff we'll be working with:
price_plot(bond_index, yield_index, "The bond and yield we'll use for Aussie stuff")
show_regression_between_xts(periodReturn(bmark_index, period = freq_long_form), periodReturn(bond_index, period = freq_long_form), "bmark", "bond")
show_regression_between_xts(periodReturn(bond_index, period = freq_long_form), periodReturn(yield_index, period = freq_long_form, type = 'log'), "bond", "yield")

# Get the stock returns of portfolio stocks, over the appropriate timeframe.
stock_returns = stock.returns(tf, sec_id_list = portfolio$sec_id, currency = currency)@xts_returns

# Do the work:  create the list of lists of tables + benchmark shocks.
# Every item in this list will itself be a list containing four things:
#  1.  shocked_portfolio, which will be our portfolio with a shocked return column (plus extra columns such as bmark_beta and factor_beta for auditing purposes)
#  2.  shocked_bmark_return, which will be the shocked benchmark.
#  3.  there's a benchmark relative factor beta (unused atm)
#  4.  the name of the shock (which will be the same as the name in the list)
# (this part takes a solid minute)
l = list(
    "AUD/USD +10%" = get_shock_results(tf, portfolio, bmark_index, get_fx_cross("AUD", "USD"), 0.1, stock_returns, "AUD/USD +10%"),
    #"AUD/USD -10%" = get_shock_results(tf, portfolio, bmark_index, get_fx_cross("AUD", "USD"), -0.1, stock_returns, "AUD/USD -10%"),
    "AUD/EUR +10%" = get_shock_results(tf, portfolio, bmark_index, get_fx_cross("AUD", "EUR"), 0.1, stock_returns, "AUD/EUR +10%"),
    #"AUD/EUR -10%" = get_shock_results(tf, portfolio, bmark_index, get_fx_cross("AUD", "EUR"), -0.1, stock_returns, "AUD/EUR -10%"),
    "AUD/JPY +10%" = get_shock_results(tf, portfolio, bmark_index, get_fx_cross("AUD", "JPY"), 0.1, stock_returns, "AUD/JPY +10%"),
    #"AUD/JPY -10%" = get_shock_results(tf, portfolio, bmark_index, get_fx_cross("AUD", "JPY"), -0.1, stock_returns, "AUD/JPY -10%"),
    "EUR/USD +10%" = get_shock_results(tf, portfolio, bmark_index, get_fx_cross("EUR", "USD"), 0.1, stock_returns, "EUR/USD +10%"),
    #"EUR/USD -10%" = get_shock_results(tf, portfolio, bmark_index, get_fx_cross("EUR", "USD"), -0.1, stock_returns, "EUR/USD -10%"),
    "Oil +50%" = get_shock_results(tf, portfolio, bmark_index, get_ticker_xts_return_index("OILB.US"), 0.5, stock_returns, "Oil +50%"),
    "Oil +20%" = get_shock_results(tf, portfolio, bmark_index, get_ticker_xts_return_index("OILB.US"), 0.2, stock_returns, "Oil +20%"),
    #"Oil -20%" = get_shock_results(tf, portfolio, bmark_index, get_ticker_xts_return_index("OILB.US"), -0.2, stock_returns, "Oil -20%"),
    #"Oil -50%" = get_shock_results(tf, portfolio, bmark_index, get_ticker_xts_return_index("OILB.US"), -0.5, stock_returns, "Oil -50%"),
    "AU 10Y Rate +100bp" = get_shock_results.yield_version(tf, portfolio, bmark_index, bond_index, yield_index, 1, stock_returns, "AU 10Y Rate +100bp"),
    "AU 10Y Rate +25bp" = get_shock_results.yield_version(tf, portfolio, bmark_index, bond_index, yield_index, 0.25, stock_returns, "AU 10Y Rate +25bp"),
    "AU 10Y Rate -25bp" = get_shock_results.yield_version(tf, portfolio, bmark_index, bond_index, yield_index, -0.25, stock_returns, "AU 10Y Rate -25bp"),
    "AU 10Y Rate -100bp" = get_shock_results.yield_version(tf, portfolio, bmark_index, bond_index, yield_index, -1, stock_returns, "AU 10Y Rate -100bp")
    )

# Now flatten all the different tables into one big table, and combine the shocked benchmark returns into a vector with named entries
# (If there's a better way of doing this, I'm all ears...  I'm certain there's a cute Hadley Wickham way using his "purrr" package)
portfolio_table = l[[1]]$shocked_portfolio
benchmark_returns = c(l[[1]]$shocked_bmark_return)
names(benchmark_returns) = names(l)[1]
for (i in 2:length(l)) {
    portfolio_table = rbind(portfolio_table, l[[i]]$shocked_portfolio)
    temp = c(l[[i]]$shocked_bmark_return)
    names(temp) = names(l)[i]
    benchmark_returns = c(benchmark_returns, temp)
}
shock_names = names(l)

# This flat table ("flat" because every row corresponds to a stock + shock combination) is now reduced to only the essential columns
df_flat = select(portfolio_table, ticker, value_subset, industry_group, weight, shock_name, shocked_return = delta_shock)

# Spread the shocks out so they each have a column:  "df" will be a dataframe where each row is a stock, which has a column for each shocked return.
df = spread(df_flat, shock_name, shocked_return)

# Make a relative return version of the this table.
make_relative = function(x) { for (n in shock_names) { x[n] = x[n] - benchmark_returns[n] }; return(x) }
df_rel = make_relative(df)

# Now we summarise our table in four ways:
#  1. This is the weighted sum of shocked returns, i.e. each column is the portfolio shocked return.
df_summary = df_flat %>% group_by(shock_name) %>% summarise(shocked_return_contrib = sum(shocked_return * weight), weight = sum(weight)) %>% spread(shock_name, shocked_return_contrib)
#  2. This is the weighted sum of returns divided by total weight.  It's the shocked portfolio return if the portfolio had 0% cash.
df_summary_adj = df_flat %>% group_by(shock_name) %>% summarise(shocked_return_adj = sum(shocked_return * weight) / sum(weight), weight = sum(weight)) %>% spread(shock_name, shocked_return_adj)
#  3. This gives the relative shocked return of the portfolio:
df_summary_rel = make_relative(df_summary)
#  4. This gives the relative shocked return of the adjusted (0% cash) portfolio:
df_summary_adj_rel = make_relative(df_summary_adj)

# Similary, we group by value subset.  
#  The first table is contributions (weighted sums of returns) which should sum to elements in df_summary.
df_value_subset = df_flat %>% group_by(value_subset, shock_name) %>% summarise(shocked_return_contrib = sum(shocked_return * weight), weight = sum(weight)) %>% spread(shock_name, shocked_return_contrib)
#  The second table is adjusted, so each value subset is a 100% fully invested portfolio
df_value_subset_adj = df_flat %>% group_by(value_subset, shock_name) %>% summarise(shocked_return_adj = sum(shocked_return * weight) / sum(weight), weight = sum(weight)) %>% spread(shock_name, shocked_return_adj)
#  The  third table is the relative shocked returns of the adjusted portfolios from the previous table
df_value_subset_adj_rel = make_relative(df_value_subset_adj)

# Group by industry group, too:
df_industry_group = df_flat %>% group_by(industry_group, shock_name) %>% summarise(shocked_return_contrib = sum(shocked_return * weight), weight = sum(weight)) %>% spread(shock_name, shocked_return_contrib)
df_industry_group_adj = df_flat %>% group_by(industry_group, shock_name) %>% summarise(shocked_return_adj = sum(shocked_return * weight) / sum(weight), weight = sum(weight)) %>% spread(shock_name, shocked_return_adj)
df_industry_group_adj_rel = make_relative(df_industry_group_adj)

