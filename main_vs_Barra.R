# Source every file in "csv", "function", "lib" and "class" subfolders.
source('./preamble.R')

# Raw Parameters for all experiment
bmark_code = "MSCIWORLDG"
pfolio_code = "PCGLUF"
watchlist_name = 'Global'
currency = "AUD"

# TO COMPARE WITH THE WORK BARRA did in November 2016, use 3y of weekly data from Oct 2013 to Oct 2016.
# Use 3-year weekly timeframe ending at the end of the latest month.
freq = "W"
end_date = previous_business_date_if_weekend(ymd("2016-10-31")) # previous_business_date_if_weekend(EOMonth(today(), -1)) # 
start_date = previous_business_date_if_weekend(EOMonth(end_date, -36))
tf = timeframe(start_date = start_date, end_date = end_date, frequency = freq)

# The benchmark factor, bond, yield and portfolio
bmark_index = get_benchmark_index(bmark_code, currency)
bond_index = get_bond_index()
yield_index = get_yield_index()
portfolio = get_portfolio(pfolio_code, end_date) # <-- use the portfolio snapshot at the time.

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
    "AUD USD up 10%" = get_shock_results(tf, portfolio, bmark_index, get_fx_cross("AUD", "USD"), 0.1, stock_returns, "AUD USD up 10%"),
    "AUD USD down 10%" = get_shock_results(tf, portfolio, bmark_index, get_fx_cross("AUD", "USD"), -0.1, stock_returns, "AUD USD down 10%"),
    "AUDEUR up 20%" = get_shock_results(tf, portfolio, bmark_index, get_fx_cross("AUD", "EUR"), 0.2, stock_returns, "AUDEUR up 20%"),
    "AUDEUR down 20%" = get_shock_results(tf, portfolio, bmark_index, get_fx_cross("AUD", "EUR"), -0.2, stock_returns, "AUDEUR down 20%"),
    "Crude Oil up 50%" = get_shock_results(tf, portfolio, bmark_index, get_ticker_xts_return_index("OILB.US"), 0.5, stock_returns, "Crude Oil up 50%"),
    "Crude Oil down 50%" = get_shock_results(tf, portfolio, bmark_index, get_ticker_xts_return_index("OILB.US"), -0.5, stock_returns, "Crude Oil down 50%"),
    "US Treasury up 100bps" = get_shock_results.yield_version(tf, portfolio, bmark_index, bond_index, yield_index, 1, stock_returns, "US Treasury up 100bps"),
    "US Treasury down 100bps" = get_shock_results.yield_version(tf, portfolio, bmark_index, bond_index, yield_index, -1, stock_returns, "US Treasury down 100bps"),
    "US Treasury up 25bps" = get_shock_results.yield_version(tf, portfolio, bmark_index, bond_index, yield_index, 0.25, stock_returns, "US Treasury up 25bps"),
    "US Treasury down 25bps" = get_shock_results.yield_version(tf, portfolio, bmark_index, bond_index, yield_index, -0.25, stock_returns, "US Treasury down 25bps")
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
df_flat = select(portfolio_table, ticker, name, value_subset, industry_group, country = sec_exchange, weight, shock_name, shocked_return = delta_shock)

# Spread the shocks out so they each have a column:  "df" will be a dataframe where each row is a stock, which has a column for each shocked return.
df = spread(df_flat, shock_name, shocked_return)

# Make a relative return version of the this table.
make_relative = function(x) { for (n in shock_names) { x[n] = x[n] - benchmark_returns[n] }; return(x) }
df_rel = make_relative(df)

# Now we summarise our table in four ways:
df_summary = df_flat %>% group_by(shock_name) %>% summarise(shocked_return_contrib = sum(shocked_return * weight), weight = sum(weight)) %>% spread(shock_name, shocked_return_contrib)

# I have output df_summary and benchmark_returns into this spreadsheet:
#
#    M:\Staff Folders\Alain LeBel\PCGLUF_cree_vs_Barra.xlsx
#
# which has the Barra shocked numbers in it.
# Conclusion:  
#   The vibe is about the same, especially when you compare shocked returns relative the benchmark shocked returns.
#   See 'preso' for some theories about why the official risk models tend to give bigger numbers.

