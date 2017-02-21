source('./preamble.R')

# Raw Parameters for all experiment
bmark_code = "MSCIWORLDG"
pfolio_code = "PCGLUF"
watchlist_name = 'Global'
currency = "AUD"

# Use 3-year weekly timeframe ending at the end of the latest month.
freq = "W"
end_date = previous_business_date_if_weekend(EOMonth(today(), -1)) # previous_business_date_if_weekend(ymd("2016-10-31")) # previous_business_date_if_weekend(EOMonth(today(), -1)) # 
start_date = previous_business_date_if_weekend(EOMonth(end_date, -36))
tf = timeframe(start_date = start_date, end_date = end_date, frequency = freq)

# The benchmark factor, bond, yield and portfolio
bmark_index = get_benchmark_index(bmark_code, currency)
bond_index = get_bond_index()
yield_index = get_yield_index()
portfolio = get_portfolio(pfolio_code)

# Get the stock returns of portfolio stocks, over the appropriate timeframe.
stock_returns = stock.returns(tf, sec_id_list = portfolio$sec_id, currency = currency)@xts_returns

# Do the work:
v1 = get_shock_results(tf, portfolio, bmark_index, get_fx_cross("AUD", "USD"), 0.1, stock_returns, "AUD/USD +10%")
v2 = get_shock_results.yield_version(tf, portfolio, bmark_index, bond_index, yield_index, 1, stock_returns, "US Treasury +100bp")

