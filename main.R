source('./factor_model_maker.R')

# Raw Parameters for all experiment
bmark_code = "MSCIWORLDG"
pfolio_code = "PCGLOB"
currency = "AUD"

######################
## BENCHMARK:
# Retrieve the benchmark returns once.  Make it a daily index:
all_bmark_returns = get_benchmark_xts_returns(bmark_code, currency, "daily")
all_bmark_returns = all_bmark_returns[paste(ymd("2001-01-01"), today(), sep = "/")]
bmark_returns_df = data.frame(index(all_bmark_returns), 1 + all_bmark_returns[, 1])
colnames(bmark_returns_df) = c("date", "returns")
bmark_returns_df = rbind(data.frame(date = c(previous_business_date_if_weekend(ymd("2000-12-31"))), returns = c(1)), bmark_returns_df)
bmark_index_df = select(mutate(bmark_returns_df, index = cumprod(returns)), date, index)
ggplot(bmark_index_df, aes(date, index)) + geom_line() + ggtitle(paste("benchmark = ", bmark_code))
bmark_index = xts(bmark_index_df[,"index"], order.by = bmark_index_df[,"date"])

######################
## BOND:
# Let's use 10y US Treasury (yield only)
setClass('myDate')
setAs("character", "myDate", function(from) as.Date(from, format = "%m/%d/%Y"))
bond_index = read.csv("C://Temp//US10YY-TU1.csv", colClasses = c('myDate', 'numeric', 'numeric')) # Some 10Y Australian Bond Index I found in FactSet.
colnames(bond_index) = c('date', 'junk', 'yield')

yield_index_df = bond_index[, c("date", "yield")]
ggplot(yield_index_df, aes(date, yield)) + geom_line() + ggtitle("factor = yield")
yield_index = xts(yield_index_df[, "yield"], order.by = yield_index_df[, "date"])


#####################
## CREATE THE UNIVERSE FACTOR MODEL (ufm):
# 

# For now, let's use 60-month timeframe ending at the end of the latest month.
freq = "M"
start_date = previous_business_date_if_weekend(EOMonth(today(), -61))
end_date = previous_business_date_if_weekend(EOMonth(today(), -1))
tf1 = timeframe(start_date = start_date, end_date = end_date, frequency = freq)

ufm = factor_model_maker(tf = tf1, benchmark_code = bmark_code, portfolio_code = pfolio_code, currency = currency, bmark_index = bmark_index, yield_index = yield_index)



#####################
## DO THE SHOCK

yield_shock = 1 # (100 bp)

# compute change in yield log return:
shocked_yield_log_return = ...

# use ufm$bmark_yield.lm to compute change in benchmark return:
shocked_benchmark_return = ...

# pass the above two variables through ufm$stock_factor_models to get individual shocked returns
...

# take the weighted sum of the shocked returns to get a portfolio return:
shocked_portfolio_return = ...

# NOW REPEAT THE ABOVE EXERCISE WITH yield_shock = 0 to get
...
unshocked_portfolio_return = ...  # should hopefully be very close to zero.



