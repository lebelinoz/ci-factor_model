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


bmark_intercept_rel_yield = ufm$bmark_yield.lm$coefficients[1]
bmark_beta_rel_yield = ufm$bmark_yield.lm$coefficients[2]

#summary(ufm$bmark_yield.lm)
#anova(ufm$bmark_yield.lm)

#####################
## DO THE SHOCK

yield_shock = 1 # (100 = 100 bp because the yield series is )

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

# take the weighted sum of the shocked returns to get a portfolio return:
# TO DO:  Multiply shocked and unshocked returns by portfolio weights
# shocked_portfolio_return = ...





