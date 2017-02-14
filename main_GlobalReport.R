#######################
## PREAMBLE:
##  WARNING! The first few steps must be run in chunks for some reason (?).  Once you have a bmark_index, everything else ought to run smoothly.
source('./factor_model_maker.R')
source('./portfolio_experiment_summary.R')
source('./show_regression.R')

# Raw Parameters for all experiment
bmark_code = "MSCIWORLDG"
pfolio_code = "PCGLUF"
watchlist_name = 'Global'
currency = "AUD"

######################
## BENCHMARK:
# Retrieve the benchmark daily index.  'factor_model_maker' will make it into returns with an appropriate frequency.
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
# Let's use 10y US Treasury (yield only).
# The retrieve data is daily, and 'factor_model_maker' will make it into returns with an appropriate frequency.
setClass('myDate')
setAs("character", "myDate", function(from) as.Date(from, format = "%m/%d/%Y"))

citi_usa_bond_index = read.csv("C://Temp//SBGT.csv", colClasses = c('myDate', 'numeric', 'numeric'))
colnames(citi_usa_bond_index) = c('date', 'citi_usa_index', 'citi_usa_yield')
bond_index = xts(citi_usa_bond_index[, "citi_usa_index"], order.by = citi_usa_bond_index[, "date"])
ggplot(citi_usa_bond_index, aes(date, citi_usa_index)) + geom_line() + ggtitle("bond = Citigroup U.S. Bond Index")

us10y_yield = read.csv("C://Temp//US10YY-TU1.csv", colClasses = c('myDate', 'numeric', 'numeric'))
us10y_yield = us10y_yield[, -2]
colnames(us10y_yield) = c('date', 'yield')
yield_index_df = us10y_yield

ggplot(yield_index_df, aes(date, yield)) + geom_line() + ggtitle("yield = US 10-year treasury")
yield_index = xts(yield_index_df[, "yield"], order.by = yield_index_df[, "date"])
# see 'bond_series.R' for charts to convince you of the high correlation between this bond and this yield

#####################
## SHOCK
# (1 = 100 bp because the yield series I got from FactSet is multiplied by 100)
yield_shock = 1


######################
## PORTFOLIO:
portfolio = get_portfolio(pfolio_code)


######################
## WATCHLIST:
watchlist = get_watchlist(watchlist_name)


#####################
## TIMEFRAME
# For now, let's use 3-year weekly timeframe ending at the end of the latest month.
freq = "W"
start_date = previous_business_date_if_weekend(EOMonth(today(), -37))
end_date = previous_business_date_if_weekend(EOMonth(today(), -1))
tf1 = timeframe(start_date = start_date, end_date = end_date, frequency = freq)

# The portfolio experiment summary gives us the benchmark and portfolio shocked returns:
pes_portfolio = portfolio_experiment_summary(tf1, yield_shock, portfolio, currency, bond_index, bmark_index, yield_index)
df_portfolio = pes_portfolio$portfolio_experiment_summary
stock_summary_pfolio = pes_portfolio$stock_factor_models

pes_watchlist = portfolio_experiment_summary(tf1, yield_shock, watchlist, currency, bond_index, bmark_index, yield_index)
df_watchlist = pes_watchlist$portfolio_experiment_summary
stock_summary_watchlist = pes_watchlist$stock_factor_models

## Let's look at the history of portfolio and watchlist shocks:
#for (i in 1:60) {
    #cat("i = ", i, "\n")
    #this_end_date = previous_business_date_if_weekend(EOMonth(end_date, - i))
    #this_start_date = previous_business_date_if_weekend(EOMonth(this_end_date, - 36))
    #this_tf = timeframe(start_date = this_start_date, end_date = this_end_date, frequency = "W")
#}

#df = df[-1,]
#rownames(df) = NULL

#ggplot(df, aes(months, pfolio_return_delta_shock, colour = frequency)) + geom_line() + ggtitle("(v bond) Next pfolio return if yield 100 bp:  model result by timespan and frequency")
#ggplot(df, aes(months, bmark_return_delta_shock, colour = frequency)) + geom_line() + ggtitle("(v bond) Next b'mark return if yield 100 bp:  model result by timespan and frequency")
#ggplot(df, aes(months, pfolio_return_delta_shock - bmark_return_delta_shock, colour = frequency)) + geom_line() + ggtitle("(v bond) Next pfolio - bmark return if yield 100 bp:  model result by timespan and frequency")


## This took a few minutes:  let's export to csv and pick it up later if necessary
#csv_filename = paste("C://Temp//portfolio_experiment_summary_useBmarkAndBond-", pfolio_code, "-", bmark_code, "-yld", 100 * yield_shock, "bps.csv", sep = "")
#write.csv(df, csv_filename, row.names = FALSE)

#df = read.csv(csv_filename)
#df$start_date = as_date(df$start_date)
#df$end_date = as_date(df$end_date)
#df_new = mutate(df, version = "vs bond")

### Compare to the old methodology, where we regressed directly against the yield (log) returns:
#old_csv_filename = paste("C://Temp//portfolio_experiment_summary-", pfolio_code, "-", bmark_code, "-yld", 100 * yield_shock, "bps.csv", sep = "")
#df_old = read.csv(old_csv_filename)
#df_old$start_date = as_date(df_old$start_date)
#df_old$end_date = as_date(df_old$end_date)
#df_old = mutate(df_old, version = "vs yield")

## Merge stuff 
#df_old_and_new = rbind(df_new, df_old)


## Let's look at how changing the timeframe affects the outputs of the model:
#ggplot(df_old_and_new, aes(months, pfolio_return_delta_shock, colour = frequency)) + geom_line() + ggtitle("Next pfolio return if yield 100 bp:  model result by timespan and frequency") + facet_wrap(~version)
#ggplot(df_old_and_new, aes(months, bmark_return_delta_shock, colour = frequency)) + geom_line() + ggtitle("Next b'mark return if yield 100 bp:  model result by timespan and frequency") + facet_wrap( ~ version)
#ggplot(df_old_and_new, aes(months, pfolio_return_delta_shock - bmark_return_delta_shock, colour = frequency)) + geom_line() + ggtitle("Next pfolio - bmark return if yield 100 bp:  model result by timespan and frequency") + facet_wrap( ~ version)


#################################
## Why is the model so bad?  Let's look at benchmark vs yield over the last 60 months, as well as a couple of stocks vs yields.
bmark_returns = periodReturn(bmark_index, period = get_frequency(tf1, long.form = TRUE))[paste(get_start_date(tf1), get_end_date(tf1), sep = "/")]
bond_returns = periodReturn(bond_index, period = get_frequency(tf1, long.form = TRUE), type = 'log')[paste(get_start_date(tf1), get_end_date(tf1), sep = "/")]
yield_returns = periodReturn(yield_index, period = get_frequency(tf1, long.form = TRUE), type = 'log')[paste(get_start_date(tf1), get_end_date(tf1), sep = "/")]
stock_returns = stock.returns(tf1, sec_id_list = portfolio$sec_id)@xts_returns

ticker = "AON.US"
asset_bmark_yield_df = data.frame(date = index(bmark_returns), asset = stock_returns[, ticker], bmark = bmark_returns[, 1], bond = bond_returns[, 1], yield = yield_returns[, 1])
rownames(asset_bmark_yield_df) = NULL
colnames(asset_bmark_yield_df) = c("date", ticker, "bmark", "bond", "yield")

# Show charts:
show_regression(asset_bmark_yield_df, "bmark", ticker)
show_regression(asset_bmark_yield_df, "yield", "bmark")
show_regression(asset_bmark_yield_df, "yield", ticker)
show_regression(asset_bmark_yield_df, "bond", ticker)

## Show three charts with the same scale:
#show_regression(asset_bmark_yield_df, "bmark", ticker) + scale_x_continuous(limits = c(-0.3,0.3)) + scale_y_continuous(limits = c(-0.075, 0.125))
#show_regression(asset_bmark_yield_df, "yield", "bmark") + scale_x_continuous(limits = c(-0.3, 0.3)) + scale_y_continuous(limits = c(-0.075, 0.125))
#show_regression(asset_bmark_yield_df, "yield", ticker) + scale_x_continuous(limits = c(-0.3, 0.3)) + scale_y_continuous(limits = c(-0.075, 0.125))
#show_regression(asset_bmark_yield_df, "bond", ticker) + scale_x_continuous(limits = c(-0.3, 0.3)) + scale_y_continuous(limits = c(-0.075, 0.125))
