#######################
## PREAMBLE:
##  WARNING! The first few steps must be run in chunks for some reason (?).  Once you have a bmark_index, everything else ought to run smoothly.
source('./preamble.R')
source('./portfolio_experiment_summary.R')

# Raw Parameters for all experiment
bmark_code = "XJO"
pfolio_code = "PCHEST"
watchlist_name = 'Domestic'
currency = "AUD"

######################
## BENCHMARK, BOND & YIELD:
# The retrieved data is daily, and 'factor_model_maker' will make it into returns with an appropriate frequency.
bmark_index = get_benchmark_index(bmark_code, currency)
ggplot(setNames(data.frame(date = index(bmark_index), index = bmark_index[, 1]), c("date", "bmark")), aes(date, bmark)) + geom_line() + ggtitle(paste("benchmark = ", bmark_code))

# Let's use Citigroup USBIG Treasury (bond index only...  the yield data is monthly)
bond_index = get_bond_index("SBABIG.ASX")
ggplot(setNames(data.frame(date = index(bond_index), bond = bond_index[, 1]), c("date", "bond")), aes(date, bond)) + geom_line() + ggtitle("bond = Citigroup Australian Broad Investment-Grade Bond Index")

# Use U.S. 10y Treasury Yield
yield_index = get_yield_index("AU10Y")
ggplot(setNames(data.frame(date = index(yield_index), yield = yield_index[, 1]), c("date", "yield")), aes(date, yield)) + geom_line() + ggtitle("yield = Aus Benchmark Bond - 10 Year yield to maturity", subtitle = "FactSet ticker TRYAU10Y-FDS")


# see 'bond_series.R' for charts to convince you of the high correlation between this bond and this yield

#####################
## SHOCK
# (1 = 100 bp because the yield series I got from FactSet is multiplied by 100)
yield_shock = 1


#####################
## PORTFOLIOS AND WATCHLIST:
portfolio_PCHEST = get_portfolio(pfolio_code)


#####################
## TIMEFRAME
# For now, let's use 3-year weekly timeframe ending at the end of the latest month.
freq = "W"
#start_date = previous_business_date_if_weekend(EOMonth(today(), -37))
end_date =  previous_business_date_if_weekend(today()-1) #  previous_business_date_if_weekend(EOMonth(today(), -1))
start_date = previous_business_date_if_weekend(end_date - 365*3)
tf1 = timeframe(start_date = start_date, end_date = end_date, frequency = freq)

# The portfolio experiment summary gives us the benchmark and portfolio shocked returns:
pes_portfolio_PCHEST = portfolio_experiment_summary(tf1, yield_shock, portfolio_PCHEST, currency, bond_index, bmark_index, yield_index)
df_portfolio_PCHEST = pes_portfolio_PCHEST$portfolio_experiment_summary
stock_summary_pfolio_PCHEST = pes_portfolio_PCHEST$stock_factor_models


##################################################################################################
## THE FOLLOWING BIT NEEDS TO BE RUN ONLY ONCE, THEN WE CAN CHART BASED ON THE .csv OUTPUT FILE

# Let's look at the history of portfolio shocks:
df = df_portfolio_PCHEST

# Time series experiment
for (i in 6:60) {
    cat("i = ", i, "\n")
    this_end_date = previous_business_date_if_weekend(EOMonth(end_date, -1))
    this_start_date = previous_business_date_if_weekend(EOMonth(this_end_date, -i))

    for (this_freq in c("D", "W", "M")) {
        this_tf = timeframe(start_date = this_start_date, end_date = this_end_date, frequency = this_freq)
        portfolio_snapshot = portfolio_PCHEST = get_portfolio(pfolio_code, this_end_date)
        pes = portfolio_experiment_summary(this_tf, yield_shock, portfolio_snapshot, currency, bond_index, bmark_index, yield_index)
        df = rbind(df, pes$portfolio_experiment_summary)
    }
}

df = df[-1,]
rownames(df) = NULL



# This took a few minutes:  let's export to csv and pick it up later if necessary
csv_filename = paste(".//csv//portfolio_experiment_summary_useBmarkAndBond-", pfolio_code, "-", bmark_code, "-yld", 100 * yield_shock, "bps.csv", sep = "")
write.csv(df, csv_filename, row.names = FALSE)

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
stock_returns = stock.returns(tf1, sec_id_list = portfolio_PCGLUF$sec_id)@xts_returns

ticker = "BHP"
asset_returns = stock_returns[, ticker]
index(asset_returns) = as_date(index(asset_returns))

# Make sure all dates coincide:
bond_returns = bond_returns[which(index(bond_returns) %in% index(yield_returns)),]
bmark_returns = bmark_returns[which(index(bmark_returns) %in% index(bond_returns)),]
bond_returns = bond_returns[which(index(bond_returns) %in% index(bmark_returns)),]
bmark_returns = bmark_returns[which(index(bmark_returns) %in% index(yield_returns)),]
yield_returns = yield_returns[which(index(yield_returns) %in% index(asset_returns)),]
bmark_returns = bmark_returns[which(index(bmark_returns) %in% index(asset_returns)),]
bond_returns = bond_returns[which(index(bond_returns) %in% index(asset_returns)),]
yield_returns = yield_returns[which(index(yield_returns) %in% index(asset_returns)),]
asset_returns = asset_returns[which(index(asset_returns) %in% index(bmark_returns)),]

asset_bmark_yield_df = data.frame(date = index(bmark_returns), asset = asset_returns, bmark = bmark_returns[, 1], bond = bond_returns[, 1], yield = yield_returns[, 1])
rownames(asset_bmark_yield_df) = NULL
colnames(asset_bmark_yield_df) = c("date", ticker, "bmark", "bond", "yield")

## Show charts:
#show_regression(asset_bmark_yield_df, "bmark", ticker)
show_regression(asset_bmark_yield_df, "bmark", "yield")
show_regression(asset_bmark_yield_df, "bond", "yield")
#show_regression(asset_bmark_yield_df, "yield", ticker)
#show_regression(asset_bmark_yield_df, "bond", ticker)

## Show three charts with the same scale:
#show_regression(asset_bmark_yield_df, "bmark", ticker) + scale_x_continuous(limits = c(-0.3,0.3)) + scale_y_continuous(limits = c(-0.075, 0.125))
#show_regression(asset_bmark_yield_df, "yield", "bmark") + scale_x_continuous(limits = c(-0.3, 0.3)) + scale_y_continuous(limits = c(-0.075, 0.125))
#show_regression(asset_bmark_yield_df, "yield", ticker) + scale_x_continuous(limits = c(-0.3, 0.3)) + scale_y_continuous(limits = c(-0.075, 0.125))
#show_regression(asset_bmark_yield_df, "bond", ticker) + scale_x_continuous(limits = c(-0.3, 0.3)) + scale_y_continuous(limits = c(-0.075, 0.125))
