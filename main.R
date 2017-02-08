#######################
## PREAMBLE:
##  WARNING! The first few steps must be run in chunks for some reason (?).  Once you have a bmark_index, everything else ought to run smoothly.
source('./factor_model_maker.R')

# Raw Parameters for all experiment
bmark_code = "MSCIWORLDG"
pfolio_code = "PCGLUF"
currency = "AUD"

######################
## PORTFOLIO:
# This will be an array of sec_id, ticker and weight
#  sec_id:  will be passed to 'factor_model_maker' to collect stock returns.
#  ticker:  'factor_model_maker' returns data labelled by ticker, not sec_id.  We need to bring it back together.
#  weight:  the final step of the model does a weighted sum of the shocked stock returns to give a shocked pfolio return.

# TO DO:  Add a date restriction, so we can experiment on how well the model worked on historic portfolio weights.
sql_pfolio = paste(
     "SELECT sec.sec_id, REPLACE(sec.sec_ticker,'.ASX','') AS [ticker], sec.sec_name as [name], pfolio.[weight] FROM PCI_REPORTING.dbo.t_data_historic_port_weights pfolio INNER JOIN PCI_CORE.dbo.t_Ref_Sec sec ON pfolio.sec_id = sec.sec_id WHERE pfolio.acct_cd = "
    , pfolio_code
    , " AND pfolio.metric_date IN (SELECT MAX(metric_date) FROM PCI_REPORTING.dbo.t_data_historic_port_weights WHERE acct_cd = "
    , pfolio_code
    , " AND sec_id IS NOT NULL)"
    , sep = "'")
portfolio = get_table_from_sql_CISMPRDSVR(sql_pfolio)
# Note that, due to Charles River glitchiness, sum(portfolio$weight) sometimes add to something like 1.9 (instead of 0.95).  Remember to always
# divide final weighted-sum answers by the total weights.

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
bond_index = read.csv("C://Temp//US10YY-TU1.csv", colClasses = c('myDate', 'numeric', 'numeric')) # Some 10Y Australian Bond Index I found in FactSet.
colnames(bond_index) = c('date', 'junk', 'yield')

yield_index_df = bond_index[, c("date", "yield")]
ggplot(yield_index_df, aes(date, yield)) + geom_line() + ggtitle("factor = yield")
yield_index = xts(yield_index_df[, "yield"], order.by = yield_index_df[, "date"])


#####################
## TIMEFRAME
# For now, let's use 60-month timeframe ending at the end of the latest month.  Eventually, we can make this the dial which we can tweak.
freq = "M"
start_date = previous_business_date_if_weekend(EOMonth(today(), -61))
end_date = previous_business_date_if_weekend(EOMonth(today(), -1))
tf1 = timeframe(start_date = start_date, end_date = end_date, frequency = freq)


#####################
## SHOCK
# (1 = 100 bp because the yield series I got from FactSet is multiplied by 100)
yield_shock = 1

portfolio_experiment_summary = function(tf, yield_shock, portfolio, currency, bmark_index, yield_index) {

    #####################
    ## CREATE THE UNIVERSE FACTOR MODEL (ufm):

    ufm = factor_model_maker(tf, portfolio$sec_id, currency, bmark_index, yield_index)

    # We now have a model which predicts benchmark moves relative yield moves:

    bmark_intercept_rel_yield = ufm$bmark_yield.lm$coefficients[1]
    bmark_beta_rel_yield = ufm$bmark_yield.lm$coefficients[2]

    ## If you're interested, here are some details of that other model:
    #summary(ufm$bmark_yield.lm)
    #anova(ufm$bmark_yield.lm)

    #####################
    ## DO THE SHOCK
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

    # Add the individual shocks back to the original portfolio:
    portfolio_with_shocks = merge(portfolio, stock_forecast_returns, by = "ticker", all.x = TRUE)

    # ... and add a weighted_delta_shock column, turning missings into zeroes
    portfolio_with_shocks = mutate(portfolio_with_shocks, weighted_delta_shock = weight * delta_shock)
    portfolio_with_shocks$weighted_delta_shock[which(is.na(portfolio_with_shocks$weighted_delta_shock))] <- 0

    # take the weighted sum of the shocked returns to get a portfolio return:
    shocked_portfolio_return = sum(portfolio_with_shocks$weighted_delta_shock) / sum(portfolio_with_shocks$weight)

    return(shocked_portfolio_return)
}

portfolio_experiment_summary(tf1, yield_shock, portfolio, currency, bmark_index, yield_index)
