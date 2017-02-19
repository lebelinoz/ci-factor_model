#######################
## PREAMBLE:
##  WARNING! The source + library parts must be run separately for some reason (?).
source('./factor_model_maker.R')
source('./portfolio_experiment_summary.R')
source('./show_regression.R')
source('./get_benchmark_index.R')
source('./get_bond_index.R')
source('./get_yield_index.R')
library(reshape2)

# Raw Parameters for all experiment
bmark_code = "MSCIWORLDG"
pfolio_code = "PCGLUF"
currency = "AUD"
yield_shock = 1

# The factors:
bmark_index = get_benchmark_index(bmark_code, currency)
bond_index = get_bond_index()
yield_index = get_yield_index()

# Use 3-year weekly timeframe ending at the end of the latest month.
freq = "W"
end_date = previous_business_date_if_weekend(EOMonth(today(), -1))
start_date = previous_business_date_if_weekend(EOMonth(end_date, -36))
tf = timeframe(start_date = start_date, end_date = end_date, frequency = freq)

# Our "portfolio" is the benchmark.
portfolio = get_index_snapshot(bmark_code)
portfolio_unfiltered = portfolio

# When we try to do portfolio risk summaries with thousands of rows, we get all kinds of trouble involving large data sets.
# Try breaking it down into smaller problems first.  And since we don't want currency bias, filter on US stocks.
#portfolio = filter(portfolio_unfiltered, sec_exchange == "US")

industry_groups = unique(portfolio$industry_group)
this_industry_group = industry_groups[1]
this_portfolio = filter(portfolio, industry_group == this_industry_group)

## Helpful debug statements in case the 'portfolio_experiment_summary' step fails:
#factor_return_type = 'arithmetic'
#sec_id_list = this_portfolio$sec_id
#factor_index = bond_index
#ufm = factor_model_maker(tf, sec_id_list, currency, bmark_index, bond_index)

pes = portfolio_experiment_summary(tf, yield_shock, this_portfolio, currency, bond_index, bmark_index, yield_index)
main_df = mutate(pes$portfolio_experiment_summary, this_ig = this_industry_group)
breakdown_df = pes$portfolio_with_shocks


for (ig in 2:length(industry_groups)) {
    this_industry_group = industry_groups[ig]
    this_portfolio = filter(portfolio, industry_group == this_industry_group)
    cat('i = ', ig, ' is ', industry_groups[ig], ' and has ', nrow(this_portfolio), " stocks\n")
    pes = portfolio_experiment_summary(tf, yield_shock, this_portfolio, currency, bond_index, bmark_index, yield_index)
    main_df = rbind(main_df, mutate(pes$portfolio_experiment_summary, this_ig = this_industry_group))
    breakdown_df = rbind(breakdown_df, pes$portfolio_with_shocks)
}

# Interestingly, if we breakdown the MSCI AC World by Industry Group, the 'portfolio_experiment_summary' works, and 
# 'main_df' above gives use a breakdown of each Industry Group's estimated shock.  The sum of the answers comes very close to 
# the simple benchmark shock estimate:  as of end of January 2017, I get 3.80% for the MSCI AC World computed for each stock,
# and 3.75% for the benchmark