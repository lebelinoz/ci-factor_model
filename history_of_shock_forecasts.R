#######################
## PREAMBLE:
##  WARNING! The first few steps must be run in chunks for some reason (?).  Once you have a bmark_index, everything else ought to run smoothly.
source('./preamble.R')
source('./factor_model_maker.R')
source('./portfolio_experiment_summary.R')

# Raw Parameters for all experiment
bmark_code = "MSCIWORLDG"
pfolio_code = "PCGLUF"
currency = "AUD"
yield_shock = 1

###########################
### HARD PART:  Only run this once, and it saves to a csv file

## The factors:
#bmark_index = get_benchmark_index(bmark_code, currency)
#bond_index = get_bond_index()
#yield_index = get_yield_index()

## Use 3-year weekly timeframe ending at the end of the latest month.
#freq = "W"
#end_date = previous_business_date_if_weekend(EOMonth(today(), -1))
#start_date = previous_business_date_if_weekend(EOMonth(end_date, -36))
#tf = timeframe(start_date = start_date, end_date = end_date, frequency = freq)

## Get the portfolio snapshot at the time
#portfolio = get_portfolio(pfolio_code, end_date)
#pes_portfolio = portfolio_experiment_summary(tf, yield_shock, portfolio, currency, bond_index, bmark_index, yield_index)
#main_df = mutate(pes_portfolio$portfolio_experiment_summary, start_date = start_date, end_date = end_date)

## Now repeat the experiment for every EOMonth over the last five years (actually, I tried 10 years and it conked out on month 97)
#for (i in 1:120) {
    #cat("i = ", i, "\n")
    #end_date = previous_business_date_if_weekend(EOMonth(end_date, -1))
    #start_date = previous_business_date_if_weekend(EOMonth(end_date, -36))
    #tf = timeframe(start_date = start_date, end_date = end_date, frequency = freq)
    #portfolio = get_portfolio(pfolio_code, end_date)
    #pes_portfolio = portfolio_experiment_summary(tf, yield_shock, portfolio, currency, bond_index, bmark_index, yield_index)
    #this_df = mutate(pes_portfolio$portfolio_experiment_summary, start_date = start_date, end_date = end_date)
    #main_df = rbind(main_df, this_df)
#}

### The above for-loop runs for 2+ minutes.  Save the outputs.
#csv_filename = paste( paste(".//csv//history_of_factor_shocks", pfolio_code, bmark_code, currency, sep = "-"), ".csv", sep = "")
#write.csv(main_df, csv_filename, row.names = FALSE)

#################################
## THE FOLLOWING ASSUMES THE CSV FILE HAS BEEN SAVED
csv_filename = paste( paste(".//csv//history_of_factor_shocks", pfolio_code, bmark_code, currency, sep = "-"), ".csv", sep = "")

df = read.csv(csv_filename)
df$start_date = as_date(df$start_date)
df$end_date = as_date(df$end_date)
df = df[,-3] # The third column is frequency, which screws up the melting

melted_df = reshape2::melt(df, id.vars = c("start_date", "end_date"))
filtered_melted_df = filter(melted_df, variable %in% c("pfolio_return_delta_shock_unadjusted_by_weighted", "pfolio_return_delta_shock", "bmark_return_delta_shock"))

ggplot(filtered_melted_df, aes(end_date, value, colour = variable)) + geom_line() + ggtitle(paste(pfolio_code, "How shock forecasts changed over time")) + theme(legend.position="bottom")

