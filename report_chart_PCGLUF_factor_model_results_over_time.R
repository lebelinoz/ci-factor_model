library(tidyverse)
library(lubridate)

bmark_code = "MSCIWORLDG"
pfolio_code = "PCGLUF"
watchlist_name = 'Global'
currency = "AUD"
yield_shock = 1

csv_filename = paste("C://Temp//portfolio_experiment_summary_useBmarkAndBond-", pfolio_code, "-", bmark_code, "-yld", 100 * yield_shock, "bps.csv", sep = "")
write.csv(df, csv_filename, row.names = FALSE)

df = read.csv(csv_filename)
df$start_date = as_date(df$start_date)
df$end_date = as_date(df$end_date)
df_new = mutate(df, version = "vs bond")

## Compare to the old methodology, where we regressed directly against the yield (log) returns:
old_csv_filename = paste("C://Temp//portfolio_experiment_summary-", pfolio_code, "-", bmark_code, "-yld", 100 * yield_shock, "bps.csv", sep = "")
df_old = read.csv(old_csv_filename)
df_old$start_date = as_date(df_old$start_date)
df_old$end_date = as_date(df_old$end_date)
df_old = mutate(df_old, version = "vs yield")

# Merge stuff 
df_old_and_new = rbind(df_new, df_old)


## Let's look at how changing the timeframe affects the outputs of the model:
#ggplot(df_old_and_new, aes(months, pfolio_return_delta_shock, colour = frequency)) + geom_line() + ggtitle("Next pfolio return if yield 100 bp:  model result by timespan and frequency") + facet_wrap( ~ version)
#ggplot(df_old_and_new, aes(months, bmark_return_delta_shock, colour = frequency)) + geom_line() + ggtitle("Next b'mark return if yield 100 bp:  model result by timespan and frequency") + facet_wrap( ~ version)
#ggplot(df_old_and_new, aes(months, pfolio_return_delta_shock - bmark_return_delta_shock, colour = frequency)) + geom_line() + ggtitle("Next pfolio - bmark return if yield 100 bp:  model result by timespan and frequency") + facet_wrap( ~ version)
