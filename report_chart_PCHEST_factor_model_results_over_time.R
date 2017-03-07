# THIS CODE MERELY REPORTS THE OUTPUT OF THE LONG EXPERIMENT RUN IN 'main_AusReport.R':
library(tidyverse)
library(lubridate)

timeseries_experiment = function(bmark_code, pfolio_code, yield_shock) {
  csv_filename = paste(".//csv//portfolio_experiment_summary_useBmarkAndBond-", pfolio_code, "-", bmark_code, "-yld", 100 * yield_shock, "bps.csv", sep = "")  
  df_new = read.csv(csv_filename)
  df_new$start_date = ymd(df_new$start_date)
  df_new$end_date = ymd(df_new$end_date)
  return(df_new)
}

df_ts_experiment = timeseries_experiment("XJO", "PCHEST", 1)
head(df_ts_experiment, 5)

df_ts_experiment = mutate(df_ts_experiment, pfolio_return_rel_bmark = pfolio_return_delta_shock - bmark_return_delta_shock)

ggplot(df_ts_experiment, aes(months, pfolio_return_rel_bmark, colour = frequency)) + geom_line() + ggtitle("Next pfolio rel return if yield +100 bp:  model result by timespan and frequency")

