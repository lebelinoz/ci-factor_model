###########################################################################
## spc risk
###########################################################################
# Libraries
library(ggplot2)
library(PerformanceAnalytics) # <-- has a potentially useful Return.cumulative function
source('Functions.R') # <-- put all your functions in one place
source('Sql_Wrapper.R')
spc_ref_table = get_table_from_sql_CISMPRDSVR("SELECT * FROM PCI_REPORTING.dbo.t_spc_ref_portfolio")


# Preamble
max_date = as.Date('2016-10-31')
min_date = as.Date('2008-10-31')
benchmark_name = "MSCIWORLD"
frequency = "Monthly"
spc_id_list = c(90) # 93:98 # c(91, 113, 114) # c(91, 110, 111) #  

# Label the synthetic portfolios with spc_code:
spc_id_list = sort(spc_id_list)
pfolio_names = as.character( spc_ref_table[which(spc_ref_table$spc_id %in% spc_id_list),]$spc_code )


# Portfolio returns
pfolio_return = get_spc_xts_raw_total_returns(spc_id_list[1], frequency)
for (spc_id in spc_id_list[-1]) {
    pfolio_return = merge(pfolio_return, get_spc_xts_raw_total_returns(spc_id, frequency))
}
colnames(pfolio_return) = pfolio_names
pfolio_return = pfolio_return[which(index(pfolio_return) > min_date & index(pfolio_return) <= max_date),]

# Benchmark returns
bmark_return = get_benchmark_xts_returns(benchmark_name, "AUD", frequency)
colnames(bmark_return) = c(benchmark_name)
bmark_return = bmark_return[which(index(bmark_return) > min_date & index(bmark_return) <= max_date),]

# Some Performance Analytics stuff:
chart.RelativePerformance(pfolio_return, bmark_return, legend.loc = "topleft")
table.SFM(pfolio_return, bmark_return)
#SystematicRisk(pfolio_return, bmark_return)
TotalRisk(pfolio_return, bmark_return)

## Output our two returns arrays
#write.zoo(pfolio_return, "C:/Temp/pfolio_return.csv")
#write.zoo(bmark_return, "C:/Temp/bmark_return.csv")