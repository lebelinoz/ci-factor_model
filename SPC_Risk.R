###########################################################################
## spc risk
###########################################################################
# Libraries
source('StockPlot.R') # <-- put all your functions in one place
source('Sql_Wrapper.R')
spc_ref_table = get_table_from_sql_CISMPRDSVR("SELECT * FROM PCI_REPORTING.dbo.t_spc_ref_portfolio")

# Preamble
min_date = as.Date('2015-11-29')
max_date = as.Date('2016-11-30')
benchmark_name = "XJO" # "MSCIAexJP" # "MSCIWORLD"
currency = "AUD"
frequency = "Monthly"
title_tail = paste("(", currency, ", ", frequency, ")", sep = "")
spc_id_list = 123:128 # c(117, 123, 140) # 132:139 # 132:139 # 93:98 # c(91, 113, 114) # c(91, 110, 111) #  

# Label the synthetic portfolios with spc_code:
spc_id_list = sort(spc_id_list)
pfolio_names = as.character(spc_ref_table[which(spc_ref_table$spc_id %in% spc_id_list),]$spc_code)
pfolio_names = gsub("ASIAN_WATCHLIST_", "", pfolio_names)
pfolio_names = gsub("XJO_PMAN_WORST20pc", "PROBM_20pc_red_flags", pfolio_names)
pfolio_names = gsub("XJO_PMAN_EX_WORST20pc", "Basket_of_safe_stocks_excludes_PROBM_20pc", pfolio_names)
pfolio_names = gsub("DOM_RL1_", "", pfolio_names)

# Portfolio returns
pfolio_return = get_spc_xts_returns(spc_id_list[1]) # get_spc_xts_raw_total_returns(spc_id_list[1], currency, frequency)
for (spc_id in spc_id_list[-1]) {
    pfolio_return = merge(pfolio_return, get_spc_xts_returns(spc_id))
}
colnames(pfolio_return) = pfolio_names
pfolio_return = pfolio_return[which(index(pfolio_return) > min_date & index(pfolio_return) <= max_date),]

# Benchmark returns
bmark_return = get_benchmark_xts_returns(benchmark_name, currency, frequency)
colnames(bmark_return) = c(benchmark_name)
bmark_return = bmark_return[which(index(bmark_return) > min_date & index(bmark_return) <= max_date),]

# Prices
pfolio_price = xts_price_from_returns(pfolio_return, min_date)
bmark_price = xts_price_from_returns(bmark_return, min_date)


#probm_price_plot = price_plot(pfolio_price, bmark_price, "ASX 200 and Domestic Stalwarts")
#probm_rel_plot = rel_plot(pfolio_return[, 1:3], bmark_return, paste("ASX 200 and Domestic Stalwarts relative actual ASX 200", title_tail))
#probm_rel_plot2 = rel_plot(pfolio_return[, 1:2], pfolio_return[,3], paste("ASX 200 and Domestic Stalwarts relative spc ASX 200", title_tail))


# Price plots
#msci_price_plot      = price_plot(pfolio_price[, 1], bmark_price, paste("MSCI Asia ex Jp computed by CAPS * and * Actual MSCI Asia ex Jp", title_tail))
#watchlist_price_plot = price_plot(pfolio_price[, 2], pfolio_price[, 1], paste("Asian Watchlist and MSCI AC Asia ex Jp", title_tail))
#valueSubset_price_plot = price_plot(pfolio_price[, 3:8], pfolio_price[, 2], paste("Value Subsets * and * Asian Watchlist", title_tail))

## Rel plots
rel_plot = rel_plot(pfolio_return, bmark_return, paste("Domestic RL1 stocks relative ASX 200", title_tail))
rel_plot
#watchlist_rel_plot = rel_plot(pfolio_return[, 2], pfolio_return[, 1], paste("Asian Watchlist vs MSCI Asia ex Jp", title_tail))
#valueSubset_rel_plot = rel_plot(pfolio_return[, 3:8], pfolio_return[, 2], paste("Value Subsets vs Asian Watchlist", title_tail))
#valueSubset_rel_plot2 = rel_plot(pfolio_return[, c(3,5,6,8)], pfolio_return[, 2], paste("Value Subsets ex Turnaround & BLE vs Asian Watchlist", title_tail))
#valSubs_rel_MSCI_plot = rel_plot(pfolio_return[, 3:8], pfolio_return[, 1], paste("Asian Watchlist Value Subsets vs MSCI Asia ex Jp", title_tail))

# Show all plots:
# dev.set(3)
#probm_price_plot
#probm_rel_plot
#probm_rel_plot2

#msci_price_plot
#watchlist_price_plot
#valueSubset_price_plot
#msci_rel_plot
#watchlist_rel_plot
#valueSubset_rel_plot
#valueSubset_rel_plot2
#valSubs_rel_MSCI_plot # limits = c(0, 0.5)

# Output on 29/11/2016
#valSubs_rel_MSCI_plot + scale_y_continuous(breaks = c(NA, 0.95, 1.0, 1.05, 1.1, NA), limits = c(0.92, 1.12))
#watchlist_rel_plot + scale_y_continuous(breaks = c(NA, 0.95, 1.0, 1.05, 1.1, NA), limits = c(0.92, 1.12))

#dfRel = flatten_xts(Return.relative(pfolio_return, bmark_return), "rel_return", "spc_id")
#ggplot(dfRel, aes(x = date, y = rel_return, colour = spc_id, fill = spc_id)) + geom_line(size = 1) + ggtitle(paste(title,"relative return"))
#chart.RelativePerformance(pfolio_return, bmark_return, legend.loc = "bottomleft")

# Some Performance Analytics stuff:

final_table = table.SFM(pfolio_return, bmark_return)
colnames(final_table) = pfolio_names

# Add Sharpe ratio, Max drawdown, Jensen's alpha, CalmarRatio
final_table = rbind(final_table, SharpeRatio(pfolio_return))
final_table = rbind(final_table, maxDrawdown(pfolio_return))
final_table = rbind(final_table, CAPM.jensenAlpha(pfolio_return, bmark_return))
final_table = rbind(final_table, CalmarRatio(pfolio_return))

write.csv(final_table, file = "C:\\Temp\\R_outputs_for_DOM_RL1_ValueSubsets_relative_XJO_12m.csv")
# table.SFM(pfolio_return, pfolio_return[,3])
#TotalRisk(pfolio_return, bmark_return)

### Output our two returns arrays
#write.zoo(pfolio_return, "C:/Temp/pfolio_return.csv")
#write.zoo(bmark_return, "C:/Temp/bmark_return.csv")