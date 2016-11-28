###########################################################################
## spc returns
###########################################################################
# Libraries
library(ggplot2)
source('Functions.R') # <-- put all your functions in one place
source('Sql_Wrapper.R')

# Preamble
max_date = as.Date('2016-10-31')
min_date = as.Date('2013-09-30')
spc_id_list =  c(50) # c(113, 91, 114) # c(110, 91, 111) # c(92, 91, 112) # 
pfolio_names = spc_id_list # c("ASX200_PROBM_candidates") # c("Red_Flag", "Control", "ex_Red_Flag")
title = "Monthly vs Daily"

###############################
## MONTHLY RETURNS & PRICES  ##
# Create a big xts crosstab of the benchmark returns in different currencies:
pfolio_return = get_spc_xts_returns(spc_id_list[1])
for (spc_id in spc_id_list[-1]) {
    pfolio_return = merge(pfolio_return, get_spc_xts_returns(spc_id))
}
colnames(pfolio_return) = pfolio_names

# Only keep the returns over the requested period (as determined by min_date and max_date, above)
pfolio_return = pfolio_return[which(index(pfolio_return) > min_date & index(pfolio_return) <= max_date),]

# Recreate normalised prices in an xts
pfolio_price = xts_price_from_returns(pfolio_return, min_date)
## MONTHLY RETURNS & PRICES  ##
###############################


###############################
##  DAILY RETURNS & PRICES   ##
pfolio_return_daily = get_spc_xts_raw_total_returns(spc_id_list[1])
for (spc_id in spc_id_list[-1]) {
    pfolio_return_daily = merge(pfolio_return_daily, get_spc_xts_raw_total_returns(spc_id))
}
colnames(pfolio_return_daily) = pfolio_names

# Only keep the returns over the requested period (as determined by min_date and max_date, above)
pfolio_return_daily = pfolio_return_daily[which(index(pfolio_return_daily) > min_date & index(pfolio_return_daily) <= max_date),]

# Recreate normalised prices in an xts
pfolio_price_daily = xts_price_from_returns(pfolio_return_daily, min_date)
##  DAILY RETURNS & PRICES   ##
###############################


# Plot the prices just given
# ...first by creating a dataframe
df = flatten_xts(pfolio_price, "price", "spc_id")
df_daily = flatten_xts(pfolio_price_daily, "price", "spc_id")

# ...next by ggplot on the dataframe
ggplot(df, aes(x = date, y = price, colour = spc_id, fill = spc_id)) + geom_line() + ggtitle(title)
ggplot(df_daily, aes(x = date, y = price, colour = spc_id, fill = spc_id)) + geom_line() + ggtitle(paste(title,"(daily)"))

## Do it again, but for returns
#dfr = flatten_xts(pfolio_return, "price", "spc_id")
#colnames(dfr["price"]) = c("return")
#ggplot(dfr, aes(x = date, y = price, fill = spc_id)) + geom_bar(stat = "identity", position = position_dodge()) + scale_x_date(date_labels = "%y", date_breaks = "1 year") + ggtitle(paste(title,"returns"))

tail(pfolio_price, 1)
tail(pfolio_price_daily, 1)