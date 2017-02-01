###################################
###                             ###
### COMPUTE A FACTOR MODEL      ###
###                             ###
###################################

### PREAMBLE
source('./lib/Sql_Wrapper.R') # <-- for grabbing data straight from our SQL database
source('./lib/StockPlot.R') # <-- for plotting relative return charts
source('single_experiment_summary.R')

# Parameters:
benchmark_code = 'XJO'
frequency = "Monthly"
currency = "AUD"
min_date = as_date("2011-11-28") # as.Date("2011-12-30") # as.Date("2015-09-29") # 
max_date = as_date("2016-11-30")
test_date = as_date("2016-12-30")

###########################
###   EQUITY RETURNS    ###

### STEP 1: GET A LIST OF sec_id's (this is the part which will change every time)
sql_sec_id = paste("EXEC PCI_CORE.dbo.get_index_constituents @index_code  = '",
                   benchmark_code,
                   "', @at_date = '",
                   format(max_date, "%Y-%m-%d"),
                   "'",
                   sep = "")
secIdList <- get_table_from_sql_CISMPRDSVR(sql_sec_id)$sec_id

# secIdList = c(1572, 1676, 2790) # ANZ, BHP, TLS


### STEP 2: GET RETURN DATA FROM THE DATABASE AT THE DESIRED FREQUENCY
# Make a sql statement which looks like this:  EXEC PCI_REPORTING.dbo.get_performance_chart @sec_id_list = '1676,2604' , @currency='AUD', @frequency='Weekly'frequency <- "Weekly"
secIdList_AsString <- paste(secIdList, collapse = ",")
sql_returns = paste("EXEC PCI_REPORTING.dbo.get_performance_chart @sec_id_list = '",
                    secIdList_AsString,
                    "', @currency='",
                    currency,
                    "', @frequency='",
                    frequency,
                    "', @minimum_date_override = '",
                    format(min_date, "%Y-%m-%d"),
                    "'",
                    sep = "")
all_stock_returns_df <- get_table_from_sql_CISMPRDSVR(sql_returns)

# There is a "missing" column in all_stock_returns_df which we can ignore
all_stock_returns_df[, "missing"] <- list(NULL)

# Rename the columns to be the ticker, not the sec_id:
sql_column_names = paste("SELECT sec_id, REPLACE(sec_ticker,'.ASX','') AS [ticker] from PCI_CORE.dbo.t_Ref_Sec WHERE sec_id in (", secIdList_AsString, ")", sep = "")
column_names <- get_table_from_sql_CISMPRDSVR(sql_column_names)
for (i in 1:length(column_names[, 1])) {
    pair = column_names[i,]
    colnames(all_stock_returns_df)[which(colnames(all_stock_returns_df) == pair[, 1])] = toString(pair[, 2])
}

# Drop all columns where there is only data for less than 50% of the data 
# (eg. for 52-week correlations, stocks which IPO'd six months ago will look off-the-charts)
all_stock_returns_df = all_stock_returns_df[, colSums(is.na(all_stock_returns_df)) < length(all_stock_returns_df[, 1]) / 2]

# Drop T-1 row (remember that SQL's get_performance_chart always tacks on a T-1 data point which may not reflect a full period)
all_stock_returns_df = all_stock_returns_df[1:(length(all_stock_returns_df[, 1]) - 1),]

# Convert to xts
all_stock_returns = xts(all_stock_returns_df[, -1], all_stock_returns_df[, 1])

# Keep only returns between max_date and min_date
stock_returns = all_stock_returns[paste(min_date, max_date, sep = "/")]
###   EQUITY RETURNS    ###
###########################


###########################
###  BENCHMARK RETURNS  ###
all_bmark_returns = get_benchmark_xts_returns(benchmark_code, currency, frequency)
bmark_returns = all_bmark_returns[paste(min_date, max_date, sep = "/")]
###  BENCHMARK RETURNS  ###
###########################


###########################
###  FACTOR RETURNS     ###
# Change of plans:  let's use the Citigroup bond index because it has more history, and it has a corresponding yield which we may find useful.
# I've exported the Aussie one (ticker SBABIG) into a csv file:

# How to format upon importing according to http://stackoverflow.com/questions/13022299/specify-custom-date-format-for-colclasses-argument-in-read-table-read-csv
setClass('myDate')
setAs("character", "myDate", function(from) as.Date(from, format = "%m/%d/%Y"))
# bond_index = read.csv("C://Temp//SBABIG.csv", colClasses = c('myDate', 'numeric', 'numeric'))  # The Citigroup index
bond_index = read.csv("C://Temp//TRYAU10Y.csv", colClasses = c('myDate', 'numeric', 'numeric'))  # Some 10Y Australian Bond Index I found in FactSet.


# Let bond index returns be the factor:
bond_index_no_na = filter(bond_index, !is.na(bond_index[, 2]))
all_bond_returns = periodReturn(xts(bond_index_no_na[, 2], order.by = bond_index_no_na[, 1]), period = tolower(frequency))

# Let bond yield log-returns be the factor:
all_yield10y_returns = periodReturn(xts(bond_index[, 3], order.by = bond_index[, 1]), period = tolower(frequency), type = "log")

all_yield10y_returns_df = data.frame(date = index(all_yield10y_returns), yr = all_yield10y_returns[, 1])
colnames(all_yield10y_returns_df) = c("date", "yr")
# ggplot(all_yield10y_returns_df, aes(date, yr)) + geom_point() + ggtitle("Time series of 10y yield log returns") + scale_y_log10()

# Actually, the link to the logreturn of the yields don't feel quite right...  (Turns out there are a couple of weird outliers....)

bond_returns = all_bond_returns[paste(min_date, max_date, sep = "/")]
yield10y_returns = all_yield10y_returns[paste(min_date, max_date, sep = "/")]

all_yield90d_df = read.csv("C://Temp//REFAU90DBA.csv", colClasses = c('myDate', 'numeric'))
all_yield90d_returns = periodReturn(xts(all_yield90d_df[, 2], order.by = all_yield90d_df[, 1]), period = tolower(frequency), type = "log")
###  FACTOR RETURNS     ###
###########################


###########################
### FACTOR MODEL        ###

df = data.frame(
        ticker = character(),
        universe = character(),
        bmark = logical(),
        bond = logical(),
        yield = logical(),
        r.squared = numeric(),
        adj.r.squared = numeric(),
        F_value = numeric(),
        p_value = numeric(),
        intercept = numeric(),
        bmark_beta = numeric(),
        bond_beta = numeric(),
        yield_beta = numeric(),
        residual_error = numeric(),
        bmark_F_value = numeric(),
        bond_F_value = numeric(),
        yield_F_value = numeric(),
        bmark_p_value = numeric(),
        bond_p_value = numeric(),
        yield_p_value = numeric()
    )

#for (ticker in colnames(stock_returns)) {
    ticker = "ANZ"
    asset_returns = stock_returns[, ticker]
    asset_and_bmark_and_factors = data.frame(index(asset_returns), asset_returns[, 1], bmark_returns[, 1], bond_returns[, 1], yield10y_returns[, 1], row.names = NULL)
    colnames(asset_and_bmark_and_factors) = c("date", "asset", "bmark", "bond", "yield")
    df = rbind(df, single_experiment_summary(ticker, benchmark_code, include_bmark = TRUE, lm_object = lm(asset ~ bmark, data = asset_and_bmark_and_factors)))
    df = rbind(df, single_experiment_summary(ticker, benchmark_code, include_bond = TRUE, lm_object = lm(asset ~ bond, data = asset_and_bmark_and_factors)))
    df = rbind(df, single_experiment_summary(ticker, benchmark_code, include_yield = TRUE, lm_object = lm(asset ~ yield, data = asset_and_bmark_and_factors)))
    df = rbind(df, single_experiment_summary(ticker, benchmark_code, include_bmark = TRUE, include_bond = TRUE, lm_object = lm(asset ~ bmark + bond, data = asset_and_bmark_and_factors)))
    df = rbind(df, single_experiment_summary(ticker, benchmark_code, include_bmark = TRUE, include_yield = TRUE, lm_object = lm(asset ~ bmark + yield, data = asset_and_bmark_and_factors)))
    df = rbind(df, single_experiment_summary(ticker, benchmark_code, include_bond = TRUE, include_yield = TRUE, lm_object = lm(asset ~ bond + yield, data = asset_and_bmark_and_factors)))
    df = rbind(df, single_experiment_summary(ticker, benchmark_code, include_bmark = TRUE, include_bond = TRUE, include_yield = TRUE, lm_object = lm(asset ~ bmark + bond + yield, data = asset_and_bmark_and_factors)))
    cat("ticker =", ticker, "\n")
#}

raw_ANZ = asset_and_bmark_and_factors

flat_ANZ = data.frame(date = raw_ANZ$date, asset = raw_ANZ$asset, factor = raw_ANZ$bmark, factor_name = rep("bmark", dim(raw_ANZ)[1]))
flat_ANZ = rbind(flat_ANZ, data.frame(date = raw_ANZ$date, asset = raw_ANZ$asset, factor = raw_ANZ$bond, factor_name = rep("bond", dim(raw_ANZ)[1])))
flat_ANZ = rbind(flat_ANZ, data.frame(date = raw_ANZ$date, asset = raw_ANZ$asset, factor = raw_ANZ$yield, factor_name = rep("yield", dim(raw_ANZ)[1])))


x = ggplot(flat_ANZ, aes(asset, factor)) + geom_point()

ggplot(flat_ANZ, aes(asset, factor)) + geom_ + facet_wrap( ~ factor_name)