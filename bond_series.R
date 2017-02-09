source('show_regression.R')

# PREAMBLE:  
#   I've been saving FactSet exports as csv files where the dates are in the format mm/dd/yyyy.  
#   The names of the files are the tickers in FactSet.
#   How to format upon importing according to http://stackoverflow.com/questions/13022299/specify-custom-date-format-for-colclasses-argument-in-read-table-read-csv
setClass('myDate')
setAs("character", "myDate", function(from) as.Date(from, format = "%m/%d/%Y"))

##############################
### BOND AND YIELD RETURNS ###

# Northfield, I suspect, uses SBABIG, the Citigroup index, for bond indices.  Unfortunately, these bonds don't have a yield return to correlate with,
# so I find myself using TRYAU10Y for Australian 10y (it seems to be a FactSet index, which as both yield and bond index.  

## If we want 90-day Aussie bank bill, use REFAU90DBA
#all_yield90d_df = read.csv("C://Temp//REFAU90DBA.csv", colClasses = c('myDate', 'numeric'))
#all_yield90d_returns = periodReturn(xts(all_yield90d_df[, 2], order.by = all_yield90d_df[, 1]), period = tolower(frequency), type = "log")

# Change of plans:  let's use the Citigroup bond index because it has more history, and it has a corresponding yield which we may find useful.
# I've exported the Aussie one (ticker SBABIG) into a csv file:


# bond_index = read.csv("C://Temp//SBABIG.csv", colClasses = c('myDate', 'numeric', 'numeric'))  # The Citigroup index
bond_index = read.csv("C://Temp//TRYAU10Y.csv", colClasses = c('myDate', 'numeric', 'numeric')) # Some 10Y Australian Bond Index I found in FactSet.
colnames(bond_index) = c('Date', 'bond', 'yield')

frequency = 'Monthly'

# Let bond index returns be the factor:
bond_index_no_na = filter(bond_index, !is.na(bond_index[, 2]))
all_bond_returns = periodReturn(xts(bond_index_no_na[, 2], order.by = bond_index_no_na[, 1]), period = tolower(frequency))

# Let bond yield log-returns be the factor:
all_yield10y_returns = periodReturn(xts(bond_index_no_na[, 3], order.by = bond_index_no_na[, 1]), period = tolower(frequency), type = "log")

#all_yield10y_returns_df = data.frame(date = index(all_yield10y_returns), yr = all_yield10y_returns[, 1])
#colnames(all_yield10y_returns_df) = c("date", "yr")
## ggplot(all_yield10y_returns_df, aes(date, yr)) + geom_point() + ggtitle("Time series of 10y yield log returns") + scale_y_log10()

## Actually, the link to the logreturn of the yields don't feel quite right...  (Turns out there are a couple of weird outliers....)

#bond_returns = all_bond_returns[paste(min_date, max_date, sep = "/")]
#yield10y_returns = all_yield10y_returns[paste(min_date, max_date, sep = "/")]


### BOND AND YIELD RETURNS ###
##############################


# Data exploration:
bond_yield_df = data.frame(date = index(all_bond_returns), bond = all_bond_returns[, 1], yield = all_yield10y_returns[, 1])
colnames(bond_yield_df) = c('Date', 'bond', 'yield')

# There are a few funny-looking outliers which are screwing up the regression, mainly due to some outrageous monthly bond index returns:
show_regression(bond_yield_df, "bond", "yield")

# Winsorizing the bond index returns gives us a better regression
x = bond_yield_df
x$bond = psych::winsor(x$bond, trim = 0.05)

show_regression(x, "bond", "yield")

# For the record, winsorizing the yield gives a higher R-squared (0.84) and doubles the F-score
x$yield = psych::winsor(x$yield, trim = 0.05)
show_regression(x, "bond", "yield")


###################
### CONCLUSION  ###
###################
#
#   If you're happy with monthly yields and the Citigroup general-purpose bond index, then SBABIG has a 91% correlation between index returns 
# and bond returns, and the straight line fit looks great.  No winsorizing is necessary.
#
#   However, if you need daily/weekly changes to yields, and if you absolutely need the bonds to be 10Y, then use TRYAU10Y-FDS.  If this is 
# the case, then winsorizing index returns to 5% is necessary to remove some outliers.