###################################
###                             ###
### COMPUTE RETURN CORRELATIONS ###
###                             ###
###################################

### PREAMBLE
source('Sql_Wrapper.R') # <-- for grabbing data straight from our SQL database
library(RColorBrewer) # <-- for the customised colour palette which I want to use in the heat map
library(gplots) # <-- if you want customised heatmaps, use heatmap.2 from the gplots package.

# Parameters:
portfolio_code = 'PCGLUF'
frequency = "Weekly"
currency = "Local"
old_way = TRUE # referring to the ordering of the columns/rows.  If true, let hierarchical clustering automatically group by euclidian distance.
min_date = as.Date("2012-01-13") # as.Date("2015-09-29") # 
max_date = as.Date("2017-01-13")


### STEP 1: GET A LIST OF sec_id's (this is the part which will change every time)
sql_sec_id = paste("select pos.sec_id from t_Ref_Positions_L pos inner join t_Ref_Portfolio pfolio on pos.portfolio_id = pfolio.id where pfolio.fund_code = '",
                   portfolio_code,
                   "' and pos.sec_id is not null",
                   sep = "")
secIdList <- get_table_from_sql_CISMPRDSVR(sql_sec_id)$sec_id
secIdList <- c(secIdList, 123456) # For fun, let's add the secId of the stock we're thinking of buying


### STEP 2: GET RETURN DATA FROM THE DATABASE AT THE DESIRED FREQUENCY
# Make a sql statement which looks like this:  EXEC PCI_REPORTING.dbo.get_performance_chart @sec_id_list = '1676,2604' , @currency='AUD', @frequency='Weekly'frequency <- "Weekly"
secIdList_AsString <- paste(secIdList, collapse = ",")
sql_returns = paste("EXEC PCI_REPORTING.dbo.get_performance_chart @sec_id_list = '",
                    secIdList_AsString,
                    "', @currency='",
                    currency,
                    "', @frequency='",
                    frequency,
                    "'",
                    sep = "")
raw_returns <- get_table_from_sql_CISMPRDSVR(sql_returns)

# There is a "missing" column in raw_returns which we can ignore
raw_returns[, "missing"] <- list(NULL)

# Keep only dates between max_date and min_date
raw_returns = raw_returns[which(as.Date(raw_returns$Date) >= min_date & as.Date(raw_returns$Date) <= max_date),]

# Reorder the columns
# The old way is to just heatmap.2 determine the order through hierarchical clustering.
# The new way is to value subset + country + ticker
if (old_way) {
    # Rename the columns to be the ticker, not the sec_id:
    sql_column_names = paste("SELECT sec_id, REPLACE(sec_ticker,'.ASX','') AS [ticker] from PCI_CORE.dbo.t_Ref_Sec WHERE sec_id in (", secIdList_AsString, ")", sep = "")
    column_names <- get_table_from_sql_CISMPRDSVR(sql_column_names)
    for (i in 1:length(column_names[, 1])) {
        pair = column_names[i,]
        colnames(raw_returns)[which(colnames(raw_returns) == pair[, 1])] = toString(pair[, 2])
    }
} else {
    if (1 == 1 | !exists("expanded_column_names")) # check for existence because the following query is slow (~twenty seconds)
        {
        sql_expanded_column_names = "EXEC PCI_REPORTING.dbo.get_universe"
        watchlist_data = unique(get_table_from_sql_CISMPRDSVR(sql_expanded_column_names))
        expanded_column_names = data.frame(
      sec_id = watchlist_data$sec_id,
      title = paste(watchlist_data$Ticker, substr(watchlist_data$ValueSubset, 0, 4), watchlist_data$Country, sep = ", "),
      ticker = watchlist_data$Ticker,
      value_subset = watchlist_data$ValueSubset,
      country = watchlist_data$Country
    )
        expanded_column_names = expanded_column_names[with(expanded_column_names, order(value_subset, country, ticker)),]
    }

    relevant_column_names = expanded_column_names[which(expanded_column_names$sec_id %in% secIdList),]

    # Reorder the columns
    raw_returns = raw_returns[c("Date", as.vector(relevant_column_names[, 1]))]

    # Rename the columns
    for (i in 1:length(relevant_column_names[, 1])) {
        pair = relevant_column_names[i,]
        colnames(raw_returns)[which(colnames(raw_returns) == pair[, 1])] = toString(pair[, 2])
    }
}


### STEP 3:  Compute correlations and show the heat map

# Drop all columns where there is only data for less than 50% of the data 
# (eg. for 52-week correlations, stocks which IPO'd six months ago will look off-the-charts)
raw_returns = raw_returns[, colSums(is.na(raw_returns)) < length(raw_returns[, 1]) / 2]

# Drop T-1 row (remember that SQL's get_performance_chart always tacks on a T-1 data point which may not reflect a full period)
raw_returns = raw_returns[1:(length(raw_returns[, 1]) - 1),]

# Compute the correlations and show the heatmap
correlation_matrix = cor(raw_returns[, 2:length(raw_returns)], use = "pairwise.complete.obs")
#heatmap(correlation_matrix, main = "Heatmap with scaling")

# Q1. Why isn't the diagonal perfectly white?
# A1. Because heatmap tries to do scaling.  Try scale = "none":
#heatmap(correlation_matrix, main = "heatmap with NO scaling", scale = "none")


# Q2. Can I put numbers *in* the heat map?
# A2. Yes.  Use heatmap.2 from gplots, which allows for heaps more control

# This is the colour scheme used in the example:
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
# And this is, I expect what the desired blue-white-red scheme (with correlation -1 blue, and correlation 0 white)
my_palette <- colorRampPalette(c("forestgreen", "white", "red"))(n = 299)
## With this, correlation 0 is blue, correlation 1 is red and correlation 0.5 is white.  Correlation < 0 stands out as 
#zero_correlation_colour <- "forestgreen"
#my_palette <- colorRampPalette(c("blue", "yellow", zero_correlation_colour, "white", "red"))(n = 299)
## (this last one isn't bad if we set zero_correlation_colour to blue or forestgreen...)

# For labeling purposes, round the values of the correlation matrix.  Cut off leading zeroes
# (so the map isn't inundated with "0." everywhere when just "." will do)
cor_format = function(x) {
    if (x == 1) {
        return("1")
    }
    else if (x == -1) {
        return("-1")
    }
    else if (x == 0) {
        return("0")
    }
    else if (x > 0 & x < 1) {
        my_number = round(x, digits = 1)
        if (my_number == 0) return(".0")
        return(substr(toString(my_number), 2, 3))
    }
    else if (x < 0) {
        return(paste("-", cor_format( - x), sep = ""))
    }
    else {
        return(toString(round(x, digits = 1)))
    }
}
cor_labels = apply(correlation_matrix, c(1, 2), cor_format)
diag(cor_labels) <- "" # don't bother labeling the diagonal, of course


# Show the chart:
chart_title = paste("Correlation for ", portfolio_code, " (", frequency, ", ", currency, ") ", min_date, " to ", max_date, sep = "")
dev.off()
heatmap.2(
  correlation_matrix
  , cellnote = cor_labels # cell labels look roughly the same as the actual numbers, except I've done some number formatting and I've left the diagonal blank
  , main = chart_title # heat map title
  , notecol = "black" # change font color of cell labels to black
  , density.info = "none" # turns off density plot inside color legend
  , trace = "none" # turns off trace lines inside the heat map
  , margins = c(12, 9) # widens margins around plot
  , col = my_palette # use our color palette defined earlier
  #,breaks=col_breaks     # enable color transition at specified limits # <-- this was an optional thing in the article... Ignoring for now
  , dendrogram = "none" # the dendogram is the flow chart showing closeness of the rows/columns as measured by hiearchical clustering.  Can be "row", "column", "both" or "none"
  , Colv = old_way # turn off column clustering
  , Rowv = old_way # turn off row clustering
  , key = FALSE # the key is the colour
  , lmat = rbind(4:3, 2:1) # the layout of the dendograms (2 & 3), key (4) and actual heatmap (1).
  , lhei = c(0.95, 4) # the height of the layout parts.  This is the smallest I seem to be able to get the non-heatmap part if there's a title in it
  , lwid = c(0.25, 4) # the width of the layout parts.  This is the smallest I seem to be able to get the left matter.
)

# Export the above chart to PNG in the user's R folder:
chart_png_filename = paste("C:\\Temp\\", chart_title, ".png", sep = "")
png(chart_png_filename, width = 850, height = 850)
heatmap.2(
  correlation_matrix
  , cellnote = cor_labels # cell labels look roughly the same as the actual numbers, except I've done some number formatting and I've left the diagonal blank
  , main = chart_title # heat map title
  , notecol = "black" # change font color of cell labels to black
  , density.info = "none" # turns off density plot inside color legend
  , trace = "none" # turns off trace lines inside the heat map
  , margins = c(12, 9) # widens margins around plot
  , col = my_palette # use our color palette defined earlier
  #,breaks=col_breaks     # enable color transition at specified limits # <-- this was an optional thing in the article... Ignoring for now
  , dendrogram = "none" # the dendogram is the flow chart showing closeness of the rows/columns as measured by hiearchical clustering.  Can be "row", "column", "both" or "none"
  , Colv = old_way # turn off column clustering
  , Rowv = old_way # turn off row clustering
  , key = FALSE # the key is the colour
  #,lmat=rbind(4:3,2:1)   # the layout of the dendograms (2 & 3), key (4) and actual heatmap (1).
  , lhei = c(0.95, 4) # the height of the layout parts.  This is the smallest I seem to be able to get the non-heatmap part if there's a title in it
  , lwid = c(0.25, 4) # the width of the layout parts.  This is the smallest I seem to be able to get the left matter.
)
dev.off()

## Export the same chart as above to PDF in the user's R folder:
#pdfFilename = paste(chart_title, "pdf", sep = ".")
#pdf(pdfFilename, paper = "a4r") # to export a chart to pdf, use this line first, then create the chart, then go dev.off()
#heatmap.2(
  #correlation_matrix
  #, cellnote = cor_labels # cell labels look roughly the same as the actual numbers, except I've done some number formatting and I've left the diagonal blank
  #, main = chart_title # heat map title
  #, notecol = "black" # change font color of cell labels to black
  #, density.info = "none" # turns off density plot inside color legend
  #, trace = "none" # turns off trace lines inside the heat map
  #, margins = c(12, 9) # widens margins around plot
  #, col = my_palette # use our color palette defined earlier
  ##,breaks=col_breaks     # enable color transition at specified limits # <-- this was an optional thing in the article... Ignoring for now
  #, dendrogram = "none" # the dendogram is the flow chart showing closeness of the rows/columns as measured by hiearchical clustering.  Can be "row", "column", "both" or "none"
  #, Colv = old_way # turn off column clustering
  #, Rowv = old_way # turn off row clustering
  #, key = FALSE # the key is the colour
  ##,lmat=rbind(4:3,2:1)   # the layout of the dendograms (2 & 3), key (4) and actual heatmap (1).
  #, lhei = c(0.95, 4) # the height of the layout parts.  This is the smallest I seem to be able to get the non-heatmap part if there's a title in it
  #, lwid = c(0.25, 4) # the width of the layout parts.  This is the smallest I seem to be able to get the left matter.
#)
#dev.off()