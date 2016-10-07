#####################################
## Compute contributions to return ##
#####################################

frequency = "Monthly"
currency  = "AUD"
min_date = as.Date("2000-06-01") # as.Date("1980-01-01")
max_date = as.Date("2016-09-30")

### PREAMBLE
library(RODBC)          # <-- for grabbing data straight from our SQL database
conn <- odbcConnect(dsn="CISMPRDSVR")

### STEP 1: GET A LIST OF sec_id's (this is the part which will change every time)
secIdList <- c(1572,1676) # BHP & ANZ

### STEP 2: GET RETURN DATA FROM THE DATABASE AT THE DESIRED FREQUENCY
# Make a sql statement which looks like this:  EXEC PCI_REPORTING.dbo.get_performance_chart @sec_id_list = '1676,2604' , @currency='AUD', @frequency='Weekly'frequency <- "Weekly"
secIdList_AsString <- paste(secIdList, collapse=",")
sql_returns = paste("EXEC PCI_REPORTING.dbo.get_performance_chart @sec_id_list = '",
                    secIdList_AsString,
                    "', @currency='",
                    currency,
                    "', @frequency='",
                    frequency,
                    "'",
                    sep="")
raw_returns <- sqlQuery(conn, sql_returns)

# There is a "missing" column in raw_returns which we can ignore
raw_returns[,"missing"] <- list(NULL)

# Keep only dates between max_date and min_date
raw_returns = raw_returns[which(as.Date(raw_returns$Date) >= min_date & as.Date(raw_returns$Date) <= max_date),]

# Rename the columns to be the ticker, not the sec_id:
sql_column_names = paste("SELECT sec_id, REPLACE(sec_ticker,'.ASX','') AS [ticker] from PCI_CORE.dbo.t_Ref_Sec WHERE sec_id in (", secIdList_AsString, ")" , sep="")
column_names <- sqlQuery(conn, sql_column_names)
for (i in 1:length(column_names[,1]))
{
  pair = column_names[i,]
  colnames(raw_returns)[which(colnames(raw_returns) == pair[,1])] = toString(pair[,2])
}

# Close the connection:  we're done with SQL
odbcClose(conn)

# Export the returns table to a form I can play with at home.
write.csv(raw_returns, "raw_returns.csv", row.names = FALSE)
  
