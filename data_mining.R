library(rattle)
library(gridExtra) # <-- required by rattle to create some charts
source('Sql_Wrapper.R')

##########################################################################
### SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL ##
currency = 'AUD'
sql_query = paste("EXEC PCI_CORE.dbo.rattle_beans '", currency, "'", sep = "")
watchlist = get_table_from_sql_CISMPRDSVR(sql_query)
### SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL ##
##########################################################################


# As a first experiment, let's concentrate on the watchlist as of 30 November 2011
# By this point, the relatively new O & F scores would have settled, and we have 12M returns to play with
watchlist_201511 = watchlist[which(watchlist$month_code == 'P2015M11'),c("ticker", "watchlist", "V", "O", "F", "return_12M")]
watchlist_201511 = watchlist_201511[which(!is.na(watchlist_201511[,"return_12M"])),]


rattle()



