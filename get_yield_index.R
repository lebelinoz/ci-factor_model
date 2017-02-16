# Use U.S. 10y Treasury Yield
get_yield_index = function() {
    us10y_yield = read.csv(".//csv//US10YY-TU1.csv")
    us10y_yield = us10y_yield[, -2]
    colnames(us10y_yield) = c('date', 'yield')
    us10y_yield$date = mdy(us10y_yield$date)
    yield_index_df = us10y_yield
    yield_index = xts(yield_index_df[, "yield"], order.by = yield_index_df[, "date"])
    return(yield_index)
}