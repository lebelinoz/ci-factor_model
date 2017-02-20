# Use U.S. 10y Treasury Yield
get_yield_index = function(yield_code = 'US10YY-TU1.US') {
    ## Old, rubbishy way:
    #us10y_yield = read.csv(".//csv//US10YY-TU1.csv")
    #us10y_yield = us10y_yield[, -2]
    #colnames(us10y_yield) = c('date', 'yield')
    #us10y_yield$date = mdy(us10y_yield$date)
    #yield_index_df = us10y_yield
    #yield_index = xts(yield_index_df[, "yield"], order.by = yield_index_df[, "date"])

    yield_index = get_ticker_xts_t_data_fs_eps(yield_code, "FG_RETURN_ICS_LOCAL")
    colnames(yield_index) = c("yield")
    return(yield_index)
}