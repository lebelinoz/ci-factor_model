# Use U.S. 10y Treasury Yield
get_yield_index = function(yield_code = 'US10YY-TU1.US') {
    if (yield_code == "AU10Y") {
        yield_index = get_ticker_xts_t_data_fs_eps("TRYAU10Y-FDS.AU", "FG_YIELD")
    } else {
        yield_index = get_ticker_xts_t_data_fs_eps(yield_code, "FG_RETURN_ICS_LOCAL")
    }    
    colnames(yield_index) = c("yield")
    return(yield_index)
}