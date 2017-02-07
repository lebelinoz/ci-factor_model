source('./class/stock.returns.R')

# Create a universe_factor_model
factor_model_maker = function(timeframe, benchmark_code, portfolio_code, sec_id_list, currency = "AUD", bmark_index, bond_index, yield_index) {

    # Step 1:  translate the first five parameters (timeframe, benchmark_code, portfolio_code and sec_id_list) into an xts structure of returns.
    stock_returns = stock.returns(timeframe, benchmark_code, portfolio_code, sec_id_list, currency)

    ## (drop all columns from the xts structure) which don't have 50% data)
    drop_count = sum(sapply(stock_returns@xts_returns, function(x) sum(is.na(x)) > nrow(stock_returns@xts_returns) / 2))
    stock_returns@xts_returns = stock_returns@xts_returns[, - which(sapply(stock_returns@xts_returns, function(x) sum(is.na(x)) > nrow(stock_returns@xts_returns) / 2))]    
    if (drop_count > 0) warning(paste(drop_count, "stocks(s) were dropped from the analysis due to insufficient data:  less than 50% of returns were available"))
    
    return(stock_returns)
}