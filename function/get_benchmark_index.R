# Retrieve the benchmark daily index.
get_benchmark_index = function(bmark_code, currency) {
    all_bmark_returns = get_benchmark_xts_returns(bmark_code, currency, "daily")
    if (bmark_code == "MSCIWORLDG") {
        # MSCIWORLDG data is monthly pre 2001.
        all_bmark_returns = all_bmark_returns[paste(ymd("2001-01-01"), today(), sep = "/")]
    }
    bmark_returns_df = data.frame(index(all_bmark_returns), 1 + all_bmark_returns[, 1])
    colnames(bmark_returns_df) = c("date", "returns")
    bmark_returns_df = rbind(data.frame(date = c(previous_business_date_if_weekend(ymd("2000-12-31"))), returns = c(1)), bmark_returns_df)
    bmark_index_df = select(mutate(bmark_returns_df, index = cumprod(returns)), date, index)
    ggplot(bmark_index_df, aes(date, index)) + geom_line() + ggtitle(paste("benchmark = ", bmark_code))
    bmark_index = xts(bmark_index_df[, "index"], order.by = bmark_index_df[, "date"])
    return(bmark_index)
}