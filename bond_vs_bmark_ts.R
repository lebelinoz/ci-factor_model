source('./lib/Sql_Wrapper.R')
source('./get_benchmark_index.R')
source('./get_bond_index.R')
source('./get_yield_index.R')

bmark_code = "MSCIWORLDG"
sp500_code = "SP500"
currency = "Local"

bmark_index = get_benchmark_index(bmark_code, currency)
sp500_index = get_benchmark_index(sp500_code, currency)
bond_index = get_bond_index()

## Chart the benchmark and bond:
#ggplot(setNames(data.frame(date = index(bmark_index), bmark = bmark_index[, 1]), c("date", "bmark")), aes(date, bmark)) + geom_line() + ggtitle(paste("benchmark = ", bmark_code, " (", currency, ")", sep = ""))
#ggplot(setNames(data.frame(date = index(sp500_index), bmark = sp500_index[, 1]), c("date", "bmark")), aes(date, bmark)) + geom_line() + ggtitle(paste("benchmark = ", "S&P500 (in USD)", sep = ""))
#ggplot(setNames(data.frame(date = index(bond_index), bond = bond_index[, 1]), c("date", "bond")), aes(date, bond)) + geom_line() + ggtitle("bond = Citigroup U.S. Bond Index")

bmark_returns = monthlyReturn(bmark_index)
sp500_returns = monthlyReturn(sp500_index)
bond_returns = monthlyReturn(bond_index)

bmark_df = setNames(data.frame(date = index(bmark_returns), bmark = bmark_returns[, 1]), c("date", "bmark"))
sp500_df = setNames(data.frame(date = index(sp500_returns), bmark = sp500_returns[, 1]), c("date", "sp500"))
bond_df = setNames(data.frame(date = index(bond_returns), bond = bond_returns[, 1]), c("date", "bond"))

num_years = 4
days_diff = num_years * 366
min_rows = num_years * 12 - 3

# MSCIWORLDG vs BOND:
bmark_bond_df = merge(bmark_df, bond_df, by = "date")
bmark_bond_ts = data.frame(start_date = character(), end_date = character(), beta = numeric(), r.squared = numeric())
for (end_date in sort(bmark_bond_df$date, decreasing = TRUE)) {
    end_date = as_date(end_date) # Strangely, we get a "POXITs" error if we don't do this.
    start_date = as_date(end_date - days_diff)
    this.df = filter(bmark_bond_df, date > start_date, date <= end_date)
    if (nrow(this.df) >= min_rows) {
        this.lm = lm(bmark ~ bond, this.df)
        beta = this.lm$coefficients[2]
        r.squared = summary(this.lm)$r.squared
        bmark_bond_ts = rbind(data.frame(start_date = start_date, end_date = end_date, beta = beta, r.squared = r.squared), bmark_bond_ts)
    }
}
rownames(bmark_bond_ts) = NULL
ggplot(bmark_bond_ts, aes(end_date, beta)) + geom_line() + ggtitle(paste(bmark_code, " vs Bond:  Beta", sep=""))
ggplot(bmark_bond_ts, aes(end_date, r.squared)) + geom_line() + ggtitle(paste(bmark_code, " vs Bond:  R-squared", sep = ""))


# But wait, S&P 500 and MSCIWORLDG are highly correlated during the 15 years they crossover:
msci_sp500_df = merge(bmark_df, sp500_df, by = "date")
msci_sp500_ts = data.frame(start_date = character(), end_date = character(), beta = numeric(), r.squared = numeric())
for (end_date in sort(msci_sp500_df$date, decreasing = TRUE)) {
    end_date = as_date(end_date) # Strangely, we get a "POXITs" error if we don't do this.
    start_date = as_date(end_date - days_diff)
    this.df = filter(msci_sp500_df, date > start_date, date <= end_date)
    if (nrow(this.df) >= min_rows) {
        this.lm = lm(bmark ~ sp500, this.df)
        beta = this.lm$coefficients[2]
        r.squared = summary(this.lm)$r.squared
        msci_sp500_ts = rbind(data.frame(start_date = start_date, end_date = end_date, beta = beta, r.squared = r.squared), msci_sp500_ts)
    }
}
rownames(msci_sp500_ts) = NULL
ggplot(msci_sp500_ts, aes(end_date, beta)) + geom_line() + ggtitle(paste(bmark_code, " vs S&P 500:  Beta", sep = ""))
ggplot(msci_sp500_ts, aes(end_date, r.squared)) + geom_line() + ggtitle(paste(bmark_code, " vs S&P 500:  R-squared", sep = ""))

# SP500 vs BOND:
sp500_bond_df = merge(sp500_df, bond_df, by = "date")
sp500_bond_ts = data.frame(start_date = character(), end_date = character(), beta = numeric(), r.squared = numeric())
for (end_date in sort(sp500_bond_df$date, decreasing = TRUE)) {
    end_date = as_date(end_date) # Strangely, we get a "POXITs" error if we don't do this.
    start_date = as_date(end_date - days_diff)
    this.df = filter(sp500_bond_df, date > start_date, date <= end_date)
    if (nrow(this.df) >= min_rows) {
        this.lm = lm(sp500 ~ bond, this.df)
        beta = this.lm$coefficients[2]
        r.squared = summary(this.lm)$r.squared
        sp500_bond_ts = rbind(data.frame(start_date = start_date, end_date = end_date, beta = beta, r.squared = r.squared), sp500_bond_ts)
    }
}
rownames(sp500_bond_ts) = NULL
ggplot(sp500_bond_ts, aes(end_date, beta)) + geom_line() + ggtitle(paste(sp500_code, " vs Bond:  Beta", sep = ""))
ggplot(sp500_bond_ts, aes(end_date, r.squared)) + geom_line() + ggtitle(paste(sp500_code, " vs Bond:  R-squared", sep = ""))


# Combine the two onto one chart:
ts_combo = rbind(mutate(bmark_bond_ts, benchmark = "MSCI"), mutate(sp500_bond_ts, benchmark = "S&P500"))
ggplot(ts_combo, aes(end_date, beta, colour = benchmark)) + geom_line() + ggtitle("Benchmarks vs Bond:  Beta")
ggplot(ts_combo, aes(end_date, r.squared, colour = benchmark)) + geom_line() + ggtitle("Benchmarks vs Bond:  R-squared")

melted_ts_combo = reshape2::melt(ts_combo, id.vars = c("start_date", "end_date", "benchmark"), measure.vars = c("beta", "r.squared"), variable.name = "metric")
ggplot(melted_ts_combo, aes(end_date, value, colour = benchmark)) + geom_line() + ggtitle("Benchmarks vs Bond:  R-squared") + facet_wrap(~metric) + theme(legend.position="bottom")
