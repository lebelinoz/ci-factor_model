## Assume main_AU.R has just been run.
## So bmark_index and bond_index should be Australian.

## Chart the benchmark and bond:
#ggplot(setNames(data.frame(date = index(bmark_index), bmark = bmark_index[, 1]), c("date", "bmark")), aes(date, bmark)) + geom_line() + ggtitle(paste("benchmark = ", bmark_code, " (", currency, ")", sep = ""))
#ggplot(setNames(data.frame(date = index(bond_index), bond = bond_index[, 1]), c("date", "bond")), aes(date, bond)) + geom_line() + ggtitle("bond = Citigroup U.S. Bond Index")

bmark_monthly_returns = monthlyReturn(bmark_index)
bond_monthly_returns = monthlyReturn(bond_index)

bmark_df = setNames(data.frame(date = (index(bmark_monthly_returns)), bmark = bmark_monthly_returns[, 1]), c("date", "bmark"))
bond_df = setNames(data.frame(date = (index(bond_monthly_returns)), bond = bond_monthly_returns[, 1]), c("date", "bond"))
rownames(bmark_df) = NULL
rownames(bond_df) = NULL

num_years = 3
days_diff = num_years * 366
min_rows = num_years * 12 - 3

# BENCHMARK vs BOND:
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
#ggplot(bmark_bond_ts, aes(end_date, beta)) + geom_line() + ggtitle(paste(bmark_code, " vs Bond:  Beta", sep=""))
#ggplot(bmark_bond_ts, aes(end_date, r.squared)) + geom_line() + ggtitle(paste(bmark_code, " vs Bond:  R-squared", sep = ""))

## Combine the two onto one chart:
#ggplot(bmark_bond_ts, aes(end_date, beta)) + geom_line() + ggtitle("ASX 200 vs Bond:  Beta")
#ggplot(bmark_bond_ts, aes(end_date, r.squared)) + geom_line() + ggtitle("ASX 200 vs Bond:  R-squared")

melted_ts_combo_au = reshape2::melt(bmark_bond_ts, id.vars = c("start_date", "end_date"), measure.vars = c("beta", "r.squared"), variable.name = "metric")

ggplot(melted_ts_combo_au, aes(end_date, value)) +
    geom_line() +
    ggtitle("ASX 200 vs Aus Bond Index:  Beta & R-squared (3y monthly rolling)") +
    facet_wrap( ~ metric, ncol = 1, scales = "free_y") +
    theme(legend.position = "bottom") +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years")
