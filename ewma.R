## Experiment with Exponentially Weighted Moving Average (ewma)
source('./preamble.R')

ewma = function(lambda, n) { return((1 - lambda) * (lambda ^ n)) }

try_this_lambda = function(all_factor1_returns, all_factor2_returns, factor_name, lambda = NULL) {

    # Use 3-year weekly (=3 * 52 = 156 weeks) timeframe ending at the end of the latest week.
    end_date = EOWeek(today(), -1) # previous_business_date_if_weekend(EOMonth(today(), -1)) # previous_business_date_if_weekend(ymd("2016-10-31")) # previous_business_date_if_weekend(EOMonth(today(), -1)) # 
    start_date = EOWeek(end_date, 1 - 3 * 52) # previous_business_date_if_weekend(EOMonth(end_date, -36))

    colnames(all_factor1_returns)[1] = "factor1"
    colnames(all_factor2_returns)[1] = "factor2"

    # The weights will be determined by lambda (and be equal-weighted if lambda is missing)
    if (missing(lambda)) {
        weights = NULL
        metric_name = "regular"
    } else {
        weights = ewma(lambda, 155:0) / sum(ewma(lambda, 155:0))
        metric_name = paste("lambda=", lambda, sep = "")
    }

    df = data.frame(date = as_date(character(0)), factor = character(0), metric = character(0), version = character(0), value = numeric(0))
    i = 0
    while (start_date > ymd("1995-01-01")) {

        #  Restrict ourselves to the desired timeframe:
        factor1_returns = all_factor1_returns[paste(start_date, end_date, sep = "/")]
        factor2_returns = all_factor2_returns[paste(start_date, end_date, sep = "/")]

        if (missing(lambda)) {
            weights_are_fine = TRUE
        } else {
            weights_are_fine = length(factor1_returns) == length(weights)
        }

        #  Create the weights
        if (length(factor1_returns) == length(factor2_returns) && (weights_are_fine)) {
            data = data.frame(date = index(factor1_returns), factor1_returns[, 1], factor2_returns[, 1])
            ewma.lm = lm(factor1 ~ factor2, data, weights = weights)
            df = rbind(df, data.frame(date = end_date, factor = factor_name, metric = metric_name, version = "r-squared", value = summary(ewma.lm)$r.squared))
            df = rbind(df, data.frame(date = end_date, factor = factor_name, metric = metric_name, version = "beta", value = ewma.lm$coefficients[2]))
            rownames(df) = NULL
        }

        end_date = EOWeek(end_date, -1)
        start_date = EOWeek(start_date, -1)
        cat("i = ", i, metric_name, "\n")
        i = i + 1
    }

    df = arrange(df, date)
    return(df)
    # print(ggplot(df, aes(x = date, y = value, colour = metric)) + geom_line() + facet_wrap("version", ncol = 1, scales = "free") + theme(legend.position = "bottom") + ggtitle(paste("lambda =", lambda)))
}

bmark_code = "SP500"
currency = "AUD"
frequency = "weekly"

# The benchmark factor, bond, yield and portfolio
bmark_index = get_benchmark_index(bmark_code, currency)
bond_index = get_bond_index() # head(bond_index)
yield_index = get_yield_index() # head(yield_index)

all_bmark_index_returns = periodReturn(bmark_index, period = frequency)
all_bond_index_returns = periodReturn(bond_index, period = frequency)
all_yield_index_returns = periodReturn(yield_index, period = frequency, type = 'log')

###################
## BOND vs YIELD ##
###################
df = try_this_lambda(all_bond_index_returns, all_yield_index_returns, "bond vs yield")
df = rbind(df, try_this_lambda(all_bond_index_returns, all_yield_index_returns, "bond vs yield", 0.90))
df = rbind(df, try_this_lambda(all_bond_index_returns, all_yield_index_returns, "bond vs yield", 0.92))
df = rbind(df, try_this_lambda(all_bond_index_returns, all_yield_index_returns, "bond vs yield", 0.94))
df = rbind(df, try_this_lambda(all_bond_index_returns, all_yield_index_returns, "bond vs yield", 0.96))
df = rbind(df, try_this_lambda(all_bond_index_returns, all_yield_index_returns, "bond vs yield", 0.98))
ggplot(df, aes(x = date, y = value, colour = metric)) + geom_line() + facet_wrap("version", ncol = 1, scales = "free") + theme(legend.position = "bottom") + ggtitle("bond vs yield", subtitle = "Exponentially Weighted Moving Average (ewma)")

###################
## BMARK vs BOND ##
###################
df1 = try_this_lambda(all_bmark_index_returns, all_bond_index_returns, "bmark vs bond")
df1 = rbind(df1, try_this_lambda(all_bmark_index_returns, all_bond_index_returns, "bmark vs bond", 0.90))
df1 = rbind(df1, try_this_lambda(all_bmark_index_returns, all_bond_index_returns, "bmark vs bond", 0.92))
df1 = rbind(df1, try_this_lambda(all_bmark_index_returns, all_bond_index_returns, "bmark vs bond", 0.94))
df1 = rbind(df1, try_this_lambda(all_bmark_index_returns, all_bond_index_returns, "bmark vs bond", 0.96))
df1 = rbind(df1, try_this_lambda(all_bmark_index_returns, all_bond_index_returns, "bmark vs bond", 0.98))

# Chart the different lambdas (vs no ewma):
ggplot(df1, aes(x = date, y = value, colour = metric)) + geom_line() + facet_wrap("version", ncol = 1, scales = "free") + theme(legend.position = "bottom") + ggtitle(paste(bmark_code, "vs bond"), subtitle = "Exponentially Weighted Moving Average (ewma)")

# Chart just lambda = 0.94 vs no ewma:
df1 %>% filter(metric %in% c("regular", "lambda=0.94", "lambda=0.94")) %>% ggplot(aes(x = date, y = value, colour = metric)) + geom_line() + facet_wrap("version", ncol = 1, scales = "free") + theme(legend.position = "bottom") + ggtitle(paste(bmark_code, "vs bond"), subtitle = "Exponentially Weighted Moving Average (ewma)")
