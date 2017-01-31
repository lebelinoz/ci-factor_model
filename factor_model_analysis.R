### PREAMBLE
source('./lib/Sql_Wrapper.R') # <-- for grabbing data straight from our SQL database
library(tidyverse)

################
###   CAPM   ###
################
# Just take the output which was painstakingly created in factor_model_backtester:
capm_test_result = read.csv("C://Temp//factor_model_backtests_XJO_Monthly_60m.csv")
capm_test_result = capm_test_result[which(capm_test_result$count >= 24),]

# How do R squared correlate to predictive power?
gb = group_by(capm_test_result, ticker)

capm_by_ticker = summarise(gb, 
    count = n(), 
    avg_r2 = mean(r_squared, na.rm = TRUE),   
    avg_error = mean(return_error, na.rm = TRUE),
    avg_beta = mean(beta),
    sd_r2 = sd(r_squared, na.rm = TRUE),
    sd_error = sd(return_error, TRUE)
)
capm_by_ticker = filter(capm_by_ticker, count > 24)
capm_by_ticker = filter(capm_by_ticker, ticker != "OGC")  # Oceanagold is a crazy outlier for some reason

# Turns out, a ticker's average r squared is a good predictor of *(clears throat)* the standard deviation of the error divided by the average beta.
ggplot(capm_by_ticker, aes(avg_r2, sd_error / avg_beta)) + geom_point() + geom_smooth() + ggtitle("CAPM Avg R-squared vs StdDev of error / beta")
ggplot(capm_by_ticker, aes(avg_r2, sd_error / avg_beta)) + geom_point() + geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 0.5)) + scale_x_continuous(limits = c(0, 0.5)) + ggtitle("Avg R-squared vs StdDev of error / beta (small number of outliers not shown)")

# Looks even better vs log of standard deviation of the error over beta
ggplot(capm_by_ticker, aes(avg_r2, log(sd_error / avg_beta))) + geom_point() + geom_smooth() + ggtitle("CAPM Avg R-squared vs log of StdDev of error / beta")
ggplot(capm_by_ticker, aes(avg_r2, log(sd_error / avg_beta))) + geom_point() + geom_smooth(method = "lm") + scale_y_continuous(limits = c(-4, -1)) + scale_x_continuous(limits = c(0, 0.5)) + ggtitle("Avg R-squared vs log of StdDev of error / beta (small number of outliers not shown)")

# The R-squared of avg r squared vs the log of standard deviation of error over beta
capm_df = data.frame(r_squared = capm_by_ticker$avg_r2, sderr_over_beta = log(capm_by_ticker$sd_error / capm_by_ticker$avg_beta))
summary(lm(capm_df))$r.squared # its R squared is 0.54

################
###   BOND   ###
################
# Just take the output which was painstakingly created in factor_model_backtester:
bond_test_result = read.csv("C://Temp//factor_model_backtests_XJO_Monthly_60m_factor_is_bond_index.csv")
bond_test_result = filter(bond_test_result, count >= 24)

# How do R squared correlate to predictive power?
gb = group_by(bond_test_result, ticker)

bond_by_ticker = summarise(gb,
    count = n(),
    avg_r2 = mean(r_squared, na.rm = TRUE),
    avg_error = mean(return_error, na.rm = TRUE),
    avg_beta = mean(beta),
    sd_r2 = sd(r_squared, na.rm = TRUE),
    sd_error = sd(return_error, TRUE)
)
bond_by_ticker = filter(bond_by_ticker, count > 24)
bond_by_ticker = filter(bond_by_ticker, ticker != "OGC")

# Turns out, a ticker's average r squared is a good predictor of *(clears throat)* the standard deviation of the error divided by the average beta.
ggplot(bond_by_ticker, aes(avg_r2, sd_error / abs(avg_beta))) + geom_point() + geom_smooth() + ggtitle("Bond Avg R-squared vs log of StdDev of error / |beta|")
ggplot(bond_by_ticker, aes(avg_r2, sd_error / abs(avg_beta))) + geom_point() + geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 0.5)) + scale_x_continuous(limits = c(0, 0.5)) + ggtitle("Bond R-squared vs log of StdDev of error / |beta| .. same axis scales as CAPM chart")

# Against log:
ggplot(bond_by_ticker, aes(avg_r2, log(sd_error / abs(avg_beta)))) + geom_point() + geom_smooth() + ggtitle("Bond Avg R-squared vs StdDev of error / |beta|")
ggplot(bond_by_ticker, aes(avg_r2, log(sd_error / abs(avg_beta)))) + geom_point() + geom_smooth(method = "lm") + scale_y_continuous(limits = c(-4, -1)) + scale_x_continuous(limits = c(0, 0.5)) + ggtitle("Bond R-squared vs StdDev of error / |beta| .. same axis scales as CAPM chart")

# For bonds, the R-squared of avg r squared vs the log of standard deviation of error over beta
bond_df = data.frame(r_squared = bond_by_ticker$avg_r2, sderr_over_beta = log(bond_by_ticker$sd_error / bond_by_ticker$avg_beta))
summary(lm(bond_df))$r.squared # its R squared is 0.3

################
###   BEEF   ###
################
# Just take the output which was painstakingly created in factor_model_backtester:
beef_test_result = read.csv("C://Temp//factor_model_backtests_XJO_Monthly_60m_factor_is_brazilian_cattle_price.csv")
beef_test_result = filter(beef_test_result, count >= 24)

# How do R squared correlate to predictive power?
gb = group_by(beef_test_result, ticker)

beef_by_ticker = summarise(gb,
    count = n(),
    avg_r2 = mean(r_squared, na.rm = TRUE),
    avg_error = mean(return_error, na.rm = TRUE),
    avg_beta = mean(beta),
    sd_r2 = sd(r_squared, na.rm = TRUE),
    sd_error = sd(return_error, TRUE)
)
beef_by_ticker = filter(beef_by_ticker, count > 24)
#beef_by_ticker = filter(beef_by_ticker, ticker != "OGC")

# Turns out, a ticker's average r squared is a good predictor of *(clears throat)* the standard deviation of the error divided by the average beta.
ggplot(beef_by_ticker, aes(avg_r2, sd_error / abs(avg_beta))) + geom_point() + geom_smooth() + ggtitle("Beef Avg R-squared vs log of StdDev of error / |beta|")
ggplot(beef_by_ticker, aes(avg_r2, sd_error / abs(avg_beta))) + geom_point() + geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 0.5)) + scale_x_continuous(limits = c(0, 0.5)) + ggtitle("Beef R-squared vs log of StdDev of error / |beta| .. same axis scales as CAPM chart")

# Against log:
ggplot(beef_by_ticker, aes(avg_r2, log(sd_error / abs(avg_beta)))) + geom_point() + geom_smooth() + ggtitle("Beef Avg R-squared vs StdDev of error / |beta|")
ggplot(beef_by_ticker, aes(avg_r2, log(sd_error / abs(avg_beta)))) + geom_point() + geom_smooth(method = "lm") + scale_y_continuous(limits = c(-4, -1)) + scale_x_continuous(limits = c(0, 0.5)) + ggtitle("Beef R-squared vs StdDev of error / |beta| .. same axis scales as CAPM chart")

# For beef, the R-squared of avg r squared vs the log of standard deviation of error over beta
beef_df = data.frame(r_squared = beef_by_ticker$avg_r2, sderr_over_beta = log(beef_by_ticker$sd_error / beef_by_ticker$avg_beta))
summary(lm(beef_df))$r.squared # its R squared is 0.49.  So, clearly, R-squared on this metric means nothing.



#############
# Let's try some other charts
#############

# Here we chart NST & WBC's lines of best fit.  They both have beta ~ 1
ticker = "WBC" # WBC is an example with high R-squared, NST is an example with low.
df_raw = capm_test_result[which(capm_test_result$ticker == ticker),]
df_raw = df_raw[which(as_date(df_raw$test_date) > as_date("2012-12-31")),]
df = data.frame(date = df_raw$test_date, bmark_return = df_raw$factor_next_return, stock_return = df_raw$actual_next_return)
ggplot(df, aes(bmark_return, stock_return)) + geom_point() + geom_smooth(method = "lm") + ggtitle(paste(ticker, "CAPM demo")) + scale_y_continuous(limits = c(-0.125, 0.125)) + scale_x_continuous(limits = c(-0.015, 0.015))

ticker = "NST" # WBC is an example with high R-squared, NST is an example with low.
df_raw = capm_test_result[which(capm_test_result$ticker == ticker),]
df_raw = df_raw[which(as_date(df_raw$test_date) > as_date("2012-12-31")),]
df = data.frame(date = df_raw$test_date, bmark_return = df_raw$factor_next_return, stock_return = df_raw$actual_next_return)
ggplot(df, aes(bmark_return, stock_return)) + geom_point() + geom_smooth(method = "lm") + ggtitle(paste(ticker, "CAPM demo")) + scale_y_continuous(limits = c(-0.125, 0.125)) + scale_x_continuous(limits = c(-0.015, 0.015))