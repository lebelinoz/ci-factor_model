### PREAMBLE
source('Sql_Wrapper.R') # <-- for grabbing data straight from our SQL database
library(tidyverse)

# Just take the output which was painstakingly created in factor_model_backtester:
test_result = read.csv("C://Temp//factor_model_backtests_XJO_Monthly_60m.csv")
test_result = test_result[which(test_result$count >= 24),]

# How do R squared correlate to predictive power?
gb = group_by(test_result, ticker)

by_ticker = summarise(gb, 
    count = n(), 
    avg_r2 = mean(r_squared, na.rm = TRUE),   
    avg_error = mean(return_error, na.rm = TRUE),
    avg_beta = mean(beta),
    sd_r2 = sd(r_squared, na.rm = TRUE),
    sd_error = sd(return_error, TRUE)
)
by_ticker = by_ticker[which(by_ticker$count > 24),]
by_ticker = by_ticker[which(by_ticker$ticker != "OGC"),]

ggplot(by_ticker, aes(avg_r2, avg_error * avg_error)) + geom_point() # + 
#    geom_smooth(method = "lm") + 
#    scale_y_continuous(limits = c(0, 0.01))

ggplot(by_ticker, aes(avg_r2, sd_error / avg_beta)) + geom_point() + geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 0.5)) + scale_x_continuous(limits = c(0, 0.5))
x = lm(avg_r2 ~ sd_error / avg_beta, by_ticker)
summary(x)

df = data.frame(r_squared = by_ticker$avg_r2, stderr_over_beta = by_ticker$sd_error / by_ticker$avg_beta)
summary(lm(r_squared ~ stderr_over_beta, df))

ggplot(by_ticker, aes(avg_r2, sd_error)) + geom_point() + geom_smooth(method = "lm")