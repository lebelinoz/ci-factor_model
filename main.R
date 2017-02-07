source('./factor_model_maker.R')

tf1 = timeframe(start_date = previous_business_date_if_weekend(EOMonth(today(), -61)), end_date = previous_business_date_if_weekend(EOMonth(today(), -1)), frequency = "M")

stock_returns = factor_model_maker(timeframe = tf1, benchmark_code = "XJO", portfolio_code = "PCHEST")

ncol(stock_returns@xts_returns)