source('show_regression.R')

# Run this after running 'experiment_on_all_tickers.R'.  This will chart return scatter plots of the last asset calculated, including the "lm" line and an R-squared label
show_regression(asset_and_bmark_and_factors, "bmark", "asset")
show_regression(asset_and_bmark_and_factors, "bond", "asset")
show_regression(asset_and_bmark_and_factors, "yield", "asset")

show_regression(asset_and_bmark_and_factors, "bond", "yield")
show_regression(asset_and_bmark_and_factors, "bond", "bmark")
show_regression(asset_and_bmark_and_factors, "yield", "bmark")

