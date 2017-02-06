# Take the residuals of the asset vs bmark linear model, and see how much better a second step will do against it.
source('experiment_on_all_tickers.R')
source('show_regression.R')

asset_bmark.lm = lm(asset ~ bmark, data = asset_and_bmark_and_factors)
asset_and_bmark_and_factors$residuals = asset_bmark.lm$residuals

show_regression(asset_and_bmark_and_factors, "residuals", "yield")

step2_bond.lm = lm(residuals ~ bond, asset_and_bmark_and_factors)
summary(step2_bond.lm)
anova(step2_bond.lm)

step2_yield.lm = lm(residuals ~ yield, asset_and_bmark_and_factors)
summary(step2_yield.lm)
anova(step2_yield.lm)