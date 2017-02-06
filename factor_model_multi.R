
#############################################################
### HERE ARE SOME OF THE RESULTS FROM THE
###    'experiment_on_all_tickers.R'
### FILE, ON THE LAST TICKER RUN IN THERE.

# Single linear regression of asset vs benchmark, bond index 
asset_benchmark.lm = lm(asset ~ bmark, data = asset_and_bmark_and_factors)
summary(asset_benchmark.lm)
anova(asset_benchmark.lm)

asset_yield.lm = lm(asset ~ yield, data = asset_and_bmark_and_factors)
summary(asset_yield.lm)
anova(asset_yield.lm)

asset_bond.lm = lm(asset ~ bond, data = asset_and_bmark_and_factors)
summary(asset_bond.lm)
anova(asset_bond.lm)

# Multilinear regression on benchmark, bond returns and yield log returns.
# Instinctively, using both the bond index and the yield together adds nothing beyond just adding one of them.  The results confirm it when compared to  
asset_benchmark_factors.lm = lm(asset ~ bmark + bond + yield, data = asset_and_bmark_and_factors)
summary(asset_benchmark_factors.lm)
anova(asset_benchmark_factors.lm)

asset_bmark_and_bond.lm = lm(asset ~ bmark + bond, data = asset_and_bmark_and_factors)
summary(asset_bmark_and_bond.lm)
anova(asset_bmark_and_bond.lm)

asset_bmark_and_yield.lm = lm(asset ~ bmark + yield, data = asset_and_bmark_and_factors)
summary(asset_bmark_and_yield.lm)
anova(asset_bmark_and_yield.lm)

asset_bond_and_yield.lm = lm(asset ~ bond + yield, data = asset_and_bmark_and_factors)
summary(asset_bond_and_yield.lm)
anova(asset_bond_and_yield.lm)


# Let's look at the interaction between the benchmark, bond prices and yield.
bmark_yield.lm = lm(bmark ~ yield, data = asset_and_bmark_and_factors)
summary(bmark_yield.lm)
anova(bmark_yield.lm)

bmark_bond.lm = lm(bmark ~ bond, data = asset_and_bmark_and_factors)
summary(bmark_bond.lm)
anova(bmark_bond.lm)

bond_yield.lm = lm(bond ~ yield, data = asset_and_bmark_and_factors)
summary(bond_yield.lm)
anova(bond_yield.lm)

ggplot(asset_and_bmark_and_factors, aes(bond, yield)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Bond vs Yield") + scale_x_continuous(limits = c(-0.15, 0.15)) + scale_y_continuous(limits = c(-0.2, 0.2))
ggplot(asset_and_bmark_and_factors, aes(bmark, yield)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Benchmark vs Yield") + scale_x_continuous(limits = c(-0.15, 0.15)) + scale_y_continuous(limits = c(-0.2, 0.2))
ggplot(asset_and_bmark_and_factors, aes(bmark, bond)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Benchmark vs Bond") + scale_x_continuous(limits = c(-0.15, 0.15)) + scale_y_continuous(limits = c(-0.2, 0.2))
ggplot(asset_and_bmark_and_factors, aes(asset, bmark)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Asset vs Benchmark") + scale_x_continuous(limits = c(-0.15, 0.15)) + scale_y_continuous(limits = c(-0.2, 0.2))
ggplot(asset_and_bmark_and_factors, aes(asset, bond)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Asset vs Bond") + scale_x_continuous(limits = c(-0.15, 0.15)) + scale_y_continuous(limits = c(-0.2, 0.2))
ggplot(asset_and_bmark_and_factors, aes(asset, yield)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Asset vs Yield") + scale_x_continuous(limits = c(-0.15, 0.15)) + scale_y_continuous(limits = c(-0.2, 0.2))
