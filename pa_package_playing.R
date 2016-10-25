# Playing with "Performance Attribution for Equity Portfolios" by Yang Lu and David Kane (December 2013)
library(pa)
data(jan)
data(quarter)
data(year)

## Single-period brinson analysis
p1 = brinson(x=jan, date.var="date", cat.var="sector", bench.weight="benchmark", portfolio.weight="portfolio", ret.var="return")
summary(p1)
plot(p1, var="sector", type="return")

## Multi-period brinson analysis
p2 = brinson(x=quarter, date.var="date", cat.var="sector", bench.weight="benchmark", portfolio.weight="portfolio", ret.var="return")
summary(p2)
                                                                                                                 

## Single-period regression analysis
r1 = regress(x=jan, date.var="date", ret.var="return", reg.var=c("sector","value","growth"), benchmark.weight="benchmark", portfolio.weight="portfolio")
summary(r1)

## Multi-period regression analysis (on one quarter)
r2 = regress(x=quarter, date.var="date", ret.var="return", reg.var=c("sector","value","growth"), benchmark.weight="benchmark", portfolio.weight="portfolio")
summary(r2)

## Multi-period regression analysis (on one year)
rb.multi2 = regress(x=year, date.var="date", ret.var="return", reg.var=c("sector","value","growth"), benchmark.weight="benchmark", portfolio.weight="portfolio")
returns(rb.multi2)
plot(rb.multi2, var="sector", type="return")



## Playing with the "hypothetic portfolio" on page 14 of the article
# Here's the hypothetic portfolio:
hp = data.frame(return=c(0.3,0.4,0.5), name=c("A","B","C"), size=c(1.2, 2.0, 0.8), value=c(3.0, 2.0, 1.5), active_weight=c(0.5,0.1,-0.6))

# Here's the exposure of each asset to each factor (see formula 3.16 on p.55 of Grinold and Kahn)
X = t(as.matrix(rbind(hp$size, hp$value)))

# Here's  the active weight of each asset
active_weight = t(as.matrix(hp$active_weight))

# Use regression to determine the factor returns
regression_model = lm(hp$return ~ X)
factor_returns = as.matrix(regression_model$coefficients[c("X1","X2")])

# Compute the active exposure and contributions
active_exposure = active_weight %*% X
contribution = as.matrix(diag(factor_returns %*% active_exposure))

# Can we compute all of the above using pa's "regress" tool?  When I try, I don't get quite the same results:
hp[["date"]] <- c("2016-01-31","2016-01-31","2016-01-31")
hp[["benchmark"]] <- c(0.15, 0.15, 0.7)
hp[["portfolio"]] <- hp$benchmark + hp$active_weight

rb.single = regress(hp, date.var="date", ret.var="return", reg.var=c("size","value"), benchmark.weight="benchmark", portfolio.weight="portfolio")
summary(rb.single)

