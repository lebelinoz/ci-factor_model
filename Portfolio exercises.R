# Portfolio exercises, to go with portfolio.R

# Example 2: our portfolio constituents
asset.names = c("MSFT", "NORD", "SBUX")
mu.vec = c(0.0427, 0.0015, 0.0285)
names(mu.vec) = asset.names
sigma.mat = matrix( c(0.1^2, 0.0018, 0.0011, 0.0018, 0.1044^2, 0.0026, 0.0011, 0.0026, 0.1411^2), nrow = 3, ncol = 3)
dimnames(sigma.mat) = list(asset.names, asset.names)


# Example 3: 
#  Suppose we hold 1/3 each:
x.vec = rep(1,3)/3
names(x.vec) = asset.names
mu.p.x = crossprod(x.vec, mu.vec)
sig2.p.x = t(x.vec) %*% sigma.mat %*% x.vec
sig.p.x = sqrt(sig2.p.x)

#  Now suppose these different weights:
y.vec = c(0.8, 0.4, -0.2)
names(y.vec) = asset.names
sig.xy = t(x.vec) %*% sigma.mat %*% y.vec
