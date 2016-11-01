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

# Example 4:
#  Find the global minimum variance portfolio
top.mat = cbind(2*sigma.mat, rep(1,3))
bot.vec = c(rep(1,3), 0)
Am.mat = rbind(top.mat, bot.vec)  # <-- the covariance matrix with an extra row of ones, then an extra column of ones with a zero at the bottom
b.vec = c(rep(0,3), 1)            # <-- the vector we want to solve for.
z.m.mat = solve(Am.mat)%*%b.vec
m.vec = z.m.mat[1:3,1]            # <-- this is the weights of the three stocks in our minimum variance portfolio
mu.gmin = as.numeric(crossprod(m.vec, mu.vec))

sig2.gmin = as.numeric(t(m.vec) %*% sigma.mat %*% m.vec)
sig.gmin = sqrt(sig2.gmin)

# Example 5:
#  Find global minimum variance again, with slightly easier calcs:
one.vec = rep(1,3)
sigma.inv.mat = solve(sigma.mat)
top.mat = sigma.inv.mat %*% one.vec
bot.val = as.numeric(t(one.vec) %*% sigma.inv.mat %*% one.vec)
m.mat = top.mat / bot.val
m.mat[, 1] # <-- the weights, again.

# Example 6:
#  Find the efficient portfolio with the same expected return as Microsoft

