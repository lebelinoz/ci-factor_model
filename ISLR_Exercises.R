################################################
## An Introduction to Statistical Learning    ##
## (with Applications in R)                   ##
## by James, Witten, Hastie & Tibshirani      ##
################################################

library(ISLR) # <- contains most data sets for the book
library(MASS) # <- contains the "Boston" dataset
library(ggplot2)

data(Wage)
summary(Wage)

# Figure 1.1a:
ggplot(Wage, aes(age, wage)) + geom_point() + geom_smooth()

# Figure 1.1b:
ggplot(Wage, aes(year, wage)) + geom_point() # + stat_smooth() # <-- I can't get the trend line to work

# Figure 1.1c:
ggplot(Wage, aes(education, wage)) + geom_boxplot()

