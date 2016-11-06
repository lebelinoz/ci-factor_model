################################
## Exercises for ggplot2 book ##
################################

library(ggplot2)
data(mpg)

# how to know which data packages are included in ggplot2?
data()

# five ways to analyze mpg data
head(mpg)
tail(mpg, 10)
str(mpg)
colnames(mpg)
rownames(mpg)
summary(mpg)

# convert data to "litres for 100 km"
