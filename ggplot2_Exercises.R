################################
## Exercises for ggplot2 book ##
################################

library(ggplot2)
data(mpg)
data(diamonds)
data(economics)


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


# Section 2.3
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()
ggplot(mpg, aes(cty,hwy)) + geom_point()
ggplot(mpg, aes(model, manufacturer)) + geom_point()
ggplot(mpg, aes(manufacturer, model)) + geom_point()

ggplot(mpg, aes(cty,hwy)) + geom_point()
ggplot(diamonds, aes(carat, price)) + geom_point()
ggplot(economics, aes(date, unemploy)) + geom_line()
ggplot(mpg, aes(cty)) + geom_histogram()

# Section 2.4
ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()
ggplot(mpg, aes(displ, hwy, shape = drv)) + geom_point()
ggplot(mpg, aes(displ, hwy, size = cyl)) + geom_point()

ggplot(mpg, aes(displ, hwy, colour = class, shape = drv, size = cyl)) + geom_point()

ggplot(mpg, aes(displ, cty, colour=class)) + geom_point()


# Section 3.4:  annotation
data(economics)
data(presidential)
summary(economics)
summary(presidential)

ggplot(economics, aes(date, unemploy)) + geom_line()
presidential <- subset(presidential, start > economics$date[1])

x = ggplot(economics) 
x = x + geom_rect(aes(xmin = start, xmax = end, fill = party), ymin = -Inf, ymax = Inf, alpha = 0.2, data = presidential) 
x = x + geom_vline(aes(xintercept = as.numeric(start)), data = presidential, colour = "grey50", alpha = 0.5)
x = x + geom_text(aes(x =  start, y = 2500, label = name), data = presidential, size = 3, vjust = 0, hjust = 0, nudge_x = 50)
x = x + geom_line(aes(date, unemploy))
x = x + scale_fill_manual(values = c("blue", "red"))
x
    