# This is my favourite way of showing a regression of two columns in a data set.
# The title of the chart will be the two column names and the r-squared of the linear model.
show_regression = function(data, x_string, y_string) {
    r.squared = summary(lm(data[, x_string] ~ data[, y_string], data))$r.squared
    title_thing = ggtitle(paste(x_string, " vs ", y_string, ", r-squared = ", format(r.squared, digits = 2), sep = ""))
    ggplot(data, aes_string(x_string, y_string)) + geom_point() + geom_smooth(method = "lm") + title_thing # + scale_y_continuous(limits = c(-0.075, 0.075)) + scale_x_continuous(limits = c(-0.075, 0.075))
}