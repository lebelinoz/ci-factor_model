# This is my favourite way of showing a regression of xts objects.
# The title of the chart will be the two column names and the r-squared of the linear model.
# If the strings are missing, assume the names are the first name in the xts.
show_regression_between_xts = function(xts_x, xts_y, x_string, y_string) {

    if (missing(x_string)) x_string = colnames(xts_x)[1]
    if (missing(y_string)) y_string = colnames(xts_y)[1]
    df_x = setNames(data.frame(date = index(xts_x), thing = xts_x[, 1]), c("date", x_string))
    rownames(df_x) = NULL
    df_y = setNames(data.frame(date = index(xts_y), thing = xts_y[, 1]), c("date", y_string))
    rownames(df_y) = NULL
    df = merge(df_x, df_y, by = "date")

    show_regression(df, x_string, y_string)
}