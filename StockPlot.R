library(ggplot2)
library(PerformanceAnalytics) # <-- has a potentially useful Return.relative function
source('Functions.R') # <-- put all your functions in one place

# Plot the prices:
price_plot = function(xts_pfolio_price, xts_bmark_price, chart_title) {
    df = flatten_xts(xts_pfolio_price, "price", "spc_id")
    df = rbind(df, flatten_xts(xts_bmark_price, "price", "spc_id"))

    x = ggplot(df, aes(x = date, y = price, colour = spc_id, fill = spc_id))
    x = x + geom_line(size = 1)
    x = x + ggtitle(chart_title)

    # Legend:  on the bottom with no title
    x = x + theme(legend.position = "bottom")
    x = x + theme(legend.title = element_blank())

    # remove x-axis labels and y-axis labels:
    x = x + theme(axis.title.x = element_blank())
    x = x + theme(axis.title.y = element_blank())
    return(x)
}

# Plot the relative returns:
rel_plot = function(xts_return, xts_bmark_return, chart_title) {
    # Compute relative returns
    xts_relative = Return.relative(xts_return, xts_bmark_return)

    # The series names all have a "/[benchmark name]" in the title, which looks clunky.  Rename
    colnames(xts_relative) = colnames(xts_return)

    # Strangely, the "1" at the start of the series doesn't get charted.  Should it?

    # Flatten and chart:
    dfRel = flatten_xts(xts_relative, "rel_return", "spc_id")
    x = ggplot(dfRel, aes(x = date, y = rel_return, colour = spc_id, fill = spc_id))
    x = x + geom_line(size = 1)
    x = x + ggtitle(paste(chart_title, "relative return"))

    # Legend:  omit if there is only one element.  Otherwise, show at the bottom without a title
    if (dim(xts_relative)[2] == 1) {
        x = x + theme(legend.position = "none")
    } else {
        x = x + theme(legend.position = "bottom")
        x = x + theme(legend.title = element_blank())
    }

    # remove x-axis labels and y-axis labels:
    x = x + theme(axis.title.x = element_blank())
    x = x + theme(axis.title.y = element_blank())
    return(x)
}