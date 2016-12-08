#################################################
## Quickly throw together some relative charts ##
#################################################
source('StockPlot.R')
source('Sql_Wrapper.R')

# options
bmark_code = 'XJO'
currency = "Local"
#ticker_list = c("AIA.ASX")
min_date = as.Date("29/05/2015", "%d/%m/%Y")
date_range_index = paste(format(min_date, "%Y-%m-%d"), "/", sep = "")
o_or_f = "O"

# O score stuff:
sql = paste("EXEC PCI_CORE.dbo.rattle_OandFscore_changes ", o_or_f, ", @simple = 1", sep = "'")
o_scores = get_table_from_sql_CISMPRDSVR(sql)
#o_scores = subset(o_scores, exchange != "NZ") # exchange != "ASX" && exchange != "NZ")
o_scores = subset(o_scores, exchange == "ASX")
ticker_list = unique(o_scores$ticker)

x = get_ticker_xts_return_index(ticker_list, currency, min_date)  # head(x)
stock_return = CalculateReturns(x)[date_range_index] # head(stock_return)
bmark_return = get_benchmark_xts_returns(bmark_code, currency)[date_range_index] # head(bmark_return)
max_date = max(index(stock_return))

# Compute a relative return series:
xts_relative = Return.relative(stock_return, bmark_return)
colnames(xts_relative) = colnames(stock_return)

dfRel = flatten_xts(xts_relative, "rel_return", "stock")
dfRel$date = as.Date(dfRel$date)

# Create a full table of scores, from start date to end:
    #this_ticker = "CYB.ASX"
for (this_ticker in ticker_list) {
    these_scores = subset(o_scores, ticker == this_ticker)
    these_scores = these_scores[order(these_scores$change_date),]
    rownames(these_scores) = NULL

    t0 = these_scores[-1,]
    t1 = these_scores[ - nrow(these_scores),]
    this_df = data.frame(start = t1$change_date, end = t0$change_date, score = t1$new_score)
    this_min_date = min(these_scores$change_date)
    this_max_date = max(these_scores$change_date)
    this_start_score = subset(these_scores, change_date == this_min_date)$old_score[1]
    this_end_score = subset(these_scores, change_date == this_max_date)$new_score[1]
    this_df = rbind(this_df, data.frame(start = min_date, end = this_min_date, score = this_start_score))
    this_df = rbind(this_df, data.frame(start = this_max_date, end = max_date, score = this_end_score))
    this_df = this_df[order(this_df$start),]

    this_df$start = as.Date(this_df$start)
    this_df$end = as.Date(this_df$end)
    this_df$score = as.character(this_df$score) # paste("o", this_df$score, sep = "")
    rownames(this_df) = NULL

    this_rel = subset(dfRel, stock == this_ticker)

    # Create the chart
    chart = ggplot(this_rel) #, colour = stock, fill = stock))
    chart = chart + ggtitle(paste(this_ticker, "total return relative", bmark_code, "(with", o_or_f, "score overlay)"))
    chart = chart + theme(axis.title.x = element_blank())
    chart = chart + theme(axis.title.y = element_blank())
    chart = chart + geom_rect(aes(xmin = start, xmax = end, fill = score), ymin = -Inf, ymax = Inf, alpha = 0.2, data = this_df)
    chart = chart + geom_vline(aes(xintercept = as.numeric(start)), data = this_df, colour = "grey50", alpha = 0.5)
    chart = chart + geom_line(aes(x = date, y = rel_return), size = 1)
    chart = chart + scale_fill_manual(values = c("1" = "darkgreen", "2" = "green", "3" = "white", "4" = "red", "5" = "darkred"))
    chart = chart + theme(legend.position = "bottom")
    ggsave(filename = paste("M:\\Information Technology\\Quant\\R\\o_scores\\", this_ticker, ".png", sep = ""), chart)
}