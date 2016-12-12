#################################################
## Quickly throw together some relative charts ##
#################################################
library(xts)
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
o_scores = get_table_from_sql_CISMPRDSVR(sql) # head(o_scores)
o_scores$change_date = as.Date(as_date(o_scores$change_date))
o_scores["change_direction"] = "upgrade"
o_scores[which(o_scores$change_amount > 0), "change_direction"] = "downgrade"

o_scores = subset(o_scores, exchange == "NZ" | exchange == "ASX")
ticker_list = unique(o_scores$ticker)

x = get_ticker_xts_return_index(ticker_list, currency, min_date)  # head(x)
stock_return = CalculateReturns(x)[date_range_index] # head(stock_return)
bmark_return = get_benchmark_xts_returns(bmark_code, currency)[date_range_index] # head(bmark_return)
max_date = max(index(stock_return))
o_scores = o_scores[(which(o_scores$change_date != max_date)),]
o_scores = o_scores[(which(o_scores$change_date != min_date)),]

# Compute a relative return series:
xts_relative = Return.relative(stock_return, bmark_return) # head(xts_relative)

# All relative charts should start at one, but these get cutoff by Return.relative:
temp_row_zero = stock_return[format(min_date, "%Y-%m-%d")]
temp_row_zero = na.fill(temp_row_zero, 1)
xts_relative = rbind.xts(temp_row_zero, xts_relative) # FIX IT! FIX IT! FIX IT! 

colnames(xts_relative) = colnames(stock_return)


dfRel = flatten_xts(xts_relative, "rel_return", "stock")
dfRel$date = as.Date(as_date(dfRel$date))

# Create a full table of scores, from start date to end:
all_scores = data.frame(ticker = character(), start = as.Date(as_date(character())), end = as.Date(as_date(character())), score = character(), rel_return = numeric(), change_direction = character(), stringsAsFactors = FALSE)

for (this_ticker in ticker_list) {
    # Grab the subset of data for this ticker
    these_scores = subset(o_scores, ticker == this_ticker)
    these_scores = these_scores[order(these_scores$change_date),]
    rownames(these_scores) = NULL

    # Create a dataframe consisting of windows of time, and the scores during those windows.
    t0 = these_scores[-1,]
    t1 = these_scores[ - nrow(these_scores),]
    this_df = data.frame(ticker = t0$ticker, start = t1$change_date, end = t0$change_date, score = t1$new_score, change_direction = t1$change_direction)
    this_min_date = as.Date(as_date(min(these_scores$change_date)))
    this_max_date = as.Date(as_date(max(these_scores$change_date)))
    this_start_score = subset(these_scores, change_date == this_min_date)$old_score[1]
    this_end_score = subset(these_scores, change_date == this_max_date)$new_score[1]
    this_end_direction = subset(these_scores, change_date == this_max_date)$change_direction[1]
    this_df = rbind(this_df, data.frame(ticker = this_ticker, start = min_date, end = this_min_date, score = this_start_score, change_direction = "start"))
    this_df = rbind(this_df, data.frame(ticker = this_ticker, start = this_max_date, end = max_date, score = this_end_score, change_direction = this_end_direction))
    this_df = this_df[order(this_df$start),]

    this_df$start = as.Date(as_date(this_df$start))
    this_df$end = as.Date(as_date(this_df$end))
    this_df$score = as.character(this_df$score) # paste("o", this_df$score, sep = "")

    this_df = this_df[which(this_df$start != this_df$end),]

    # Grab the relevant relative returns
    this_rel = subset(dfRel, stock == this_ticker)

    # Compute the changes in relative returns over each window.
    with_start_price = merge(x = this_df, y = this_rel, by.x = "start", by.y = "date", all.x = TRUE)
    with_end_price = merge(x = with_start_price, y = this_rel, by.x = "end", by.y = "date", all.x = TRUE)

    this_df = data.frame(
        ticker = with_end_price$stock.x,
        start = with_end_price$start, 
        end = with_end_price$end,
        score = with_end_price$score,
        rel_return = with_end_price$rel_return.y / with_end_price$rel_return.x - 1,
        change_direction = with_end_price$change_direction
    )

    # Let's accumulate all score windows into a massive dataframe
    rownames(this_df) = NULL
    all_scores = rbind(all_scores, this_df)

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
    chart = chart + geom_text(aes(x = start, y = 1, label = paste(round(100 * (with_end_price$rel_return.y / with_end_price$rel_return.x - 1), digits = 1), "%", sep = "")), data = this_df, nudge_x = 30)
    ggsave(filename = paste("M:\\Information Technology\\Quant\\R\\o_scores\\", this_ticker, ".png", sep = ""), chart)
}

# add before-and-after returns
y = data.frame(ticker = all_scores$ticker, change_date = all_scores$end, return_before = all_scores$rel_return)
returns_before_change = merge(o_scores, y, by.x = c("change_date", "ticker"), by.y = c("change_date", "ticker"), all.x = TRUE)
y = data.frame(ticker = all_scores$ticker, change_date = all_scores$start, return_after = all_scores$rel_return)
returns_around_change = merge(returns_before_change, y, by.x = c("change_date", "ticker"), by.y = c("change_date", "ticker"), all.x = TRUE)

# Classify the eight types of score changes into buckets
returns_around_change["bucket"] = character()
x = returns_around_change

x[which(x$change_direction == "upgrade"   & x$return_before > 0 & x$return_after < 0), "bucket"] = "1. bad:  up, upgrade, down"
x[which(x$change_direction == "downgrade" & x$return_before < 0 & x$return_after > 0), "bucket"] = "2. bad:  down, downgrade, up"
x[which(x$change_direction == "downgrade" & x$return_before > 0 & x$return_after < 0), "bucket"] = "3. good at picking top: up, downgrade, down"
x[which(x$change_direction == "upgrade"   & x$return_before < 0 & x$return_after > 0), "bucket"] = "4. good at picking bottom: down, upgrade score, up"
x[which(x$change_direction == "upgrade"   & x$return_before > 0 & x$return_after > 0), "bucket"] = "5. chasing momentum up: up, upgrade, up more"
x[which(x$change_direction == "downgrade" & x$return_before < 0 & x$return_after < 0), "bucket"] = "6. chasing momentum down: down, downgrade, down more"
x[which(x$change_direction == "downgrade" & x$return_before > 0 & x$return_after > 0), "bucket"] = "7. shooting star:up, downgrade, up more"
x[which(x$change_direction == "upgrade"   & x$return_before < 0 & x$return_after < 0), "bucket"] = "8. falling knife:down, upgrade, down more"

returns_around_change = data.frame(
    ticker = x$ticker, 
    change_date = x$change_date, 
    old_score = x$old_score, 
    new_score = x$new_score, 
    change_direction = x$change_direction, 
    return_before = x$return_before, 
    return_after = x$return_after, 
    bucket = x$bucket
   )

write.csv(returns_around_change, file = "C:\\Temp\\O_score_change_analysis.csv")