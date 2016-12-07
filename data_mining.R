library(rattle)
library(rpart) # <-- required to do trees without rattle
# library(gridExtra) # <-- required by rattle to create some charts
library(ggplot2)
source('Sql_Wrapper.R')

##########################################################################
### SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL ##
currency = 'AUD'
sql_query = paste("EXEC PCI_CORE.dbo.rattle_beans '", currency, "'", sep = "")
watchlist = get_table_from_sql_CISMPRDSVR(sql_query)
### SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL SQL ##
##########################################################################

watchlist[, "O_"] = paste('O', watchlist$O, sep = "")
watchlist[, "F_"] = paste('F', watchlist$F, sep = "")
watchlist[, "O_1M_chg_"] = paste('O_1M_chg_', watchlist$O_1M_chg, sep = "")
watchlist[, "O_3M_chg_"] = paste('O_3M_chg_', watchlist$O_3M_chg, sep = "")
watchlist[, "F_1M_chg_"] = paste('F_1M_chg_', watchlist$F_1M_chg, sep = "")
watchlist[, "F_3M_chg_"] = paste('F_3M_chg_', watchlist$F_3M_chg, sep = "")

# As a first experiment, let's concentrate on the watchlist as of 30 November 2011
# By this point, the relatively new O & F scores would have settled, and we have 12M returns to play with
watchlist_201511 = watchlist[which(watchlist$month_code == 'P2015M11'),c("ticker", "watchlist", "V", "O_", "F_", "return_12M")]
watchlist_201511 = watchlist_201511[which(!is.na(watchlist_201511[, "return_12M"])),]

# Also, let's see about predicting 1M returns
watchlist_1M = watchlist[which(!is.na(watchlist[, "return_1M"])), 
    c("ticker", "watchlist", "V", "O_", "O_1M_chg_", "O_3M_chg_", "F_", "F_1M_chg_", "F_3M_chg_", "return_1M", "return_3M", "ret_minus_1M", "ret_minus_3M")]

rattle()

###########################################################################################
## 12 MONTH RETURNS BASED ON V, O & F SCORES
##
## Discovery on 1-year data (Nov 2015 to Nov 2016) on all 202 Watchlist RL1 stocks:
##  Best results with the 28 stocks which have O score 4 or 5.  Better still with even-numbered F scores
##  Of the 174 remaining stocks, favour O score of 3 (88 stocks) over O score of 1 or 2 (86).
##    With O Score of 1 or 2, low V scores (< 1.6) are bad.
##    With O Score of 3, low V scores (< 1.4) are good.
## Note only two global stocks had O scores of 4.  All the other 4's and 5's came for domestic stocks.
## The big O-score returns come from mainly mining, mining support, steel.

# Build the Decision Tree model (it doesn't matter if "watchlist" is one of the parameters: the tree is the same)
decision_tree = rpart(return_12M ~ ., data = watchlist_201511[, c("V", "O_", "F_", "return_12M")],
    method = "anova", parms = list(split = "information"), control = rpart.control(usesurrogate = 0, maxsurrogate = 0))
decision_tree_title = "Decision Tree - watchlist RL1 (Global & Domestic) as of Nov 2015 - predict 12m return"

## Display (looks rubbish):
#plot(decision_tree, uniform = TRUE, main = decision_tree_title)
#text(decision_tree, use.n = TRUE, all = TRUE)

# Nice display (requires rattle):
fancyRpartPlot(decision_tree, main = "Decision Tree - watchlist RL1 (Global & Domestic) as of Nov 2015 - predict 12m return")

# A box-plot of O-score is quite enlightening (even though there a only six stocks with O1, and six with O5)...
ggplot(watchlist_201511, aes(y = return_12M)) +
  geom_boxplot(aes(x = "All"), notch = TRUE, fill = "grey") +
  stat_summary(aes(x = "All"), fun.y = mean, geom = "point", shape = 8) +
  geom_boxplot(aes(x = O_, fill = O_), notch = FALSE) +
  stat_summary(aes(x = O_), fun.y = mean, geom = "point", shape = 8) +
  xlab("O_\n\nRattle 2016-Dec-02 15:28:56 alebel") +
  ggtitle("Distribution of 12M total return\nby O Score") +
theme(legend.position = "none")

# Distribution of O & F
ggplot(watchlist_201511, aes(O_, fill = watchlist)) + geom_bar()
ggplot(watchlist_201511, aes(F_, fill = watchlist)) + geom_bar()
##
## 12 MONTH RETURNS BASED ON V, O & F SCORES
###########################################################################################


#######################################################################################
## 1 MONTH RETURNS BASED ON V+O+F SCORES AND THE CHANGES TO THE SCORES
summary(watchlist_1M$O_1M_chg_)
ggplot(watchlist_1M[which(watchlist_1M$O_1M_chg_ != 'O_1M_chg_0' && watchlist_1M$O_1M_chg_ != 'O_1M_chg_0'),], aes(O_1M_chg_, fill = watchlist)) + geom_bar()