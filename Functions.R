library(xts) # <-- time series class used in our favourite quant packages
library(zoo) # <-- another useful time series class

# Convert price series into daily/weekly/monthly returns
xts_returns = function(frequency, given_xts, min_date, max_date)
{
  my_xts = given_xts[paste(min_date, max_date, sep='::')]
  temp = xts(order.by=index(my_xts))
  
  if (frequency == "weekly") {
    my_xts = my_xts[which(weekdays(index(my_xts)) == "Friday"),]
  }
  
  for(i in 1:ncol(my_xts))
  {
    if (frequency == "weekly") { 
      temp2 = weeklyReturn(my_xts[,i], leading = FALSE)
    } else if (frequency == "daily") {
      temp2 = dailyReturn(my_xts[,i], leading = FALSE)
    } else { 
      temp2 = monthlyReturn(my_xts[,i], leading = FALSE) 
    }
    names(temp2) = names(my_xts[,i])
    temp = merge(temp, temp2)
  }
  temp = temp[rowSums(is.na(temp)) != ncol(temp)]  # <-- remove rows which are all NA
  return(temp)
}

# Convert return series into normalised prices, with a 100 start price at the given min_date
xts_price_from_returns = function(xts_return, min_date) 
{ 
    xts_price = xts(100, min_date)
    if (dim(xts_return)[2] > 1) {
        for (i in 2:dim(xts_return)[2]) {
            xts_price = merge(xts_price, xts(100, min_date))
        }
    }
    
    if (dim(xts_return)[1] > 1) {
        for (i in 1:dim(xts_return)[1]) {
            d = index(xts_return)[i]
            singleton = zoo(100 * (1 + Return.cumulative(xts_return[which(index(xts_return) <= d),])), d)
            xts_price = rbind(xts_price, as.xts(singleton))
        }
    }
    colnames(xts_price) = colnames(xts_return)
    return(xts_price)
}

# Compute the average time series from a collection time series, excluding specified columns. 
# Name the column
xts_add_average_series = function(my_xts, new_series_name, exclude_these_columns)
{
  new_xts = rowMeans(my_xts, na.rm = TRUE)
  return_xts = cbind(my_xts, new_xts)
  names(return_xts)[ncol(return_xts)] = new_series_name
  return(return_xts)
}


# Rolling Correlation
rolling_correlation = function(xts1, xts2, width)
{
  raw = sapply(1:(nrow(xts1) - width), function(u) cor(xts1[u:(u+width),],xts2[u:(u+width),]))
  return_xts = as.xts(raw, order.by = index(xts1[(width+1):nrow(xts1),]))
  return(return_xts)
}

# Flatten xts to a data.frame
flatten_xts = function(my_xts, value_name, bucket_name)
{
    first_name = colnames(my_xts)[1]
    df = data.frame(date = index(my_xts), price = coredata(my_xts[, first_name]), currency = rep(first_name, dim(my_xts)[1]))
    names(df) = c("date", value_name, bucket_name)
    rownames(df) = NULL
    if (dim(my_xts)[2] > 1) {
        for (i in 2:dim(my_xts)[2]) {
            ccy = colnames(my_xts)[i]
            df2 = data.frame(date = index(my_xts), price = coredata(my_xts[, ccy]), currency = rep(ccy, dim(my_xts)[1]))
            names(df2) = c("date", value_name, bucket_name)
            df = rbind(df, df2)
        }
    }
    return(df)
}

# Copied from http://stackoverflow.com/questions/8979241/can-i-write-an-xts-object-using-write-csv-in-r
write.zoo = function(x, file = "", index.name = "Index", row.names = FALSE, col.names = NULL, ...) 
{
    if (is.null(col.names))
        col.names <- !is.null(colnames(x))
    dx <- as.data.frame(x)
    stopifnot(all(names(dx) != index.name))
    dx[[index.name]] <- index(x)
    dx <- dx[, c(ncol(dx), 1:(ncol(dx) - 1))]
    write.table(dx, file = file, sep = ",", row.names = row.names, col.names = col.names,
        ...)
}