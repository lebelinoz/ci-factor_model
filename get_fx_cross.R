get_USD_fx = function(currency) {
    currency = toupper(currency)
    if (currency %in% c("LOC", "LOCAL")) stop("currency cannot be 'Local'")
    sql = paste("SELECT fx_date AS [date], fx_value FROM dbo.t_Data_FX where fx_code = 'USD", currency, "'", sep = "")
    df = get_table_from_sql_CISMPRDSVR(sql)
    x = xts(df[, 2], order.by = as_date(df[, 1]))
    return(x)
}

get_fx_cross = function(ccy1, ccy2) {
    fx1 = get_USD_fx(ccy1)
    fx2 = get_USD_fx(ccy2)
    x = fx2 / fx1 
    colnames(x)[1] = paste(ccy1, ccy2, sep = "")
    return(x)
}