get_currency_index = function(currency) {
    currency = toupper(currency)
    if (!currency %in% c("AUD", "LOCAL")) stop("currency must be AUD or LOCAL")
    sql = paste(
        "SELECT metric_date as [date], metric_value as [DXY_", currency,
         "] FROM PCI_REPORTING.dbo.t_data_fs_eps WHERE sec_id = dbo.GetSecId('DXY-IFUS.US') AND fs_code = 'FG_RETURN_ICS_", 
         currency, "'", sep = "")
    df = get_table_from_sql_CISMPRDSVR(sql)
    x = xts(df[, 2], order.by = as_date(df[, 1]))
    return(x)
}