# Let's use Citigroup USBIG Treasury (bond index only...  the yield data is monthly)
get_bond_index = function() {
    citi_usa_bond_index = read.csv(".//csv//SBGT.csv")
    colnames(citi_usa_bond_index) = c('date', 'citi_usa_index', 'citi_usa_yield')
    citi_usa_bond_index$date = mdy(citi_usa_bond_index$date)
    bond_index = xts(citi_usa_bond_index[, "citi_usa_index"], order.by = citi_usa_bond_index[, "date"])
    return(bond_index)
}

