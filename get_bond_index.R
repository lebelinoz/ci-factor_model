# Let's use Citigroup USBIG Treasury (bond index only...  the yield data is monthly)
get_bond_index = function(bond_code = 'SBGT.US') {
    bond_index = get_ticker_xts_t_data_fs_eps(bond_code, "FG_RETURN_ICS_LOCAL")
    colnames(bond_index) = c("bond")
    return(bond_index)
}

