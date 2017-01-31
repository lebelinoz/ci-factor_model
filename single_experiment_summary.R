single_experiment_summary = function(
    ticker,
    universe,
    include_bmark = FALSE,
    include_bond = FALSE,
    include_yield = FALSE,
    lm_object
) {
    if (!is.character(ticker)) stop("ticker must be a string and must be specified")
    if (!is.character(universe)) stop("universe must be a string and must be specified")
    if (!is.logical(include_bmark)) stop("include_bmark must a boolean")
    if (!is.logical(include_bond)) stop("include_bond must a boolean")
    if (!is.logical(include_yield)) stop("include_yield must a boolean")
    if (!is(lm_object, "lm")) stop("lm_object must be supplied")
    if (!any(c(include_bmark, include_bond, include_yield))) stop("at least one of the 'include_' parameters must be true")

    df = data.frame(
        ticker = ticker,
        universe = universe,
        bmark = include_bmark,
        bond = include_bond,
        yield = include_yield,
        multi_r_squared = numeric(1),
        adjusted_r_squared = numeric(1),
        F_value = numeric(1),
        p_value = numeric(1),
        intercept = numeric(1),
        bmark_beta = numeric(1),
        bond_beta = numeric(1),
        yield_beta = numeric(1),
        residual_error = numeric(1),
        bmark_F_value = numeric(1),
        bond_F_value = numeric(1),
        yield_F_value = numeric(1),
        bmark_p_value = numeric(1),
        bond_p_value = numeric(1),
        yield_p_value = numeric(1)
    )



    return(df)
}

# a = single_experiment_summary("WBC", "XJO", lm_object = asset_bond.lm)
x = single_experiment_summary("WBC", "XJO", include_bond = TRUE, lm_object = asset_bond.lm)
x$F_value[1]
