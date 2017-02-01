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
        r.squared = numeric(1),
        adj.r.squared = numeric(1),
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

    next_coefficient_index = 2

    df$intercept = lm_object$coefficients[1]
    s = summary(lm_object)
    df$F_value = s$fstatistic[1]
    df$p_value = pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = F)
    df$r.squared = s$r.squared
    df$adj.r.squared = s$adj.r.squared
    df$residual_error = s$sigma

    a = anova(lm_object)

    if (include_bmark) {
        df$bmark_beta = lm_object$coefficients[next_coefficient_index]
        df$bmark_F_value = a$`F value`[next_coefficient_index - 1]
        df$bmark_p_value = a$`Pr(>F)`[next_coefficient_index - 1]
        next_coefficient_index = next_coefficient_index + 1
    } else {
        df$bmark_beta = NA
        df$bmark_F_value = NA
        df$bmark_p_value = NA
    }

    if (include_bond) {
        df$bond_beta = lm_object$coefficients[next_coefficient_index]
        df$bond_F_value = a$`F value`[next_coefficient_index - 1]
        df$bond_p_value = a$`Pr(>F)`[next_coefficient_index - 1]
        next_coefficient_index = next_coefficient_index + 1
    } else {
        df$bond_beta = NA
        df$bond_F_value = NA
        df$bond_p_value = NA
    }

    if (include_yield) {
        df$yield_beta = lm_object$coefficients[next_coefficient_index]
        df$yield_F_value = a$`F value`[next_coefficient_index - 1]
        df$yield_p_value = a$`Pr(>F)`[next_coefficient_index - 1]
        next_coefficient_index = next_coefficient_index + 1
    } else {
        df$yield_beta = NA
        df$yield_F_value = NA
        df$yield_p_value = NA
    }

    return(df)
}

## Test:
#x = single_experiment_summary("WBC", "XJO", include_bmark = TRUE, lm_object = asset_benchmark.lm)
#x = rbind(x, single_experiment_summary("WBC", "XJO", include_bond = TRUE, lm_object = asset_bond.lm))
#x = rbind(x, single_experiment_summary("WBC", "XJO", include_yield = TRUE, lm_object = asset_yield.lm))
#x = rbind(x, single_experiment_summary("WBC", "XJO", include_bmark = TRUE, include_bond = TRUE, lm_object = asset_bmark_and_bond.lm))
#x = rbind(x, single_experiment_summary("WBC", "XJO", include_bmark = TRUE, include_yield = TRUE, lm_object = asset_bmark_and_yield.lm))
#x = rbind(x, single_experiment_summary("WBC", "XJO", include_bond = TRUE, include_yield = TRUE, lm_object = asset_bond_and_yield.lm))
#x = rbind(x, single_experiment_summary("WBC", "XJO", include_bmark = TRUE, include_bond = TRUE, include_yield = TRUE, lm_object = asset_benchmark_factors.lm))

