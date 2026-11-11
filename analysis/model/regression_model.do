/****************************************************************************************************
DO-FILE NAME:                regression_model.do
DATE:                        18/12/2025
UPDATE:                      29/07/2026 (to include mutually adjusted model)
DESCRIPTION:                 This do-file runs regression models for the following outcomes:
                             - APC (all, planned, unplanned, and due to any ACSC conditions)
                             - EC (all conditions)
                             

                             For each active cohort–analysis–outcome combination,
                             the script fits:
                             1. A random-intercept Poisson model
                             2. A random-intercept negative binomial model

                             For each single-exposure analysis, three adjustment
                             models are fitted:
                             1. Crude
                             2. Age- and sex-adjusted
                             3. Maximally adjusted

                             For each mutually adjusted analysis, one model is
                             fitted:
                             1. A mutually adjusted model containing all selected
                                exposures

                             Exposure naming:
                             - exp_num_*: numeric exposures
                             - exp_cat_*: categorical exposures

                             Covariate naming:
                             - cov_core_*: age and sex covariates
                             - cov_other_*: other covariates

                             Outputs:
                             - Poisson model results
                             - Negative binomial model results
                             - Poisson versus negative binomial LR tests

                             The script is designed to be reusable and is executed once per model
                             specified in `lib/active_analyses.rds` (created by `active_analyses.R`).

                             Model outputs are stored in frames and exported as CSV files:
                             analysis        // poisson | negbin | lr_test
                             model // mdl_crude | mdl_age_sex | mdl_max_adj | mdl_mut_adj
                             term  // exp_num_* | exp_cat_* | cov_* | _cons | poisson_vs_negbin
							 IRR for possion/negbin:
							 irr
							 lci
							 uci
							 se_coef
							 p_value
							 Random-intercept variance:
                             ri_variance
							 ri_se
							 ri_lci
							 ri_uci
							 ri_lb
							 ri_ub
                             Model-level statistics:
							 n_obs
							 aic
							 bic
                             r2_marginal r2_conditional
                             Predicted rates per patient-week (all exposure contrasts):
                             rate_reference rate_comparison
                             rate_reference_se rate_comparison_se rate_pair_cov
                             rate_reference_level rate_comparison_level
							 likelihood-ratio test:
							 chi2_lr 
							 p_lr 
							 n_obs
                             error (to capture error codes)
NOTES:                       This script is adapted from:
                             /analysis/outcome_time_var/reg_all_cond.do
*****************************************************************************************************/	
**# //SETTING DIRECTORIES & IMPORTING DATA
adopath + "analysis/ado"

* Specify parameters
local name "`1'"

* Specify parameters locally
*local name "cohort_postcovid1-main-all-apc_acsc_any"
*local name "cohort_precovid-main-all-apc"

//Read and describe data
clear frames 
use "./output/model/model_input-`name'.dta", clear 
describe
	
//Genarating key variables 	
gen log_dnm = log(out_denom)

//Setting as a panel variable
xtset practice_id week_number

//Identify analysis type

* Mutually adjusted analyses use "all" in the analysis name
local is_mutually_adjusted = strpos("`name'", "-all-") > 0

display "Mutually adjusted analysis: `is_mutually_adjusted'"

//Identify exposures (numeric)

capture ds exp_num_*

if _rc {
    local exposure_num ""
}
else {
    local exposure_num `r(varlist)'
}

//Identify exposures (categorical)

capture ds exp_cat_*

if _rc {
    local exposure_cat ""
}
else {
    local exposure_cat `r(varlist)'
}

//CREATE EXPOSURE MODEL SPECIFICATION

* Numeric exposures enter directly.
local exposure_spec "`exposure_num'"

* Categorical exposures enter using Stata factor-variable notation.
foreach var of local exposure_cat {
    local exposure_spec ///
        "`exposure_spec' i.`var'"
}

display "Exposure specification: `exposure_spec'"

//Identify covariates
capture ds cov_core_*

if _rc {
    local cov_core ""
}
else {
    local cov_core `r(varlist)'
}

//Identify other covariates
capture ds cov_other_*

if _rc {
    local cov_other ""
}
else {
    local cov_other `r(varlist)'
}

//CREATE CORE-COVARIATE MODEL SPECIFICATION

local cov_core_spec ""

foreach var of local cov_core {
    local vallab : value label `var'

    if "`vallab'" != "" {
        local cov_core_spec ///
            "`cov_core_spec' i.`var'"
    }
    else {
        local cov_core_spec ///
            "`cov_core_spec' `var'"
    }
}


//CREATE OTHER-COVARIATE MODEL SPECIFICATION

local cov_other_spec ""

foreach var of local cov_other {
    local vallab : value label `var'

    if "`vallab'" != "" {
        local cov_other_spec ///
            "`cov_other_spec' i.`var'"
    }
    else {
        local cov_other_spec ///
            "`cov_other_spec' `var'"
    }
}

display "Core covariates: `cov_core_spec'"
display "Other covariates: `cov_other_spec'"

// R-squared for the Poisson / NB2 models with one practice random intercept.
// Nakagawa et al. (2017), lognormal approximation:
// https://doi.org/10.1098/rsif.2017.0213
capture program drop wp_count_r2
program define wp_count_r2, rclass
    args family variance_scalar

    if e(converged) != 1 {
        noisily display as text "R-squared skipped: the fitted model did not report convergence."
        exit 498
    }

    tempvar xb mu_i
    tempname v_practice v_fixed alpha mu v_obs v_total

    // Reuse the fitted practice variance already extracted from r(table).
    // Zero and near-zero variances are valid for this calculation.
    scalar `v_practice' = scalar(`variance_scalar')
    if missing(scalar(`v_practice')) | scalar(`v_practice') < 0 {
        noisily display as text "R-squared skipped: fitted practice variance is missing or negative."
        exit 498
    }

    // Exclude the offset from the variance explained by predictors.
    quietly predict double `xb' if e(sample), xb nooffset
    quietly summarize `xb' if e(sample)
    if r(N) != e(N) | r(N) < 2 | missing(r(Var)) {
        noisily display as text "R-squared skipped: fixed-predictor variance is unavailable for the estimation sample."
        exit 498
    }
    scalar `v_fixed' = r(Var)

    // Use expected weekly COUNTS, at observed denominators, for V_obs.
    // Integrate over the normal practice random intercept: E(exp(u))=exp(V/2).
    quietly generate double `mu_i' = ///
        exp(`xb' + log_dnm + scalar(`v_practice') / 2) if e(sample)
    quietly summarize `mu_i' if e(sample), meanonly
    if r(N) != e(N) | missing(r(mean)) | r(mean) <= 0 {
        noisily display as text "R-squared skipped: mean fitted count is missing or nonpositive."
        exit 498
    }
    scalar `mu' = r(mean)

    // Poisson: variance = mu. Default menbreg (NB2): mu + alpha * mu^2.
    scalar `alpha' = 0
    if "`family'" == "negbin" {
        if "`e(dispersion)'" != "mean" {
            noisily display as text "R-squared skipped: the fitted negative binomial model does not use mean dispersion."
            exit 498
        }
        scalar `alpha' = exp(_b[/lnalpha])
    }

    scalar `v_obs' = ln(1 + 1 / scalar(`mu') + scalar(`alpha'))
    scalar `v_total' = scalar(`v_fixed') + scalar(`v_practice') + scalar(`v_obs')
    if missing(scalar(`v_total')) | scalar(`v_total') <= 0 {
        noisily display as text "R-squared skipped: total variance is missing or nonpositive."
        exit 498
    }

    return scalar marginal = scalar(`v_fixed') / scalar(`v_total')
    return scalar conditional = ///
        (scalar(`v_fixed') + scalar(`v_practice')) / scalar(`v_total')
end

// Predict all estimable levels of a categorical exposure in one margins call.
// Read the base from the fitted model so the rates use the IRR reference.
capture program drop wp_categorical_rates
program define wp_categorical_rates, rclass
    args factor
    tempname fit_b rates rate_vce
    matrix `fit_b' = e(b)
    local fit_terms : colnames `fit_b'

    local ref_code ""
    local comparison_codes ""
    foreach fitted_term of local fit_terms {
        if regexm("`fitted_term'", "^([0-9]+)b[.]`factor'$") {
            local ref_code = regexs(1)
        }
        else if regexm("`fitted_term'", "^([0-9]+)[.]`factor'$") {
            local code = regexs(1)
            local comparison_codes "`comparison_codes' `code'"
        }
    }
    if "`ref_code'" == "" | trim("`comparison_codes'") == "" {
        exit 498
    }

    // A separate at() for each level makes the reference the first margin.
    local levels "`ref_code' `comparison_codes'"
    local at_options ""
    foreach code of local levels {
        local at_options "`at_options' at(`factor'=`code')"
    }
    quietly margins, `at_options' predict(mu marginal nooffset)
    matrix `rates' = r(b)
    matrix `rate_vce' = r(V)

    local n_levels : word count `levels'
    if colsof(`rates') != `n_levels' | ///
        rowsof(`rate_vce') != `n_levels' | colsof(`rate_vce') != `n_levels' {
        exit 498
    }

    return local levels "`levels'"
    return matrix rates = `rates'
    return matrix rate_vce = `rate_vce'
end

//Results frame
frame create results_poisson ///
    str15 model ///
    str100 term ///
    double irr lci uci se_coef p_value ///
    double ri_variance ri_se ri_lci ri_uci ri_lb ri_ub ///
    double n_obs aic bic ///
    double(r2_marginal r2_conditional) ///
    double error ///
    double(rate_reference rate_comparison rate_reference_se rate_comparison_se rate_pair_cov) ///
    str80 rate_reference_level ///
    str80 rate_comparison_level

frame create results_negbin ///
    str15 model ///
    str100 term ///
    double irr lci uci se_coef p_value ///
    double ri_variance ri_se ri_lci ri_uci ri_lb ri_ub ///
    double n_obs aic bic ///
    double(r2_marginal r2_conditional) ///
    double error ///
    double(rate_reference rate_comparison rate_reference_se rate_comparison_se rate_pair_cov) ///
    str80 rate_reference_level ///
    str80 rate_comparison_level

frame create results_lrtest ///
    str15 model ///
	str100 term ///
    double chi2_lr p_lr n_obs


// Define model specifications
if `is_mutually_adjusted' {
    local models "mdl_mut_adj"
}
else {
    local models "mdl_crude mdl_age_sex mdl_max_adj"
}

tempname rate_b rate_V cat_rate_b cat_rate_V

foreach mdl of local models {

    * Define adjustment variables
    if "`mdl'" == "mdl_crude" {
        local covs ""
    }

    if "`mdl'" == "mdl_age_sex" {
        local covs "`cov_core_spec'"
    }

    if "`mdl'" == "mdl_max_adj" {
        local covs ///
            "`cov_core_spec' `cov_other_spec'"
    }

    if "`mdl'" == "mdl_mut_adj" {
        * All predictors are included in exposure_spec.
        local covs ""
    }

// Fit poisson and negative binomial models
    foreach analysis in poisson negbin {

        if "`analysis'" == "poisson" {
            local cmd "mepoisson"
        }
        if "`analysis'" == "negbin" {
            local cmd "menbreg"
        }

        capture noisily `cmd' out_num `exposure_spec' `covs', ///
            offset(log_dnm) ///
            || practice_id:, irr

        if _rc != 0 {
            local model_error = _rc
            display _n "`cmd' failed for model `mdl'"
            display "Stata error code: `model_error'"

            * Post a placeholder row so the failure is logged
            frame post results_`analysis' ///
                ("`mdl'") ///
                ("MODEL_FAILED") ///
                (.) (.) (.) (.) (.) ///
                (.) (.) (.) (.) (.) (.) ///
                (.) (.) (.) (.) (.) ///
                (`model_error') ///
                (.) (.) (.) (.) (.) ///
                ("") ("")
            
            continue
        }
        else {
            * ---- copy coefficient table before any postestimation commands ----
            matrix b = r(table)
            est store `analysis'_`mdl'
            scalar n_obs = e(N)
            
            * ---- model-level statistics
            estat ic
            matrix IC = r(S)
            scalar aic = IC[1,5]
            scalar bic = IC[1,6]

            * ---- fixed-effect coefficients and random-intercept variance ----
            local colnames : colnames b
            
            ** random-intercept variance
            local ri_col = .
            local c = 1
            foreach cn of local colnames {
                if strpos("`cn'", "var(") {
                    local ri_col = `c'
                }
                local ++c
            }
            
            if `ri_col' < . {
                scalar ri_variance = b[1, `ri_col']
                scalar ri_se = b[2, `ri_col']
                scalar ri_lci = b[5, `ri_col']
                scalar ri_uci = b[6, `ri_col']
                scalar ri_lb = exp(-1.96 * sqrt(ri_variance))
                scalar ri_ub = exp( 1.96 * sqrt(ri_variance))
            }
            else {
                scalar ri_variance = .
                scalar ri_se = .
                scalar ri_lci = .
                scalar ri_uci = .
                scalar ri_lb = .
                scalar ri_ub = .
            }
            
            * ---- variance-based R-squared; leave missing if unavailable ----
            scalar r2_marginal = .
            scalar r2_conditional = .
            capture quietly wp_count_r2 `analysis' ri_variance
            local r2_rc = _rc
            if `r2_rc' == 0 {
                scalar r2_marginal = r(marginal)
                scalar r2_conditional = r(conditional)
            }
            else {
                display as text "R-squared unavailable for `analysis' `mdl' (code `r2_rc')"
            }

            ** fixed-effect coefficients
            local k = colsof(b)
            local cached_rate_factor ""
            local cached_rate_levels ""
            local cached_ref_label ""

            forvalues j = 1/`k' {
                local term : word `j' of `colnames'
                local is_numeric_exposure : list term in exposure_num
                local is_categorical_exposure 0

                * Reset indicators for every coefficient
                local is_reference 0
                local is_omitted 0

                * Identify an omitted coefficient
                if strpos("`term'", "o.") > 0 {
                    local is_omitted 1
                }

                * Retain the intercept for the mutually adjusted model for prediction, skip otherwise
                if "`term'" == "_cons" {
                    if !`is_mutually_adjusted' {
                        continue
                    }
                }
                
                * Skip offset
                if "`term'" == "log_dnm" continue
                
                * Skip random-effect variance
                if strpos("`term'", "var(") continue

                * Translate factor-variable codes to category labels
                if strpos("`term'", ".") > 0 {

                    * Find the position of the first period
                    local dot_position = strpos("`term'", ".")

                    * Extract the category code before the period
                    local category_code = substr( ///
                        "`term'", ///
                        1, ///
                        `dot_position' - 1 ///
                    )

                    * Extract the variable name after the period
                    local factor_variable = substr( ///
                        "`term'", ///
                        `dot_position' + 1, ///
                        strlen("`term'") - `dot_position' ///
                    )

                    local is_categorical_exposure : list factor_variable in exposure_cat

                    * Check whether the term is the reference category
                    if strpos("`category_code'", "b") > 0 {
                        local is_reference 1
                    }

                    * Check whether the category was omitted
                    if strpos("`category_code'", "o") > 0 {
                        local is_omitted 1
                    }

                    * Remove Stata factor-variable markers from the category code
                    local category_code = subinstr( ///
                        "`category_code'", ///
                        "b", ///
                        "", ///
                        . ///
                    )

                    local category_code = subinstr( ///
                        "`category_code'", ///
                        "o", ///
                        "", ///
                        . ///
                    )

                    local category_code = subinstr( ///
                        "`category_code'", ///
                        "n", ///
                        "", ///
                        . ///
                    )

                    local category_label "`category_code'"

                    * Check whether the variable exists
                    capture confirm variable `factor_variable'

                    if _rc == 0 {

                        * Obtain its value-label name
                        local factor_vallab : value label `factor_variable'

                        if "`factor_vallab'" != "" {

                            * Translate the numeric category code into its label
                            local category_label : label ///
                                `factor_vallab' ///
                                `category_code'

                            * Create a readable output term
                            if `is_reference' == 1 {
                                local term ///
                                    "`factor_variable'_`category_label' (ref)"
                            }
                            else if `is_omitted' == 1 {
                                local term ///
                                    "`factor_variable'_`category_label' (omitted)"
                            }
                            else {
                                local term ///
                                    "`factor_variable'_`category_label'"
                            }
                        }
                    }
                }

                * Rename omitted numeric exposure terms
                if `is_omitted' == 1 ///
                    & strpos("`term'", "o.") == 1 {

                    local term = subinstr( ///
                        "`term'", ///
                        "o.", ///
                        "", ///
                        1 ///
                    )

                    local term "`term' (omitted)"
                }

                if `is_omitted' {
                    scalar irr = .
                    scalar lci = .
                    scalar uci = .
                    scalar se_coef = .
                    scalar p_value = .
                }
                else {
                    scalar irr = b[1, `j']
                    scalar lci = b[5, `j']
                    scalar uci = b[6, `j']
                    scalar se_coef = b[2, `j']
                    scalar p_value = b[4, `j']
                }

                * ---- predicted rates per patient-week ----
                scalar rate_reference = .
                scalar rate_comparison = .
                scalar rate_reference_se = .
                scalar rate_comparison_se = .
                scalar rate_pair_cov = .
                local rate_reference_level ""
                local rate_comparison_level ""

                if `is_numeric_exposure' & !`is_omitted' & e(converged) == 1 {
                    local rate_reference_level "Median"
                    local rate_comparison_level "Median + 1 MAD"

                    * On the upstream median-centred, MAD-scaled exposure scale:
                    * 0 = median; 1 = median + one MAD.
                    * Average over the same practice-weeks and integrate out
                    * the practice random intercept. No offset => per patient.
                    capture quietly margins, at(`term'=(0 1)) ///
                        predict(mu marginal nooffset)
                    local rate_rc = _rc
                    if `rate_rc' == 0 {
                        matrix `rate_b' = r(b)
                        matrix `rate_V' = r(V)
                        if colsof(`rate_b') == 2 {
                            scalar rate_reference = `rate_b'[1,1]
                            scalar rate_comparison = `rate_b'[1,2]
                        }
                        if rowsof(`rate_V') == 2 & colsof(`rate_V') == 2 & ///
                            !missing(rate_reference, rate_comparison) {
                            scalar rate_reference_se = sqrt(`rate_V'[1,1])
                            scalar rate_comparison_se = sqrt(`rate_V'[2,2])
                            scalar rate_pair_cov = `rate_V'[1,2]
                        }
                        if missing(rate_reference, rate_comparison) {
                            display as text "Predicted rates not estimable for `analysis' `mdl' `term'"
                        }
                    }
                    else {
                        display as text "Predicted rates unavailable for `analysis' `mdl' `term' (code `rate_rc')"
                    }
                }

                if `is_categorical_exposure' & !`is_reference' & ///
                    !`is_omitted' & e(converged) == 1 {
                    // Reuse the joint margins and covariance across levels
                    // of the same categorical exposure in this model.
                    if "`factor_variable'" != "`cached_rate_factor'" {
                        local cached_rate_factor "`factor_variable'"
                        local cached_rate_levels ""
                        local cached_ref_label ""
                        capture quietly wp_categorical_rates `factor_variable'
                        local rate_rc = _rc
                        if `rate_rc' == 0 {
                            matrix `cat_rate_b' = r(rates)
                            matrix `cat_rate_V' = r(rate_vce)
                            local cached_rate_levels "`r(levels)'"
                            local ref_code : word 1 of `cached_rate_levels'
                            local cached_ref_label "`ref_code'"
                            local ref_vallab : value label `factor_variable'
                            if "`ref_vallab'" != "" {
                                local cached_ref_label : label `ref_vallab' `ref_code'
                            }
                        }
                        else {
                            display as text "Categorical rates unavailable for `analysis' `mdl' `factor_variable' (code `rate_rc')"
                        }
                    }

                    local cat_index : list posof "`category_code'" in cached_rate_levels
                    if `cat_index' > 1 {
                        local rate_reference_level "`cached_ref_label'"
                        local rate_comparison_level "`category_label'"
                        scalar rate_reference = `cat_rate_b'[1,1]
                        scalar rate_comparison = `cat_rate_b'[1,`cat_index']
                        if !missing(rate_reference, rate_comparison) {
                            scalar rate_reference_se = sqrt(`cat_rate_V'[1,1])
                            scalar rate_comparison_se = sqrt(`cat_rate_V'[`cat_index',`cat_index'])
                            scalar rate_pair_cov = `cat_rate_V'[1,`cat_index']
                        }
                    }
                }

                frame post results_`analysis' ///
                    ("`mdl'") ///
                    ("`term'") ///
                    (irr) (lci) (uci) (se_coef) (p_value) ///
                    (ri_variance) (ri_se) (ri_lci) (ri_uci) (ri_lb) (ri_ub) ///
                    (n_obs) (aic) (bic) (r2_marginal) (r2_conditional) ///
                    (.) ///
                    (rate_reference) (rate_comparison) ///
                    (rate_reference_se) (rate_comparison_se) (rate_pair_cov) ///
                    ("`rate_reference_level'") ("`rate_comparison_level'")
            }
        }
    }

// Likelihood ratio test: poisson vs negbin (model-level)
    capture lrtest poisson_`mdl' negbin_`mdl'
    if _rc == 0 {
        frame post results_lrtest ///
		("`mdl'") ///
        ("poisson_vs_negbin") ///
		(r(chi2)) ///
		(r(p)) ///
		(n_obs)
	}
}

// Export model outputs to respective csv files
frame change results_poisson
export delimited using ///
    "./output/model/model_output_poisson-`name'.csv", replace

frame change results_negbin
export delimited using ///
    "./output/model/model_output_negbin-`name'.csv", replace

frame change results_lrtest
export delimited using ///
    "./output/model/model_output_lrtest-`name'.csv", replace		
