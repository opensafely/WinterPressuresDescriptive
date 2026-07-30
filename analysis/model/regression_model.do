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

//Results frame
frame create results_poisson ///
    str15 model ///
    str100 term ///
    double irr lci uci se_coef p_value ///
    double ri_variance ri_se ri_lci ri_uci ri_lb ri_ub ///
    double n_obs aic bic ///
	double error

frame create results_negbin ///
    str15 model ///
    str100 term ///
    double irr lci uci se_coef p_value ///
    double ri_variance ri_se ri_lci ri_uci ri_lb ri_ub ///
    double n_obs aic bic ///
	double error

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
                (.) (.) (.) ///
                (`model_error')
            
            continue
        }
        else {
            est store `analysis'_`mdl'

            * ---- copy coefficient table ----
            matrix b = r(table)
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
            
            ** fixed-effect coefficients
            local k = colsof(b)

            forvalues j = 1/`k' {
                local term : word `j' of `colnames'

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

                frame post results_`analysis' ///
                    ("`mdl'") ///
                    ("`term'") ///
                    (irr) (lci) (uci) (se_coef) (p_value) ///
                    (ri_variance) (ri_se) (ri_lci) (ri_uci) (ri_lb) (ri_ub) ///
                    (n_obs) (aic) (bic) ///
                    (.)
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
