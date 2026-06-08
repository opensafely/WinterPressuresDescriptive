/****************************************************************************************************
DO-FILE NAME:                regression_model.do
DATE:                        18/12/2025
DESCRIPTION:                 This do-file runs regression models for the following outcomes:
                             - APC (all, planned, unplanned, and due to any ACSC conditions)
                             - EC (all conditions)
                             

                             For each exposure–outcome combination, the script fits:
                             1.  A random-intercept Poisson model
                             2.  A random-intercept negative binomial model

							 For each type of analysis, fits three models per analysis:
						     1.  Crude
							 2.  Core-adjusted (age, sex)
							 3.  Fully adjusted

                             The script is designed to be reusable and is executed once per model
                             specified in `lib/active_analyses.rds` (created by `active_analyses.R`).

                             Model outputs are stored in frames and exported as CSV files:
                             analysis        // poisson | negbin | lr_test
                             model           // mdl_crude | mdl_age_sex | mdl_max_adj
                             term            // exp_prop | cov_* | possion_vs_negbin 
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
*local name "cohort_precovid-main-age_80-apc_plan_acsc_any"

//Read and describe data
clear frames 
use "./output/model/model_input-`name'.dta", clear 
describe
	
//Genarating key variables 	
gen log_dnm = log(out_denom)

//Setting as a panel variable
xtset practice_id week_number

//Identify covariates
ds cov_core_*
local cov_core `r(varlist)'

ds cov_other_*
local cov_other `r(varlist)'

//Results frame
frame create results_poisson ///
    str15 model ///
    str50 term ///
    double irr lci uci se_coef p_value ///
    double ri_variance ri_se ri_lci ri_uci ri_lb ri_ub ///
    double n_obs aic bic ///
	double error

frame create results_negbin ///
    str15 model ///
    str50 term ///
    double irr lci uci se_coef p_value ///
    double ri_variance ri_se ri_lci ri_uci ri_lb ri_ub ///
    double n_obs aic bic ///
	double error

frame create results_lrtest ///
    str15 model ///
	str50 term ///
    double chi2_lr p_lr n_obs


// Define model specifications
local models "mdl_crude mdl_age_sex mdl_max_adj"

foreach mdl of local models {

    if "`mdl'" == "mdl_crude" {
        local covs ""
    }

    if "`mdl'" == "mdl_age_sex" {
        local covs "`cov_core'"
    }

    if "`mdl'" == "mdl_max_adj" {
        local covs "`cov_core' `cov_other'"
    }

// Fit poisson and negative binomial models
    foreach analysis in poisson negbin {

        if "`analysis'" == "poisson" {
            local cmd "mepoisson"
        }
        if "`analysis'" == "negbin" {
            local cmd "menbreg"
        }

        capture `cmd' out_num exp_prop `covs', ///
            offset(log_dnm) ///
            || practice_id:, irr

        if _rc != 0 {
            di _n "`cmd' failed for model `mdl'"
            di "STATA error code: " _rc
            * Post a placeholder row so the failure is logged
            frame post results_`analysis' ///
                ("`mdl'") ///
                ("MODEL_FAILED") ///
                (.) (.) (.) (.) (.) ///
                (.) (.) (.) (.) (.) (.) ///
                (.) (.) (.) ///
                (_rc)
            
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

                * Skip intercept
                if "`term'" == "_cons" continue
                
                * Skip offset
                if "`term'" == "log_dnm" continue
                
                * Skip distributional overdispersion parameter in the negative binomial models (we will use lrtest for decision making)
                * if "`term'" == "lnalpha" continue
                
                * Skip random-effect variance
                if strpos("`term'", "var(") continue

                scalar irr = b[1,`j']
                scalar se_coef  = b[2,`j']
                scalar p_value   = b[4,`j']
                scalar lci = b[5,`j']
                scalar uci = b[6,`j']

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
	
	
	
	
	
	
	

		