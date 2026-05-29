/****************************************************************************************************
DO-FILE NAME:                icc_outcome.do
DATE:                        28/05/2026
DESCRIPTION:                 This do-file runs mixed-effects models to estimate Intraclass correlation coefficients (ICC)
                             
                             ICC: the proportion of total variation in weekly hospital use attributable to between-practice differences, relative to within-practice week-to-week variation during the winter period. 
                             Hospital use outcomes are:
                             - APC (all, planned, unplanned, and due to any ACSC conditions)
                             - EC (all and EC due to any ACSC conditions)
                             
                         
                             For each outcome, the script calculates:
                             1.  Crude ICC model (intercept only): 
							     How much of the total variability in weekly hospital use is attributable to differences between practices?

							 2.  Week-adjusted ICC model: 
							     How much of the remaining variability is attributable to differences between practices after accounting for an overall linear week trend?

                             Comparing the above ICCs, and tell us to support the random-intercept model we used in main analysis.

                             Model outputs are stored in frames and exported as CSV files:
                             outcome (n=48)    
							 [APC]
										"apc_main"                  "apc_unpl_main"                "apc_plan_main"                "apc_acsc_any_main"   "apc_unpl_acsc_any_main"    "apc_plan_acsc_any_main"      
										"apc_acsc_any_sub_asth"     "apc_unpl_acsc_any_sub_asth"   "apc_plan_acsc_any_sub_asth"   "apc_sub_asth"        "apc_unpl_sub_asth"         "apc_plan_sub_asth" 
										"apc_acsc_any_sub_copd"     "apc_unpl_acsc_any_sub_copd"   "apc_plan_acsc_any_sub_copd"   "apc_sub_copd"        "apc_unpl_sub_copd"         "apc_plan_sub_copd" 
										"apc_acsc_any_sub_diab"     "apc_unpl_acsc_any_sub_diab"   "apc_plan_acsc_any_sub_diab"   "apc_sub_diab"        "apc_unpl_sub_diab"         "apc_plan_sub_diab"
										"apc_acsc_any_sub_htn"      "apc_unpl_acsc_any_sub_htn"    "apc_plan_acsc_any_sub_htn"    "apc_sub_htn"         "apc_unpl_sub_htn"          "apc_plan_sub_htn"
										"apc_acsc_any_sub_sevmh"    "apc_unpl_acsc_any_sub_sevmh"  "apc_plan_acsc_any_sub_sevmh"  "apc_sub_sevmh"       "apc_unpl_sub_sevmh"        "apc_plan_sub_sevmh"          
							 [EC]	
										"ec_acsc_any_main"          "ec_main"   
										"ec_acsc_any_sub_asth"      "ec_sub_asth" 
										"ec_acsc_any_sub_copd"      "ec_sub_copd"  
										"ec_acsc_any_sub_diab"      "ec_sub_diab"      
										"ec_acsc_any_sub_htn"       "ec_sub_htn"        
										"ec_acsc_any_sub_sevmh"     "ec_sub_sevmh" 
    
                             model           // mdl_crude | mdl_time_adj
							 icc
							 variance_between
							 variance_within
							 b_week
							 se_week
							 n_obs_midpoint6
							 n_practice_midpoint6
                             error (to capture error codes)
*****************************************************************************************************/	

**# //SETTING DIRECTORIES & IMPORTING DATA
adopath + "analysis/ado"

* Specify parameters
local cohort "`1'"

* Sepecify parameters locally
*local cohort "precovid"

//Creating the file paths for outputs
cap mkdir output/icc_outcome

//Read and describe data
clear frames 
use "./output/dataset_clean/icc_input-`cohort'.dta", clear 
describe

//Setting as a panel variable
destring week_number, replace
xtset practice_id week_number

//Identify outcome (starting with apc or ec)
ds apc_* ec_*
local outcomes `r(varlist)'

//Identify covariates
ds week_number
local cov_core `r(varlist)'

//Results frame
frame create icc_outcome ///
    str30 outcome ///
    str15 model ///
    double icc variance_between variance_within ///
    double b_week se_week ///
    double n_obs_midpoint6 n_practice_midpoint6 ///
	double error

foreach var of local outcomes {
*****************************************************
* Model 1: Crude
*****************************************************

	cap mixed `var' || practice_id:

	if _rc {
		frame post icc_outcome ///
			("`var'") ///
			("mdl_crude") ///
			(.) (.) (.) ///
			(.) (.) ///
			(.) (.) ///
			(_rc)
		continue
	}
	
	scalar var_btw = exp(_b[lns1_1_1:_cons])^2 // mixed stores log(SD); convert to variances
	scalar var_wth = exp(_b[lnsig_e:_cons])^2
	scalar var_icc = (var_btw/(var_btw+var_wth))
	
	scalar n_obs = e(N)
    scalar n_practice = el(e(N_g),1,1)
	
	scalar n_obs = ceil(n_obs/6)*6 - 3
    scalar n_practice = ceil(n_practice/6)*6 - 3

	frame post icc_outcome ///
		("`var'") ///
		("mdl_crude") ///
		(var_icc) ///
		(var_btw) ///
		(var_wth) ///
		(.) (.) ///
		(n_obs) ///
		(n_practice) ///
		(.)

*****************************************************
* Model 2: Week-adjusted
*****************************************************

	cap mixed `var' week_number || practice_id:

	if _rc {
		frame post icc_outcome ///
			("`var'") ///
			("mdl_week_adj") ///
			(.) (.) (.) ///
			(.) (.) ///
			(.) (.) ///
			(_rc)
		continue
	}
	
	scalar var_btw = exp(_b[lns1_1_1:_cons])^2 // mixed stores log(SD); convert to variances
	scalar var_wth = exp(_b[lnsig_e:_cons])^2
	scalar var_icc = (var_btw/(var_btw+var_wth))
	
	scalar b_cov = _b[`var':week_number]
	scalar se_cov = _se[`var':week_number]
	
	scalar n_obs = e(N)
    scalar n_practice = el(e(N_g),1,1)

	scalar n_obs = ceil(n_obs/6)*6 - 3
    scalar n_practice = ceil(n_practice/6)*6 - 3

	frame post icc_outcome ///
		("`var'") ///
		("mdl_week_adj") ///
		(var_icc) ///
		(var_btw) ///
		(var_wth) ///
		(b_cov) ///
		(se_cov) ///
		(n_obs) ///
		(n_practice) ///
		(.)

}

frame change icc_outcome

export delimited using "./output/icc_outcome/icc_outcome-`cohort'.csv", replace