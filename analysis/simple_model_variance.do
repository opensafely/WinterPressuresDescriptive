/*============================================================================
DO FILE NAME:			simple_models_variance.do
DATE: 					13/10/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Runs a simple random-effects regression model to estimate the variance between and within practices 
==============================================================================*/	

//Setting directory for user-written commands
	//xframeappend; grc1leg; egenmore 
adopath + ../workspace/analysis/ado 


//Creating the file paths for outputs
// mkdir ..workspace/output
cap mkdir output/model_variance

 
//Creating the frame we'll store the results of each model in  
clear frames
	frame create model_variance str20 cohort str100 model ///
						 str20 outcome b_outcome se_outcome ///
						 str20 covariate b_covariate se_covariate ///
						 var_btw var_wth icc lr_test_p ///
						 error
						 
// Importing data
foreach cohort in precovid postcovid1 postcovid2 postcovid3 {
import delimited using ../workspace/output/analytic_data_long_`cohort'.csv, varnames(1) clear 

//Exclude practices with fewer than <1000 patients 
	count if exp_denom <1000
	qui levelsof practice_pseudo_id if exp_denom <1000
	di "We will drop `r(r)' unique practices, comprising `r(N)' total observations in this longitudinal data"
	drop if exp_denom <1000
	
	
//Renaming & generating variables to make it easier to code
	rename out_* *
	rename acscs_* * 
	rename denom dnm
		
	rename *diabetes* *dbts*
	rename *asthma* *ast*
	rename *_angina_* *_ang_* 
		
	gen cohort = "`cohort'"
		
//Setting as a panel variable
	xtset practice_pseudo_id week_number
		
	set maxiter 16000
		
//Running the regressions on each outcome	
	foreach var of varlist prop_* {
		local model_name Model 1, intercept only
		cap mixed `var' || practice_pseudo_id: // Model 1: Random intercept only 
			if _rc != 0 {
				di _n "`cohort' model 1 `var' did not run, error:" _rc
				frame post model_variance ("`cohort'") ("`model_name'") ("`var'") ///
					(.) (.) ("") (.) (.) (.) (.) (.) (.) ///
					(_rc)
				continue	
			}
			scalar b_out = _b[`var': _cons]
			scalar se_out = _se[`var': _cons]
			scalar var_btw = exp(_b[lns1_1_1:_cons])^2 //var(_cons): between-practice variance
			scalar var_wth = exp(_b[lnsig_e:_cons])^2 //var(Residual): within-practice variance 
			cap scalar lr_test_p = `e(p_c)'
			
			cap scalar var_icc = (var_btw/(var_btw + var_wth)) //What prop. of the TOTAL variance is bw practices?	
		
		frame post model_variance ("`cohort'") ("`model_name'") ///
				("`var'") (b_out) (se_out) ///
				("") (.) (.) ///
				(var_btw) (var_wth) (var_icc) (lr_test_p) (.)
	}
		
	foreach var of varlist prop_* {
		local model_name Model 2, time covariate
		cap mixed `var' week_number || practice_pseudo_id: //Model 2: Random intercept + time covariate 
			if _rc != 0 {
				di _n "`cohort' model 1 `var' did not run, error:" _rc
				frame post model_variance ("`cohort'") ("`model_name'") ("`var'") ///
					(.) (.) ("") (.) (.) (.) (.) (.) (.) ///
					(_rc)
				continue		
			}
			scalar b_out = _b[`var': _cons]
			scalar se_out = _se[`var': _cons]
			scalar b_cov = _b[`var': week_number]
			scalar se_cov = _se[`var': week_number]
				
			scalar var_btw= exp(_b[lns1_1_1:_cons])^2 //var(_cons): between-practice variance
			scalar var_wth = exp(_b[lnsig_e:_cons])^2 //var(Residual): within-practice variance 
			scalar lr_test_p = `e(p_c)'

			scalar var_icc = (var_btw/(var_btw + var_wth)) //What prop. of the TOTAL variance is bw practices?	
				
		frame post model_variance ("`cohort'") ("`model_name'") ///
			("`var'") (b_out) (se_out) ///
			("Week") (b_cov) (se_cov) ///
			(var_btw) (var_wth) (var_icc) (lr_test_p) (.)
			
	}
}	
	
	
//Exporting the frame as a .csv	
	frame change model_variance
	export delimited using ../workspace/output/model_variance/model_variance.csv, replace			
	
	
	
	
	
	
	
	
	