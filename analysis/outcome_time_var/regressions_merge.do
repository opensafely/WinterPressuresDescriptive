/*============================================================================
DO FILE NAME:			regressions_merge.do
DATE: 					04/11/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Merges the regression results across cohorts 
==============================================================================*/	

//User-written commands - ssc install: xframeappend; grc1leg; egenmore; spost 
adopath + ../workspace/analysis/ado 

//Creating the file paths for outputs
cap mkdir ..workspace/output
cap mkdir output/regressions



//Importing data, and putting each cohort into a frame
clear frames 			 
foreach cohort in precovid postcovid1 postcovid2 postcovid3{
	//For APC/EC
	import delimited using ../workspace/output/regressions/results_all_cond_`cohort'.csv, varnames(1) clear
		frame copy default results_all_cond_`cohort', replace 
		frame results_all_cond_`cohort': replace cohort = "`cohort'"
		
	//For ACSCs within APC/EC
	import delimited using ../workspace/output/regressions/results_acsc_`cohort'.csv, varnames(1) clear
		frame copy default results_acsc_`cohort'
		frame results_acsc_`cohort': replace cohort = "`cohort'"
		frame results_acsc_`cohort': cap rename v_random_int variance_ri
}


//Making sure all the variables in each frame are in the right format  
//String: cohort hosp_type acscs model_form out_var exp_var lrtest_comparing check_*

	foreach cohort in precovid postcovid1 postcovid2 postcovid3 {
		frame change results_all_cond_`cohort'
			foreach var of varlist cohort hosp_type acscs model_form out_var exp_var lrtest_comparing check_*{
				cap confirm string variable `var'
					if _rc != 0 {
						di _n "Cohort: `cohort', outcome: `outcome', variable: `var' changed to string"
						tostring `var', replace
					}
			}
			foreach var of varlist obs *_exp *_cons *_ri *_lrtest ll p_ll aic bic error {
				cap confirm numeric variable `var'
					if _rc != 0 {
						di "`var' is NOT numeric"
					}
			}
		frame change results_acsc_`cohort'
			foreach var of varlist cohort hosp_type acscs model_form out_var exp_var lrtest_comparing check_*{
				cap confirm string variable `var'
					if _rc != 0 {
						di _n "Cohort: `cohort', outcome: `outcome', variable: `var' changed to string"
						tostring `var', replace
					}
				}
			foreach var of varlist obs *_exp *_cons *_cons_inf *_ri *alpha *_lrtest ll p_ll aic bic psuedo_r2 error {
				cap confirm numeric variable `var'
					if _rc != 0 {
						di "`var' is NOT numeric"
					}
			}
		frame change default 	
	}
	

//Appending the frames together
	//For APC/EC
	frame copy results_all_cond_precovid results_all_cond, replace
	frame results_all_cond: xframeappend results_all_cond_postcovid1 results_all_cond_postcovid2 results_all_cond_postcovid3

	//For ACSCs within APC/EC	
	frame copy results_acsc_precovid results_acsc, replace 
	frame results_acsc: xframeappend results_acsc_postcovid1 results_acsc_postcovid2 results_acsc_postcovid3
	
	
//Final bits of data management & exporting the merged frames 
foreach frame in all_cond acsc {
frame change results_`frame'
	
	gen model_form_num = . 
		replace model_form_num = 1 if model_form == "Negative binomial random intercepts"
		replace model_form_num = 2 if model_form == "Poisson random intercepts"
	
	gen exp_var_long = exp_var
		replace exp_var_long = "Ethnicity: Asian" if exp_var == "asian"
		replace exp_var_long = "Ethnicity: Black" if exp_var == "black"
		replace exp_var_long = "Ethnicity: White" if exp_var == "white"
		replace exp_var_long = "Ethnicity: Mixed" if exp_var == "mixed"
		replace exp_var_long = "Ethnicity: Other" if exp_var == "other"
		replace exp_var_long = "Age: <5 years" if exp_var == "u5y"
		replace exp_var_long = "Age: 65-74 years" if exp_var == "65_74"
		replace exp_var_long = "Age: 75-79 years" if exp_var == "75_79"
		replace exp_var_long = "Age: 80-84 years" if exp_var == "80_84"
		replace exp_var_long = "Age: ≥85 years" if exp_var == "85p"
		replace exp_var_long = "Sex: female" if exp_var == "female"
		replace exp_var_long = "IMD 1 (most dep.)" if exp_var == "imd1"
		replace exp_var_long = "IMD 5 (least dep.)" if exp_var == "imd5"
		replace exp_var_long = "Urban maj. conurb." if exp_var == "urb1"
		replace exp_var_long = "Rural/village" if exp_var == "urb5"
		replace exp_var_long = "Has asthma" if exp_var == "ast"
		replace exp_var_long = "Has diabetes" if exp_var == "dbts"
		replace exp_var_long = "Has hypertension" if exp_var == "hypt"
		replace exp_var_long = "Has obesity" if exp_var == "obs"
		replace exp_var_long = "Current smoker" if exp_var == "smoker"

	gen exp_var_num = . 
		replace exp_var_num = 1 if exp_var == "asian"
		replace exp_var_num = 2 if exp_var == "black"
		replace exp_var_num = 3 if exp_var == "white"
		replace exp_var_num = 4 if exp_var == "mixed"
		replace exp_var_num = 5 if exp_var == "other"
		replace exp_var_num = 6 if exp_var == "u5y"
		replace exp_var_num = 7 if exp_var == "65_74"
		replace exp_var_num = 8 if exp_var == "75_79"
		replace exp_var_num = 9 if exp_var == "80_84"
		replace exp_var_num = 10 if exp_var == "85p"
		replace exp_var_num = 11 if exp_var == "female"
		replace exp_var_num = 12 if exp_var == "imd1"
		replace exp_var_num = 13 if exp_var == "imd5"
		replace exp_var_num = 14 if exp_var == "urb1"
		replace exp_var_num = 15 if exp_var == "urb5"
		replace exp_var_num = 16 if exp_var == "ast"
		replace exp_var_num = 17 if exp_var == "dbts"
		replace exp_var_num = 18 if exp_var == "hypt"
		replace exp_var_num = 19 if exp_var == "obs"
		replace exp_var_num = 20 if exp_var == "smoker"
	
	export delimited using ../workspace/output/regressions/results_`frame'.csv, replace	
	
frame change default 	
}
	
	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	