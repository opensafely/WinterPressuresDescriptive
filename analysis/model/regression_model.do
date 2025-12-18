/****************************************************************************************************
DO-FILE NAME:                regression_model.do
DATE:                        18/12/2025
DESCRIPTION:                 This do-file runs regression models for the following outcomes:
                             - APC (all, planned, unplanned, and due to any ACSC conditions)
                             - EC (all conditions)
                             

                             For each exposure–outcome combination, the script fits:
                             1.  A random-intercept Poisson model
                             2.  A random-intercept negative binomial model

                             The script is designed to be reusable and is executed once per model
                             specified in `lib/active_analyses.rds` (created by `active_analyses.R`).

                             Model outputs are stored in frames and exported as CSV files.

NOTES:                       This script is adapted from:
                             /analysis/outcome_time_var/reg_all_cond.do
*****************************************************************************************************/	
**# //SETTING DIRECTORIES & IMPORTING DATA
//User-written commands - ssc install: xframeappend; grc1leg; egenmore; spost 
adopath + ../workspace/analysis/ado 

//Creating the file paths for outputs
// mkdir ..workspace/output
cap mkdir output/regressions

* Specify parameters
local cohort "`1'"
local analysis "`2'"
local exposure "`3'"
local outcome "`4'"
local covariate_core "`5'"
local covariate_other "`6'"

/*
* Sepecify parameters locally
local cohort "prevax"
local analysis "main"
local exposure "ethinicity_white"
local outcome "apc_unpl_main"
local covariate_core "age_80;sex_female"
local covariate_other "exp_cat_region;imd_1_most;rurality_urban_comb;smoking_current;carehome;obesity;cons_mean"
*/

//Importing data 
clear frames 
import delimited using ../workspace/output/dataset_clean/input_`cohort'_clean.csv, varnames(1) clear 
	
	
//Genarating key variables 	
	gen cohort = "`1'"
	
	gen log_dnm = log(dnm)

	//Combining the 80_84 and 85p age categories 
		gen exp_num_80p = exp_num_80_84 + exp_num_85p
		gen exp_prop_80p = exp_num_80p/exp_dnm
		
		
//Make every exposure variable scale from 0 - 100
local exp_var_list u5y 65_74 75_79 80p white asian black other mixed imd1 imd5 ast dbts hypt obs urb1 urb5 female smoker

	foreach char in `exp_var_list'{
		replace exp_prop_`char' = exp_prop_`char'*100
	}

//Setting as a panel variable
	xtset practice_pseudo_id week_number
		
	
**# //REGRESSIONS & EXPORTING RESULTS 
//All-condition APC & EC: Random-intercept Poisson; Random-intercept negative binomial 
	if "`2'" == "unadjusted" {
		local cov unad
	}
	if "`2'" == "adjusted" {
		local cov adj
	}
	
//Frame storing regression results  
frame create results_all_cond_`cov'_`1' str10 cohort str4 hosp_type str15 acscs ///
								  str40 model_form str10 out_var str7 exp_var str45 co_var ///
								  obs overdispersion ///
								  irr_exp se_exp p_exp lci_exp uci_exp 			///
								  irr_cons se_cons p_cons lci_cons uci_cons  		///
								  variance_ri se_ri lci_ri uci_ri 				///
								  lb_ri_irr ub_ri_irr							///
								  str50 lrtest_comparing chi2_lrtest p_lrtest 	///
								  ll p_ll aic bic  								///
								  error 
							
//Loop that runs each regression, and then exports the results to a frame 	
local exp_var_list u5y 65_74 75_79 80p white asian black other mixed imd1 imd5 ast dbts hypt obs urb1 urb5 female smoker
	foreach regression in mepoisson menbreg {
		if "`regression'" == "mepoisson"{
			local reg_name Poisson
			local est_name mep 
			local lrtest_text fixed-effects only Poisson
			local a = 0
		}
		if "`regression'" == "menbreg" {
			local reg_name Negative binomial
				local est_name menb 
			local lrtest_text fixed-effects only negative binomial
			local a = 1
		}
			
		foreach var of varlist num_apc_w num_ec_w {
			if strpos("`var'", "apc") > 0 {
				local hosp apc
			}
			if strpos("`var'", "ec") > 0 {
				local hosp ec
			}
			foreach char in `exp_var_list'{
				local stub: subinstr local var "num_" "", all
				local stub: subinstr local stub "_w" "", all
				local acsc: subinstr local stub "_`hosp'" "", all
					
				if "`2'" == "unadjusted"{ 
					local covariate_list 
				}
				if "`2'" == "adjusted"{
					local exclude_list exp_prop_u5y exp_prop_80p exp_prop_female
						if strpos("`exclude_list'", "`char'") {
							local covariate_list: subinstr local exclude_list "exp_prop_`char'" "", all		
						}
						else{
							local covariate_list exp_prop_u5y exp_prop_80p exp_prop_female
						}
				}
				cap `regression' `var' exp_prop_`char' `covariate_list', offset(log_dnm) || practice_pseudo_id:, irr
					if _rc != 0  { 
						di _n "`regression' regression error: `var' --> exp_prop_`char'"
						di "STATA error code: " _rc
							
						frame post results_all_cond_`cov'_`1'														///
							("`1'") ("`hosp'") ("All conditions") 											///
							("`reg_name' random intercepts") ("`stub'") ("`char'") ("`covariate_list'") 	///
							(.) (.)							///
							(.) (.) (.) (.) (.)  			/// 
							(.) (.) (.) (.) (.) 			///
							(.) (.) (.) (.) 				///
							(.) (.) 						///
							("`lrtest_text'") (.) (.)		///
							(.) (.) (.) (.)					///
							(_rc)	
					}
					else {
						est store `est_name'_`stub'_`char'_`cov'
						local var_count: word count `covariate_list' //counts how many covariates 
							local i = `var_count' + 3			//Matrix column position for od (overdispersion)
							local j = `var_count' + 2			//Matrix column position for constant 
							local k = `var_count' + 3 + `a' 	//Matrix column position for random-intercept variance 

						matrix b = r(table) 	//Naming regression output matrix
							scalar obs = e(N) 					//Number of observations in model 
							scalar od = exp(b[1,`i'])*`a'		//Overdispersion (only provided by nbreg)
							
							scalar irr_exp = b[1,1] 
							scalar se_exp = b[2,1]
							scalar p_exp = b[4,1] 		//p-value, exposure 
							scalar lci_exp = b[5,1] 	//lower 95% CI, exposure
							scalar uci_exp = b[6,1] 	//upper 95% CI, exposure
								
							scalar irr_cons = b[1,`j']
							scalar se_cons = b[2,`j']
							scalar p_cons = b[4,`j'] 		//p-value, constant 
							scalar lci_cons = b[5,`j'] 		//lower 95% CI, constant
							scalar uci_cons = b[6,`j']		//upper 95% CI, constant
									
							scalar v_ri = b[1,`k'] 		//Variance of the random intercepts (log)
							scalar se_ri = b[2,`k']     //SE of the random intercepts (log)
							scalar lci_ri = b[5,`k']
							scalar uci_ri = b[6,`k']
							scalar lb_ri = exp(-1.96*sqrt(v_ri))	//Lower bound of the IRR range that 95% of practices will have for their random-intercept value  
							scalar ub_ri = exp(1.96*sqrt(v_ri)) 	//Upper bound of the IRR range that 95% of practices will have for their random-intercept value  
							
							scalar ll = e(ll) 							//Model's log-likelihood
							scalar p_ll = e(p)						 	//p-value, model 
							scalar chi2_lrtest = e(chi2_c)				//LR test chi2
							scalar p_lrtest = e(p_c)					//p-value, LR test
								
						qui estat ic 
						matrix ic_b = r(S)
							scalar aic = ic_b[1,5]				//AIC score
							scalar bic = ic_b[1,6]				//BIC score 
						
						frame post results_all_cond_`cov'_`1' 													///
							("postcovid3") ("`hosp'") ("All conditions") 									///
							("`reg_name' random intercepts") ("`stub'") ("`char'") ("`covariate_list'")		///
							(obs) (od)													///
							(irr_exp) (se_exp) (p_exp) (lci_exp) (uci_exp)  			///
							(irr_cons) (se_cons) (p_cons) (lci_cons) (uci_cons) 		///
							(v_ri) (se_ri) (lci_ri) (uci_ri)							///
							(lb_ri) (ub_ri)												///
							("`lrtest_text'") (chi2_lrtest) (p_lrtest)					///
							(ll) (p_ll) (aic) (bic)										///
							(.)	
													
							continue
						}	
				}			
			}	
		}
		
	
	
//Checking the regression statistics have expected ranges
frame change results_all_cond_`cov'_`1' 
		
	gen str check_p = ""
		foreach var of varlist p_* {
			cap assert `var' <1 if `var' !=.
				if _rc != 0 {
					replace check_p = check_p + "`var' > 1; "
					continue 
				}
		}
		
	gen str check_irr = ""
		foreach var of varlist irr_* {
			cap assert `var' > 0
				if _rc != 0 {
					replace check_irr = check_irr + "`var' < 0; " if `var' <=0 
					continue 
				}
		}
	gen str check_chi2 = ""
		foreach var of varlist chi2_* {
			cap assert `var' > 0 if `var' != . 
				if _rc !=0 {
					replace check_chi2 = check_chi2 + "`var' <= 0; " if `var' <=0
					continue
				}
		}

		
//Doing the likelihood ratio tests comparing the RI Poisson to RI NB 
	encode model_form, gen(model_form_num)
	
	gen lrtest_comparing2 = ""
		replace lrtest_comparing2 = "RI nbreg to RI Poisson" if model_form_num == 1
		
	gen chi2_lrtest2 = . 
	gen p_lrtest2 = . 
	gen error_lrtest2 = .
	
	replace overdisp = . if model_form_num == 2 //overdispersion stat not provided as a default output of Poisson reg
	
local exp_var_list u5y 65_74 75_79 80p white asian black other mixed imd1 imd5 ast dbts hypt obs urb1 urb5 female smoker
	foreach var in apc ec {
		foreach char in `exp_var_list'{
			cap lrtest mep_`var'_`char'_`cov' menb_`var'_`char'_`cov'
				if _rc != 0  { 
					replace error_lrtest2 = _rc if exp_var == "`char'" & out_var == "`var'" & co_var != "" & model_form_num == 1
					continue
				}
				else {
					replace chi2_lrtest2 = `r(chi2)' if exp_var == "`char'" & out_var == "`var'" & co_var != "" & model_form_num == 1
					replace p_lrtest2 = `r(p)' if exp_var == "`char'" & out_var == "`var'" & co_var != "" & model_form_num == 1
				}
				
		}
	}
		
		
//Final bits of data management to make forest plots nicer 
	gen exp_var_long = exp_var
		replace exp_var_long = "Ethnicity: Asian" if exp_var == "asian"
		replace exp_var_long = "Ethnicity: Black" if exp_var == "black"
		replace exp_var_long = "Ethnicity: White" if exp_var == "white"
		replace exp_var_long = "Ethnicity: Mixed" if exp_var == "mixed"
		replace exp_var_long = "Ethnicity: Other" if exp_var == "other"
		replace exp_var_long = "Age: <5 years" if exp_var == "u5y"
		replace exp_var_long = "Age: 65-74 years" if exp_var == "65_74"
		replace exp_var_long = "Age: 75-79 years" if exp_var == "75_79"
		replace exp_var_long = "Age: ≥80 years" if exp_var == "80p"
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
		replace exp_var_num = 9 if exp_var == "80p"
		replace exp_var_num = 10 if exp_var == "female"
		replace exp_var_num = 11 if exp_var == "imd1"
		replace exp_var_num = 12 if exp_var == "imd5"
		replace exp_var_num = 13 if exp_var == "urb1"
		replace exp_var_num = 14 if exp_var == "urb5"
		replace exp_var_num = 15 if exp_var == "ast"
		replace exp_var_num = 16 if exp_var == "dbts"
		replace exp_var_num = 17 if exp_var == "hypt"
		replace exp_var_num = 18 if exp_var == "obs"
		replace exp_var_num = 19 if exp_var == "smoker"		
	
	
	
//Exporting the frame as a .csv 
export delimited using ../workspace/output/regressions/results_all_cond_`2'_`1'.csv, replace	
	
	
	
	
	
	
	

		