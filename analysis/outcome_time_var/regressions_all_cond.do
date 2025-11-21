/*============================================================================================
DO FILE NAME:			regressions_all_cond.do
DATE: 					04/11/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	For the APC, EC (all conditions) outcomes, runs a random-intercept 
						Poisson model and a random-intercept negative bonomial model to describe
						the association between each exposure x outcome. 
						Then stores the results in a frame and exports this as a .csv file
===============================================================================================*/	
**# //SETTING DIRECTORIES & IMPORTING DATA
//User-written commands - ssc install: xframeappend; grc1leg; egenmore; spost 
adopath + ../workspace/analysis/ado 

//Creating the file paths for outputs
// mkdir ..workspace/output
cap mkdir output/regressions

//Importing data 
clear frames 
import delimited using ../workspace/output/analytic_data_long_`1'.csv, varnames(1) clear 


**# //DATA MANAGEMENT
//Excluding practices with fewer than <1000 patients 
	count if exp_denom <1000
	qui levelsof practice_pseudo_id if exp_denom <1000
	di "We will drop `r(r)' unique practices, comprising `r(N)' total observations in this longitudinal data"
	drop if exp_denom <1000
	
	
//Renaming & generating variables to make it easier to code
	drop exp_interval_* *_missing exp_*_af *_alcoholproblem *_anxietydepression *_cancer *_chd *_ckd *_constipation *_copd *_ctd *_dementia *_epilepsy *_hearingloss *_hf *_ibs *_osteoarthritis *_psychosis *_stroketia *smoker_ever *_smoker_never
	cap drop *_vax_* *_cons_*
	
	rename *diabetes* *dbts*
	rename *asthma* *ast*
	rename *obesity* *obs*
	rename *_angina_* *_ang_* 
	rename *_to_* *_*
	rename *smoker_current *smoker
	rename *urb_major *urb1
	rename *urb_minor *urb2
	rename *urb_town *urb3
	rename *rural_fringe *urb4
	rename *rural_village *urb5
	rename *_eth_* *_*
	rename *_least *
	rename *_most *
	rename *imd_# *imd#
	rename *under* *u*
	rename *_age_*_plus *_*p
	rename *denom* *dnm*

	drop *_5_11* *_12_17* *_18_29* *_30_44* *_45_54* *_55_64* 
	drop  *_imd3* *_imd3* *_imd4* 
	drop  *_urb3* *_urb4*
	drop  *_male*
	
	rename out_* *
	rename acscs_* * 
	
	gen cohort = "`1'"
	
	gen log_dnm = log(dnm)

//Setting as a panel variable
	xtset practice_pseudo_id week_number
		
	
**# //REGRESSIONS & EXPORTING RESULTS 
//All-condition APC & EC: Random-intercept Poisson; Random-intercept negative binomial 
	
//Frame storing regression results  
frame create results_all_cond_`1' str10 cohort str4 hosp_type str15 acscs ///
								  str40 model_form str10 out_var str7 exp_var obs 	///
								  irr_exp se_exp p_exp lci_exp uci_exp 			///
								  irr_cons se_cons p_cons lci_cons uci_cons  		///
								  variance_ri se_ri lci_ri uci_ri 				///
								  str50 lrtest_comparing chi2_lrtest p_lrtest 	///
								  ll p_ll aic bic  								///
								  error
							
//Loop that runs each regression, and then exports the results to a frame 	
local exp_var_list u5y 65_74 75_79 80_84 85p white asian black other mixed imd1 imd5 ast dbts hypt obs urb1 urb5 female smoker
	foreach regression in mepoisson menbreg {
		if "`regression'" == "mepoisson"{
			local reg_name Poisson
			local lrtest_text fixed-effects only Poisson
		}
		if "`regression'" == "menbreg" {
			local reg_name Negative binomial
			local lrtest_text fixed-effects only negative binomial
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
			
			cap `regression' `var' exp_prop_`char', offset(log_dnm) || practice_pseudo_id:, irr
				if _rc != 0  {
					 
					di _n "`regression' regression error: `var' --> exp_prop_`char'"
					di "STATA error code: " _rc
					
				frame post results_all_cond_`1'		///
					("postcovid3") ("`hosp'") ("All conditions") ("`reg_name' random intercepts") ///
					("`stub'") ("`char'") (.) 		///
					(.) (.) (.) (.) (.)  			///
					(.) (.) (.) (.) (.) 			///
					(.) (.) (.) (.) 				///
					("`lrtest_text'") (.) (.)		///
					(.) (.) (.) (.)					///
					(_rc)	
					
				}
				else {
					matrix b = r(table) 	//Naming regression output matrix
						scalar obs = e(N) 					//Number of observations in model 
						
						scalar irr_exp = b[1,1] 
						scalar se_exp = b[2,1]
						scalar p_exp = b[4,1] 		//p-value, exposure 
						scalar lci_exp = b[5,1] 	//lower 95% CI, exposure
						scalar uci_exp = b[6,1] 	//upper 95% CI, exposure
						
						scalar irr_cons = b[1,2]
						scalar se_cons = b[2,2]
						scalar p_cons = b[4,2] 		//p-value, constant 
						scalar lci_cons = b[5,2] 	//lower 95% CI, constant
						scalar uci_cons = b[6,2]	//upper 95% CI, constant
						
						scalar v_ri = b[1,3] 		//Variance of the random intercepts 
						scalar se_ri = b[2,3]       //SE of the random intercepts 
						scalar lci_ri = b[5,3]
						scalar uci_ri = b[6,3]
						
						scalar ll = e(ll) 							//Model's log-likelihood
						scalar p_ll = e(p)						 	//p-value, model 
						scalar chi2_lrtest = e(chi2_c)				//LR test chi2
						scalar p_lrtest = e(p_c)					//p-value, LR test
						
					qui estat ic 
					matrix ic_b = r(S)
						scalar aic = ic_b[1,5]				//AIC score
						scalar bic = ic_b[1,6]				//BIC score 
				
				frame post results_all_cond_`1' 	///
					("postcovid3") ("`hosp'") ("All conditions") ("`reg_name' random intercepts") ///
					("`stub'") ("`char'") (obs) 	///
					(irr_exp) (se_exp) (p_exp) (lci_exp) (uci_exp)  			///
					(irr_cons) (se_cons) (p_cons) (lci_cons) (uci_cons) 		///
					(v_ri) (se_ri) (lci_ri) (uci_ri) 							///
					("`lrtest_text'") (chi2_lrtest) (p_lrtest)					///
					(ll) (p_ll) (aic) (bic)										///
					(.)	
											
					continue
				}	
		}			
		}	
	}
	
//Checking the regression statistics have expected ranges
frame change results_all_cond_`1' 
		
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
					replace check_irr = check_irr + "`var' < 0; "
					continue 
				}
		}
	gen str check_chi2 = ""
		foreach var of varlist chi2_* {
			cap assert `var' > 0 
				if _rc !=0 {
					replace check_chi2 = check_chi2 + "`var' < 0; "
					continue
				}
		}

//Exporting the frame as a .csv 
export delimited using ../workspace/output/regressions/results_all_cond_`1'.csv, replace	
	

		