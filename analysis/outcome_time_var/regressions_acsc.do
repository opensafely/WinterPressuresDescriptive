/*============================================================================
DO FILE NAME:			regressions_acsc.do
DATE: 					04/11/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Creates a summary table of the outcomes, then;
						Runs a random-intercept Poisson model to describe the 
						association between each exposure x outcome, then;
						stores the results in a frame
==============================================================================*/	


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
	

//Setting as a panel variable
	xtset practice_pseudo_id week_number
		
	
**# //REGRESSIONS & EXPORTING RESULTS 
//ACSCs APC & EC: Zero-inflated negative binomial; negative binomial; Poisson
	//Not using random-intercepts in the first instance because of the zero-inflation 
		//If the negative binomial model is the best fit, then can try nbreg with randome intercepts
		//Alternatively, can fit using robust SEs
	
	
//Frame storing regression results  
frame create results_acsc_`1' str10 cohort str4 hosp_type str15 acscs 			///
							  str40 model_form str10 out_var str7 exp_var obs 	///
							irr_exp se_exp p_exp lci_exp uci_exp 				///
							irr_cons se_cons p_cons lci_cons uci_cons  			///
							or_cons_inf se_cons_inf p_cons_inf lci_cons_inf uci_cons_inf /// 
							v_random_int alpha lci_alpha uci_alpha 						///
							str50 lrtest_comparing chi2_lrtest p_lrtest 				///
							ll p_ll aic bic psuedo_r2 									///
							error

//Loop that runs each regression, and then exports the results to a frame 							
local exp_var_list u5y 65_74 75_79 80_84 85p white asian black other mixed imd1 imd5 ast dbts hypt obs urb1 urb5 female smoker
	foreach regression in zinb nbreg poisson{
		if "`regression'" == "zinb"{
			local reg_name Zero-inflated negative binomial 
			local reg_opts inflate(_cons) zip 	//Regression options specific to ZINB
			local p p 							// ZINB stores LRtest chi2 and p as e(chi2_cp)
			local p_model chi2tail(1, e(chi2)) 	//p-value associated with the count model 
												//Calculate by hand for zinb bc not included in stored outputs
			local lrtest_text alpha = 0 (no overdispersion)
			local a_col 5  					//Column that alpha results are stored in 
			local i_col 3					//Column that inflated model values are stored in 
		}
		if "`regression'" == "nbreg" {
			local reg_name Negative binomial
			local reg_opts
			local p 
			local p_model e(p) 	//p-value associated with overall model 
			local lrtest_text alpha = 0 (no overdispersion)
			local a_col 4  		//Column that alpha results are stored in 
			local i_col 99 		//Setting as 99 so nothing will be picked up 	
		}
		if "`regression'" == "poisson" {
			local reg_name Poisson 
			local reg_opts
			local p p 
			local p_model e(p) 	//p-value associated with overall model 
			local lrtest_text 	//Keep blank b/c no lr test is preformed in the Poisson reg
			local a_col 99 		//Setting as 99 so nothing will be picked up 
			local i_col 99 		//Setting as 99 so nothing will be picked up 	
		}
		
		foreach var of varlist num_*_apc_w num_*_ec_w {
			if strpos("`var'", "apc") > 0 {
				local hosp apc
			}
			if strpos("`var'", "ec") > 0 {
				local hosp ec
			}
		foreach char in `exp_var_list'{
			local stub: subinstr local var "num_" "", all 	//Extracting specific strings from the variable names
			local stub: subinstr local stub "_w" "", all	//These will be used to identify which outcome & exposure
			local acsc: subinstr local stub "_`hosp'" "", all 	//each model represents
			
			cap `regression' `var' exp_prop_`char', offset(dnm) irr `reg_opts'
				if _rc != 0  {
					di _n "`regression' regression error: `var' --> exp_prop_`char'"
					di "STATA error code: " _rc

					frame post results_acsc_`1' 	///
						("postcovid3") ("`hosp'") ("`acsc'") ("`reg_name'") ("`stub'") ("`char'") (.) 	///
						(.) (.) (.) (.) (.)  			///
						(.) (.) (.) (.) (.) 			///
						(.) (.) (.) (.) (.)				///
						(.) (.) (.) (.) 				///
						("") (.) (.)					///
						(.) (.) (.) (.) (.)				///
						(_rc)	
						
					continue
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
						
						scalar or_cons_inf = exp(b[1,`i_col']) 			//OR of _cons (inflation eq)
						scalar se_cons_inf = or_cons_inf* b[2,`i_col'] 	//SE of OR cons_inf
						scalar p_cons_inf = b[4,`i_col'] 				//p-value, constant (inflation eq)
						scalar lci_cons_inf = exp(b[5,`i_col']) 		//95% CI, constant (inflation eq)
						scalar uci_cons_inf = exp(b[6,`i_col']) 		//95% CI, constant (inflation eq)
						
						//scalar v_ri = b[3,1] 		//Variance of the random intercepts 
						scalar alpha = b[1,`a_col'] 				//Alpha: overdispersion measure
						scalar lci_alpha = b[5,`a_col']			//95% CI, alpha
						scalar uci_alpha = b[6,`a_col']			//95% CI, alpha
						
						scalar chi2_lrtest = e(chi2_c`p')						//Chi2 stat, LR test
						scalar p_lrtest = chi2tail(e(df_m), e(chi2_c`p'))/2		//p-value, LR test
						
						scalar ll = e(ll) 						//log-likelihood, model overall (count if zinb)
						scalar p_ll = `p_model'					//p-value, model overall
						scalar psuedo_r2 = e(r2_p)				//psuedo r2
						
					qui estat ic 
						matrix ic_b = r(S)
							scalar aic = ic_b[1,5]				//AIC score
							scalar bic = ic_b[1,6]				//BIC score 
				
				frame post results_acsc_`1' 		///
					("postcovid3") ("`hosp'") ("`acsc'") ("`reg_name'") ("`stub'") ("`char'") (obs) 	///
					(irr_exp) (se_exp) (p_exp) (lci_exp) (uci_exp)  			///
					(irr_cons) (se_cons) (p_cons) (lci_cons) (uci_cons) 		///
					(or_cons_inf) (se_cons_inf) (p_cons_inf) (lci_cons_inf) (uci_cons_inf)	///
					(.) (alpha) (lci_alpha) (uci_alpha) 									///
					("`lrtest_text'") (chi2_lrtest) (p_lrtest)							///
					(ll) (p_ll) (aic) (bic) (psuedo_r2)										///
					(.)
				}	
		}			
	}	
	}
		
//Checking the regression statistics have expected ranges
frame change results_acsc_`1'
		
	gen check_p = ""
		foreach var of varlist p_* {
			cap assert `var' <1 if `var' !=.
				if _rc != 0 {
					replace check_p = check_p + "`var' > 1; "
					continue 
				}
		}
	gen check_irr = ""
		foreach var of varlist irr_* or_*{
			cap assert `var' > 0
				if _rc != 0 {
					replace check_irr = check_irr + "`var' < 0; "
					continue 
				}
		}
	gen check_chi2 = ""
		foreach var of varlist chi2_* {
			cap assert `var' >= 0 
				if _rc !=0 {
					replace check_chi2 = check_chi2 + "`var' < 0; "
					continue
				}
		}		
		

//Exporting the frame as a .csv 
export delimited using ../workspace/output/regressions/results_acsc_`1'.csv, replace	

}
