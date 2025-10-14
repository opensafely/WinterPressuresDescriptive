/*============================================================================
DO FILE NAME:			figures_graphs_out.do
DATE: 					09/09/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Produces histograms of the outcome  
==============================================================================*/	


//===================================================
//OUTCOME - HOSPITAL ATTENDANCES/ADMISSIONS
//===================================================


//Setting directory for user-written commands
	//ssc install xframeappend 
adopath + ../workspace/analysis/ado 


//Creating the file paths for outputs
// mkdir ..workspace/output
 cap mkdir output/figure1
 cap mkdir output/temp_figure1
 
 
// Importing data
import delimited using ../workspace/output/analytic_data_long_`1'.csv, varnames(1) clear

//Exclude practices with fewer than <1000 patients 
	count if exp_denom <1000
	qui levelsof practice_pseudo_id if exp_denom <1000
	di "We will drop `r(r)' unique practices, comprising `r(N)' total observations in this longitudinal data"
	drop if exp_denom <1000

	
//Renaming variables	
	rename *diabetes* *dbts*
	rename *asthma* *ast*
	rename *_angina_* *_ang_* 
	
//Creating the histograms
//Data is organised as one row per practice in long (so no summary measures used here)
	
	//APC - all conditions
		hist out_prop_apc_w, freq ///
			title("") ///
				subtitle("All admissions", size(small)) ///
				xtitle("Weekly admission rate per practice", size(small),) ///
				ytitle("Frequency", size(small)) ///
			name("apc_hg_`1'", replace) ///
			saving("apc_hg_`1'", replace)
			
		graph export ../workspace/output/figure1/apc_hg_`1'.svg, as(svg) name("apc_hg_`1'") replace
	
	//APC - by ACSCs		
		foreach var in copd ast hypt dbts ang {
			hist out_acscs_prop_`var'_apc_w, freq ///
				title("") ///
					subtitle("Admissions for ACSC: `var'", size(small)) ///
					xtitle("Weekly admission rate per practice", size(small),) ///
					ytitle("Frequency", size(small)) ///
				name("apc_acscs_`var'_hg_`1'", replace) ///
				saving("apc_acscs_`var'_hg_`1'", replace)
		}	
		
	//Combining APC graphs into one:
		graph combine apc_hg_`1' apc_acscs_copd_hg_`1' apc_acscs_ast_hg_`1' apc_acscs_hypt_hg_`1' apc_acscs_dbts_hg_`1' apc_acscs_ang_hg_`1', ///
			iscale(0.5) ///
			title("Histogram: weekly rate of APC admissions, cohort `1'", size(medsmall)) ///
			name("apc_hg_acscs_`1'", replace) ///
			saving("apc_hg_acscs_`1'",replace )
			
		graph export ../workspace/output/figure1/apc_hg_acscs_`1'.svg, as(svg) name("apc_hg_acscs_`1'") replace
			
			
			
				
	//EC - all conditions
		hist out_prop_ec_w, freq ///
			title("") ///
				subtitle("All admissions", size(small)) ///
				xtitle("Weekly admission rate per practice", size(small),) ///
				ytitle("Frequency", size(small)) ///
			name("ec_hg_`1'", replace) ///
			saving("ec_hg_`1'", replace)
	 
	 	graph export ../workspace/output/figure1/ec_hg_`1'.svg, as(svg) name("ec_hg_`1'") replace
			
	//EC - by ACSCs		
		foreach var in copd ast hypt dbts ang {
			hist out_acscs_prop_`var'_ec_w, freq ///
				title("") ///
					subtitle("Admissions for ACSC: `var'", size(small)) ///
					xtitle("Weekly admission rate per practice", size(small),) ///
					ytitle("Frequency", size(small)) ///
				name("ec_acscs_`var'_hg_`1'", replace) ///
				saving("ec_acscs_`var'_hg_`1'", replace)	
		}	
		
	//Combining EC graphs into one:
		graph combine ec_hg_`1' ec_acscs_copd_hg_`1' ec_acscs_ast_hg_`1' ec_acscs_hypt_hg_`1' ec_acscs_dbts_hg_`1' ec_acscs_ang_hg_`1', ///
			iscale(0.5) ///
			title("Histogram: weekly rate of EC admissions, cohort `1'", size(medsmall)) ///
			name("ec_hg_acscs_`1'", replace) ///
			saving("ec_hg_acscs_`1'",replace )
		
		graph export ../workspace/output/figure1/ec_hg_acscs_`1'.svg, as(svg) name("ec_hg_acscs_`1'") replace
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		