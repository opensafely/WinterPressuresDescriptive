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
//import delimited "C:\Users\ShrinkhalaDawadi\Documents\GitHub\WinterPressuresDescriptive\output\analytic_data_long_postcovid1.csv", clear


//Exclude practices with fewer than <1000 patients 
	count if exp_denom <1000
	qui levelsof practice_pseudo_id if exp_denom <1000
	di "We will drop `r(r)' unique practices, comprising `r(N)' total observations in this longitudinal data"
	drop if exp_denom <1000

//Creating the histograms
//Data is organised as one row per practice in long (so no summary measures used here)
	
	//APC - all conditions
		hist out_prop_apc_w, freq ///
			title("") ///
				subtitle("All admissions", size(small)) ///
				xtitle("Weekly admission rate per practice", size(small),) ///
				ytitle("Frequency", size(small)) ///
			name("apc_hist_`1'", replace) ///
			saving("apc_hist_`1'", replace)
			
		graph export ../workspace/output/figure1/apc_hist_`1'.svg, as(svg) name("apc_hist_`1'") replace
	
	//APC - by ACSCs		
		foreach var in copd asthma hypt diabetes angina {
			hist out_acscs_prop_`var'_apc_w, freq ///
				title("") ///
					subtitle("Admissions for ACSC: `var'", size(small)) ///
					xtitle("Weekly admission rate per practice", size(small),) ///
					ytitle("Frequency", size(small)) ///
				name("apc_acscs_`var'_hist_`1'", replace) ///
				saving("apc_acscs_`var'_hist_`1'", replace)
		}	
		
	//Combining APC graphs into one:
		graph combine apc_hist_`1' apc_acscs_copd_hist_`1' apc_acscs_asthma_hist_`1' apc_acscs_hypt_hist_`1' apc_acscs_diabetes_hist_`1' apc_acscs_angina_hist_`1', ///
			iscale(0.5) ///
			title("Histogram: weekly rate of APC admissions, cohort `1'", size(medsmall)) ///
			name("apc_hist_acscs_`1'", replace) ///
			saving("apc_hist_acscs_`1'",replace )
			
		graph export ../workspace/output/figure1/apc_hist_acscs_`1'.svg, as(svg) name("apc_hist_acscs_`1'") replace
			
			
			
				
	//EC - all conditions
		hist out_prop_ec_w, freq ///
			title("") ///
				subtitle("All admissions", size(small)) ///
				xtitle("Weekly admission rate per practice", size(small),) ///
				ytitle("Frequency", size(small)) ///
			name("ec_hist_`1'", replace) ///
			saving("ec_hist_`1'", replace)
	 
	 	graph export ../workspace/output/figure1/ec_hist_`1'.svg, as(svg) name("ec_hist_`1'") replace
			
	//EC - by ACSCs		
		foreach var in copd asthma hypt diabetes angina {
			hist out_acscs_prop_`var'_ec_w, freq ///
				title("") ///
					subtitle("Admissions for ACSC: `var'", size(small)) ///
					xtitle("Weekly admission rate per practice", size(small),) ///
					ytitle("Frequency", size(small)) ///
				name("ec_acscs_`var'_hist_`1'", replace) ///
				saving("ec_acscs_`var'_hist_`1'", replace)	
		}	
		
	//Combining EC graphs into one:
		graph combine ec_hist_`1' ec_acscs_copd_hist_`1' ec_acscs_asthma_hist_`1' ec_acscs_hypt_hist_`1' ec_acscs_diabetes_hist_`1' ec_acscs_angina_hist_`1', ///
			iscale(0.5) ///
			title("Histogram: weekly rate of EC admissions, cohort `1'", size(medsmall)) ///
			name("ec_hist_acscs_`1'", replace) ///
			saving("ec_hist_acscs_`1'",replace )
		
		graph export ../workspace/output/figure1/ec_hist_acscs_`1'.svg, as(svg) name("ec_hist_acscs_`1'") replace
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		