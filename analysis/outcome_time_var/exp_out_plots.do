/*============================================================================
DO FILE NAME:			exp_out_plots.do
DATE: 					26/11/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Creates plots showing the correlation between the exposures and outcomes
==============================================================================*/	

**# //SETTING DIRECTORIES 
//User-written commands - ssc install: xframeappend; grc1leg; egenmore; spost 
adopath + ../workspace/analysis/ado 

//Creating the file paths for outputs
cap mkdir ..workspace/output
cap mkdir output/regressions


**# // IMPORTING & DATA MANAGEMENT 	
//Importing each dataset into a frame and doing basic data management
clear frames 			 

import delimited using ../workspace/output/analytic_data_long_`1'.csv, varnames(1) clear

//Renaming variables
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
	
//Generating key variables 	
	gen cohort = "`1'"
				
	//Combining the 80_84 and 85p age categories 
		gen exp_num_80p = exp_num_80_84 + exp_num_85p
		gen exp_prop_80p = exp_num_80p/exp_dnm
		
		
		
	
**# // CREATING THE PLOTS 
//The final graph will be a 20 x 2 grid: each exposure has a row, each outcome (APC, EC) has a column
//Each cohort will have it's own graph

local exp_var_list u5y 65_74 75_79 80p white asian black other mixed imd1 imd5 ast dbts hypt obs urb1 urb5 female smoker
	foreach hosp in apc ec {
		local `hosp'_`1'_graphs ""
		
		foreach char in `exp_var_list' {
			local `hosp'_`1'_graphs ``hosp'_`1'_graphs' `hosp'_`char'_`1'
			
			twoway scatter prop_`hosp'_w exp_prop_`char' ||		///
				   lowess prop_`hosp'_w exp_prop_`char', 		///
				   aspect(1) legend(off) name("`hosp'_`char'_`1'", replace)
		}
	}

//Combining each column, then combining alltogether 
	//APC
		grc1leg2 `apc_`1'_graphs',  ///
			row(19) ysize(20) xsize(1) title("APC `1'", size(small)) name("apc_`1'", replace)
		
	//EC 	
		grc1leg2 `ec_`1'_graphs', ///
			row(19) ysize(20) xsize(1) title("EC `1'", size(small)) name("ec_`1'", replace)
		
	//Combining & saving 
		grc1leg2 apc_`1' ec_`1', row(19) col(2) ysize(20) xsize(2.25) name("exp_all_`1'", replace)
		
		graph export output/regressions/exp_all_`1'.svg, as(svg) width(675) height(5000) name("exp_all_`1'", replace) replace
	
	
	

	
	

	
//-----------------------------------------------	
//		grc1leg2 `apc_precovid_graphs' `ec_precovid_graphs', row(20) ysize(20) xsize(1) title("APC precovid", size(small)) name("precovid", replace)

//		graph export output/regressions/exp_apc.svg, as(svg) width(2000) height(10000) name("precovid", replace) replace
	


//local 1 precovid
//local exp_var_list u5y 65_74 75_79 80_84 85p white asian black other mixed imd1 imd5 ast dbts hypt obs urb1 urb5 female smoker
//	foreach hosp in apc ec {
//		local `hosp'_`1'_graphs ""
//		
//		foreach char in `exp_var_list' {
//			local `hosp'_`1'_graphs ``hosp'_`1'_graphs' `hosp'_`char'_`1'
//			
//		
//		}
//	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	








