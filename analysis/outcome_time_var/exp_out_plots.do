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
foreach cohort in precovid postcovid1 postcovid2 postcovid3{
	import delimited using ../workspace/output/analytic_data_long_`cohort'.csv, varnames(1) clear
		frame copy default analytic_`cohort', replace 
		frame change analytic_`cohort'
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
				
				gen cohort = "`cohort'"
				
		frame change default 			
}	

	frame copy analytic_precovid analytic, replace
	frame analytic: xframeappend analytic_postcovid1 analytic_postcovid2 analytic_postcovid3

	
	
**# // CREATING THE PLOTS 
//The final graph will be a 20 x 4 grid: each exposure has a row, each cohort has a column 

frame change analytic 

local exp_var_list u5y 65_74 75_79 80_84 85p white asian black other mixed imd1 imd5 ast dbts hypt obs urb1 urb5 female smoker

foreach cohort in precovid postcovid1 postcovid2 postcovid3 {
	foreach hosp in apc ec {
		local `hosp'_`cohort'_graphs ""
		
		foreach char in `exp_var_list' {
			local `hosp'_`cohort'_graphs ``hosp'_`cohort'_graphs' `hosp'_`char'_`cohort'
			
			twoway scatter prop_`hosp'_w exp_prop_`char' if cohort == "`cohort'" ||		///
				   lowess prop_`hosp'_w exp_prop_`char' if cohort == "`cohort'", 		///
				   aspect(1) scale(0.8) name("`hosp'_`char'_`cohort'", replace)
					
		}
	}
}


//Combining each column, then combining alltogether 
	//APC 
		grc1leg `apc_precovid_graphs', ///
			row(20) ysize(20) xsize(1) title("APC precovid", size(small)) name("apc_precovid", replace)
		grc1leg `apc_postcovid1_graphs', ///
			row(20) ysize(20) xsize(1) title("APC postcovid1", size(small)) name("apc_postcovid1", replace)
		grc1leg `apc_postcovid2_graphs', ///
			row(20) ysize(20) xsize(1) title("APC postcovid2", size(small)) name("apc_postcovid2", replace)
		grc1leg `apc_postcovid3_graphs', ///
			row(20) ysize(20) xsize(1) title("APC postcovid3", size(small)) name("apc_postcovid3", replace)
		
		grc1leg apc_precovid apc_postcovid1 apc_postcovid2 apc_postcovid3, ///
			row(20) col(4) imargin(zero) ysize(21) xsize(4.5)  ///
			name("exp_apc", replace)
			
	//Saving	
		graph export ../workspace/output/regressions/exp_apc.svg, ///
			as(svg) width(100) height(500) ///
			name("exp_apc", replace)
	
	
	//EC 
		grc1leg `ec_precovid_graphs', ///
			row(20) ysize(20) xsize(1) title("EC precovid", size(small)) name("ec_precovid", replace)
		grc1leg `ec_postcovid1_graphs', ///
			row(20) ysize(20) xsize(1) title("EC postcovid1", size(small)) name("ec_postcovid1", replace)
		grc1leg `ec_postcovid2_graphs', ///
			row(20) ysize(20) xsize(1) title("EC postcovid2", size(small)) name("ec_postcovid2", replace)
		grc1leg `ec_postcovid3_graphs', ///
			row(20) ysize(20) xsize(1) title("EC postcovid3", size(small)) name("ec_postcovid3", replace)
		
		grc1leg ec_precovid ec_postcovid1 ec_postcovid2 ec_postcovid3, ///
			row(20) col(4) imargin(zero) ysize(21) xsize(4.5)  ///
			name("exp_ec", replace)
	
	//Saving
		graph export ../workspace/output/regressions/exp_ec.svg, ///
			as(svg) width(100) height(500) ///
			name("exp_ec", replace)
	













