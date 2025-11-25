/*============================================================================
DO FILE NAME:			exp_out_plots.do
DATE: 					23/11/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Creates plots showing the correlation between the exposures and outcomes
==============================================================================*/	

//Creating the file paths for outputs
cap mkdir ..workspace/output
cap mkdir output/regressions


//Importing the data 
import delimited using ../workspace/output/regressions/results_all_cond_`cohort'.csv, varnames(1) 

import delimited "C:\Users\ShrinkhalaDawadi\Documents\GitHub\WinterPressuresDescriptive\output\analytic_data_long_postcovid1.csv"

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
	

//Creating the plots 
twoway scatter prop_apc_w 