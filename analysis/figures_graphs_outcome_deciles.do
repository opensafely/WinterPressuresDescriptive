/*============================================================================
DO FILE NAME:			figures_graphs_outcome_deciles.do
DATE: 					10/10/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Produces a table used to generate descriptive graphs of the outcome by practice deciles
==============================================================================*/	

//=============================================================
//OUTCOME - HOSPITAL ATTENDANCES/ EMERGENCY ADMISSIONS
//=============================================================

//Setting directory for user-written commands
	//ssc install xframeappend 
adopath + ../workspace/analysis/ado 

//Creating the file paths for outputs
// mkdir ..workspace/output
 cap mkdir output/figure1
 cap mkdir output/temp_figure1
 
//Defining program to mid-point round the raw numerator/denominator variables
capture program drop round_mp6 
program round_mp6
	gen `2' = ceil(`1'/6)*6 - (floor(6/2)*(`1'!=0))
end


// Importing data
import delimited using ../workspace/output/analytic_data_long_`1'.csv, varnames(1) clear
//import delimited "C:\Users\ShrinkhalaDawadi\Documents\GitHub\WinterPressuresDescriptive\output\analytic_data_long_postcovid1.csv", clear


**#//DATA MANAGEMENT
//Exclude practices with fewer than <1000 patients 
	count if exp_denom <1000
	qui levelsof practice_pseudo_id if exp_denom <1000
	di "We will drop `r(r)' unique practices, comprising `r(N)' total observations in this longitudinal data"
	drop if exp_denom <1000
	
	
//Dropping vars: all exposure vars, all GP consultation vars 
	drop exp_*
	
//Renaming variables to make it easier to code
	rename out_acscs_* out_* 
	rename out_denom out_dnm
	
	rename *diabetes* *dbts*
	rename *asthma* *ast*
	rename *_angina_* *_ang_* 
	
//Group variables: deciles of practices, by their APC or EC rate
	foreach var of varlist out_prop* {
		xtile dec_`var' = `var', nq(10)
	}

	sort practice_pseudo_id out_interval_start

	
preserve
//Outcome numerators - rounded to midpoint 6
	foreach var of varlist out_num_* {
		//Displays which numerators are <=7, and when 
		qui levelsof out_interval_start if `var' <= 7, local(date_list) clean  
			if `r(N)' !=0 {
				di _n "Variable `var' contains counts <=7 for dates: `r(levels)'"
			}
			
		//Creating local strings to make variable naming easier 
		local out_var: subinstr local var "num_" "", all 	
		local group_var: subinstr local var "num_" "prop_", all
		
		//Now actually generating the rounded numerators
		round_mp6 `var' mp6_`out_var' ///Rounded numerator per practice, per date
			
		egen t_`out_var'= 	///Rounded total numerator, PER DATE
			total(`var'), by(out_interval_start)	
			round_mp6 t_`out_var' mp6_t_`out_var'
			
		egen t_dec_`out_var' = ///Rounded total num, PER DATE & DECILE VAR
			total(`var'), by(out_interval_start dec_`group_var')
			round_mp6 t_dec_`out_var' mp6_t_dec_`out_var'
	}	
//Outcome denominators - rounded to midpoint 6 
	round_mp6 out_dnm mp6_out_dnm  ///Rounded denominator per practice, per date
	
	egen t_out_dnm = ///Rounded total denominator, PER DATE
		total(out_dnm), by(out_interval_start) 
		round_mp6 t_out_dnm mp6_t_out_dnm 
		
	foreach condition in 
	
	
	egen t_dec_out_dnm = total(out_dnm), by(out_interval_start dec_out_prop_apc_w)
	foreach group_var of varlist dec_out_prop*{
		egen t_dec_out_dnm = ///Rounded total denominator, PER DATE & DECILE VAR
			total(out_dnm), by(out_interval_start `group_var')
			round_mp6 t_dec_out_dnm mp6_t_dec_out_dnm
	}
	
	
	
		
		foreach var of varlist out_num* {
			local out_var: subinstr local var "num_" "", all 	
			di _n "`var' --> `out_var'"
			
			local group_var: subinstr local var "num_" "prop_", all
			di "`var' --> dec_`group_var'"
		}	
			
		foreach var of varlist dec_out_prop* {
			local new_var: subinstr local var "num_" "prop_", all
			di "`var' --> `new_var'"
		}	
			
	
	
	
	
//Outcome medians - generated from rounded numerators & denominators
//	We have the proportions of hospitalised patients per practice per week
//		mp6_md_`out_var': is the median of this distribution per week
//		mp6_md`group_var'_`out_var': is the median of this distribution per week and by the tertiles of the grouping variable 
local group_var_list _u5y _65_74 _75_79 _80_84 _85p _white _asian _black _imd1 _imd2 _ast _dbts _hypt _obs _urb1 _urb2 _female _smoker

	foreach var of varlist mp6_out*{
	local out_var: subinstr local var "mp6_" "", all
		egen mp6_md_`out_var' = /// Median of the proportion distribution, AT EACH DATE
			median(((mp6_`out_var'/mp6_out_dnm)*1000)), by(out_interval_start)
		
	if strlen("`group_var_list'") != 0{
	foreach group_var in `group_var_list'{
		egen mp6_md`group_var'_`out_var' = ///Median of the prop. dist, BY GROUP & DATE
				median(((mp6_`out_var'/mp6_out_dnm)*1000)), by(out_interval_start tert_exp_prop`group_var')
	}		
	}
	}
	
	//Removing the uneccessary median of the denominator var that was created in this loop
	drop mp6_md*out_dnm
		