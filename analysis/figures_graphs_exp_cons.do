/*============================================================================
DO FILE NAME:			figures_graphs_exp_cons.do
DATE: 					12/09/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Produces a table used to generate descriptive graphs 
==============================================================================*/	

//===================================================
//OUTCOME - GP CONSULTATIONS
//===================================================

//Setting directory for user-written commands
	//ssc install xframeappend 
adopath + ./analysis/ado 


//Defining program to mid-point round the raw numerator/denominator variables
capture program drop round_mp6 
program round_mp6
	gen `2' = ceil(`1'/6)*6 - (floor(6/2)*(`1'!=0))
end


//Importing the data & clearing frames
clear frames 
import delimited using ../workspace/output/analytic_data_wide_`1'.csv, varnames(1) clear

**#//DATA MANAGEMENT
//Dropping cars
	//Date variables, vars related to missingness in exposures, vaccination variables
	//CMS: Drop all conditions except hypertension, asthma, diabetes.
		//^^The 3 most prevalent conditions in Payne et al.2018
		//(excluding hearing loss & those requiring prescription codelists)
	//Proportion of consulations (we'll need to re-calculate this with rounding)
	//Exposure vars we're not currently stratifying by 
	
	keep practice_pseudo_id exp_* 	//Dropping the hospitalisation outcome vars		
	
	drop *_interval_* *_missing 
	drop exp_*_af *_alcoholproblem *_anxietydepression *_cancer *_chd *_ckd *_constipation *_copd *_ctd *_dementia *_epilepsy *_hearingloss *_hf *_ibs *_osteoarthritis *_psychosis *_stroketia *smoker_ever *_smoker_never
	drop exp_prop_cons*
	cap drop *_vax_* 
	
	rename *diabetes* *dbts*
	rename *asthma* *ast*
	rename *obesity* *obs*
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
	rename *cons_20* *cons_*

	drop *_5_11* *_12_17* *_18_29* *_30_44* *_45_54* *_55_64* *_65_74* *_75_79* *_80_84* *_85p*
	drop *_mixed* *_asian* *_black* *_other*
	drop  *_imd2* *_imd3* *_imd3* *_imd4* *_imd5*
	drop  *_urb2*  *_urb3* *_urb4* *_urb5*
	drop  *_male*
	
	
//Group variables: tertiles of select exposures
//I.e. grouping practices into tertiles based on their registered patient case-mix
	foreach var of varlist exp_prop_*{
		di "`var'"
		xtile tert_`var' = `var', nq(3)
	}
		

//Summary numerator and denominator for GP consultations - rounded to midpoint 6
local group_var_list _u5y _white _imd1 _ast _dbts _hypt _obs _urb1 _female _smoker
	
	foreach var of varlist *cons_* {
		round_mp6 `var' mp6_`var' 
		
		egen t_`var' = total(`var') //Num and denom totals by month
		round_mp6 t_`var' mp6_t_`var' 
		
	if strlen("`group_var_list'") != 0{
	foreach group_var in `group_var_list'{  
		egen t`group_var'_`var' = /// Num and denom totals by month & grouping var
			total(`var'), by(tert_exp_prop`group_var') 
			round_mp6 t`group_var'_`var' mp6_t`group_var'_`var' 
	}
	}
	}
	
	
//Summary medians of the distribution of GP consultation rate, generated from rounded vars
//	We have the consultation rate per practice per month 
//		mp6_md_`cons': is the median of this distribution, stratified by month
//		mp6_md`group_var'_`cons': is the median of this distribution, stratified by month and the grouping variable tertiles 
	
	foreach var of varlist exp_num_cons_* {
	local cons: subinstr local var "exp_num_" "", all 
	
		egen mp6_md_`cons' = ///Median of GP cons rate, stratified by month
			median(mp6_exp_num_`cons'/mp6_exp_dnm_`cons')
			
	if strlen("`group_var_list'") != 0{
	foreach group_var in `group_var_list' {
		egen mp6_md`group_var'_`cons' =  ///median of GP cons rate, stratified by month & group
			median(mp6_exp_num_`cons'/mp6_exp_dnm_`cons'), by(tert_exp_prop`group_var')
	}	
	}
	}


//Summary total proportions of the GP consulation rate, generated from rounded vars			
//	We have the COUNTS of consulations across all practices, per month
//		mp6_prop_`cons: total count cons/total count pts PER MONTH
//		mp6_prop`group_var'_`cons': total count cons/ total count pts PER MONTH & GROUP
// 		NOTE: These are not proportions per practice. These are total proportions, stratified by month, and by month & grouping variable 
	foreach var of varlist exp_num_cons_*{
	local cons: subinstr local var "exp_num_" "", all
		gen mp6_prop_`cons' = /// Proportion per month
			(mp6_t_exp_num_`cons'/mp6_t_exp_dnm_`cons') 	
		
	if strlen("`group_var_list'") != 0{
	foreach group_var in `group_var_list' {
		gen mp6_prop`group_var'_`cons' = /// Proportion per month per group var
			(mp6_t`group_var'_exp_num_`cons'/mp6_t`group_var'_exp_dnm_`cons') 
	}
	}
	}
			
//Second drop of variables we no longer need
	drop exp_num* exp_prop* exp_dnm*
	drop mp6_exp* mp6_t_exp* t_exp* 
	
	
	
	
**#//GENERATING THE TABLES		
//Collapsing across all practices
	foreach stat in prop md {
		local first_frame `stat'_cons_all
		local cons_vars mp6_`stat'_cons*

		preserve
			keep practice_pseudo_id `cons_vars'
		
			foreach var of varlist `cons_vars'* {
				qui levelsof `var' if `var' !=. 
				assert `r(r)' == 1
			}
			collapse (first) mp6_`stat'_cons*
			gen grouped_by = "All practices"
			frame copy default `first_frame', replace 
			
		restore
	}
	

//Collapsing by the tertiles 
	local exp_var_list u5y white imd1 ast dbts hypt obs urb1 female smoker
	foreach stat in prop md {
		local first_frame `stat'_cons_exp
		local cons_vars mp6_`stat'_*_cons*
		local out_file `stat'_cons_exp.txt
		local dta_file `stat'_cons_exp.dta
		local j = 1
		
		foreach exp_var in `exp_var_list' {
		preserve
			keep practice_pseudo_id tert_exp_prop_`exp_var' mp6_`stat'_`exp_var'_cons_*
		
			foreach var of varlist *_cons_* {
				qui levelsof tert_exp_prop_`exp_var', local(exp_var_levels)
				foreach level in `exp_var_levels'{
					di "Consultation variable: `var', grouping variable: `exp_var' with levels: `exp_var_levels'"
					levelsof `var' if tert_exp_prop_`exp_var' == `level'
					assert `r(r)' == 1
				}
			}
			collapse (first) *_cons_*, by(tert_exp_prop_`exp_var')
			
			gen grouped_by = "", after (tert_exp_prop_`exp_var')
				qui levelsof tert_exp_prop_`exp_var'
				forvalues i = 1/`r(r)'{
					replace grouped_by = "Proportion `exp_var', tertile `i'" in `i'
				}
			drop tert_exp_prop_`exp_var'
			rename mp6_`stat'_`exp_var'_cons* mp6_`stat'_cons*
		
			frame copy default `exp_var', replace 
				if `j' == 1 {
					frame copy `exp_var' `first_frame', replace
				} 
				if `j' != 1{
					frame `first_frame': xframeappend `exp_var' 
				}
		local ++ j
		restore
	}
	}	
	

//Merging the all-practice and tertile datasets together	
	frame prop_cons_all: xframeappend prop_cons_exp	
	frame md_cons_all: xframeappend md_cons_exp	
	
//Saving as a .dta file, and exporting as a tab-delimited file 	
	frame prop_cons_all: save ../workspace/output/prop_cons_all_`1'.dta, replace
	frame md_cons_all: save ../workspace/output/md_cons_all.dta_`1', replace
	
	frame prop_cons_all: export delimited using ../workspace/output/prop_cons_all_`1'.csv, replace
	frame md_cons_all: export delimited using ../workspace/output/md_cons_all_`1'.csv, replace

	
	
	
**# //GENERATING GRAPHS
