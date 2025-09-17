/*============================================================================
DO FILE NAME:			figures_graphs_out.do
DATE: 					12/09/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Produces a table used to generate descriptive graphs 
==============================================================================*/	


//===================================================
//OUTCOME - HOSPITAL ATTENDANCES/ADMISSIONS
//===================================================


//Setting directory for user-written commands
	//ssc install xframeappend 
adopath + ../workspace/analysis/ado 


//Defining program to mid-point round the raw numerator/denominator variables
capture program drop round_mp6 
program round_mp6
	gen `2' = ceil(`1'/6)*6 - (floor(6/2)*(`1'!=0))
end


//Importing the data & clearing frames
clear frames 
import delimited using ../workspace/output/analytic_data_long_`1'.csv, varnames(1) clear


**#//DATA MANAGEMENT
//Dropping vars
	//Date variables, vars related to missingness in exposures, vaccination variables
	//CMS: Drop all conditions except hypertension, asthma, diabetes.
		//^^The 3 most prevalent conditions in Payne et al.2018
		//(excluding hearing loss & those requiring prescription codelists)
	//Exposure vars we're not currently stratifying by 
	//All GP consultation vars
	
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

	drop *_5_11* *_12_17* *_18_29* *_30_44* *_45_54* *_55_64* *_65_74* *_75_79* *_80_84* *_85p*
	drop *_mixed* *_asian* *_black* *_other*
	drop  *_imd2* *_imd3* *_imd3* *_imd4* *_imd5*
	drop  *_urb2*  *_urb3* *_urb4* *_urb5*
	drop  *_male*
	
	
//Group variables: tertiles of select exposures
//I.e. grouping practices into tertiles based on their registered patient case-mix
	foreach var of varlist exp_prop*{
		di "`var'"
		xtile tert_`var' = `var', nq(3)
	}
		
	sort practice_pseudo_id out_interval_start
	

//Outcome numerators - rounded to midpoint 6
local group_var_list _u5y _white _imd1 _ast _dbts _hypt _obs _urb1 _female _smoker

	foreach var of varlist out_num_* out_acscs_num_*{
		local out_var: subinstr local var "num_" "", all 	
		local out_var: subinstr local out_var "acscs_" "", all
		
		round_mp6 `var' mp6_`out_var' ///Rounded numerator per practice, per date
			
		egen t_`out_var'= 	///Rounded total numerator, PER DATE
			total(`var'), by(out_interval_start)	
			round_mp6 t_`out_var' mp6_t_`out_var'
			
		if strlen("`group_var_list'") != 0{
		foreach group_var in `group_var_list'{  
			egen t`group_var'_`out_var' = ///Rounded total num, PER DATE & GROUP VAR
				total(`var'), by(out_interval_start tert_exp_prop`group_var')
				round_mp6 t`group_var'_`out_var' mp6_t`group_var'_`out_var'
		}
		}
		}
		
//Outcome denominators - rounded to midpoint 6 
	round_mp6 out_dnm mp6_out_dnm  ///Rounded denominator per practice, per date
	
	egen t_out_dnm = ///Rounded total denom, PER DATE
		total(out_dnm), by(out_interval_start) 
		round_mp6 t_out_dnm mp6_t_out_dnm 
		
	if strlen("`group_var_list'") != 0{		
	foreach group_var in `group_var_list'{
		egen t`group_var'_out_dnm = ///Rounded total num, PER DATE & GROUP VAR
			total(out_dnm), by(out_interval_start tert_exp_prop`group_var')
			round_mp6 t`group_var'_out_dnm mp6_t`group_var'_out_dnm
	}
	}
	
//Outcome medians - generated from rounded numerators & denominators
//	We have the proportions of hospitalised patients per practice per week
//		mp6_md_`out_var': is the median of this distribution per week
//		mp6_md`group_var'_`out_var': is the median of this distribution per week and by the tertiles of the grouping variable 

	foreach var of varlist mp6_out*{
	local out_var: subinstr local var "mp6_" "", all
		egen mp6_md_`out_var' = /// Median of the proportion distribution, AT EACH DATE
			median(mp6_`out_var'/mp6_out_dnm), by(out_interval_start)
		
	if strlen("`group_var_list'") != 0{
	foreach group_var in `group_var_list'{
		egen mp6_md`group_var'_`out_var' = ///Median of the prop. dist, BY GROUP & DATE
				median(mp6_`out_var'/mp6_out_dnm), by(out_interval_start tert_exp_prop`group_var')
	}		
	}
	}
	

//Outcome total proportions - generated from rounded total numerators & denominators
//	We have the COUNT of hospitalised patients across all practices per week
//		mp6_prop_`out_var': total count hospitalised/all registered patients PER WEEK
//		mp6_prop`group_var'_`out_var': total count hospitalised/all registered patients PER WEEK AND GROUP var
//		NOTE: These are not proportions per practice. They are proportions per week, and per grouping variable & week 

	foreach var of varlist mp6_out*{
	local out_var: subinstr local var "mp6_" "", all
		gen mp6_prop_`out_var' = /// count outcome/count pts, PER DATE
			mp6_t_`out_var'/ mp6_t_out_dnm 	
		
	if strlen("`group_var_list'") != 0{
		foreach group_var in `group_var_list' {	
		gen mp6_prop`group_var'_`out_var' = /// count outcome/count pts, BY GROUP & DATE
			mp6_t`group_var'_`out_var'/ mp6_t`group_var'_out_dnm 	
	}
	}		
	}
			

//Second drop of variables we no longer need
	drop exp_num* exp_prop* exp_dnm*
	drop out_num* out_acscs_num* out_dnm out_prop* out_acscs_prop*
	drop t_out* t_*_out* 
	drop mp6_out* mp6_t_*_out*
		
<<<<<<< Updated upstream
		
		
=======

		
>>>>>>> Stashed changes
		
**#//GENERATING THE TABLES	
//Collapsing across all practices
	foreach stat in prop md {
	foreach hosp in apc ec {
		local first_frame  `stat'_out_`hosp'
		local out_vars mp6_`stat'_out_`hosp'
		
	preserve
		keep practice_pseudo_id week_number `out_vars'*
		
		reshape wide `out_vars'_w, i(practice_pseudo_id) j(week_number)
			
		foreach var of varlist `out_vars'* {
			qui  levelsof `var' if `var'!=.
			assert `r(r)' == 1
			}	
			
		collapse (first) `out_vars'* 
		
		rename mp6_`stat'_out_`hosp'_w* mp6_`stat'_out_w*
		
		gen grouped_by = "All practices"
		gen acscs = "No - all conditions"
		
		if "`hosp'" == "apc"{
			gen outcome_type = "Admitted patient care" 
		}
		else if "`hosp'" == "ec" {
			gen outcome_type = "Emergency attendance"
		}
		
		frame copy default `first_frame', replace 	
		
	restore
	}
	}	
	
	
//Collapsing by tertiles
	local exp_var_list u5y white imd1 ast dbts hypt obs urb1 female smoker
	local acscs_list copd ast hypt dbts ang
	foreach stat in prop md {
	foreach hosp in apc ec {
		local first_frame `stat'_out_`hosp'_all
		local j = 1
		
		foreach exp_var in `exp_var_list'  {
		foreach acscs in `acscs_list' {
		local out_vars mp6_`stat'_`exp_var'_out_`acscs'_`hosp' 
		preserve
			keep practice_pseudo_id week_number tert_exp_prop_`exp_var' `out_vars'_w
		
			reshape wide `out_vars'_w, i(practice_pseudo_id tert_exp_prop_`exp_var') j(week_number)
			
			foreach var of varlist `out_vars'_w* {
				qui levelsof tert_exp_prop_`exp_var', local(exp_var_levels)
				foreach level in `exp_var_levels'{
					di "Outcome: `var', grouping variable: `exp_var' with levels: `exp_var_levels'"
					levelsof `var' if tert_exp_prop_`exp_var' == `level'
					
					cap assert `r(r)' == 1 	
						if _rc != 0 {
							di _n "Collapse was not completed"
							di "Variable:`var', does not have the same values within each level of exp_var: `exp_var'" 
							continue
						}
				}	
			}
	
			collapse (first) `out_vars'_w*, by(tert_exp_prop_`exp_var')
				
			rename mp6_`stat'_`exp_var'_out_`acscs'_`hosp'_w* mp6_`stat'_out_w*	
	
			gen grouped_by = "", after (tert_exp_prop_`exp_var')
				qui levelsof tert_exp_prop_`exp_var'
				forvalues i = 1/`r(r)'{
					replace grouped_by = "Proportion `exp_var', tertile `i'" in `i'
				}
			gen outcome_type = ""
				replace outcome_type = "Admitted patient care"  if "`hosp'"=="apc"
				replace outcome_type = "Emergency attendance"  if "`hosp'"=="ec"
			gen acscs = ""
				replace acscs = "COPD" if "`acscs'" == "copd"
				replace acscs = "Asthma" if "`acscs'" == "ast"
				replace acscs = "Hypertension" if "`acscs'" == "hypt"
				replace acscs = "Diabetes" if "`acscs'" == "dbts"
				replace acscs = "Angina" if "`acscs'" == "ang"
		
			drop tert_exp_prop_`exp_var'
				
			frame copy default out_`exp_var'_`acscs'_`hosp', replace 
				if `j' == 1 {
					frame copy out_`exp_var'_`acscs'_`hosp' `first_frame', replace
				} 
				if `j' != 1{
					frame `first_frame': xframeappend out_`exp_var'_`acscs'_`hosp', drop
				}
				
		local ++ j
		restore
	}
	}	
	}
	}	
	
	
//Appending the frames together 
	frame md_out_apc_all: xframeappend md_out_apc 
	frame md_out_ec_all: xframeappend md_out_ec 
	frame prop_out_apc_all: xframeappend prop_out_apc 
	frame prop_out_ec_all: xframeappend prop_out_ec 

//Dropping remaining frames	
	cap frame drop out_u5y_copd_apc out_u5y_copd_ec prop_out_apc prop_out_ec md_out_apc md_out_ec

//Saving as a .dta file, and exporting as a tab-delimited file 	
//"/output/`frame'.dta"
//"/output/`frame'.csv"
	foreach frame in md_out_apc_all md_out_ec_all prop_out_apc_all prop_out_ec_all {
		frame `frame': save ../workspace/output/`frame'_`1'.dta, replace
		frame `frame': export delimited using ../workspace/output/`frame'_`1'.csv, replace	
	}
	
 
	
**# //GRAPHS





	
	