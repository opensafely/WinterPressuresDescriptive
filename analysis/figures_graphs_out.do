//STATA coding in OS
		//Have to install ssc commands specially
		//Have to round numbers + apply disclosure control?
		//Have to generate a table
	
	
//GP CONSULTATIONS

//Importing the data & clearing frames
clear frames 
import delimited "C:\Users\ShrinkhalaDawadi\Documents\GitHub\WinterPressuresDescriptive\output\analytic_data_long_postcovid1.csv", varnames(1) clear


//DATA MANAGEMENT

//Dropping
	//Exposure - date vars
	//Exposure - GP consultation vars
	//Variables showing the missing proportions
	//Vaccination variables
	//CMS: Drop all conditions except hypertension, asthma, diabetes.
		//^^The 3 most prevalent conditions in Payne et al.2018
		//(excluding hearing loss & those requiring prescription codelists)

	drop exp_interval_* *_cons_* *_missing *_vax_* exp_*_af *_alcoholproblem *_anxietydepression *_cancer *_chd *_ckd *_constipation *_copd *_ctd *_dementia *_epilepsy *_hearingloss *_hf *_ibs *_osteoarthritis *_psychosis *_stroketia *smoker_ever *_smoker_never
	
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
	rename *under* *u*
	rename *_age_*_plus *_*p
	rename *denom* *dnm*

	
//Group variables: tertiles of select exposures
//I.e. grouping practices into tertiles based on their registered patient case-mix
	foreach var of varlist exp_prop*{
		di "`var'"
		xtile tert_`var' = `var', nq(3)
	}
		
	sort practice_pseudo_id out_interval_start
	
//Disclosure control & generating summary variables
//Midpoint rounding the numerator and denominator variables we'll use to generate the mean and medians

local group_var_list _u5y _5_11 _12_17 _18_29 _30_44 _45_54 _55_64 _65_74 _80_84 _85p _white _mixed _asian _black _other _imd_1 _imd_2 _imd_3 _imd_4 _imd_5 _ast _dbts _hypt _obs _urb1 _urb2 _urb3 _urb4 _urb5 _male _female _smoker
		
//Outcome denominators - rounded to midpoint 6 
	//Per practice
		gen mp6_out_dnm = ceil(out_dnm/6)*6 - (floor(6/2)*(out_dnm!=0))
		
	//Overall
		egen t_out_dnm = total(out_dnm), by(out_interval_start)
		gen mp6_t_out_dnm = ceil(t_out_dnm/6)*6 - (floor(6/2)*(t_out_dnm!=0))
	
	//Overall by tertile
		foreach group_var in `group_var_list'{
			egen t`group_var'_out_dnm = total(out_dnm), by(out_interval_start tert_exp_prop`group_var')
		
	}
		
//Outcome numerators, totals, means, medians - rounded to midpoint 6
	foreach var of varlist out_num_* out_acscs_num_*{
		local new_var_name: subinstr local var "num_" "", all
		local new_var_name: subinstr local new_var_name "acscs_" "", all
		
		gen mp6_`new_var_name' = ceil(`var'/6)*6 - (floor(6/2)*(`var'!=0))
			
		egen t_`new_var_name' = total(`var'), by(out_interval_start)
		gen mp6_t_`new_var_name' = ceil(t_`new_var_name'/6)*6 - (floor(6/2)*(t_`new_var_name'!=0))
		
		gen mp6_mean_`new_var_name' = mp6_t_`new_var_name'/ mp6_t_out_dnm
			
		egen mp6_md_`new_var_name' = median((mp6_`new_var_name'/mp6_out_dnm))
		
		if strlen("`group_var_list'") != 0{
			foreach group_var in `group_var_list'{  
				egen t`group_var'_`new_var_name' = total(`var'), by(out_interval_start tert_exp_prop`group_var')
				gen mp6_t`group_var'_`new_var_name' = ceil(t`group_var'_`new_var_name'/6)*6 - (floor(6/2)*(t`group_var'_`new_var_name'!=0))
				
				gen mp6_mean`group_var'_`new_var_name' =  mp6_t`group_var'_`new_var_name'/ t`group_var'_out_dnm 
				
				egen mp6_md`group_var'_`new_var_name' = median(mp6_t`group_var'_`new_var_name'/t`group_var'_out_dnm )
		}
		}
		}


//Second drop of variables we no longer need
	drop exp_num* exp_prop* exp_dnm*
	drop out_num* out_acscs_num* out_dnm out_prop* out_acscs_prop*
	drop t_out* t_*_out* 
	drop mp6_out* mp6_t_*_out*
		
	drop *_5_11* *_12_17* *_18_29* *_30_44* *_45_54* *_55_64* *_65_74* *_75_79* *_80_84* *_85p*
	drop *_mixed* *_asian* *_black* *_other*
	drop  *_imd_2* *_imd_3* *_imd_3* *_imd_4* *_imd_5*
	drop  *_urb2*  *_urb3* *_urb4* *_urb5*
	drop  *_male*
	
	
//Collapsing across all practices
	foreach stat in mean md {
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
		
		gen grouped_by = "All practices"
		frame copy default `first_frame', replace 	
		restore
		}
	}
		

	
	
//Collapsing by tertiles
	local exp_var_list u5y white imd_1 ast dbts hypt obs urb1 female smoker
	foreach stat in mean md {
	foreach hosp in apc ec {
		local first_frame `stat'_out_`hosp'_all
		local j = 1
		
		foreach exp_var in `exp_var_list'  {
		local out_vars mp6_`stat'_`exp_var'_out_*_`hosp' 
		preserve
			keep practice_pseudo_id week_number tert_exp_prop_`exp_var' `out_vars'_w
		
			reshape wide `out_vars'_w, i(practice_pseudo_id tert_exp_prop_`exp_var') j(week_number)
			
			foreach var of varlist `out_vars'_w* {
				qui levelsof tert_exp_prop_`exp_var', local(exp_var_levels)
				foreach level in `exp_var_levels'{
					di "Outcome: `var', grouping variable: `exp_var' with levels: `exp_var_levels'"
					levelsof `var' if tert_exp_prop_`exp_var' == `level'
					assert `r(r)' == 1
				}	
			}
	
			collapse (first) `out_vars'_w*, by(tert_exp_prop_`exp_var')
			
				
			gen grouped_by = "", after (tert_exp_prop_`exp_var')
				qui levelsof tert_exp_prop_`exp_var'
				forvalues i = 1/`r(r)'{
					replace grouped_by = "Proportion `exp_var', tertile `i'" in `i'
				}
			drop tert_exp_prop_`exp_var'
			rename mp6_`stat'_`exp_var'_out_* mp6_`stat'_out_*
			
			
			frame copy default out_`exp_var'_`hosp', replace 
				if `j' == 1 {
					frame copy out_`exp_var'_`hosp' `first_frame', replace
				} 
				if `j' != 1{
					frame `first_frame': xframeappend out_`exp_var'_`hosp',
				}
				
		local ++ j
		restore
	}
	}	
	}
		
	
//Merging the all-practice datasets together

//Merging the tertile datasets together	
	frame mean_cons_all: xframeappend mean_cons_exp	
	frame md_cons_all: xframeappend md_cons_exp	
	
//Saving as a .dta file, and exporting as a tab-delimited file 	
	foreach frame in md_out_apc md_out_ec mean_out_apc mean_out_ec md_out_apc_all md_out_ec_all mean_out_apc_all mean_out_ec_all {
		frame `frame': save `frame'.dta, replace
		frame `frame': export delimited using `frame'.txt, delim(tab)
	}



	


	
**# //GRAPHS	
//DISTRIBUTION of the proportion of hospitalised patients per practice
	
	//Overall
//		hist out_prop_apc_w, freq		
	
	//By region
	
	
//Line graphs of the prop. of hospitalised patients - OVER TIME
//tsset practice_pseudo_id out_interval_start
	
	//Overall
//		tsline mean_out_apc_w, name(mean_out_apc)
//		tsline mean_out_ec_w, name(mean_out_ec)
	
	//By region

	
	//By registered patient case-mix
		//Practices in the 90th percent
//			tsline mean_out_apc_w, by(tert_exp_prop_under5y) overlay
	
//grc1leg reg_total_gp_fte reg_total_gp_extg_fte reg_total_gp_extgl_fte, ///
//		legend(reg_total_gp_extgl_fte) ///
//			cols(3) pos(12) ring(0) ///
//		title("GP FTE from 2015 - 2024", size(small)) ///
//		name("gp_fte", replace) ///
//		saving("gp_fte", replace)	
	
	
//Line graphs of GP consultations - OVER TIME

//Two-way correlation: relationship
	//How changes over time.....

//	tsline out_prop_apc_w if practice_pseudo_id == 3
	
//	xtset practice_pseudo_id out_interval_start
//	xtline out_prop_apc_w, overlay legend(off)
//	xtline mean_out_prop_apc_w, overlay legend(off)
	
//	xtline mean_out_apc_w, overlay legend(off)
//	
//	if practice_pseudo_id == 3

	//Line graphs of the total number of hospital admissions over time
		//Overall
		//By practice region
		
//Exposure: GP characteristics - cross-sectional
	//Scatter plot of of the distribution of the proportion of registered patients
//	hist exp_prop_female
//	twoway scatter out_prop_apc_w exp_prop_female



//Useful code snippets
//	foreach var of varlist out*_num_*{
//		di "`var'"
//		local new_var_name: subinstr local var "num_" "", all
//		di "`new_var_name'"
//	}	
	







	
	