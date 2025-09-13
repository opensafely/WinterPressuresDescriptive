
//GP CONSULTATIONS

//Importing the data & clearing frames
clear frames 
import delimited "C:\Users\ShrinkhalaDawadi\Documents\GitHub\WinterPressuresDescriptive\output\analytic_data_wide_postcovid1.csv", varnames(1) clear


//DATA MANAGEMENT

//Dropping
	//Date variables
	//exp_prop_cons (we'll need to re-calculate after midpoint rounding)
	//Variables showing the missing proportions
	//Vaccination variables
	//CMS: Drop all conditions except hypertension, asthma, diabetes.
		//^^The 3 most prevalent conditions in Payne et al.2018
		//(excluding hearing loss & those requiring prescription codelists)
	
	keep practice_pseudo_id exp_* 	//Keeping only the exposure variables		
	
	drop *_interval_* exp_prop_cons* *_missing *_vax_* exp_*_af *_alcoholproblem *_anxietydepression *_cancer *_chd *_ckd *_constipation *_copd *_ctd *_dementia *_epilepsy *_hearingloss *_hf *_ibs *_osteoarthritis *_psychosis *_stroketia *smoker_ever *_smoker_never
	
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
	rename *under* *u*
	rename *_age_*_plus *_*p
	rename *denom* *dnm*
	rename *_2021* *_21*
	rename *_2022* *_22*

	
//Group variables: tertiles of select exposures
//I.e. grouping practices into tertiles based on their registered patient case-mix
	foreach var of varlist exp_prop_*{
		di "`var'"
		xtile tert_`var' = `var', nq(3)
	}
		
		
//Disclosure control & generating summary variables
//Midpoint rounding the numerator and denominator variables we'll use to generate the mean and medians

//Summary numerator and denominator for GP consultations	
	local group_var_list tert_*
	local str_remove tert_exp_prop_
	
	foreach var of varlist *cons_* {
		gen mp6_`var' = ceil(`var'/6)*6 - (floor(6/2)*(`var'!=0)) 	//Num & Denom
		
		egen t_`var' = total(`var')  //Total num & denom across all practices
		gen mp6_t_`var' = ceil(t_`var' /6)*6 - (floor(6/2)*(t_`var' !=0))	//Rounded 
		
		if strlen("`group_var_list'") != 0{
			foreach group_var of varlist `group_var_list'{  
			local new_var_name: subinstr local group_var "`str_remove'" "", all
				egen t_`new_var_name'_`var' = total(`var'), by(`group_var') //Total num & denom by group var
				gen mp6_t_`new_var_name'_`var' = ceil(t_`new_var_name'_`var' /6)*6 - (floor(6/2)*(t_`new_var_name'_`var' !=0)) //Rounded

			}
		}
	}
	
//Summary means and medians of GP consultations
	local group_var_list tert_*
	local str_remove1 exp_num_
	local str_remove2 tert_exp_prop_
	
	foreach var of varlist exp_num_cons_* {
		local new_var_name: subinstr local var "`str_remove1'" "", all 
			gen mp6_mean_`new_var_name' = (mp6_t_exp_num_`new_var_name'/mp6_t_exp_dnm_`new_var_name') 
			egen mp6_md_`new_var_name' = median((mp6_exp_num_`new_var_name'/mp6_exp_dnm_`new_var_name')) 
			
		if strlen("`group_var_list'") != 0{
		foreach group_var of varlist `group_var_list' {
			local new_gvar_name: subinstr local group_var "`str_remove2'" "", all
			gen mp6_mean_`new_gvar_name'_`new_var_name' = (mp6_t_`new_gvar_name'_exp_num_`new_var_name'/mp6_t_`new_gvar_name'_exp_dnm_`new_var_name') 
			
			egen mp6_md_`new_gvar_name'_`new_var_name' = median(mp6_t_`new_gvar_name'_exp_num_`new_var_name'/mp6_t_`new_gvar_name'_exp_dnm_`new_var_name') 
			
		}	
		}
		}


//Second drop of variables we no longer need
	drop exp_num* exp_prop* exp_dnm*
	drop mp6_exp* mp6_t_exp* t_exp* 
	drop *_5_11* *_12_17* *_18_29* *_30_44* *_45_54* *_55_64* *_65_74* *_75_79* *_80_84* *_85p*
	drop *_mixed* *_asian* *_black* *_other*
	drop  *_imd_2* *_imd_3* *_imd_3* *_imd_4* *_imd_5*
	drop  *_urb2*  *_urb3* *_urb4* *_urb5*
	drop  *_male*
	
	
//Collapsing across all practices
	foreach stat in mean md {
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
	local exp_var_list u5y white imd_1 ast dbts hypt obs urb1 female smoker
	foreach stat in mean md {
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
	frame mean_cons_all: xframeappend mean_cons_exp	
	frame md_cons_all: xframeappend md_cons_exp	
	
//Saving as a .dta file, and exporting as a tab-delimited file 	
	frame mean_cons_all: save mean_cons_all.dta, replace
	frame md_cons_all: save md_cons_all.dta, replace
	
	frame mean_cons_all: export delimited using mean_cons_all.txt, delim(tab) replace
	frame md_cons_all: export delimited using md_cons_all.txt, delim(tab) replace

	
**# //GENERATING GRAPHS
//Figures
//Variation in each outcome over time
//Variation in GP cons over time
//Bar graph of registered patient case-mix proportions, clustered by cohorts
	//Also by region of England 
//Relationship between each outcome and the patient case-mix proportions	
	//Y-axis: outcome rate, X-axis:patient case-mix	

	
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
//	 overlay legend(off)
	
//	if practice_pseudo_id == 3
	
	
	//Summarise practices by region 

	
	//Line graphs of the total number of hospital admissions over time
		//Overall
		//By practice region
		
	//Can't be by practice because it'll be too messy
		
//Exposure: GP characteristics - cross-sectional
	//Scatter plot of of the distribution of the proportion of registered patients

//	hist exp_prop_female
	
//	twoway scatter out_prop_apc_w exp_prop_female

	
	
	
	
	
	
	