/*============================================================================
DO FILE NAME:			f1_out_dec_data_management.do
DATE: 					10/10/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Creates variables required to generate descriptive graphs of the outcome by practice deciles
==============================================================================*/	

//=============================================================
//OUTCOME - HOSPITAL ATTENDANCES/ EMERGENCY ADMISSIONS
//=============================================================

//Setting directory for user-written commands
	//ssc install xframeappend 
	//ssc install grc1leg
	//ssc install egenmore 
adopath + ../workspace/analysis/ado 


//Creating the file paths for outputs
// mkdir ..workspace/output
cap mkdir output/f1_out_dec 
 
 
//Defining program to mid-point round the raw numerator/denominator variables
capture program drop round_mp6 
program round_mp6
	gen `2' = ceil(`1'/6)*6 - (floor(6/2)*(`1'!=0))
end


set maxvar 10000
// Importing data
import delimited using ../workspace/output/analytic_data_long_`1'.csv, varnames(1) clear

**#// DATA MANAGEMENT
//Exclude practices with fewer than <1000 patients 
	count if exp_denom <1000
	qui levelsof practice_pseudo_id if exp_denom <1000
	di "We will drop `r(r)' unique practices, comprising `r(N)' total observations in this longitudinal data"
	drop if exp_denom <1000
	
//Dropping vars: all exposure vars, all GP consultation vars 
	drop exp_*
	
//Renaming variables to make it easier to code
	rename out_* *
	rename acscs_* * 
	rename denom dnm
	
	rename *diabetes* *dbts*
	rename *asthma* *ast*
	rename *_angina_* *_ang_* 
	
//Cumulative admission rates per practice (i.e. (all admissions summed over 20 weeks)/(registered patients at index date summed over 20 weeks))	
//This will be used to calculate the cumulative deciles 
	//Outcome numerators, per practice 		
		foreach var of varlist num_* {
			local stub : subinstr local var "num_" "", all
			egen c_`var' = total(`var'), by(practice_pseudo_id) 
				round_mp6 c_`var' mp6_c_`var'
		}

	//Outcome denominators, per practice
		egen c_dnm = total(dnm), by(practice_pseudo_id) 
			round_mp6 c_dnm mp6_c_dnm	
		
	//Grouping practices into deciles, by their outcome variable values 
		//Cumulative deciles
			foreach var of varlist num_* {
				local stub: subinstr local var "num_" "", all
				xtile c_dec_`stub' = ((c_`var'/c_dnm)*1000), nq(10)
					label variable c_dec_`stub' "Deciles of cumulative rate: `stub'"
			}
		//Dynamic deciles 
			sort interval_start practice 
			foreach var of varlist num_* {
				local stub: subinstr local var "num_" "", all 	
				egen w_dec_`stub' = xtile(prop_`stub'), nq(10)
					label variable w_dec_`stub' "Deciles of weekly rate: `stub'"
			}

//Now calculating our outcome rates per week, and grouping by the cumulative decile var			
//Outcome numerators - rounded to midpoint 6
	foreach var of varlist num_* {
		local stub: subinstr local var "num_" "", all //Use to refer to appropriate decile var
		
		round_mp6 `var' mp6_`var' //Rounded numerator per practice, per week
			
		egen t_`var'= total(`var'), by(interval_start)	
			round_mp6 t_`var' mp6_t_`var' //Rounded total numerator, PER WEEK
		
		egen c_dec_`var' = total(`var'), by(interval_start c_dec_`stub') 
			round_mp6 c_dec_`var' mp6_c_dec_`var' //Rounded total num, PER WEEK & C.DECILE VAR
	}	
		
//Outcome denominators - rounded to midpoint 6 
	round_mp6 dnm mp6_dnm  //Rounded denominator per practice, per week
	
	egen t_dnm = total(dnm), by(interval_start) 
		round_mp6 t_dnm mp6_t_dnm //Rounded total denominator, PER WEEK

	foreach var of varlist c_dec_num_*{
		local stub: subinstr local var "c_dec_num_" "", all 	//local referring to denominator
		egen c_dec_dnm_`stub' = total(dnm), by(interval_start c_dec_`stub')
			round_mp6 c_dec_dnm_`stub' mp6_c_dec_dnm_`stub' //Rounded total denominator, PER WEEK & DECILE VAR
	}
	
//Outcome medians - generated from rounded numerators & denominators
//	We have the proportions of hospitalised patients per practice per week
//		mp6_md_`stub': is the median of this distribution per week
//		mp6_md_dec_`stub': is the median of this distribution per week, by practice deciles 

	foreach var of varlist mp6_num*{
		local c_dec_var: subinstr local var "mp6_num_" "c_dec_", all 
		local stub: subinstr local var "mp6_num_" "", all
		
		egen mp6_md_`stub' = /// Median of the proportion distribution, AT EACH DATE
			median(((`var'/mp6_dnm)*1000)), by(interval_start)
		
		egen mp6_md_dec_`stub' = ///Median of the prop. dist, BY GROUP & DATE
				median(((`var'/mp6_dnm)*1000)), by(interval_start `c_dec_var')
	}		
	
//Outcome row proportions - generated from rounded total numerators & denominators
//Also creating variables to store the decile values of the row proportion distribution, each week 
	foreach var of varlist mp6_num_*{
		local stub : subinstr local var "mp6_num_" "", all
		
		gen mp6_prop_`stub' = (`var'/ mp6_dnm)*1000  /// count outcome/count pts, PER DATE
		
		egen mp6_p10_`stub' = pctile(mp6_prop_`stub'), by(interval_start) p(10)
		egen mp6_p20_`stub' = pctile(mp6_prop_`stub'), by(interval_start) p(20)
		egen mp6_p30_`stub' = pctile(mp6_prop_`stub'), by(interval_start) p(30)
		egen mp6_p40_`stub' = pctile(mp6_prop_`stub'), by(interval_start) p(40)
		egen mp6_p50_`stub' = pctile(mp6_prop_`stub'), by(interval_start) p(50)
		egen mp6_p60_`stub' = pctile(mp6_prop_`stub'), by(interval_start) p(60)
		egen mp6_p70_`stub' = pctile(mp6_prop_`stub'), by(interval_start) p(70)
		egen mp6_p80_`stub' = pctile(mp6_prop_`stub'), by(interval_start) p(80)
		egen mp6_p90_`stub' = pctile(mp6_prop_`stub'), by(interval_start) p(90)
		egen mp6_p99_`stub' = pctile(mp6_prop_`stub'), by(interval_start) p(99)
	}
	

//Outcome total proportions - generated from rounded total numerators & denominators
//	We have the COUNT of hospitalised patients across all practices per week
//		mp6_t_prop_`stub': total count hospitalised/all registered patients PER WEEK
//		NOTE: These are not proportions per practice. They are proportions per week, and per grouping variable & week 
//		mp6_c_dec_prop_`stub': total count hosp/ all reg pts PER WEEK & DECILE 

	foreach var of varlist num_*{
		local stub : subinstr local var "num_" "", all
		
		gen mp6_t_prop_`stub' = (mp6_t_num_`stub'/ mp6_t_dnm)*1000  /// count outcome/count pts, PER DATE
		
		gen mp6_c_dec_prop_`stub' = (mp6_c_dec_num_`stub'/mp6_c_dec_dnm_`stub')*1000  //count outcome/count pts, PER CUMULATIVE DECILE & DATE 
		
	}

	
//Saving the dataset
//save ../workspace/output/f1_out_dec/out_dec_data_long_`1'.dta, replace
export delimited using ../workspace/output/f1_out_dec/out_dec_data_long_`1'.csv, replace

		
	
	


















//CODE USED IN DEVELOPMENT
//foreach stat in prop md {
//foreach hosp in apc ec {
//	local `stat'_`hosp'_list mp6_`stat'_dec*`hosp'_w
//		di _n "``stat'_`hosp'_list'"
//		
//		foreach out_var of varlist ``stat'_`hosp'_list'{
//			local stub : subinstr local out_var "mp6_`stat'_dec_" "", all
//			local stub : subinstr local stub "_w" "", all
//			local dec_var dec_prop_`stub'_w
//			di "Outcome: `out_var' --> Decile variable: `dec_var' ---> Stub: `stub'"
//		}
//}
//}	
	//Outcome means, per practice (cumulative across time)
	//	foreach var of varlist num_* {
	//		local stub: subinstr local var "num_" "", all
	//		gen mp6_c_prop_`stub' = (mp6_c_`var'/mp6_c_dnm)*1000
	//	}
