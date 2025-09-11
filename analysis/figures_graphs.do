//Figures
//Variation in each outcome over time
//Variation in GP cons over time
//Bar graph of registered patient case-mix proportions, clustered by cohorts
	//Also by region of England 
//Relationship between each outcome and the patient case-mix proportions	
	//Y-axis: outcome rate, X-axis:patient case-mix
	
//Set STATA log on
//Have the STATA log be a moderately sensitive output? 



//Importing the data
import delimited "C:\Users\ShrinkhalaDawadi\Documents\GitHub\WinterPressuresDescriptive\output\analytic_data_long_postcovid1.csv", varnames(1) clear


//Data management	
	foreach var of varlist *_interval_* {
		rename `var' `var'_str
		gen `var' = date(`var'_str, "YMD")
			format `var' %td
		
		qui count if `var' != . 
			local non_miss = `r(N)'
		qui count if `var' == . 
			local miss = `r(N)'
			assert `non_miss' >= `miss'
	}
	



// Distrubtion of proportion 
	hist out_prop_apc_w, freq

//VARIABLE GENERATION	
// Generate summary variables - overall
	//Mean across all practices: summing all the appts, summing all the patients, then dividing
	
	egen t_out_denom = total(out_denom), by(out_interval_start)
	
	foreach var of varlist out*_num_*{
		egen t_`var' = total(`var'), by(out_interval_start)
		gen mean_`var' = t_`var' / t_out_denom
	}
	
	
	
	rename mean_out*num_* mean_out*
	
	
	
	
	foreach var of varlist out*_num_*{
		di "`var'"
	
	}
	
	
	
	
	egen t_out_num_apc_w = total(out_num_apc_w), by(out_interval_start) 
	
	gen mean_out_apc_w = t_out_num_apc_w/t_out_denom
		order t_out_num_apc_w t_out_denom mean_out_apc_w mean_out_prop_apc_w, after(out_prop_apc_w)
	
	egen mean_out_prop_apc_w = mean(out_prop_apc_w), by(out_interval_start)	
	
	num_apc_w
	
	//Median across all practices 
	
	
	
	
// Generate summary variables by region 


	
	//Total number of hospital admissions...per practice
		
		
	//What is our outcome variable saying?
		//The proportion of registered patients with a hospital admission per practice-level
		
		
		//How to summarise this proportion over time and by region?
			
		
**# //GRAPHS		
// Line graphs of each outcome variable
	tsline mean_out_apc_w
	tsline mean_out_ec_w
	
	
	tsset practice_pseudo_id out_interval_start
	tsline out_prop_apc_w if practice_pseudo_id == 3
	
	xtset practice_pseudo_id out_interval_start
	xtline out_prop_apc_w, overlay legend(off)
	xtline mean_out_prop_apc_w, overlay legend(off)
	
	xtline mean_out_apc_w, overlay legend(off)
	 overlay legend(off)
	
	if practice_pseudo_id == 3
	
	
	//Summarise practices by region 
	
	
	exp_cat_region
	
	
	//Merging in the practice-level region variable....
	
	
	
	
	//Line graphs of the total number of hospital admissions over time
		//Overall
		//By practice region
		
	//Can't be by practice because it'll be too messy
	
		
//Exposure: GP characteristics - cross-sectional
	//Scatter plot of of the distribution of the proportion of registered patients

	hist exp_prop_female
	
	twoway scatter out_prop_apc_w exp_prop_female






























	
	
	
	
	
	
	
	
	
	
	