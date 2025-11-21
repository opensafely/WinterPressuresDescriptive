/*============================================================================
DO FILE NAME:			regressions_merge.do
DATE: 					04/11/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Merges the regression results across cohorts 
==============================================================================*/	


//Creating the file paths for outputs
cap mkdir ..workspace/output
cap mkdir output/regressions



//Importing data, and putting each cohort into a frame
global path C:\Users\ShrinkhalaDawadi\Documents\GitHub\WinterPressuresDescriptive
cd "${path}"

clear frames 			 
foreach cohort in precovid postcovid1 postcovid2 postcovid3{
	//For APC/EC
	//import delimited using ../workspace/output/regressions/results_all_cond_`cohort'.csv, varnames(1) clear
	import delimited using ${path}/output/regressions/results_all_cond_`cohort'.csv, varnames(1) clear
		frame copy default results_all_cond_`cohort', replace 
		
	//For ACSCs within APC/EC
	//import delimited using ../workspace/output/regressions/results_acsc_`cohort'.csv, varnames(1) clear
	import delimited using ${path}/output/regressions/results_acsc_`cohort'.csv, varnames(1) clear
		frame copy default results_acsc_`cohort'
}



//Making sure all the variables in each frame are in the right format  
//String: cohort hosp_type acscs model_form out_var exp_var lrtest_comparing check_*

	foreach cohort in postcovid1 postcovid2 postcovid3 {
		frame change results_all_cond_`cohort'
		
			foreach var of varlist cohort hosp_type acscs model_form out_var exp_var lrtest_comparing check_*{
				cap confirm string variable `var'
				if _rc != 0 {
					di _n "`var' is changed to string"
					tostring `var', replace
				}
			}
			foreach var of varlist obs *_exp *_cons *_ri *_lrtest p_ll aic bic error ll {
				cap confirm numeric variable `var'
				if _rc != 0 {
					di "`var' is NOT numeric"
				}
			}
		frame change default 
	}

		//Making sure all the variables are in the right format 
		
	















metan hr_log lci_log uci_log if time2==`time', eform  effect(Hazard Ratio) notable forestplot(null(1) dp(2) xlab(.25 .5 1 2 3, force) favours("Favours Anti-VEGF             "   #   "             Favours control", nosymmetric) xtitle(, size(tiny)) graphregion(margin(zero) color(white)) texts(100) astext(65)) by(group) nowt nosubgroup nooverall nobox scheme(sj) label(namevar=outcome)  lcols(events events_control rate rate_control) 



//Appending the frames together
	//For APC/EC
	frame copy results_all_cond_precovid results_all_cond, replace
	frame results_all_cond: xframeappend results_all_cond_postcovid1 results_all_cond_postcovid2 results_all_cond_postcovid3, drop 	
		
	//For ACSCs within APC/EC	
	frame copy results_acsc_precovid results_acsc, replace 
	frame results_acsc: ///
		xframeappend results_acsc_postcovid1 results_acsc_postcovid2 results_acsc_postcovid3, drop 	

		
//Exporting as a .csv
	frame results_all_cond: export delimited using ../workspace/output/regressions/results_all_cond.csv, replace	
	
	frame results_acsc: export delimited using ../workspace/output/regressions/results_acsc.csv, replace	

