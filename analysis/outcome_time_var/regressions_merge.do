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
clear frames 			 
foreach cohort in precovid postcovid1 postcovid2 postcovid3{
	//For APC/EC
	import delimited using ../workspace/output/regressions/results_all_cond_`1'.csv, varnames(1) clear
		frame copy default results_all_cond_`cohort', replace 
	
	//For ACSCs within APC/EC
	import delimited using ../workspace/output/regressions/results_acsc_`1'.csv, varnames(1) clear
		frame copy default results_acsc_`cohort'
}


//Appending the frames together
	//For APC/EC
	frame copy results_all_cond_precovid results_all_cond, replace
	frame results_all_cond: ///
		xframeappend results_all_cond_postcovid1 results_all_cond_postcovid2 results_all_cond_postcovid3, drop 	
		
	//For ACSCs within APC/EC	
	frame copy results_acsc_precovid results_acsc, replace 
	frame results_acsc: ///
		xframeappend results_acsc_postcovid1 results_acsc_postcovid2 results_acsc_postcovid3, drop 	

		
//Exporting as a .csv
	frame results_all_cond: export delimited using ../workspace/output/regressions/results_all_cond.csv, replace	
	
	frame results_acsc: export delimited using ../workspace/output/regressions/results_acsc.csv, replace	

