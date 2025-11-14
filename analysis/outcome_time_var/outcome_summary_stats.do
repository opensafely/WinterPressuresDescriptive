/*============================================================================
DO FILE NAME:			outcome_summary_stats.do
DATE: 					04/11/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Creates a summary table of the outcomes, then;
						Runs a random-intercept Poisson model to describe the 
						association between each exposure x outcome, then;
						stores the results in a frame
==============================================================================*/	

//User-written commands - ssc install: xframeappend; grc1leg; egenmore 
adopath + ../workspace/analysis/ado 

//Creating the file paths for outputs
// mkdir ..workspace/output
cap mkdir output/regressions
	
		
//Importing data - looping through the analytic dataset for each cohort		
clear frames 			 
foreach cohort in precovid postcovid1 postcovid2 postcovid3{
	import delimited using ../workspace/output/analytic_data_long_`cohort'.csv, varnames(1) clear		
	
	gen cohort = "`cohort'"
	
//Creating a frame to store the summary statistics 	
	frame copy default summary_`cohort', replace
	frame change summary_`cohort'
	
		keep out_num_* out_acscs_num* practice_pseudo_id week_number out_denom
		rename out_* *
		rename denom dnm
		rename acscs_* * 
		rename *diabetes* *dbts*
		rename *asthma* *ast*
		rename *_angina_* *_ang_* 
		
	//Generating the summary statistics for each outcome variable 
		foreach var of varlist num_* dnm {
			local stub: subinstr local var "num_" "", all
			local stub: subinstr local stub "_w" "", all
			
			egen min_`stub' = min(`var') 			//Min
			egen q1_`stub' = pctile(`var'), p(25) 	//Q1
			egen med_`stub' = pctile(`var'), p(50)	//Median
			egen q3_`stub' = pctile(`var'), p(75)	//Q3
			egen max_`stub' = max(`var')
			
			egen p10_`stub' = pctile(`var'), p(10)	//p10
			egen p20_`stub' = pctile(`var'), p(20)
			egen p30_`stub' = pctile(`var'), p(30)
			egen p40_`stub' = pctile(`var'), p(40)
			egen p50_`stub' = pctile(`var'), p(50)	//Median repeated (easier to run eyes along data)
			egen p60_`stub' = pctile(`var'), p(60)
			egen p70_`stub' = pctile(`var'), p(70)
			egen p80_`stub' = pctile(`var'), p(80)
			egen p90_`stub' = pctile(`var'), p(90)
			egen p99_`stub' = pctile(`var'), p(99)	//p99
			
			gen range_`stub' = max_`stub' - min_`stub' 	//Range
			egen total_`stub' = total(`var') 			//Total admission count
			
			egen mean_`stub' = mean(`var')				//Mean
			egen sd_`stub' = sd(`var')					//SD
			gen crude_disp_`stub' = (sd_`stub')^2/mean_`stub'	//Crude dispersion (variance/mean)
			
			qui count if `var' !=0 		//Number of practices with non-zero observations
				gen non_zero_obs_`stub' = `r(N)'
			
			qui count if `var' == 0 
				local count_zero = `r(N)'
			qui count 	
				local count_total = `r(N)'
			gen prop_zero_`stub' = `count_zero'/`count_total'  //Prop. counts that are zero overall
			
			forvalues i = 1/20 {
				qui count if `var' == 0 & week_number == `i'
					local count_zero = `r(N)'
				qui count if week_number == `i'
					local count_total = `r(N)'	
				gen wprop_zero_`stub'_w`i' = `count_zero'/`count_total' //Prop. counts that are zero per WEEK 	
			}	
		}
		
		drop practice_pseudo_id num_* 
	
	//Putting the statistics summarised over all time into a new frame 
		frame put min_* q1_* med_* q3_* max_* p10_* p20_* p30_* p40_* p50_* p60_* p70_* p80_* p90_* p99_* range_* total_* mean_* sd_* crude_disp_* non_zero_obs_* prop_zero_*, into(a_summary_`cohort')
		frame change a_summary_`cohort'
			collapse (first) min_apc - prop_zero_dnm
			gen id = 1
		//Reshaping to long - one row per outcome, each column represents a statistic	
			reshape long min_ q1_ med_ q3_ max_ p10_ p20_ p30_ p40_ p50_ p60_ p70_ p80_ p90_ p99_ range_ total_ mean_ sd_ crude_disp_ non_zero_obs_ prop_zero_ , i(id) j(outcome) string
		frame change summary_`cohort'	
	
	//Putting the statistics summarised per WEEK into a new frame	
		frame put week_number wprop_zero_*, into(w_summary_`cohort')
		frame change w_summary_`cohort'
			collapse (first) wprop_zero_apc_w1 - wprop_zero_dnm_w20
			gen id = 1
			
		//Because this variable is reported per week, two steps for reshape
		//End result is the same: one row per outcome, each column represents a statistic		
			reshape long wprop_zero_, i(id) j(outcome) string
			
			split outcome, p(_w)
				destring outcome2, gen(week_number)
				drop outcome2 outcome
				
			reshape wide wprop_zero_, i(outcome1) j(week_number)
			
			rename wprop_zero_* prop_zero_w*
			rename outcome1 outcome
			drop id 
	
	//Merging the frames together
		frame change a_summary_`cohort'
			frlink 1:1 outcome, frame(w_summary_`cohort')
			frget prop_zero_w*, from(w_summary_`cohort')
			
			gen cohort = "`cohort'", before(outcome)
			drop id 
		
	//Dropping the now-unneccesary frames 
		frame drop w_summary_`cohort' 
		frame drop summary_`cohort'
		
	//Renaming variables to make what they represent clearer	
		rename *_ *
		rename prop_zero prop_zero_overall

//Going back to the default frame at the end of the loop 		
	frame change default
}

//Appending the frames for each cohort together
	frame a_summary_precovid: ///
		xframeappend a_summary_postcovid1 a_summary_postcovid2 a_summary_postcovid3, drop 	
	frame a_summary_precovid: drop w_summary_*
//Exporting the frame as a .csv 
	frame a_summary_precovid: export delimited using ../workspace/output/regressions/outcome_summary_stats.csv, replace	
	

	
	
	