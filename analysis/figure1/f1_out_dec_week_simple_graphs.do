/*============================================================================
DO FILE NAME:			f1_out_dec_week_simple_graphs.do
DATE: 					10/10/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Creates the dataset + graphs of the outcome PER WEEK by SIMPLE OUTCOME deciles
==============================================================================*/	
//NOTE: These data combine the cohorts, so we won't pass the cohort names from the .yaml file


//Setting directory for user-written commands
	//ssc install xframeappend 
	//ssc install grc1leg
	//ssc install egenmore 
adopath + ../workspace/analysis/ado 

 
//Creating the file paths for outputs
// mkdir ..workspace/output
cap mkdir f1_out_dec 


**# //DATA MANAGEMENT- DECILE LINE GRAPHS PER OUTCOME PER WEEK 
//i.e. The x-axis on these plots represent the week. Each line represemts a DECILE practice group

//Creating the frame the re-shaped & collapsed data will be stored in 
clear frames 
frame create out_dec_week_simple_all
		
//Looping through the analytic dataset for each cohort
//We need to reshape the outcome variables, then collapse by decile 
	foreach cohort in precovid postcovid1 postcovid2 postcovid3{
	import delimited using ../workspace/output/f1_out_dec/out_dec_data_long_`cohort'.csv, varnames(1) clear

		keep practice_pseudo_id week_number interval* mp6_p10* mp6_p20* mp6_p30* mp6_p40* mp6_p50* mp6_p60* mp6_p70* mp6_p80* mp6_p90* mp6_p99*
		
		collapse (first) mp6_p10* mp6_p20* mp6_p30* mp6_p40* mp6_p50* mp6_p60* mp6_p70* mp6_p80* mp6_p90* mp6_p99*, by(week_number)

		gen cohort = "`cohort'"
	
	frame out_dec_week_simple_all: xframeappend default
	}	
	
//Adding week labels 
	frame change out_dec_week_simple_all
	
	label define week ///
			1 "Oct 1"  2 "Oct 8"  3 "Oct 15"  4 "Oct 22" ///
			 5 "Oct 29"  6 "Nov 5"  7  "Nov 12"  8 "Nov 19"  9 "Nov 26" ///
			 10 "Dec 3"  11 "Dec 10"  12 "Dec 17"  13 "Dec 24"  14 "Dec 31" ///
			 15 "Jan 7"  16 "Jan 14"  17 "Jan 21"  18 "Jan 28" /// 
			 19 "Feb 4"  20 "Feb 11"
		label values week week
		
//Saving this dataset 		
	export delimited using ../workspace/output/f1_out_dec/out_dec_week_simple_all.csv, replace		
	
	
//Graphs
//Each figure will contain 8 individual graphs:
	//Top row: APC admissions, one graph per cohort 
	//Bottom row: EC admissions, one graph per cohort 
	
	foreach cohort in precovid postcovid1 postcovid2 postcovid3 {
	foreach var in apc ec ang_apc ast_apc copd_apc dbts_apc hypt_apc ang_ec ast_ec copd_ec dbts_ec hypt_ec {
		if strpos("`var'", "apc"){
			local hosp_type APC admissions
		}
		if strpos("`var'", "ec"){
			local hosp_type EC attendances
		}
		local cond_type All conditions
		local ylab_max 18
		if strpos("`var'", "ang"){
			local cond_type Angina
			local ylab_max 5
		}
		if strpos("`var'", "ast"){
			local cond_type Asthma
			local ylab_max 5
		}
		if strpos("`var'", "copd"){
			local cond_type COPD
			local ylab_max 5
		}
		if strpos("`var'", "dbts"){
			local cond_type Diabetes
			local ylab_max 5
		}
		if strpos("`var'", "hypt"){
			local cond_type Hypertension
			local ylab_max 5
		}
		if "`cohort'" == "precovid"{
			local cohort_year 2018/19
		}
		if "`cohort'" == "postcovid1"{
			local cohort_year 2022/23
		}
		if "`cohort'" == "postcovid2"{
			local cohort_year 2023/24
		}
		if "`cohort'" == "postcovid3"{
			local cohort_year 2024/25
		}
		twoway line mp6_p10_`var'_w week_number if cohort == "`cohort'", sort lcolor(navy) || ///
			line mp6_p20_`var'_w week_number if cohort == "`cohort'", sort lcolor(midblue) || ///
			line mp6_p30_`var'_w week_number if cohort == "`cohort'", sort lcolor(eltblue) || ///
			line mp6_p40_`var'_w week_number if cohort == "`cohort'", sort lcolor(mint) || ///
			line mp6_p50_`var'_w week_number if cohort == "`cohort'", sort lcolor(midgreen) || ///
			line mp6_p60_`var'_w week_number if cohort == "`cohort'", sort lcolor(green) || ///
			line mp6_p70_`var'_w week_number if cohort == "`cohort'", sort lcolor(emerald) || ///
			line mp6_p80_`var'_w week_number if cohort == "`cohort'", sort lcolor(lavender) || ///
			line mp6_p90_`var'_w week_number if cohort == "`cohort'", sort lcolor(magenta) || ///
			line mp6_p99_`var'_w week_number if cohort == "`cohort'", sort lcolor(pink) ///
				title("`hosp_type', `cohort_year'", pos(11) size(medsmall) j(left)) ///
					ytitle("") ///
					xtitle("") ///
				ylab(0(2)`ylab_max', labsize(medsmall)) ///	 
				xlab(1(2)20, valuelabel angle(45) labsize(small)) ///
				legend(order(1 "Decile 1" 2 "Decile 2" 3 "Decile 3" 4 "Decile 4" 5 "Decile 5" 6 "Decile 6" 7 "Decile 7" 8 "Decile 8" 9 "Decile 9" 10  "Decile 10") cols(5) pos(6) size(tiny) ring(3)) ///
				name("x_`var'_`cohort'", replace)
	}		
	}
	
//Combining the graphs - all conditions	
	grc1leg ///
		x_apc_precovid x_apc_postcovid1 x_apc_postcovid2 x_apc_postcovid3 ///
		x_ec_precovid x_ec_postcovid1 x_ec_postcovid2 x_ec_postcovid3, ///
		row(2) imargin(zero) iscale(0.5) ycommon ///
		title("Hospital admission/emergency attendance rates over time.", size(small) ring(1) pos(6)) ///
		subtitle("Each line represents the decile value of the rate distribution. Admissions are for any condition. All rates are per 1000 patients.", size(vsmall) ring(2) pos(6)) /// 
		l1title("Rate per 1000 patients", size(vsmall) ring(1)) ///
		name("x_all_cond", replace) 
	
	graph export ../workspace/output/f1_out_dec/x_all_cond.svg, as(svg) name("x_all_cond") replace
	
//Combining the graphs - acscs conditions 	
	foreach cond in ang ast copd dbts hypt {
		grc1leg ///
			x_`cond'_apc_precovid x_`cond'_apc_postcovid1 x_`cond'_apc_postcovid2 x_`cond'_apc_postcovid3 ///
			x_`cond'_ec_precovid x_`cond'_ec_postcovid1 x_`cond'_ec_postcovid2 x_`cond'_ec_postcovid3, ///
			row(2) imargin(zero) iscale(0.5) ycommon ///
			title("Hospital admission/emergency attendance rates over time for: `cond_type'", size(small) ring(1) pos(6)) ///
			subtitle("Each line represents the decile value of the rate distribution. All rates are per 1000 patients.", size(vsmall) ring(2) pos(6)) /// 
			l1title("Rate per 1000 patients", size(vsmall) ring(1)) ///
			name("x_`cond'", replace) 
			
		graph export ../workspace/output/f1_out_dec/x_`cond'.svg, as(svg) name("x_`cond'") replace	
	}
			
	
	
	
	
