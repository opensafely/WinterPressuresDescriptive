/*============================================================================
DO FILE NAME:			f1_out_dec_graphs.do
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

	
		keep practice_pseudo_id week_number interval* mp6_p*
		drop mp6_prop*
	
		
		collapse (first) mp6_p*, by(week_number)
	
		reshape long mp6_p10 mp6_p20 mp6_p30 mp6_p40 mp6_p50 mp6_p60 mp6_p70 mp6_p80 mp6_p90 mp6_p99, i(week_number) j(type) string
	
		reshape wide mp6_p10 mp6_p20 mp6_p30 mp6_p40 mp6_p50 mp6_p60 mp6_p70 mp6_p80 mp6_p90 mp6_p99, i(week) j(type) string
		
		gen cohort = "`cohort'"
	
	frame out_dec_week_simple_all: xframeappend default
	}	
	

//Saving this dataset 
	frame change out_dec_week_all
		//save ../workspace/output/f1_out_dec/out_dec_week_all.dta, replace
		export delimited using ../workspace/output/f1_out_dec/out_dec_week_simple_all.csv, replace

//Graphs
foreach cohort in precovid postcovid1 postcovid2 postcovid3 {
foreach var in _apc_w _ec_w {
	twoway ///
	line mp6_p10 week_number if type == "`var'" & cohort == "`cohort'", sort connect(L) lcolor(navy) || //
	line mp6_p20 week_number if type == "`var'" & cohort == "`cohort'", sort connect(L) lcolor(midblue) || //
	line mp6_p30 week_number if type == "`var'" & cohort == "`cohort'", sort connect(L) lcolor(eltblue) || //
	line mp6_p40 week_number if type == "`var'" & cohort == "`cohort'", sort connect(L) lcolor(mint) || //
	line mp6_p50 week_number if type == "`var'" & cohort == "`cohort'", sort connect(L) lcolor(midgreen) || //
	line mp6_p60 week_number if type == "`var'" & cohort == "`cohort'", sort connect(L) lcolor(green) || //
	line mp6_p70 week_number if type == "`var'" & cohort == "`cohort'", sort connect(L) lcolor(emerald) || //
	line mp6_p80 week_number if type == "`var'" & cohort == "`cohort'", sort connect(L) lcolor(lavender) || //
	line mp6_p90 week_number if type == "`var'" & cohort == "`cohort'", sort connect(L) lcolor(magenta) || //
	line mp6_p99 week_number if type == "`var'" & cohort == "`cohort'", sort connect(L) lcolor(pink) ///
		title("Percentiles of `var', cohort: `cohort'", pos(11) size(medsmall) j(left)) ///
				ytitle("Rate per 1000 patients", size(small)) ///
				xtitle("Week", size(small)) ///
					ylab(0(1)8, labsize(medsmall)) ///	 
					xlab(1(2)20, valuelabel angle(45) labsize(small)) ///
					legend(order(1 "Decile 1" 2 "Decile 2" 3 "Decile 3" 4 "Decile 4" 5 "Decile 5" ///
								 6 "Decile 6" 7 "Decile 7" 8 "Decile 8" 9 "Decile 9" 10  "Decile 10" ) ///
						   cols(5) pos(6) size(tiny) ring(3)) ///
					name("x`var'_`cohort'", replace)
		}
		
	grc1leg x_apc_w_`cohort' x_ec_w_`cohort', ///
			row(2) imargin(zero) iscale(0.5) ycommon ///
			title("Line graphs of hospital admission or emergency attendance rates over time", size(small) ring(1) pos(6)) ///
				subtitle("Each line represents the percentile value (10 deciles) of the outcome variable", ///
				size(vsmall) ring(2) pos(6)) ///
			name("xdec_`cohort'", replace) 

	graph export ../workspace/output/f1_out_dec/xdec_`cohort'.svg, as(svg) name("xdec_`cohort'") replace		
		}
	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	