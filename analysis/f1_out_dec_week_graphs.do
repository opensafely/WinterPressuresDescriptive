/*============================================================================
DO FILE NAME:			f1_out_dec_graphs.do
DATE: 					10/10/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Creates the dataset + graphs of the outcome PER WEEK by practice deciles
==============================================================================*/	
//NOTE: These data combine the cohorts, so we won't pass the cohort names from the .yaml file
//(even though the graphs are per cohort so technically we could split this .do file and run as separate .yaml actions)
//However, I'm choosing to keep everything in one file because it's neater and matches the pattern in the other .do file used to generate graphs

//Setting directory for user-written commands
	//ssc install xframeappend 
	//ssc install grc1leg
	//ssc install egenmore 
adopath + ../workspace/analysis/ado 

 
//Creating the file paths for outputs
// mkdir ..workspace/output
cap mkdir output/f1_out_dec




**# //DATA MANAGEMENT- DECILE LINE GRAPHS PER OUTCOME PER WEEK 
//i.e. The x-axis on these plots represent the week. Each line represemts a DECILE practice group

//Creating the frame the re-shaped & collapsed data will be stored in 
clear frames 
frame create out_dec_week_all

//Looping through the analytic dataset for each cohort
//We need to reshape the outcome variables, then collapse by decile 
	foreach cohort in precovid postcovid1 postcovid2 postcovid3{
	import delimited using ../workspace/output/f1_out_dec/out_dec_data_long_`cohort'.csv, varnames(1) clear

		keep practice_pseudo_id week_number interval* mp6_prop_dec* mp6_md_dec* c_dec*
		
		reshape long mp6_prop_dec mp6_md_dec c_dec, i(practice week_number) j(hosp) string

		collapse (first) mp6_prop_dec mp6_md_dec, by(week_number c_dec hosp)
		
		reshape wide mp6_prop_dec mp6_md_dec, i(c_dec week) j(hosp) string
		
		rename c_dec decile
			label variable decile "Deciles of the cumulative outcome"
		
		gen cohort = "`cohort'"
	
		frame out_dec_week_all: xframeappend default
	}

//Saving this dataset 
	frame change out_dec_week_all
		save ../workspace/output/f1_out_dec/out_dec_week_all.dta, replace
		export delimited using ../workspace/output/f1_out_dec/out_dec_week_all.csv, replace



**# //DECILE LINE GRAPHS PER OUTCOME PER WEEK 
//Figures for all conditions 
	foreach cohort in precovid postcovid1 postcovid2 postcovid3 {
		if "`cohort'" == "precovid" {
			local cohort_year 2018/19
		}
		if "`cohort'" == "postcovid1" {
			local cohort_year 2022/23
		}
		if "`cohort'" == "postcovid2" {
			local cohort_year 2023/24
		}
		if "`cohort'" == "postcovid3" {
			local cohort_year 2024/25
		}
		foreach stat in prop md {
		foreach hosp in apc ec {
			if "`stat'" == "prop"{
				local stat_text Mean
			}
			if "`stat'" == "md" {
				local stat_text Median
			}
			if "`hosp'" == "apc" {
				local hosp_type_long `stat_text' APC admissions
			}
			if "`hosp'" == "ec" {
				local hosp_type_long `stat_text' EC attendances
			}
			qui sum mp6_`stat'_dec_`hosp'_w
				local yscale_max = ceil(`r(max)')
			
			twoway line mp6_`stat'_dec_`hosp'_w week_number if decile ==  1 & cohort == "`cohort'", sort connect(L) lcolor(navy) || ///
				line mp6_`stat'_dec_`hosp'_w week_number if decile == 2 & cohort == "`cohort'", sort connect(L) lcolor(midblue) || ///
				line mp6_`stat'_dec_`hosp'_w week_number if decile == 3 & cohort == "`cohort'", sort connect(L) lcolor(eltblue) || ///
				line mp6_`stat'_dec_`hosp'_w week_number if decile == 4 & cohort == "`cohort'", sort connect(L) lcolor(mint) || ///
				line mp6_`stat'_dec_`hosp'_w week_number if decile == 5 & cohort == "`cohort'", sort connect(L) lcolor(midgreen) || ///
				line mp6_`stat'_dec_`hosp'_w week_number if decile == 6 & cohort == "`cohort'", sort connect(L) lcolor(green) || ///
				line mp6_`stat'_dec_`hosp'_w week_number if decile == 7 & cohort == "`cohort'", sort connect(L) lcolor(emerald) || ///
				line mp6_`stat'_dec_`hosp'_w week_number if decile == 8 & cohort == "`cohort'", sort connect(L) lcolor(lavender) || ///
				line mp6_`stat'_dec_`hosp'_w week_number if decile == 9 & cohort == "`cohort'", sort connect(L) lcolor(magenta) || ///
				line mp6_`stat'_dec_`hosp'_w week_number if decile == 10 & cohort == "`cohort'", sort connect(L) lcolor(pink)  /// 
					title("`hosp_type_long' `cohort_year'", pos(11) size(medsmall) j(left)) ///
						ytitle("Rate per 1000 patients", size(small)) ///
						xtitle("Week", size(small)) ///
					ylab(0(1)`yscale_max', labsize(medsmall)) ///	 
					xlab(1(2)20, valuelabel angle(45) labsize(small)) ///
					legend(order(1 "Decile 1" 2 "Decile 2" 3 "Decile 3" 4 "Decile 4" 5 "Decile 5" ///
								 6 "Decile 6" 7 "Decile 7" 8 "Decile 8" 9 "Decile 9" 10  "Decile 10" ) ///
						   cols(5) pos(6) size(tiny) ring(3)) ///
					name("`stat'_wdec_`hosp'_`cohort'", replace)
		}
		}
		
		//Combining APC and EC plots, per cohort 
		grc1leg prop_wdec_apc_`cohort' prop_wdec_ec_`cohort' md_wdec_apc_`cohort' md_wdec_ec_`cohort', ///
			row(2) imargin(zero) iscale(0.5) ycommon ///
			title("Line graphs of hospital admission or emergency attendance rates over time", size(small) ring(1) pos(6)) ///
				subtitle("Each line represents practices grouped into deciles of the outcome variable", ///
				size(vsmall) ring(2) pos(6)) ///
			name("wdec_`cohort'", replace) 

		graph export ../workspace/output/f1_out_dec/wdec_`cohort'.svg, as(svg) name("wdec_`cohort'") replace		
	}
	


//Figures for the ACSCS conditions:
//Each figure will contain 10 individual graphs:
	//Top row: APC --> for each of the five ACSCS 
	//Bottom row: EC --> for each of the five ACSCS 
//There will be one figure for the means, one for the medians --> 8 figures in total 

	foreach cohort in precovid postcovid1 postcovid2 postcovid3 {
		if "`cohort'" == "precovid" {
			local cohort_year 2018/19
		}
		if "`cohort'" == "postcovid1" {
			local cohort_year 2022/23
		}
		if "`cohort'" == "postcovid2" {
			local cohort_year 2023/24
		}
		if "`cohort'" == "postcovid3" {
			local cohort_year 2024/25
		}
		foreach cond in ang ast copd dbts hypt {
			if "`cond'" == "ang" {
				local cond_text Angina 
			}
			if "`cond'" == "ast" {
				local cond_text Asthma 
			}
			if "`cond'" == "copd" {
				local cond_text COPD 
			}
			if "`cond'" == "dbts" {
				local cond_text Diabetes 
			}
			if "`cond'" == "hypt"{
				local cond_text Hypertension 
			}
			foreach stat in prop md {
			foreach hosp in apc ec {
				if "`stat'" == "prop"{
				local stat_text Mean
				}
				if "`stat'" == "md" {
					local stat_text Median
				}
				if "`hosp'" == "apc" {
					local hosp_type_long `stat_text' APC admissions
				}
				if "`hosp'" == "ec" {
					local hosp_type_long `stat_text' EC attendances
				}
				qui sum mp6_`stat'_dec_`cond'_`hosp'_w
				local yscale_max = ceil(`r(max)')
			
				twoway ///
					line mp6_`stat'_dec_`cond'_`hosp'_w week_number if decile==1 & cohort== "`cohort'", ///
						sort connect(L) lcolor(navy) || ///
					line mp6_`stat'_dec_`cond'_`hosp'_w week_number if decile==2 & cohort== "`cohort'", ///
						sort connect(L) lcolor(midblue) || ///
					line mp6_`stat'_dec_`cond'_`hosp'_w week_number if decile==3 & cohort== "`cohort'", ///
						sort connect(L) lcolor(eltblue) || ///
					line mp6_`stat'_dec_`cond'_`hosp'_w week_number if decile==4 & cohort== "`cohort'", ///
						sort connect(L) lcolor(mint) || ///
					line mp6_`stat'_dec_`cond'_`hosp'_w week_number if decile==5 & cohort== "`cohort'", ///
						sort connect(L) lcolor(midgreen)|| ///
					line mp6_`stat'_dec_`cond'_`hosp'_w week_number if decile==6 & cohort== "`cohort'", ///
						sort connect(L) lcolor(green) || ///
					line mp6_`stat'_dec_`cond'_`hosp'_w week_number if decile==7 & cohort== "`cohort'", ///
						sort connect(L) lcolor(emerald) || ///
					line mp6_`stat'_dec_`cond'_`hosp'_w week_number if decile==8 & cohort== "`cohort'", ///
						sort connect(L) lcolor(lavender)|| ///
					line mp6_`stat'_dec_`cond'_`hosp'_w week_number if decile==9 & cohort== "`cohort'", ///
						sort connect(L) lcolor(magenta) || ///
					line mp6_`stat'_dec_`cond'_`hosp'_w week_number if decile==10 & cohort== "`cohort'", ///
						sort connect(L) lcolor(pink)  /// 
						title("`hosp_type_long' for `cond_text', `cohort_year'", pos(11) size(small) j(left)) ///
							ytitle("Rate per 1000 patients", size(small)) ///
							xtitle("Week", size(small)) ///
						ylab(0(1)`yscale_max', labsize(small)) ///	 
						xlab(1(2)20, valuelabel angle(45) labsize(small)) ///
						legend(order(1 "Decile 1" 2 "Decile 2" 3 "Decile 3" 4 "Decile 4" 5 "Decile 5" ///
									 6 "Decile 6" 7 "Decile 7" 8 "Decile 8" 9 "Decile 9" 10  "Decile 10" ) ///
							   cols(5) pos(6) size(tiny) ring(3)) ///
						name("`stat'_wdec_`cond'_`hosp'_`cohort'", replace)
			}
			}
		}
		grc1leg ///
		prop_wdec_ang_apc_`cohort' prop_wdec_ast_apc_`cohort' prop_wdec_copd_apc_`cohort' prop_wdec_dbts_apc_`cohort' prop_wdec_hypt_apc_`cohort' ///
		prop_wdec_ang_ec_`cohort' prop_wdec_ast_ec_`cohort' prop_wdec_copd_ec_`cohort' prop_wdec_dbts_ec_`cohort' prop_wdec_hypt_ec_`cohort', ///
			row(2) imargin(zero) iscale(0.5) ycommon ///
			title("Line graphs of hospital admission or emergency attendance rates over time", size(small) ring(1) pos(6)) ///
				subtitle("Each line represents practices grouped into deciles of the outcome variable", ///
				size(vsmall) ring(2) pos(6)) ///
			name("prop_wdec_acscs_`cohort'", replace) 
			
			graph export ../workspace/output/f1_out_dec/prop_wdec_acscs_`cohort'.svg, as(svg) name("prop_wdec_acscs_`cohort'") replace	
			
		grc1leg ///
		md_wdec_ang_apc_`cohort' md_wdec_ast_apc_`cohort' md_wdec_copd_apc_`cohort' md_wdec_dbts_apc_`cohort' md_wdec_hypt_apc_`cohort' ///
		md_wdec_ang_ec_`cohort' md_wdec_ast_ec_`cohort' md_wdec_copd_ec_`cohort' md_wdec_dbts_ec_`cohort' md_wdec_hypt_ec_`cohort', ///
			row(2) imargin(zero) iscale(0.5) ycommon ///
			title("Line graphs of hospital admission or emergency attendance rates over time", size(small) ring(1) pos(6)) ///
				subtitle("Each line represents practices grouped into deciles of the outcome variable", ///
				size(vsmall) ring(2) pos(6)) ///
			name("md_wdec_acscs_`cohort'", replace) 
			
			graph export ../workspace/output/f1_out_dec/md_wdec_acscs_`cohort'.svg, as(svg) name("md_wdec_acscs_`cohort'") replace	
	}
			

			


























