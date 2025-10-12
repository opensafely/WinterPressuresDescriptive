/*============================================================================
DO FILE NAME:			f1_out_dec_graphs.do
DATE: 					10/10/2025
AUTHOR:					Shrinkhala Dawadi
DESCRIPTION OF FILE:	Creates the dataset + graphs of the outcome by practice deciles
==============================================================================*/	
//NOTE: These data combine the cohorts, so we won't pass the cohort names from the .yaml file


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
 
 
**#// DATASET FOR DECILE PLOTS (OVERALL, PER COHORT)
//i.e. The x-axis on these plots are practices, grouped into deciles. Each line represemts a COHORT
//Creating a new frame to store the data for these graphs
clear frames 
frame create out_dec_all

//Reshaping the outcome variables, then collapsing by the outcome deciles  
	foreach cohort in precovid postcovid1 postcovid2 postcovid3{
		import delimited using ../workspace/output/f1_out_dec/out_dec_data_long_`cohort'.csv, varnames(1) clear

		keep practice_pseudo_id week_number interval* mp6_prop_dec* mp6_md_dec* c_dec*
		
		reshape long mp6_prop_dec mp6_md_dec c_dec, i(practice week_number) j(hosp) string

		collapse (first) mp6_prop_dec mp6_md_dec, by(c_dec hosp)
		
		reshape wide mp6_prop_dec mp6_md_dec, i(c_dec) j(hosp) string
		
		rename c_dec decile
			label variable decile "Deciles of the cumulative outcome"
		
		gen cohort = "`cohort'"
		
		frame out_dec_all: xframeappend default
	}

//Saving this dataset 
	frame change out_dec_all
		save ../workspace/output/f1_out_dec/out_dec_all.dta, replace
		export delimited using ../workspace/output/f1_out_dec/out_dec_all.csv, replace
		
	
**# //DECILE PLOTS PER OUTCOME, PER COHORT
//Each figure will have four plots: Mean APC, Mean EC, Median APC, Median EC
//We will have 6 figures in total: 1 for all conditions, then 1 each for the 5 ACSCs

//Figure for all conditions
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
		
		twoway line mp6_`stat'_dec_`hosp'_w decile if cohort == "precovid", sort connect(L) lcolor(navy) || ///
			line mp6_`stat'_dec_`hosp'_w decile if cohort == "postcovid1", sort connect(L) lcolor(blue) || ///
			line mp6_`stat'_dec_`hosp'_w decile if cohort == "postcovid2", sort connect(L) lcolor(midblue) || ///
			line mp6_`stat'_dec_`hosp'_w decile if cohort == "postcovid3", sort connect(L) lcolor(eltblue)  ///
				title("`hosp_type_long'", pos(11) size(medsmall) j(left)) ///
					ytitle("Rate per 1000 patients", size(small)) ///
					xtitle("Deciles", size(small)) ///
				legend(order(1 "2018/19" 2 "2022/23" 3 "2023/24" 4 "2024/25") ///
						   rows(1) pos(6) size(vsmall) ring(3)) ///
				name("`stat'_dec_`hosp'_all", replace)
	}
	}
	
	//Combining APC and EC plots
		grc1leg prop_dec_apc_all prop_dec_ec_all md_dec_apc_all md_dec_ec_all, ///
			row(2) imargin(zero) iscale(0.5) ycommon ///
			title("Decile plots of hospital admission or emergency attendance rates", size(small) ring(1) pos(6)) ///
				subtitle("The x-axis represents practices grouped into deciles of the outcome", ///
				size(vsmall) ring(2) pos(6)) ///
			name("dec_all", replace) ///
			
		graph export ../workspace/output/f1_out_dec/dec_all.svg, as(svg) name("dec_all") replace
			
			 
//Figures for each ACSCs 
	foreach cond in ang ast copd dbts hypt {
		foreach stat in prop md {
		foreach hosp in apc ec {
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
			
			twoway line mp6_`stat'_dec_`cond'_`hosp'_w decile if cohort == "precovid", sort connect(L) lcolor(navy) || ///
				line mp6_`stat'_dec_`cond'_`hosp'_w decile if cohort == "postcovid1", sort connect(L) lcolor(blue) || ///
				line mp6_`stat'_dec_`cond'_`hosp'_w decile if cohort == "postcovid2", sort connect(L) lcolor(midblue) || ///
				line mp6_`stat'_dec_`cond'_`hosp'_w decile if cohort == "postcovid3", sort connect(L) lcolor(eltblue)  ///
					title("`hosp_type_long' for `cond_text'", pos(11) size(medsmall) j(left)) ///
						ytitle("Rate per 1000 patients", size(small)) ///
						xtitle("Deciles", size(small)) ///
					legend(order(1 "2018/19" 2 "2022/23" 3 "2023/24" 4 "2024/25") ///
							   rows(1) pos(6) size(vsmall) ring(3)) ///
					name("`stat'_dec_`cond'_`hosp'_all", replace)
		}
		}
		//Combining APC and EC plots
			grc1leg prop_dec_`cond'_apc_all prop_dec_`cond'_ec_all md_dec_`cond'_apc_all md_dec_`cond'_ec_all, ///
				row(2) imargin(zero) iscale(0.5) ycommon ///
				title("Decile plots of hospital admission or emergency attendance rates", size(small) ring(1) pos(6)) ///
					subtitle("The x-axis represents practices grouped into deciles of the outcome", ///
					size(vsmall) ring(2) pos(6)) ///
				name("dec_`cond'_all", replace)	
			
			graph export ../workspace/output/f1_out_dec/dec_`cond'_all.svg, as(svg) name("dec_`cond'_all") replace
	}
 