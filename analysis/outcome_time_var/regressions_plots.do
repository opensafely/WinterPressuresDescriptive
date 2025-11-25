//PROCESS------------------------------------------------------

//Produce a forest plot of:
	//Regression coefficients (IRRs) for exposure variables
	//APC & EC only: 
		//random-intercept variance and 95%CIs
		//P of lrtest comparing random-intercept models to fixed-effects only 
		//AIC values; BIC values	
	
	
	//Produce a summary graph of the random-intercept variance in this regression
	
//Coding pipeline
	//Each cohort and condition type will have a different .yaml action
		//run regressions_precovid --> APC & EC only
		//run regressions_postcovid1 --> APC & EC only
		//run regressions_postcovid2 --> APC & EC only
		//run regressions_postcovid3 --> APC & EC only
		//run regressions_precovid --> ACSCs for APC & EC
		//run regressions_postcovid1 --> ACSCs for APC & EC
		//run regressions_postcovid2 --> ACSCs for APC & EC
		//run regressions_postcovid3 --> ACSCs for APC & EC
		
		
	//.yaml action to combine all the regression outputs into one .csv
	//Then make the graphs
		//APC: precovid, postcovid1, postcovid2, postcovid3. Overlay estimates from poisson and nbreg?
		
	

//CODE SNIPPETS--------------------------------------------------------	
	local hosp apc
	foreach var of varlist num_*_apc_w num_*_ec_w {
		local stub: subinstr local var "num_" "", all
		local stub: subinstr local stub "_w" "", all
		local acsc: subinstr local stub "_`hosp'" "", all
		di "`var' --> `stub' --> `acsc'"	
	}	
	

	
//TO DO: 
//Amend code so it's one .yaml action per cohort and model
//Double check I'm using the correct exposure variables 	

	
	
//ssc install metan
	
	
	
	//Creating a table of the hospital admission rates per exposure variable 
	//Per exposure variable

	
	
	
	
	
//Making forest plots
frame change results_all_cond
	

		
		
	metan irr_exp lci_exp uci_exp ///
		if cohort == "precovid" & hosp_type == "apc" & model_form_num == 1, ///
		notable nooverall ///
		keepall keeporder sortby(exp_var_num)  	///
		forestplot( 							///
			nowt nonames null(1) effect("IRR")	 	///
			xtitle("Incidence-rate ratio of one-to-one regressions", size(vsmall)) ///
			savedims(precovid_apc_m1) 			///
			name(fp_precovid_apc_m1, replace) 	///
			) 
		
	metan variance_ri lci_ri uci_ri ///
		if cohort == "precovid" & hosp_type == "apc" & model_form_num == 1, ///
		notable nooverall ///
		keepall keeporder sortby(exp_var_num)  	///
		forestplot( 							///
			nowt nonames null(1) effect("Variance: random-intercepts") ///
			xtitle("RI variance", size(vsmall)) ///
			usedims(precovid_apc_m1)	///
			name(fp_precovid_apc_rim1, replace) ///
			) 
			
			
	metan variance_ri lci_ri uci_ri ///
		if cohort == "precovid" & hosp_type == "apc" & model_form_num == 1, ///
		notable nooverall ///
		keepall keeporder sortby(exp_var_num)  ///
		labtitle("Exposure Variables" " ") 	///
		forestplot( 	///
			nowt nostats colsonly	///
			lcols(exp_var_long) ///
			usedims(precovid_apc_m1) ///
			name(fp_precovid_apc_cols, replace) ///
			) 
			
	graph combine fp_precovid_apc_cols fp_precovid_apc_m1 fp_precovid_apc_rim1 , rows(1) imargin(zero)		
				
	
	`"`" "' `"Cutoff"'"'
	
//----------------------------------------------------------------
	metan irr_exp lci_exp uci_exp ///
		if cohort == "precovid" & hosp_type == "apc" & model_form_num == 1, ///
		notable nowt nooverall ///
		keepall keeporder sortby(exp_var_num)  ///
		labtitle("Exposure variables") ///
		forestplot( 	///
			null(1) savedims(precovid_apc_m1)	///
			lcols(exp_var_long) ///
			effect(IRR) ///
			xtitle("Incidence-rate ratio of one-to-one regressions", size(vsmall)) ///
			name(fp_precovid_apc_m1, replace) ///
			)
			

//RC code 
metan hr_log lci_log uci_log if time2==`time', ///
	eform effect(Hazard Ratio) notable ///
	forestplot(null(1) dp(2) xlab(.25 .5 1 2 3, force) ///
	favours("Favours Anti-VEGF             "   #   "             Favours control", nosymmetric) ///
	xtitle(, size(tiny)) graphregion(margin(zero) color(white)) texts(100) astext(65)) by(group) nowt nosubgroup nooverall nobox scheme(sj) label(namevar=outcome)  lcols(events events_control rate rate_control) 

	
	
	
	
	
//TO DO:
//Figure out why all cohorts are labelled as cohort = postcovid3 
		
	
//All cond: 4 cohorts * 2 models * 2 hosp types * 20 exposure variables = 320 regressions in total
//ACSCs: 4 cohorts * 3 models * 2 hosp types * 5 ACSC * 20 exposure variables = 2400 regressions in total	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	