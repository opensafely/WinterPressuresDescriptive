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

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	