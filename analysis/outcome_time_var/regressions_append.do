//Append the frames:
	//Per cohort
	//Then, per regression 
	
	
	
	frame create re_results str50 (cohort type model) n ///
							irr_exp se_exp p_exp lci_exp uci_exp ///
							irr_cons se_cons p_cons lci_cons uci_cons  ///
							v_random_int ll p_ll chi2_lrtest p_lrtest aic bic ///
							error
							
							
							
						 	