from module_table_imports import *
from config_setup import *

if practice_measures == True:

    # Import variables function
    measures = create_measures()
    measures.configure_disclosure_control(enabled=False)    #disabling disclosure control for demonstration
    measures.configure_dummy_data(population_size=100, legacy = True)

    # Import longitudinal variables (focusing on time period)
    from variables_measure import generate_variables
    variables_dynamic = generate_variables(INTERVAL.start_date, INTERVAL.end_date)
    # Extract variables from the dictionary so they can be directly used
    globals().update(variables_dynamic)

    # Import cross-sectional variables (focusing on a time point)
    from variables_dataset import generate_variables
    variables_cs = generate_variables(INTERVAL.start_date)
    # Extract variables from the dictionary so they can be directly used
    globals().update(variables_cs)
    # ---------------------- Measures Dictionaries ----------------------
    measures_age = {
        "exp_prop_under5y": exp_bin_under_5y,
        "exp_prop_5_to_16": exp_bin_5_16y,
        "exp_prop_65_to_74": exp_bin_65_74y,
        "exp_prop_75_to_84": exp_bin_75_84y,
        "exp_prop_age_85_plus": exp_bin_85y_plus,
    }

    measures_hospital_ACSC = {
        "out_count_apc_asthma_w": out_num_asthma_apc
    }
    # ---------------------- Cross-Sectional Measures ----------------------
    if CS == True:
        measures.define_defaults(
            denominator= inex_bin_reg_cs & inex_bin_alive & inex_bin_age & 
                        inex_bin_sex & inex_bin_ethinicity & inex_bin_imd & inex_bin_region,
            group_by={
                "practice_pseudo_id": practice_id
            },
            intervals=months(1).starting_on(start_cohort),
        )  
        if Age == True:
            for measure in measures_age.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_age[measure]
            )
        if Sex == True:
            measures.define_measure(
                name = "exp_prop_female",
                numerator = exp_bin_female
            ) 
    # ---------------------- Longitudinal Measures ----------------------
    if Long == True:
        measures.define_defaults(
            denominator= inex_bin_reg_long & inex_bin_alive & inex_bin_age & 
                        inex_bin_sex & inex_bin_ethinicity & inex_bin_imd & inex_bin_region,
            group_by={
                "practice_pseudo_id": practice_id
            },
            intervals=weeks(20).starting_on(start_cohort),
        )
        if Consultation == True:
            measures.define_measure(
                name = "exp_count_consultation_m",
                numerator = exp_num_consrate,
                intervals = months(12).ending_on(start_cohort)
            )     
        if apc == True:
            measures.define_measure(
                name = "out_count_apc_w",
                numerator = out_num_apc
            )
        if apc_ACSCs == True:
            for measure in measures_hospital_ACSC.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_hospital_ACSC[measure]
            )
if patient_measures == True:
# create dataset for different cohorts based on different start_cohort date

    dataset = create_dataset()

    dataset.define_population(
        patients.date_of_birth.is_not_null()
    )

    dataset.configure_dummy_data(population_size=1000)

    # Import preliminary date variables (death date, vax dates)

    from variables_vax import prelim_date_variables

    ## Add the imported variables to the dataset

    for var_name, var_value in prelim_date_variables.items():
        setattr(dataset, var_name, var_value)

    # Import jcvi variables ( JCVI group and derived variables; eligible date for vaccination based on JCVI group)
    from variables_vax import jcvi_variables

    ## Add the imported variables to the dataset
    for var_name, var_value in jcvi_variables.items():
        setattr(dataset, var_name, var_value)

    # Import multimorbidity conditions (20)

    from variables_dataset import generate_variables
    variables_cs = generate_variables(start_cohort)

    for var_name, var_value in variables_cs.items():
        setattr(dataset, var_name, var_value)
