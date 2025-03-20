from module_table_imports import *
from config_setup import *

# Import variables function
measures = create_measures()
measures.configure_disclosure_control(enabled=False)    #disabling disclosure control for demonstration
measures.configure_dummy_data(population_size=100, legacy = True)

from variables import generate_variables
variables = generate_variables(INTERVAL.start_date, INTERVAL.end_date)
# Extract variables from the dictionary so they can be directly used
globals().update(variables)

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