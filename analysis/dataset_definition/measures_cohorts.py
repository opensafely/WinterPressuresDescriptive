from module_table_imports import *
from config_setup import *

if practice_measures:

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
    # =========================
    # Age-related measures
    # =========================
    measures_age = {
        "exp_prop_under5y": exp_bin_under_5y,
        "exp_prop_5_to_16": exp_bin_5_16y,
        "exp_prop_65_to_74": exp_bin_65_74y,
        "exp_prop_75_to_84": exp_bin_75_84y,
        "exp_prop_age_85_plus": exp_bin_85y_plus,
    }

    # =========================
    # Sex-related measures
    # =========================
    measures_sex = {
        "exp_prop_female": exp_bin_female,
    }

    # =========================
    # Ethnicity-related measures
    # =========================
    measures_ethnicity = {
        "exp_prop_eth_missing": exp_bin_eth_missing,
        "exp_prop_eth_white": exp_bin_eth_white,
        "exp_prop_eth_mixed": exp_bin_eth_mixed,
        "exp_prop_eth_southasian": exp_bin_eth_southasian,
        "exp_prop_eth_black": exp_bin_eth_black,
        "exp_prop_eth_other": exp_bin_eth_other,
    }

    # =========================
    # Rurality-related measures
    # =========================
    measures_rurality = {
        "exp_prop_urb_major": urb_major,
        "exp_prop_urb_minor": urb_minor,
        "exp_prop_urb_town": urb_town,
        "exp_prop_urb_town_sp": urb_town_sp,
        "exp_prop_rural_fringe": rural_fringe,
        "exp_prop_rural_fringe_sp": rural_fringe_sp,
        "exp_prop_rural_village": rural_village,
        "exp_prop_rural_village_sp": rural_village_sp,
    }

    # =========================
    # IMD-related measures
    # =========================
    measures_imd = {
        "exp_prop_imd_1_most": exp_bin_imd_1_most,
        "exp_prop_imd_2": exp_bin_imd_2,
        "exp_prop_imd_3": exp_bin_imd_3,
        "exp_prop_imd_4": exp_bin_imd_4,
        "exp_prop_imd_5_least": exp_bin_imd_5_least,
    }

    # =========================
    # Smoking-related measures
    # =========================
    measures_smoking = {
        "exp_prop_smoker": exp_bin_smoker,
    }

    # =========================
    # Multimorbidity-related measures
    # =========================
    measures_multimorbidity = {
        "exp_prop_af": exp_bin_af,
        "exp_prop_alcoholproblem": exp_bin_alcoholproblem,
        "exp_prop_anxietydepression": exp_bin_anxietydepression,
        "exp_prop_asthma": exp_bin_asthma,
        "exp_prop_cancer": exp_bin_cancer,
        "exp_prop_chd": exp_bin_chd,
        "exp_prop_ckd": exp_bin_ckd,
        "exp_prop_constipation": exp_bin_constipation,
        "exp_prop_copd": exp_bin_copd,
        "exp_prop_ctd": exp_bin_ctd,
        "exp_prop_dementia": exp_bin_dementia,
        "exp_prop_diabetes": exp_bin_diabetes,
        "exp_prop_epilepsy": exp_bin_epilepsy,
        "exp_prop_hearingloss": exp_bin_hearingloss,
        "exp_prop_hf": exp_bin_hf,
        "exp_prop_hypertension": exp_bin_hypertension,
        "exp_prop_ibs": exp_bin_ibs,
        "exp_prop_psychosis": exp_bin_psychosis,
        "exp_prop_stroketia": exp_bin_stroketia,
        "exp_prop_osteoarthritis": exp_bin_osteoarthritis,
    }

    # =========================
    # Consultation-related measures
    # =========================
    measures_consultation = {
        "exp_num_consrate2019": exp_num_consrate2019,
        "exp_num_consrate": exp_num_consrate,
    }

    # =========================
    # Emergency care (EC) measures
    # =========================
    measures_ec = {
        "out_num_ec_w": out_num_ec,
    }

    # =========================
    # Admitted patient care (APC) measures
    # =========================
    measures_apc = {
        "out_num_apc_w": out_num_apc,
    }

    # =========================
    # ACSC-related measures - EC
    # =========================
    measures_ec_acsc = {
        "out_num_copd_ec_w": out_num_copd_ec,
        "out_num_asthma_ec_w": out_num_asthma_ec,
        "out_num_hypertension_ec_w": out_num_hypertension_ec,
        "out_num_diabetes_ec_w": out_num_diabetes_ec,
        "out_num_angina_ec_w": out_num_angina_ec,
    }

    # =========================
    # ACSC-related measures - APC
    # =========================
    measures_apc_acsc = {
        "out_num_copd_apc_w": out_num_copd_apc,
        "out_num_asthma_apc_w": out_num_asthma_apc,
        "out_num_hypertension_apc_w": out_num_hypertension_apc,
        "out_num_diabetes_apc_w": out_num_diabetes_apc,
        "out_num_angina_apc_w": out_num_angina_apc,
    }

    # ---------------------- Cross-Sectional Measures ----------------------
    if CS:
        measures.define_defaults(
            denominator= inex_bin_reg_cs & inex_bin_alive,
            group_by={
                "practice_pseudo_id": practice_id
            },
            intervals=months(1).starting_on(start_cohort),
        )

        if Age:
            for measure in measures_age.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_age[measure]
                )

        if Sex:
            for measure in measures_sex.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_sex[measure]
                )

        if Ethnicity:
            for measure in measures_ethnicity.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_ethnicity[measure]
                )

        if IMD:
            for measure in measures_imd.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_imd[measure]
                )

        if Rurality:
            for measure in measures_rurality.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_rurality[measure]
                )

        if Smoking:
            for measure in measures_smoking.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_smoking[measure]
                )

        if Multimorbidity:
            for measure in measures_multimorbidity.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_multimorbidity[measure]
                )

    # ----------------------
    # Longitudinal Measures
    # ----------------------
    if Long:
        measures.define_defaults(
            denominator= inex_bin_reg_cs & inex_bin_alive,
            group_by={
                "practice_pseudo_id": practice_id
            },
            intervals=weeks(20).starting_on(start_cohort),
        )

        if Consultation:
            measures.define_measure(
                name = "exp_count_consultation_m",
                numerator = exp_num_consrate,
                intervals = months(12).ending_on(start_cohort)
            )

        if ec:
            measures.define_measure(
                name = "out_count_ec_w",
                numerator = out_num_ec
            )

        if apc:
            measures.define_measure(
                name = "out_count_apc_w",
                numerator = out_num_apc
            )

        if ec_ACSCs:
            for measure in measures_ec_acsc.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_ec_acsc[measure]
                )

        if apc_ACSCs:
            for measure in measures_apc_acsc.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_apc_acsc[measure]
                )
if patient_measures:
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
