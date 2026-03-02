from ehrql import claim_permissions

from module_table_imports import *
from config_setup import *

claim_permissions("appointments", "sgss_covid_all_tests", "occupation_on_covid_vaccine_record")

if practice_measures:

    # Import variables function
    measures = create_measures()
    measures.configure_disclosure_control(enabled=False)    #disabling disclosure control for demonstration
    measures.configure_dummy_data(population_size=10000, legacy=True)

    # Import longitudinal variables (focusing on time period)
    from variables_longitudinal import generate_variables
    variables_dynamic = generate_variables(INTERVAL.start_date, INTERVAL.end_date)
    # Extract variables from the dictionary so they can be directly used
    globals().update(variables_dynamic)

    # Import cross-sectional variables (focusing on a time point)
    from variables_cross_sectional import generate_measure_variables
    variables_cs = generate_measure_variables(INTERVAL.start_date)
    # Extract variables from the dictionary so they can be directly used
    globals().update(variables_cs)

    # ---------------------- Measures Dictionaries ----------------------
    # =========================
    # Age-related measures
    # =========================
    measures_age = {
        "age_0_4":       exp_bin_under_5y,
        "age_5_11":      exp_bin_5_11y,
        "age_12_17":     exp_bin_12_17y,
        "age_18_29":     exp_bin_18_29y,
        "age_30_44":     exp_bin_30_44y,
        "age_45_54":     exp_bin_45_54y,
        "age_55_64":     exp_bin_55_64y,
        "age_65_74":     exp_bin_65_74y,
        "age_75_79":     exp_bin_75_79y,
        "age_80_84":     exp_bin_80_84y,
        "age_85":        exp_bin_85y_plus,
        "age_missing":   exp_bin_age_missing,
    }

    # =========================
    # Sex-related measures
    # =========================
    measures_sex = {
        "sex_male":        exp_bin_male,
        "sex_female":      exp_bin_female,
        "sex_missing":     exp_bin_sex_missing,
    }

    # =========================
    # Ethnicity-related measures
    # =========================
    measures_ethnicity = {
        "ethnicity_white":   exp_bin_eth_white,
        "ethnicity_mixed":   exp_bin_eth_mixed,
        "ethnicity_asian":   exp_bin_eth_asian,
        "ethnicity_black":   exp_bin_eth_black,
        "ethnicity_other":   exp_bin_eth_other,
        "ethnicity_missing": exp_bin_eth_missing,
    }

    # =========================
    # Rurality-related measures
    # =========================
    measures_rurality = {
        "rurality_urban_major":        exp_bin_urb_major,
        "rurality_urban_minor":        exp_bin_urb_minor,
        "rurality_urban_town":         exp_bin_urb_town,
        "rurality_rural_fringe":       exp_bin_rural_fringe,
        "rurality_rural_village":      exp_bin_rural_village,
        "rurality_missing":            exp_bin_rurality_missing,
    }

    # =========================
    # IMD-related measures
    # =========================
    measures_imd = {
        "imd_1_most":  exp_bin_imd_1_most,
        "imd_2":       exp_bin_imd_2,
        "imd_3":       exp_bin_imd_3,
        "imd_4":       exp_bin_imd_4,
        "imd_5_least": exp_bin_imd_5_least,
        "imd_missing": exp_bin_imd_missing,
    }

    # =========================
    # Care home-related measures
    # =========================
    measures_carehome = {
        "carehome": exp_bin_carehome,
    }

    # =========================
    # Smoking-related measures
    # =========================
    measures_smoking = {
        "smoking_current": exp_bin_smoker_current,
        "smoking_ever":    exp_bin_smoker_ever,
        "smoking_never":   exp_bin_smoker_never,
        "smoking_missing": exp_bin_smoker_missing,
    }

    # =========================
    # Obesity measures
    # =========================
    measures_obesity = {
        "obesity": exp_bin_obesity,
    }

    # =========================
    # Multimorbidity-related measures
    # =========================
    measures_multimorbidity = {
        "cms_af":                exp_bin_af,
        "cms_alcohol":           exp_bin_alcoholproblem,
        "cms_anxdep":            exp_bin_anxietydepression,
        "cms_asthma":            exp_bin_asthma,
        "cms_cancer":            exp_bin_cancer,
        "cms_chd":               exp_bin_chd,
        "cms_ckd":               exp_bin_ckd,
        "cms_constip":           exp_bin_constipation,
        "cms_copd":              exp_bin_copd,
        "cms_ctd":               exp_bin_ctd,
        "cms_dem":               exp_bin_dementia,
        "cms_diabetes":          exp_bin_diabetes,
        "cms_epilepsy":          exp_bin_epilepsy,
        "cms_hl":                exp_bin_hearingloss,
        "cms_hf":                exp_bin_hf,
        "cms_htn":               exp_bin_hypertension,
        "cms_ibs":               exp_bin_ibs,
        "cms_oa":                exp_bin_osteoarthritis,
        "cms_psych":             exp_bin_psychosis,
        "cms_stia":              exp_bin_stroketia,
    }

    # =========================
    # Consultation-related measures
    # =========================
    measures_consultation = {
        "consrate2019": exp_num_consrate2019,
        "consrate_m":   exp_num_consrate,
    }

    # =========================
    # Vaccination-related measures
    # =========================
    measures_covid = {
        "vax_covid_y": exp_bin_vax_covid,
    }
    measures_flu = {
        "vax_flu_y": exp_bin_vax_flu,
    }
    measures_pneumococcal = {
        "vax_pneum_y": exp_bin_vax_pneumo,    
    }
    # =========================
    # Emergency care (EC) measures
    # =========================
    measures_ec = {
        "ec": out_num_ec,
    }

    # =========================
    # Admitted patient care (APC) measures
    # =========================
    measures_apc = {
        "apc": out_num_apc,
        "apc_unpl": out_num_apc_unplanned,
        "apc_plan": out_num_apc_planned,
    }

    # =========================
    # ACSC-related measures - EC
    # =========================
    measures_ec_acsc = {
        "ec_acsc_copd":         out_num_copd_ec,
        "ec_acsc_asth":         out_num_asthma_ec,
        "ec_acsc_htn":          out_num_hypertension_ec,
        "ec_acsc_diab":         out_num_diabetes_ec,
        "ec_acsc_ang":          out_num_angina_ec,
        "ec_acsc_any":          out_num_acsc_ec,
    }

    # =========================
    # ACSC-related measures - APC
    # =========================
    measures_apc_acsc = {
        "apc_acsc_copd":          out_num_copd_apc,
        "apc_acsc_asth":        out_num_asthma_apc,
        "apc_acsc_htn":           out_num_hypertension_apc,
        "apc_acsc_diab":      out_num_diabetes_apc,
        "apc_acsc_ang":        out_num_angina_apc,
        "apc_acsc_any":           out_num_acsc_apc,
        "apc_unpl_acsc_any": out_num_acsc_apc_unplanned,
        "apc_plan_acsc_any":   out_num_acsc_apc_planned,
    }

    # ---------------------- Cross-Sectional Measures ----------------------
    if CS:
        measures.define_defaults(
            denominator= inex_bin_reg_cs & inex_bin_alive,
            group_by={
                "practice_id": practice_id
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

        if Carehome:
            for measure in measures_carehome.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_carehome[measure]
                )

        if Smoking:
            for measure in measures_smoking.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_smoking[measure]
                )

        if Obesity:
            for measure in measures_obesity.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_obesity[measure]
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
    if Long_all:
        measures.define_defaults(
            denominator= inex_bin_reg_cs & inex_bin_alive,
            group_by={
                "practice_id": practice_id
            },
            intervals = weeks(20).starting_on(start_cohort),
        )

        if Consultation:
            measures.define_measure(
                name = "cons_m",
                numerator = exp_num_consrate,
                intervals = months(12).starting_on(start_cohort - months(12))
            )

        if vax_flu:
            for measure in measures_flu.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_flu[measure],
                    denominator = inex_bin_reg_cs & inex_bin_alive & (inex_bin_elig_flu_65y | inex_bin_elig_flu_2_3y | inex_bin_elig_flu_pregnancy),
                    intervals = years(1).ending_on(start_cohort - days(1))
                )

        if vax_pneum:        
            for measure in measures_pneumococcal.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_pneumococcal[measure],
                    denominator = inex_bin_reg_cs & inex_bin_alive & inex_bin_elig_pneum_65y,
                    intervals = years(1).ending_on(start_cohort - days(1))
                )

        if vax_covid:
            for measure in measures_covid.keys():
                measures.define_measure(
                    name = measure,
                    numerator = measures_covid[measure],
                    denominator = inex_bin_reg_cs & inex_bin_alive & inex_bin_elig_covid_75y,
                    intervals = years(1).ending_on(start_cohort - days(1))
                )
 
        if ec_all:
            for measure in measures_ec.keys():
                measures.define_measure(
                    name = measure + "_main",
                    numerator = measures_ec[measure]
                )

        if apc_all:
            for measure in measures_apc.keys():
                measures.define_measure(
                    name = measure + "_main",
                    numerator = measures_apc[measure]
                )

        if ec_ACSCs:
            for measure in measures_ec_acsc.keys():
                measures.define_measure(
                    name = measure + "_main",
                    numerator = measures_ec_acsc[measure]
                )

        if apc_ACSCs:
            for measure in measures_apc_acsc.keys():
                measures.define_measure(
                    name = measure + "_main",
                    numerator = measures_apc_acsc[measure]
                )

    if Long_sub_asthma:
        measures.define_defaults(
            denominator= inex_bin_reg_cs & inex_bin_alive,
            group_by={
        "practice_id": practice_id,
        "sub_asth": sub_bin_asthma,
            },
            intervals = weeks(20).starting_on(start_cohort),
        )
        if ec_all:
            for measure in measures_ec.keys():
                measures.define_measure(
                    name = measure + "_sub_asth",
                    numerator = measures_ec[measure]
                )
        if apc_all:
            for measure in measures_apc.keys():
                measures.define_measure(
                    name = measure + "_sub_asth",
                    numerator = measures_apc[measure]
                )       
        if ec_ACSCs:
            for measure in measures_ec_acsc.keys():
                measures.define_measure(
                    name = measure + "_sub_asth",
                    numerator = measures_ec_acsc[measure]
                )
        if apc_ACSCs:
            for measure in measures_apc_acsc.keys():
                measures.define_measure(
                    name = measure + "_sub_asth",
                    numerator = measures_apc_acsc[measure]
                )
    if Long_sub_copd:
        measures.define_defaults(
            denominator= inex_bin_reg_cs & inex_bin_alive,
            group_by={
        "practice_id": practice_id,
        "sub_copd": sub_bin_copd,
            },
            intervals = weeks(20).starting_on(start_cohort),
        )
        if ec_all:
            for measure in measures_ec.keys():
                measures.define_measure(
                    name = measure + "_sub_copd",
                    numerator = measures_ec[measure]
                )
        if apc_all:
            for measure in measures_apc.keys():
                measures.define_measure(
                    name = measure + "_sub_copd",
                    numerator = measures_apc[measure]
                )       
        if ec_ACSCs:
            for measure in measures_ec_acsc.keys():
                measures.define_measure(
                    name = measure + "_sub_copd",
                    numerator = measures_ec_acsc[measure]
                )
        if apc_ACSCs:
            for measure in measures_apc_acsc.keys():
                measures.define_measure(
                    name = measure + "_sub_copd",
                    numerator = measures_apc_acsc[measure]
                )
    
    if Long_sub_hypertension:
        measures.define_defaults(
            denominator= inex_bin_reg_cs & inex_bin_alive,
            group_by={
        "practice_id": practice_id,
        "sub_htn": sub_bin_hypertension,
            },
            intervals = weeks(20).starting_on(start_cohort),
        )
        if ec_all:
            for measure in measures_ec.keys():
                measures.define_measure(
                    name = measure + "_sub_htn",
                    numerator = measures_ec[measure]
                )
        if apc_all:
            for measure in measures_apc.keys():
                measures.define_measure(
                    name = measure + "_sub_htn",
                    numerator = measures_apc[measure]
                )       
        if ec_ACSCs:
            for measure in measures_ec_acsc.keys():
                measures.define_measure(
                    name =  measure + "_sub_htn",
                    numerator = measures_ec_acsc[measure]
                )
        if apc_ACSCs:
            for measure in measures_apc_acsc.keys():
                measures.define_measure(
                    name = measure + "_sub_htn",
                    numerator = measures_apc_acsc[measure]
                )
    if Long_sub_diabetes:
        measures.define_defaults(
            denominator= inex_bin_reg_cs & inex_bin_alive,
            group_by={
        "practice_id": practice_id,
        "sub_diab": sub_bin_diabetes,
            },
            intervals = weeks(20).starting_on(start_cohort),
        )
        if ec_all:
            for measure in measures_ec.keys():
                measures.define_measure(
                    name =  measure + "_sub_diab",
                    numerator = measures_ec[measure]
                )
        if apc_all:
            for measure in measures_apc.keys():
                measures.define_measure(
                    name = measure + "_sub_diab",
                    numerator = measures_apc[measure]
                )       
        if ec_ACSCs:
            for measure in measures_ec_acsc.keys():
                measures.define_measure(
                    name = measure + "_sub_diab",
                    numerator = measures_ec_acsc[measure]
                )
        if apc_ACSCs:
            for measure in measures_apc_acsc.keys():
                measures.define_measure(
                    name = measure + "_sub_diab",
                    numerator = measures_apc_acsc[measure]
                )
    if Long_sub_sev_mental_ill:
        measures.define_defaults(
            denominator= inex_bin_reg_cs & inex_bin_alive,
            group_by={
        "practice_id": practice_id,
        "sub_sevmh": sub_bin_sev_mental_ill,
            },
            intervals = weeks(20).starting_on(start_cohort),
        )
        if ec_all:
            for measure in measures_ec.keys():
                measures.define_measure(
                    name = measure + "_sub_sevmh",
                    numerator = measures_ec[measure]
                )
        if apc_all:
            for measure in measures_apc.keys():
                measures.define_measure(
                    name = measure + "_sub_sevmh",
                    numerator = measures_apc[measure]
                )       
        if ec_ACSCs:
            for measure in measures_ec_acsc.keys():
                measures.define_measure(
                    name = measure + "_sub_sevmh",
                    numerator = measures_ec_acsc[measure]
                )
        if apc_ACSCs:
            for measure in measures_apc_acsc.keys():
                measures.define_measure(
                    name = measure + "_sub_sevmh",
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

    from variables_vax_covid import prelim_date_variables

    ## Add the imported variables to the dataset

    for var_name, var_value in prelim_date_variables.items():
        setattr(dataset, var_name, var_value)

    # Import jcvi variables ( JCVI group and derived variables; eligible date for vaccination based on JCVI group)
    from variables_vax_covid import jcvi_variables

    ## Add the imported variables to the dataset
    for var_name, var_value in jcvi_variables.items():
        setattr(dataset, var_name, var_value)

    # Import other dataset variables including region and multimorbidity conditions (20)

    from variables_cross_sectional import generate_dataset_variables
    variables_cs = generate_dataset_variables(start_cohort)

    for var_name, var_value in variables_cs.items():
        setattr(dataset, var_name, var_value)
