from module_table_imports import *

# Codelists from codelists.py (which pulls all variables from the codelist folder)
from codelists import *

# Call functions from variable_helper_functions
from variable_helper_functions import (
    ever_matching_event_clinical_ctv3_before,
    ever_matching_event_apc_between,
    ever_matching_event_ec_snomed_between,
    last_matching_event_clinical_ctv3_before,
    last_matching_event_clinical_snomed_before,
    last_matching_med_dmd_before,
    last_matching_event_apc_before,
    filter_codes_by_category,

)

# Define generate variables function
def generate_variables(interval_start, interval_end):  
    ## Inclusion/exclusion criteria-------------------------------------------------------------------------

    ### Registered throughout the study period and 90 days before for each cohort (for longitudinal measures, i.e. consultation rate/hospital admission)
    inex_bin_reg_long = (practice_registrations.spanning(
        interval_start - days(90), interval_end
    )).exists_for_patient()
    ### Registered 90 days before the start of winter/flu months for each cohort (for cross-sectional measures, i.e. patient  sociodemographic/case-mix )
    inex_bin_reg_cs = (practice_registrations.spanning(
    interval_start - days(90), interval_start
    )).exists_for_patient()

    ### Alive on index date
    inex_bin_alive = (((patients.date_of_death.is_null()) | (patients.date_of_death.is_after(interval_start))) & 
    ((ons_deaths.date.is_null()) | (ons_deaths.date.is_after(interval_start))))

    ### Age is known and valid
    tmp_exp_num_age = patients.age_on(interval_start)
    inex_bin_age = (tmp_exp_num_age >= 0) & (
    tmp_exp_num_age <= 110)

    ### Sex is known (female or male)
    inex_bin_sex = patients.sex.is_in(["female", "male"])

    ### Ethnicity is known
    inex_bin_ethinicity = clinical_events.where(clinical_events.ctv3_code.is_in(opensafely_ethnicity_codes_6)).exists_for_patient()

    ### Imd is known
    inex_bin_imd = addresses.for_patient_on(interval_start).imd_rounded.is_not_null()

    ### Region is known
    inex_bin_region = practice_registrations.for_patient_on(interval_start).practice_nuts1_region_name.is_not_null()

    ## Exposures--------------------------------------------------------------------------------------------

    ### Age
    exp_bin_under_5y = tmp_exp_num_age <5    
    exp_bin_5_16y = (tmp_exp_num_age >= 5) & (tmp_exp_num_age <= 16)      
    exp_bin_65_74y = (tmp_exp_num_age >= 65) & (tmp_exp_num_age <= 74)
    exp_bin_75_84y = (tmp_exp_num_age >= 75) & (tmp_exp_num_age <= 84)
    exp_bin_85y_plus = tmp_exp_num_age >= 85 

    ### Sex
    exp_bin_female = (patients.sex == "female")

    ### Ethnicity (need to decide category for calculating proportion)
    exp_cat_ethnicity = (
        clinical_events.where(
            clinical_events.ctv3_code.is_in(opensafely_ethnicity_codes_6)
        )
        .sort_by(clinical_events.date)
        .last_for_patient()
        .ctv3_code.to_category(opensafely_ethnicity_codes_6)
    )

    ### Region
    exp_cat_region = practice_registrations.for_patient_on(interval_start).practice_nuts1_region_name

    ### Practice data taken at the start of the interval
    practice_id = (practice_registrations.for_patient_on(interval_start).practice_pseudo_id)

    ### Patient urban-rural classification
    tmp_cat_rur_urb = (addresses.for_patient_on(interval_start).rural_urban_classification)

    ### Deprivation
    tmp_cat_imd = case(
        when((addresses.for_patient_on(interval_start).imd_rounded >= 0) & 
                (addresses.for_patient_on(interval_start).imd_rounded < int(32844 * 1 / 5))).then("1 (most deprived)"),
        when(addresses.for_patient_on(interval_start).imd_rounded < int(32844 * 2 / 5)).then("2"),
        when(addresses.for_patient_on(interval_start).imd_rounded < int(32844 * 3 / 5)).then("3"),
        when(addresses.for_patient_on(interval_start).imd_rounded < int(32844 * 4 / 5)).then("4"),
        when(addresses.for_patient_on(interval_start).imd_rounded < int(32844 * 5 / 5)).then("5 (least deprived)"),
        otherwise="unknown",
    )
    exp_bin_mostdeprived1 = (tmp_cat_imd == "1 (most deprived)")
    exp_bin_mostdeprived2 = (tmp_cat_imd == "2")
    exp_bin_mostdeprived3 = (tmp_cat_imd == "3")
    exp_bin_mostdeprived4 = (tmp_cat_imd == "4")
    exp_bin_leastdeprived5 = (tmp_cat_imd == "5 (least deprived)") 

    ### Smoking status
    tmp_most_recent_smoking_cat = (
        last_matching_event_clinical_ctv3_before(smoking_clear, interval_start)
        .ctv3_code.to_category(smoking_clear)
    )
    tmp_ever_smoked = ever_matching_event_clinical_ctv3_before(
        (filter_codes_by_category(smoking_clear, include=["S", "E"])), interval_start)

    tmp_cat_smoking = case(
        when(tmp_most_recent_smoking_cat == "S").then("S"),
        when((tmp_most_recent_smoking_cat == "E") | ((tmp_most_recent_smoking_cat == "N") & (tmp_ever_smoked == True))).then("E"),
        when((tmp_most_recent_smoking_cat == "N") & (tmp_ever_smoked == False)).then("N"),
        otherwise="M"
    )
    exp_bin_smoker = (tmp_cat_smoking == "S")

    ### Consultation rate in 2019 (using seen maybe)
    tmp_exp_num_consrate2019 = appointments.where(
        appointments.status.is_in([
            "Arrived",
            "In Progress",
            "Finished",
            "Visit",
            "Waiting",
            "Patient Walked Out",
            ]) & appointments.start_date.is_on_or_between("2019-01-01", "2019-12-31")
            ).count_for_patient()    

    exp_num_consrate2019 = case(
        when(tmp_exp_num_consrate2019 <= 365).then(tmp_exp_num_consrate2019),
        otherwise=365,
    )
    
    ### Consultation rate during follow-up of exposure
    tmp_exp_num_consrate = appointments.where(
        appointments.status.is_in([
            "Arrived",
            "In Progress",
            "Finished",
            "Visit",
            "Waiting",
            "Patient Walked Out",
            ]) & appointments.start_date.is_on_or_between(interval_start, interval_end)
            ).count_for_patient()    

    exp_num_consrate = case(
        when(tmp_exp_num_consrate <= 365).then(tmp_exp_num_consrate),
        otherwise=365,
    )

    ### Multimorbidity conditions (n=20)

    exp_bin_hypertension = (
        (last_matching_event_clinical_snomed_before(
            hypertension_snomed, interval_start
        ).exists_for_patient()) |
        (last_matching_med_dmd_before(
            hypertension_drugs_dmd, interval_start
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            hypertension_icd10, interval_start
        ).exists_for_patient())
    )

    exp_bin_anxdep = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_AnxietyDepression_snomed"], interval_start
        ).exists_for_patient())
    )

    exp_bin_asthma = (
        (last_matching_event_clinical_ctv3_before(
            multimorbidity_dict["MS_Asthma_ctv3"], interval_start
        ).exists_for_patient())
    )    

    ## Outcomes---------------------------------------------------------------------------------
    ### A&E attendance 
    out_num_ec = (
        ec.where(
            ec.arrival_date.is_on_or_between(interval_start, interval_end)
        ).count_for_patient()
    )
    ### Hospital admission
    out_num_apc = (
        apcs.where(
            apcs.admission_date.is_on_or_between(interval_start, interval_end)
        ).count_for_patient()
    )

    ### ACSC 
    out_num_asthma_ec = ever_matching_event_ec_snomed_between(
        asthma_snomed, interval_start, interval_end
    ).count_for_patient()

    out_num_asthma_apc = ever_matching_event_apc_between(
        asthma_icd10, interval_start, interval_end
    ).count_for_patient()

    dynamic_variables = dict(
        inex_bin_reg_long = inex_bin_reg_long,
        inex_bin_reg_cs = inex_bin_reg_cs,
        inex_bin_alive = inex_bin_alive,
        inex_bin_age = inex_bin_age,
        inex_bin_sex = inex_bin_sex,
        inex_bin_ethinicity = inex_bin_ethinicity,
        inex_bin_imd = inex_bin_imd,
        inex_bin_region = inex_bin_region,
        practice_id = practice_id,
        exp_bin_female = exp_bin_female,
        exp_bin_under_5y = exp_bin_under_5y,
        exp_bin_5_16y = exp_bin_5_16y,
        exp_bin_65_74y = exp_bin_65_74y,
        exp_bin_75_84y = exp_bin_75_84y,
        exp_bin_85y_plus = exp_bin_85y_plus,
        exp_num_consrate = exp_num_consrate,
        out_num_apc =out_num_apc,
        out_num_asthma_apc = out_num_asthma_apc,
    )
    return dynamic_variables

