from module_table_imports import *

# Codelists from codelists.py (which pulls all variables from the codelist folder)
from codelists import *

# Call functions from variable_helper_functions
from variable_helper_functions import (
    ever_matching_event_clinical_ctv3_before,
    last_matching_event_clinical_ctv3_before,
    last_matching_event_clinical_snomed_before,
    filter_codes_by_category,
)

# Define generate variables function
def generate_variables(cohort_start):  
    ### Registered on the start date of winter/flu months for each cohort (for cross-sectional measures, i.e. patient  sociodemographic/case-mix )
    inex_bin_reg_cs = practice_registrations.exists_for_patient_on(cohort_start)

    ### Alive on index date
    inex_bin_alive = (((patients.date_of_death.is_null()) | (patients.date_of_death.is_after(cohort_start))) & 
    ((ons_deaths.date.is_null()) | (ons_deaths.date.is_after(cohort_start))))

    ### Age is known and valid
    tmp_exp_num_age = patients.age_on(cohort_start)
    inex_bin_age = (tmp_exp_num_age >= 0) & (
    tmp_exp_num_age <= 110)

    ### Sex is known (female or male)
    inex_bin_sex = patients.sex.is_in(["female", "male"])

    ### Ethnicity is known
    inex_bin_ethinicity = clinical_events.where(clinical_events.ctv3_code.is_in(opensafely_ethnicity_codes_6)).exists_for_patient()

    ### Imd is known
    inex_bin_imd = addresses.for_patient_on(cohort_start).imd_rounded.is_not_null()

    ### Region is known
    inex_bin_region = practice_registrations.for_patient_on(cohort_start).practice_nuts1_region_name.is_not_null()

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
    tmp_exp_cat_ethnicity = (
        clinical_events.where(
            clinical_events.ctv3_code.is_in(opensafely_ethnicity_codes_6)
        )
        .sort_by(clinical_events.date)
        .last_for_patient()
        .ctv3_code.to_category(opensafely_ethnicity_codes_6)
    )
    exp_bin_eth_missing = (tmp_exp_cat_ethnicity == "0")
    exp_bin_eth_white = (tmp_exp_cat_ethnicity == "1")
    exp_bin_eth_mixed = (tmp_exp_cat_ethnicity == "2")
    exp_bin_eth_southasian = (tmp_exp_cat_ethnicity == "3")
    exp_bin_eth_black = (tmp_exp_cat_ethnicity == "4")
    exp_bin_eth_other = (tmp_exp_cat_ethnicity == "5")
    ### Region
    exp_cat_region = practice_registrations.for_patient_on(cohort_start).practice_nuts1_region_name

    ### Practice data taken at the start of the interval
    practice_id = (practice_registrations.for_patient_on(cohort_start).practice_pseudo_id)

    ### Patient urban-rural classification
    tmp_exp_cat_rur_urb = (addresses.for_patient_on(cohort_start).rural_urban_classification)
    urb_major        = (tmp_exp_cat_rur_urb == 1)  # Urban major conurbation
    urb_minor        = (tmp_exp_cat_rur_urb == 2)  # Urban minor conurbation
    urb_town         = (tmp_exp_cat_rur_urb == 3)  # Urban city and town
    urb_town_sp      = (tmp_exp_cat_rur_urb == 4)  # Urban city and town in sparse setting
    rural_fringe     = (tmp_exp_cat_rur_urb == 5)  # Rural town and fringe
    rural_fringe_sp  = (tmp_exp_cat_rur_urb == 6)  # Rural town and fringe in sparse setting
    rural_village    = (tmp_exp_cat_rur_urb == 7)  # Rural village and dispersed
    rural_village_sp = (tmp_exp_cat_rur_urb == 8)  # Rural village and dispersed in sparse setting
    ### Deprivation
    tmp_exp_cat_imd = case(
        when((addresses.for_patient_on(cohort_start).imd_rounded >= 0) & 
                (addresses.for_patient_on(cohort_start).imd_rounded < int(32844 * 1 / 5))).then("1 (most deprived)"),
        when(addresses.for_patient_on(cohort_start).imd_rounded < int(32844 * 2 / 5)).then("2"),
        when(addresses.for_patient_on(cohort_start).imd_rounded < int(32844 * 3 / 5)).then("3"),
        when(addresses.for_patient_on(cohort_start).imd_rounded < int(32844 * 4 / 5)).then("4"),
        when(addresses.for_patient_on(cohort_start).imd_rounded < int(32844 * 5 / 5)).then("5 (least deprived)"),
        otherwise="unknown",
    )
    exp_bin_imd_1_most   = (tmp_exp_cat_imd == "1 (most deprived)")
    exp_bin_imd_2        = (tmp_exp_cat_imd == "2")
    exp_bin_imd_3        = (tmp_exp_cat_imd == "3")
    exp_bin_imd_4        = (tmp_exp_cat_imd == "4")
    exp_bin_imd_5_least  = (tmp_exp_cat_imd == "5 (least deprived)")

    ### Smoking status
    tmp_most_recent_smoking_cat = (
        last_matching_event_clinical_ctv3_before(smoking_clear, cohort_start)
        .ctv3_code.to_category(smoking_clear)
    )
    tmp_ever_smoked = ever_matching_event_clinical_ctv3_before(
        (filter_codes_by_category(smoking_clear, include=["S", "E"])), cohort_start)

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
    
    ## Multimorbidity conditions (n=20)----------------------------------------------------------------------

    ### Atrial Fibrillation
    exp_bin_af = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_AF_snomed"], cohort_start
        ).exists_for_patient())
    )

    ### Alcohol Problems
    exp_bin_alcoholproblem = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_AlcoholProblem_snomed"], cohort_start
        ).exists_for_patient())
    )

    ### Anxiety / Depression
    exp_bin_anxietydepression = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_AnxietyDepression_snomed"], cohort_start
        ).exists_for_patient())
    )

    ### Asthma
    exp_bin_asthma = (
        (last_matching_event_clinical_ctv3_before(
            multimorbidity_dict["MS_Asthma_ctv3"], cohort_start
        ).exists_for_patient())
    )

    ### Cancer
    exp_bin_cancer = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_Cancer_snomed"], cohort_start
        ).exists_for_patient())
    )

    ### Coronary Heart Disease
    exp_bin_chd = (
        (last_matching_event_clinical_ctv3_before(
            multimorbidity_dict["MS_CHD_ctv3"], cohort_start
        ).exists_for_patient())
    )

    ### Chronic Kidney Disease
    exp_bin_ckd = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_CKD_snomed"], cohort_start
        ).exists_for_patient())
    )

    ### Constipation
    exp_bin_constipation = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_Constipation_snomed"], cohort_start
        ).exists_for_patient())
    )

    ### Chronic Obstructive Pulmonary Disease (COPD)
    exp_bin_copd = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_COPD_snomed"], cohort_start
        ).exists_for_patient())
    )

    ### Connective Tissue Disorder
    exp_bin_ctd = (
        (last_matching_event_clinical_ctv3_before(
            multimorbidity_dict["MS_CTD_ctv3"], cohort_start
        ).exists_for_patient())
    )

    ### Dementia
    exp_bin_dementia = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_Dementia_snomed"], cohort_start
        ).exists_for_patient())
    )

    ### Diabetes Mellitus
    exp_bin_diabetes = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_Diabetes_snomed"], cohort_start
        ).exists_for_patient())
    )

    ### Epilepsy
    exp_bin_epilepsy = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_Epilepsy_snomed"], cohort_start
        ).exists_for_patient())
    )

    ### Hearing Loss
    exp_bin_hearingloss = (
        (last_matching_event_clinical_ctv3_before(
            multimorbidity_dict["MS_HL_ctv3"], cohort_start
        ).exists_for_patient())
    )

    ### Heart Failure
    exp_bin_hf = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_HF_snomed"], cohort_start
        ).exists_for_patient())
    )

    ### Hypertension
    exp_bin_hypertension = (
        (last_matching_event_clinical_ctv3_before(
            multimorbidity_dict["MS_Hypertension_ctv3"], cohort_start
        ).exists_for_patient())
    )

    ### Irritable Bowel Syndrome (IBS)
    exp_bin_ibs = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_IBS_snomed"], cohort_start
        ).exists_for_patient())
    )

    ### Psychosis / Bipolar Disorder
    exp_bin_psychosis = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_Psychosis_snomed"], cohort_start
        ).exists_for_patient())
    )

    ### Stroke / Transient Ischemic Attack (TIA)
    exp_bin_stroketia = (
        (last_matching_event_clinical_ctv3_before(
            multimorbidity_dict["MS_StrokeTIA_ctv3"], cohort_start
        ).exists_for_patient())
    )

    ### Osteoarthritis (Painful Condition)
    exp_bin_osteoarthritis = (
        (last_matching_event_clinical_ctv3_before(
            MS_Osteoarthritis_ctv3, cohort_start
        ).exists_for_patient())
    )

    dynamic_variables = dict(
        # Inclusion/exclusion binary flags
        inex_bin_reg_cs = inex_bin_reg_cs,
        inex_bin_alive = inex_bin_alive,
        inex_bin_age = inex_bin_age,
        inex_bin_sex = inex_bin_sex,
        inex_bin_ethinicity = inex_bin_ethinicity,
        inex_bin_imd = inex_bin_imd,
        inex_bin_region = inex_bin_region,
        # Practice ID
        practice_id = practice_id,
        # Sex binary flags
        exp_bin_female = exp_bin_female,
        # Age binary flags
        exp_bin_under_5y = exp_bin_under_5y,
        exp_bin_5_16y = exp_bin_5_16y,
        exp_bin_65_74y = exp_bin_65_74y,
        exp_bin_75_84y = exp_bin_75_84y,
        exp_bin_85y_plus = exp_bin_85y_plus,
        # Ethnicity binary flags
        exp_bin_eth_missing = exp_bin_eth_missing,
        exp_bin_eth_white = exp_bin_eth_white,
        exp_bin_eth_mixed = exp_bin_eth_mixed,
        exp_bin_eth_southasian = exp_bin_eth_southasian,
        exp_bin_eth_black = exp_bin_eth_black,
        exp_bin_eth_other = exp_bin_eth_other,
        exp_cat_region =exp_cat_region,
        # Rurality binary flags 
        urb_major = urb_major,
        urb_minor = urb_minor,
        urb_town = urb_town,
        urb_town_sp = urb_town_sp,
        rural_fringe = rural_fringe,
        rural_fringe_sp = rural_fringe_sp,
        rural_village = rural_village,
        rural_village_sp = rural_village_sp,
        # IMD binary flags   
        exp_bin_imd_1_most = exp_bin_imd_1_most,
        exp_bin_imd_2 = exp_bin_imd_2,
        exp_bin_imd_3 = exp_bin_imd_3,
        exp_bin_imd_4 = exp_bin_imd_4,
        exp_bin_imd_5_least = exp_bin_imd_5_least,
        exp_bin_smoker = exp_bin_smoker,
        exp_num_consrate2019 = exp_num_consrate2019,
        # Multimorbidity conditions
        exp_bin_af = exp_bin_af,                         # Atrial Fibrillation
        exp_bin_alcoholproblem = exp_bin_alcoholproblem, # Alcohol Problems
        exp_bin_anxietydepression = exp_bin_anxietydepression, # Anxiety / Depression
        exp_bin_asthma = exp_bin_asthma,                 # Asthma
        exp_bin_cancer = exp_bin_cancer,                 # Cancer
        exp_bin_chd = exp_bin_chd,                       # Coronary Heart Disease
        exp_bin_ckd = exp_bin_ckd,                       # Chronic Kidney Disease
        exp_bin_constipation = exp_bin_constipation,     # Constipation
        exp_bin_copd = exp_bin_copd,                     # Chronic Obstructive Pulmonary Disease (COPD)
        exp_bin_ctd = exp_bin_ctd,                       # Connective Tissue Disorder
        exp_bin_dementia = exp_bin_dementia,             # Dementia
        exp_bin_diabetes = exp_bin_diabetes,             # Diabetes Mellitus
        exp_bin_epilepsy = exp_bin_epilepsy,             # Epilepsy
        exp_bin_hearingloss = exp_bin_hearingloss,       # Hearing Loss
        exp_bin_hf = exp_bin_hf,                         # Heart Failure
        exp_bin_hypertension = exp_bin_hypertension,     # Hypertension
        exp_bin_ibs = exp_bin_ibs,                       # Irritable Bowel Syndrome (IBS)
        exp_bin_psychosis = exp_bin_psychosis,           # Psychosis / Bipolar Disorder
        exp_bin_stroketia = exp_bin_stroketia,           # Stroke / Transient Ischemic Attack (TIA)
        exp_bin_osteoarthritis = exp_bin_osteoarthritis  # Osteoarthritis (Painful Condition)
    )
    return dynamic_variables

