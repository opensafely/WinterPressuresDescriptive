from module_table_imports import *
from codelists import *

# Call functions from variable_helper_functions
from variable_helper_functions import (
    ever_matching_event_clinical_ctv3_before,
    last_matching_event_clinical_ctv3_before,
    last_matching_event_clinical_snomed_before,
    last_matching_event_clinical_snomed_on_or_before,
    last_matching_event_apc_before,
    filter_codes_by_category,
    last_matching_med_dmd_before,
)

# Define generate variables function
def generate_dataset_variables(cohort_start):
    ## Inclusion/exclusion criteria-------------------------------------------------------------------------
    ### Registered on the start date of winter/flu months for each cohort (for cross-sectional measures, i.e. patient  sociodemographic/case-mix )
    inex_bin_reg_cs = practice_registrations.exists_for_patient_on(cohort_start)

    ### Alive on index date
    inex_bin_alive = (((patients.date_of_death.is_null()) | (patients.date_of_death.is_after(cohort_start))) & 
    ((ons_deaths.date.is_null()) | (ons_deaths.date.is_after(cohort_start))))

    ### Age is known and valid
    exp_num_age = patients.age_on(cohort_start)
    inex_bin_age = (exp_num_age >= 0) & (
    exp_num_age <= 110)

    ### Sex is known (female or male)
    inex_bin_sex = patients.sex.is_in(["female", "male"])

    ### Ethnicity is known
    inex_bin_ethinicity = (
        clinical_events.where(clinical_events.snomedct_code.is_in(ethnicity_snomed))
        .where(clinical_events.date.is_on_or_before(cohort_start))
        .exists_for_patient()
    )

    ### Imd is known
    inex_bin_imd = addresses.for_patient_on(cohort_start).imd_rounded.is_not_null()

    ### Region is known
    inex_bin_region = practice_registrations.for_patient_on(cohort_start).practice_nuts1_region_name.is_not_null()

    ### Had asthma ever recorded prior to the cohort start date
    inex_bin_asthma = (
        (last_matching_event_clinical_snomed_before( 
            asthma_snomed, cohort_start
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            asthma_icd10, cohort_start
        ).exists_for_patient()) |
        (last_matching_med_dmd_before(
            asthma_drugs_dmd, cohort_start
        ).exists_for_patient())
    )

    ### Had COPD ever recorded prior to the cohort start date
    inex_bin_copd = (
        (last_matching_event_clinical_snomed_before(
            copd_snomed, cohort_start
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            copd_icd10, cohort_start
        ).exists_for_patient()) |
        (last_matching_med_dmd_before(
            copd_drugs_dmd, cohort_start
        ).exists_for_patient())
    )

    ### Had hypertension ever recorded prior to the cohort start date
    inex_bin_hypertension = (
        (last_matching_event_clinical_ctv3_before(
            hypertension_ctv3, cohort_start
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            hypertension_icd10, cohort_start
        ).exists_for_patient()) |
        (last_matching_med_dmd_before(
            hypertension_drugs_dmd, cohort_start
        ).exists_for_patient())
    )

    ### Had diabetes ever recorded prior to the cohort start date
    inex_bin_diabetes = (
        (last_matching_event_clinical_snomed_before(
            diabetes_snomed, cohort_start
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            diabetes_icd10, cohort_start
        ).exists_for_patient()) |
        (last_matching_med_dmd_before(
            diabetes_drugs_dmd, cohort_start
        ).exists_for_patient())
    )

    ### Had severe mental illness ever recorded prior to the cohort start date
    inex_bin_sev_mental_ill = (
        (last_matching_event_clinical_snomed_before(
            severe_mental_illness_snomed + self_harm_aged10_snomed + self_harm_aged15_snomed, cohort_start
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            bipolar_and_mood_disorders_icd10 + other_psychotic_disorders_icd10 + schizophrenia_icd10 + self_harm_aged10_icd10 + self_harm_aged15_icd10, cohort_start
        ).exists_for_patient()) |
        (last_matching_med_dmd_before(
            second_generation_antipsychotics_drugs_dmd + prochlorperazine_drugs_dmd, cohort_start
        ).exists_for_patient())
    )

    ### Region
    exp_cat_region = practice_registrations.for_patient_on(cohort_start).practice_nuts1_region_name

    ### Practice data taken at the start of the interval
    practice_id = (practice_registrations.for_patient_on(cohort_start).practice_pseudo_id)
    
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

    cs_dataset_variables = dict(
        # Practice ID
        practice_id          = practice_id,
        # Practice region
        exp_cat_region       = exp_cat_region,
        # Patient age
        exp_num_age          = exp_num_age,
        # Inclusion/exclusion criteria
        inex_bin_reg_cs     = inex_bin_reg_cs,
        inex_bin_alive      = inex_bin_alive,
        inex_bin_age        = inex_bin_age,
        inex_bin_sex        = inex_bin_sex,
        inex_bin_ethinicity = inex_bin_ethinicity,
        inex_bin_imd        = inex_bin_imd,
        inex_bin_region     = inex_bin_region,
        # Inclusion for subgroups
        inex_bin_asthma     = inex_bin_asthma,
        inex_bin_copd       = inex_bin_copd,
        inex_bin_hypertension = inex_bin_hypertension,
        inex_bin_diabetes   = inex_bin_diabetes,
        inex_bin_sev_mental_ill = inex_bin_sev_mental_ill,
        # Cambridge Multimorbidity Conditions (20)
        exp_bin_af                = exp_bin_af,                # Atrial fibrillation
        exp_bin_alcoholproblem    = exp_bin_alcoholproblem,    # Alcohol problems
        exp_bin_anxietydepression = exp_bin_anxietydepression, # Anxiety/depression
        exp_bin_asthma            = exp_bin_asthma,            # Asthma
        exp_bin_cancer            = exp_bin_cancer,            # Cancer
        exp_bin_chd               = exp_bin_chd,               # Coronary heart disease
        exp_bin_ckd               = exp_bin_ckd,               # Chronic kidney disease
        exp_bin_constipation      = exp_bin_constipation,      # Constipation
        exp_bin_copd              = exp_bin_copd,              # Chronic obstructive pulmonary disease
        exp_bin_ctd               = exp_bin_ctd,               # Connective tissue disorder
        exp_bin_dementia          = exp_bin_dementia,          # Dementia
        exp_bin_diabetes          = exp_bin_diabetes,          # Diabetes mellitus
        exp_bin_epilepsy          = exp_bin_epilepsy,          # Epilepsy
        exp_bin_hearingloss       = exp_bin_hearingloss,       # Hearing loss
        exp_bin_hf                = exp_bin_hf,                # Heart failure
        exp_bin_hypertension      = exp_bin_hypertension,      # Hypertension
        exp_bin_ibs               = exp_bin_ibs,               # Irritable bowel syndrome
        exp_bin_osteoarthritis    = exp_bin_osteoarthritis,    # Osteoarthritis (painful condition)
        exp_bin_psychosis         = exp_bin_psychosis,         # Psychosis/bipolar disorder
        exp_bin_stroketia         = exp_bin_stroketia,         # Stroke/transient ischaemic attack    
    )
    return cs_dataset_variables

# Define generate variables function
def generate_measure_variables(cohort_start):

    # start with dataset variables
    cs_dataset_variables = generate_dataset_variables(cohort_start)

    # Reuse variables from dataset variables
    inex_bin_sex        = cs_dataset_variables["inex_bin_sex"]
    exp_num_age         = cs_dataset_variables["exp_num_age"]

    ### inclusion and exclusion criteria for flu and pneumococcal vax (https://www.gov.uk/government/publications/flu-vaccination-programme-information-for-healthcare-practitioners/flu-vaccination-programme-2023-to-2024-information-for-healthcare-practitioners)
    tmp_num_flu_age_child = patients.age_on(cohort_start - days(31))   #all children aged 2 or 3 years on 31 August {cohort year-1}
    tmp_num_flu_age_elder = patients.age_on(cohort_start + months(6))  #those aged 65 years and over (including those who are 64 but will be 65 on or before 31 March {cohort year})
    tmp_num_covid_age_elder = patients.age_on(cohort_start + months(9))#those aged 75 years and over (including those who are 74 but will be 75 on or before 30 June {cohort year})
    
    inex_bin_elig_pneum_65y = exp_num_age >= 65
    inex_bin_elig_flu_65y = tmp_num_flu_age_elder >= 65
    inex_bin_elig_flu_2_3y = (tmp_num_flu_age_child <4) & (tmp_num_flu_age_child >= 2) 
    ### Pregnancy
    inex_bin_elig_flu_pregnancy = last_matching_event_clinical_snomed_on_or_before(
        pregnancy_snomed, cohort_start
    ).exists_for_patient()
    inex_bin_elig_covid_75y = tmp_num_covid_age_elder >= 75
    
    ## Exposures--------------------------------------------------------------------------------------------

    ### Age
    exp_bin_age_missing = (exp_num_age.is_null()) | (exp_num_age < 0)
    exp_bin_under_5y  = (exp_num_age < 5)
    exp_bin_5_11y     = (exp_num_age >= 5)  & (exp_num_age < 12)
    exp_bin_12_17y    = (exp_num_age >= 12) & (exp_num_age < 18)
    exp_bin_18_29y    = (exp_num_age >= 18) & (exp_num_age < 30)
    exp_bin_30_44y    = (exp_num_age >= 30) & (exp_num_age < 45)
    exp_bin_45_54y    = (exp_num_age >= 45) & (exp_num_age < 55)
    exp_bin_55_64y    = (exp_num_age >= 55) & (exp_num_age < 65)
    exp_bin_65_74y    = (exp_num_age >= 65) & (exp_num_age < 75)
    exp_bin_75_79y    = (exp_num_age >= 75) & (exp_num_age < 80)
    exp_bin_80_84y    = (exp_num_age >= 80) & (exp_num_age < 85)
    exp_bin_85y_plus  = (exp_num_age >= 85)

    ### Sex
    exp_bin_male = (patients.sex == "male")
    exp_bin_female = (patients.sex == "female")
    exp_bin_sex_missing = (inex_bin_sex == False)

    ### Ethnicity (need to decide category for calculating proportion)
    tmp_exp_cat_eth_code = (
        last_matching_event_clinical_snomed_on_or_before(ethnicity_snomed, cohort_start)
        .snomedct_code.to_category(ethnicity_snomed, default="Missing")
    )
    tmp_exp_cat_eth_sus = case(
        when(ethnicity_from_sus.code.is_in(["A", "B", "C"])).then("1"),
        when(ethnicity_from_sus.code.is_in(["D", "E", "F", "G"])).then("2"),
        when(ethnicity_from_sus.code.is_in(["H", "J", "K", "L"])).then("3"),
        when(ethnicity_from_sus.code.is_in(["M", "N", "P"])).then("4"),
        when(ethnicity_from_sus.code.is_in(["R", "S"])).then("5"),
        otherwise="Missing",
    )

    tmp_exp_cat_ethnicity = case(
        when(tmp_exp_cat_eth_code != "Missing").then(tmp_exp_cat_eth_code),
        when((tmp_exp_cat_eth_code == "Missing") & (tmp_exp_cat_eth_sus != "Missing")).then(tmp_exp_cat_eth_sus),
        otherwise="Missing",
    )

    exp_bin_eth_missing = (tmp_exp_cat_ethnicity == "Missing")
    exp_bin_eth_white = (tmp_exp_cat_ethnicity == "1")
    exp_bin_eth_mixed = (tmp_exp_cat_ethnicity == "2")
    exp_bin_eth_asian = (tmp_exp_cat_ethnicity == "3")
    exp_bin_eth_black = (tmp_exp_cat_ethnicity == "4")
    exp_bin_eth_other = (tmp_exp_cat_ethnicity == "5")

    ### Patient urban-rural classification
    tmp_exp_cat_rur_urb = (addresses.for_patient_on(cohort_start).rural_urban_classification)
    exp_bin_rurality_missing = tmp_exp_cat_rur_urb.is_null()
    exp_bin_urb_major        = (tmp_exp_cat_rur_urb == 1)  # Urban major conurbation
    exp_bin_urb_minor        = (tmp_exp_cat_rur_urb == 2)  # Urban minor conurbation
    exp_bin_urb_town         = (tmp_exp_cat_rur_urb == 3) | (tmp_exp_cat_rur_urb == 4)  # Urban city and town + Urban city and town in sparse setting
    exp_bin_rural_fringe     = (tmp_exp_cat_rur_urb == 5) | (tmp_exp_cat_rur_urb == 6)  # Rural town and fringe + Rural town and fringe in sparse setting
    exp_bin_rural_village    = (tmp_exp_cat_rur_urb == 7) | (tmp_exp_cat_rur_urb == 8)  # Rural village and dispersed + Rural village and dispersed in sparse setting
    
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
    exp_bin_imd_missing  = (tmp_exp_cat_imd == "unknown")
    exp_bin_imd_1_most   = (tmp_exp_cat_imd == "1 (most deprived)")
    exp_bin_imd_2        = (tmp_exp_cat_imd == "2")
    exp_bin_imd_3        = (tmp_exp_cat_imd == "3")
    exp_bin_imd_4        = (tmp_exp_cat_imd == "4")
    exp_bin_imd_5_least  = (tmp_exp_cat_imd == "5 (least deprived)")

    ### Care home status
    exp_bin_carehome = (
        addresses.for_patient_on(cohort_start).care_home_is_potential_match |
        addresses.for_patient_on(cohort_start).care_home_requires_nursing |
        addresses.for_patient_on(cohort_start).care_home_does_not_require_nursing
    )

    ### Smoking status
    tmp_most_recent_smoking_cat = (
        last_matching_event_clinical_ctv3_before(smoking_clear, cohort_start)
        .ctv3_code.to_category(smoking_clear)
    )
    tmp_ever_smoked = ever_matching_event_clinical_ctv3_before(
        (filter_codes_by_category(smoking_clear, include=["S", "E"])), cohort_start
        ).exists_for_patient()

    tmp_cat_smoking = case(
        when(tmp_most_recent_smoking_cat == "S").then("S"),
        when((tmp_most_recent_smoking_cat == "E") | ((tmp_most_recent_smoking_cat == "N") & (tmp_ever_smoked == True))).then("E"),
        when((tmp_most_recent_smoking_cat == "N") & (tmp_ever_smoked == False)).then("N"),
        otherwise="M"
    )
    exp_bin_smoker_current = (tmp_cat_smoking == "S")
    exp_bin_smoker_ever = (tmp_cat_smoking == "E")
    exp_bin_smoker_never = (tmp_cat_smoking == "N")
    exp_bin_smoker_missing = (tmp_cat_smoking == "M")

    ### Obesity
    exp_bin_obesity = (
        last_matching_event_clinical_snomed_before(
            bmi_obesity_snomed, cohort_start
        ).exists_for_patient()
        |
        last_matching_event_apc_before(
            bmi_obesity_icd10, cohort_start
        ).exists_for_patient()
    )

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

    cs_measure_variables = dict(

        **cs_dataset_variables,   # include dataset vars
        # Inclusion/exclusion binary flags (VACCINE ELIGIBILITY)
        inex_bin_elig_pneum_65y     = inex_bin_elig_pneum_65y,           #Pneumococcal vaccine
        inex_bin_elig_flu_65y       = inex_bin_elig_flu_65y,             #Flu vaccine
        inex_bin_elig_flu_2_3y      = inex_bin_elig_flu_2_3y,
        inex_bin_elig_flu_pregnancy = inex_bin_elig_flu_pregnancy,
        inex_bin_elig_covid_75y     = inex_bin_elig_covid_75y,           #COVID SPRING vaccine 
        # Sex binary flags
        exp_bin_male        = exp_bin_male,
        exp_bin_female      = exp_bin_female,
        exp_bin_sex_missing = exp_bin_sex_missing,
        # Age binary flags
        exp_bin_under_5y    = exp_bin_under_5y,
        exp_bin_5_11y       = exp_bin_5_11y,
        exp_bin_12_17y      = exp_bin_12_17y,
        exp_bin_18_29y      = exp_bin_18_29y,
        exp_bin_30_44y      = exp_bin_30_44y,
        exp_bin_45_54y      = exp_bin_45_54y,
        exp_bin_55_64y      = exp_bin_55_64y,
        exp_bin_65_74y      = exp_bin_65_74y,
        exp_bin_75_79y      = exp_bin_75_79y,
        exp_bin_80_84y      = exp_bin_80_84y,
        exp_bin_85y_plus    = exp_bin_85y_plus,
        exp_bin_age_missing = exp_bin_age_missing,
        # Ethnicity binary flags
        exp_bin_eth_white   = exp_bin_eth_white,
        exp_bin_eth_mixed   = exp_bin_eth_mixed,
        exp_bin_eth_asian   = exp_bin_eth_asian,
        exp_bin_eth_black   = exp_bin_eth_black,
        exp_bin_eth_other   = exp_bin_eth_other,
        exp_bin_eth_missing = exp_bin_eth_missing,
        # Rurality binary flags
        exp_bin_urb_major        = exp_bin_urb_major,
        exp_bin_urb_minor        = exp_bin_urb_minor,
        exp_bin_urb_town         = exp_bin_urb_town,
        exp_bin_rural_fringe     = exp_bin_rural_fringe,
        exp_bin_rural_village    = exp_bin_rural_village,
        exp_bin_rurality_missing = exp_bin_rurality_missing,
        # IMD binary flags
        exp_bin_imd_1_most  = exp_bin_imd_1_most,
        exp_bin_imd_2       = exp_bin_imd_2,
        exp_bin_imd_3       = exp_bin_imd_3,
        exp_bin_imd_4       = exp_bin_imd_4,
        exp_bin_imd_5_least = exp_bin_imd_5_least,
        exp_bin_imd_missing = exp_bin_imd_missing,
        # Care home status
        exp_bin_carehome = exp_bin_carehome,
        # Smoking status binary flags
        exp_bin_smoker_current = exp_bin_smoker_current,
        exp_bin_smoker_ever    = exp_bin_smoker_ever,
        exp_bin_smoker_never   = exp_bin_smoker_never,
        exp_bin_smoker_missing = exp_bin_smoker_missing,
        # Obesity
        exp_bin_obesity = exp_bin_obesity,
        # Consultation rate in 2019
        exp_num_consrate2019 = exp_num_consrate2019,   
    )
    return cs_measure_variables