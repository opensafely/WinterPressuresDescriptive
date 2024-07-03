from ehrql import (codelist_from_csv, create_dataset, days, minimum_of, case, when,)
# Bring table definitions from the TPP backend 
from ehrql.tables.tpp import (patients, practice_registrations, addresses, apcs, ec, opa, opa_diag, clinical_events, medications, ons_deaths,)
# Codelists from codelists.py (which pulls all variables from the codelist folder)
from codelists import *
# Define study start and end dates
study_start_date = "2017-09-30"
study_end_date = "2023-11-28"
# Define a dataset 
dataset = create_dataset()
dataset.configure_dummy_data(population_size=100)
# Define population
## Known age/sex between 0 and 110 inclusive on the study start date 
is_female_or_male = patients.sex.is_in(["female", "male"])
all_ages = (patients.age_on(study_start_date) >= 0) & (patients.age_on(study_start_date) <= 110)
## Alive on the study start date
was_alive = (((patients.date_of_death.is_null()) | (patients.date_of_death.is_after(study_start_date))) & 
((ons_deaths.date.is_null()) | (ons_deaths.date.is_after(study_start_date))))
## Registered for a minimum of 365 days prior to the study start date for individuals aged 1 year old or older
## Registered since birth for those aged less than 1 year old
was_registered = (((practice_registrations.for_patient_on(study_start_date - days(365))).exists_for_patient()) | 
((patients.age_on(study_start_date) < 1) & ((practice_registrations.for_patient_on(study_start_date)).exists_for_patient())))

dataset.define_population(
    is_female_or_male
    & all_ages
    & was_alive
    & was_registered
)

# Outcomes for general population- total number of apc, opc, ec, and the date of death
## time period depending on the time period of winter pressure
dataset.out_ct_apc=apcs.where(
    apcs.admission_date.is_on_or_between(
        study_start_date, study_end_date
    )
).count_for_patient()
    
dataset.out_ct_opc=opa.where(
    opa.appointment_date.is_on_or_between(
        study_start_date, study_end_date
    )
).count_for_patient()

dataset.out_ct_ec=ec.where(
    ec.arrival_date.is_on_or_between(
        study_start_date, study_end_date
    )
).count_for_patient()

dataset.out_date_death_tpp=patients.date_of_death
dataset.out_date_death_ons=ons_deaths.date
dataset.out_date_death_min=minimum_of(patients.date_of_death, ons_deaths.date)

# Cohorts indicator (return a Boolean variable to indicate T-had disease before start date; otherwise F)
## Asthma (?bnf codes not in medications table, dm+d map does not work)

dataset.had_asthma = (
    (clinical_events.where(
        (clinical_events.snomedct_code.is_in(ast_codelist)) &
        (clinical_events.date.is_before(study_start_date))
    ).exists_for_patient()) |
    (medications.where(
        (medications.dmd_code.is_in(salbutamol_codes + ics_codes + pred_codes)) &
        (medications.date.is_before(study_start_date))
    ).exists_for_patient())
)

## Diabetes
dataset.had_diabetes = (
    (clinical_events.where(
        (clinical_events.ctv3_code.is_in(diabetes_type1_snomed_clinical + 
        diabetes_type2_snomed_clinical + 
        diabetes_diagnostic_snomed_clinical + 
        diabetes_other_snomed_clinical + 
        diabetes_gestational_snomed_clinical + 
        diabetes_permanent_snomed_clinical)) &
        (clinical_events.date.is_before(study_start_date))
    ).exists_for_patient()) |
    (medications.where(
        ((medications.dmd_code.is_in(insulin_snomed_clinical)) | 
        (medications.dmd_code.is_in(antidiabetic_drugs_snomed_clinical)) |
        (medications.dmd_code.is_in(non_metformin_dmd))) &
        (medications.date.is_before(study_start_date))
    ).exists_for_patient()) |
    (apcs.where(
        ((apcs.primary_diagnosis.is_in(diabetes_type1_icd10)) | 
        (apcs.primary_diagnosis.is_in(diabetes_type2_icd10)) |
        (apcs.secondary_diagnosis.is_in(diabetes_type1_icd10)) |
        (apcs.secondary_diagnosis.is_in(diabetes_type2_icd10))) &
        (apcs.admission_date.is_before(study_start_date))
    ).exists_for_patient()) |
        (opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(diabetes_type1_icd10)) | 
        (opa_diag.primary_diagnosis_code.is_in(diabetes_type2_icd10)) |
        (opa_diag.secondary_diagnosis_code_1.is_in(diabetes_type1_icd10)) |
        (opa_diag.secondary_diagnosis_code_1.is_in(diabetes_type2_icd10))) &
        (opa_diag.appointment_date.is_before(study_start_date))
    ).exists_for_patient())
)

## Hypertension
dataset.had_hypertension = (
    (clinical_events.where(
        ((clinical_events.snomedct_code.is_in(hypertension_snomed_clinical)) |
        (clinical_events.ctv3_code.is_in(hypertension_codes))) &
        (clinical_events.date.is_before(study_start_date))
    ).exists_for_patient()) |
    (medications.where(
        ((medications.dmd_code.is_in(hypertension_drugs_dmd))) &
        (medications.date.is_before(study_start_date))
    ).exists_for_patient()) |
    (apcs.where(
        ((apcs.primary_diagnosis.is_in(hypertension_icd10)) |
        (apcs.secondary_diagnosis.is_in(hypertension_icd10))) &
        (apcs.admission_date.is_before(study_start_date))
    ).exists_for_patient()) |
            (opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(hypertension_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(hypertension_icd10))) &
        (opa_diag.appointment_date.is_before(study_start_date))
    ).exists_for_patient())
)
## COPD (?bnf codes not in medications table, dm+d map does not work)
dataset.had_copd = (
    (clinical_events.where(
        ((clinical_events.snomedct_code.is_in(copd_snomed_clinical))) &
        (clinical_events.date.is_before(study_start_date))
    ).exists_for_patient()) |
    (apcs.where(
        ((apcs.primary_diagnosis.is_in(copd_icd10)) |
        (apcs.secondary_diagnosis.is_in(copd_icd10))) &
        (apcs.admission_date.is_before(study_start_date))
    ).exists_for_patient()) |
            (opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(copd_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(copd_icd10))) &
        (opa_diag.appointment_date.is_before(study_start_date))
    ).exists_for_patient())
)
## Severe mental illness
dataset.had_severemh = (
    (clinical_events.where(
        ((clinical_events.snomedct_code.is_in(serious_mental_illness_snomed_clinical))) &
        (clinical_events.date.is_before(study_start_date))
    ).exists_for_patient()) |
    (medications.where(
        ((medications.dmd_code.is_in(antipsychotics_dmd + prochlorperazine_dmd))) &
        (medications.date.is_before(study_start_date))
    ).exists_for_patient()) |
    (apcs.where(
        ((apcs.primary_diagnosis.is_in(bipolar_other_mood_icd10 + psychotic_disorders_other_icd10 + schizophrenia_icd10)) |
        (apcs.secondary_diagnosis.is_in(bipolar_other_mood_icd10 + psychotic_disorders_other_icd10 + schizophrenia_icd10))) &
        (apcs.admission_date.is_before(study_start_date))
    ).exists_for_patient()) |
            (opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(bipolar_other_mood_icd10 + psychotic_disorders_other_icd10 + schizophrenia_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(bipolar_other_mood_icd10 + psychotic_disorders_other_icd10 + schizophrenia_icd10))) &
        (opa_diag.appointment_date.is_before(study_start_date))
    ).exists_for_patient())
)

## Self-harm
dataset.had_selfharm = (
    (clinical_events.where(
        ((clinical_events.snomedct_code.is_in(self_harm_15_10_combined_snomed))) &
        (clinical_events.date.is_before(study_start_date))
    ).exists_for_patient()) |
    (apcs.where(
        ((apcs.primary_diagnosis.is_in(self_harm_15_10_combined_icd10)) |
        (apcs.secondary_diagnosis.is_in(self_harm_15_10_combined_icd10))) &
        (apcs.admission_date.is_before(study_start_date))
    ).exists_for_patient()) |
            (opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(self_harm_15_10_combined_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(self_harm_15_10_combined_icd10))) &
        (opa_diag.appointment_date.is_before(study_start_date))
    ).exists_for_patient())
)



# Covariates
## Age
dataset.cov_date_of_birth=patients.date_of_birth
dataset.cov_num_age=patients.age_on(study_start_date)
## Sex
dataset.cov_cat_sex=patients.sex
## Ethnicity 
dataset.cov_cat_ethnicity = (
    clinical_events.where(
        clinical_events.ctv3_code.is_in(ethnicity_codelist)
    )
    .sort_by(clinical_events.date)
    .last_for_patient()
    .ctv3_code.to_category(ethnicity_codelist)
)
## IMD
imd_rounded = addresses.for_patient_on(study_start_date).imd_rounded
dataset.cov_cat_imd = case(
    when((imd_rounded >=0) & (imd_rounded < int(32844 * 1 / 5))).then("1 (most deprived)"),
    when(imd_rounded < int(32844 * 2 / 5)).then("2"),
    when(imd_rounded < int(32844 * 3 / 5)).then("3"),
    when(imd_rounded < int(32844 * 4 / 5)).then("4"),
     when(imd_rounded < int(32844 * 5 / 5)).then("5 (least deprived)"),
    otherwise="unknown",
)

# All Health Outcomes for Different Cohorts - total number of events in apc and opc

## Asthma Exacerbation
dataset.out_ast_exacerbation_ct_apc=apcs.where(
        ((apcs.primary_diagnosis.is_in(ast_exacerbation_icd10)) |
        (apcs.secondary_diagnosis.is_in(ast_exacerbation_icd10))) &
        (apcs.admission_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()
    
dataset.out_ast_exacerbation_ct_opc=opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(ast_exacerbation_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(ast_exacerbation_icd10))) &
        (opa_diag.appointment_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()

## Pneumonia due to [asthma or COPD]
dataset.out_pneumonia_ct_apc=apcs.where(
        ((apcs.primary_diagnosis.is_in(pneu_icd10)) |
        (apcs.secondary_diagnosis.is_in(pneu_icd10))) &
        (apcs.admission_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()
    
dataset.out_pneumonia_ct_opc=opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(pneu_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(pneu_icd10))) &
        (opa_diag.appointment_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()


## Coronary Artery Disease due to [diabetes or hypertension]
dataset.out_cad_ct_apc=apcs.where(
        ((apcs.primary_diagnosis.is_in(cad_icd10)) |
        (apcs.secondary_diagnosis.is_in(cad_icd10))) &
        (apcs.admission_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()
    
dataset.out_cad_ct_opc=opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(cad_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(cad_icd10))) &
        (opa_diag.appointment_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()

# Peripheral Artery Disease due to [diabetes or hypertension]
dataset.out_pad_ct_apc=apcs.where(
        ((apcs.primary_diagnosis.is_in(pad_icd10)) |
        (apcs.secondary_diagnosis.is_in(pad_icd10))) &
        (apcs.admission_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()
    
dataset.out_pad_ct_opc=opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(pad_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(pad_icd10))) &
        (opa_diag.appointment_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()


## Acute Ischaemic Stroke due to [diabetes or hypertension]
dataset.out_stroke_ct_apc=apcs.where(
        ((apcs.primary_diagnosis.is_in(stroke_icd10)) |
        (apcs.secondary_diagnosis.is_in(stroke_icd10))) &
        (apcs.admission_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()
    
dataset.out_stroke_ct_opc=opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(stroke_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(stroke_icd10))) &
        (opa_diag.appointment_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()

dataset.out_ischstroke_ct_apc=apcs.where(
        ((apcs.primary_diagnosis.is_in(stroke_isch_icd10)) |
        (apcs.secondary_diagnosis.is_in(stroke_isch_icd10))) &
        (apcs.admission_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()
    
dataset.out_ischstroke_ct_opc=opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(stroke_isch_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(stroke_isch_icd10))) &
        (opa_diag.appointment_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()

## Chronic Kidney Disease due to [diabetes]
dataset.out_ckd_ct_apc=apcs.where(
        ((apcs.primary_diagnosis.is_in(ckd_icd10)) |
        (apcs.secondary_diagnosis.is_in(ckd_icd10))) &
        (apcs.admission_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()
    
dataset.out_ckd_ct_opc=opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(ckd_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(ckd_icd10))) &
        (opa_diag.appointment_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()

## Cardiovascular including MI and Heart failure
dataset.out_mi_and_heartfailure_ct_apc=apcs.where(
        ((apcs.primary_diagnosis.is_in(cardiovascular_icd10)) |
        (apcs.secondary_diagnosis.is_in(cardiovascular_icd10))) &
        (apcs.admission_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()
    
dataset.out_mi_and_heartfailure_ct_opc=opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(cardiovascular_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(cardiovascular_icd10))) &
        (opa_diag.appointment_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()

## Severe mental health
dataset.out_severemh_ct_apc=apcs.where(
        ((apcs.primary_diagnosis.is_in(bipolar_other_mood_icd10 + psychotic_disorders_other_icd10 + schizophrenia_icd10)) |
        (apcs.secondary_diagnosis.is_in(bipolar_other_mood_icd10 + psychotic_disorders_other_icd10 + schizophrenia_icd10))) &
        (apcs.admission_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()
    
dataset.out_severemh_ct_opc=opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(bipolar_other_mood_icd10 + psychotic_disorders_other_icd10 + schizophrenia_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(bipolar_other_mood_icd10 + psychotic_disorders_other_icd10 + schizophrenia_icd10))) &
        (opa_diag.appointment_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()

## Suicide
dataset.out_suicide_ct_apc=apcs.where(
        ((apcs.primary_diagnosis.is_in(suicide_icd10)) |
        (apcs.secondary_diagnosis.is_in(suicide_icd10))) &
        (apcs.admission_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()
    
dataset.out_suicide_ct_opc=opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(suicide_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(suicide_icd10))) &
        (opa_diag.appointment_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()


## Negative Control Outcome-Fractures
dataset.out_fractures_ct_apc=apcs.where(
        ((apcs.primary_diagnosis.is_in(fracture_icd10)) |
        (apcs.secondary_diagnosis.is_in(fracture_icd10))) &
        (apcs.admission_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()
    
dataset.out_fractures_ct_opc=opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(fracture_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(fracture_icd10))) &
        (opa_diag.appointment_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()


## Negative Control Outcome-Concussion
dataset.out_concussion_ct_apc=apcs.where(
        ((apcs.primary_diagnosis.is_in(concussion_icd10)) |
        (apcs.secondary_diagnosis.is_in(concussion_icd10))) &
        (apcs.admission_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()
    
dataset.out_concussion_ct_opc=opa_diag.where(
        ((opa_diag.primary_diagnosis_code.is_in(concussion_icd10)) | 
        (opa_diag.secondary_diagnosis_code_1.is_in(concussion_icd10))) &
        (opa_diag.appointment_date.is_on_or_between(study_start_date, study_end_date))
    ).count_for_patient()