#import the function codelist_from_csv from ehrql library
from ehrql import codelist_from_csv
#import codelists

#################################################
############Cohorts##############################
#################################################
# Asthma (medication using bnf, so not imported)
ast_codelist = codelist_from_csv(
    "codelists/opensafely-asthma-diagnosis-snomed.csv",
    column="id"
)
## !!this is bnf codes
astmed_codelist = codelist_from_csv(
    "codelists/bristol-asthma-medications-bnf.csv",
    column="code"
)

salbutamol_codes = codelist_from_csv(
    "codelists/opensafely-asthma-inhaler-salbutamol-medication.csv",
    column="id"
)

ics_codes = codelist_from_csv(
    "codelists/opensafely-asthma-inhaler-steroid-medication.csv",
    column="id"
)

pred_codes = codelist_from_csv(
    "codelists/opensafely-asthma-oral-prednisolone-medication.csv",
    column="snomed_id"
)
# Diabetes
# Type 1 diabetes
diabetes_type1_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-type-1-diabetes.csv",
    column="code"
)

# Type 2 diabetes
diabetes_type2_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-type-2-diabetes.csv",
    column="code"
)

# Non-diagnostic diabetes codes
diabetes_diagnostic_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-nondiagnostic-diabetes-codes.csv",
    column="code"
)

# Other or non-specific diabetes
diabetes_other_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-other-or-nonspecific-diabetes.csv",
    column="code"
)

# Gestational diabetes
diabetes_gestational_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-gestational-diabetes.csv",
    column="code"
)

# Permanent diabetes
diabetes_permanent_snomed_clinical = codelist_from_csv(
    "codelists/opensafely-diabetes.csv",
    column="CTV3ID"
)

# Insulin medication 
insulin_snomed_clinical = codelist_from_csv(
     "codelists/opensafely-insulin-medication.csv",
     column="id"
)

# Antidiabetic drugs
antidiabetic_drugs_snomed_clinical = codelist_from_csv(
     "codelists/opensafely-antidiabetic-drugs.csv",
     column="id"
)

# Antidiabetic drugs - non metformin !system snomed not dmd?
non_metformin_dmd = codelist_from_csv(
    "codelists/user-r_denholm-non-metformin-antidiabetic-drugs_bristol.csv", 
    column="id"
)

# Type 1 diabetes secondary care
diabetes_type1_icd10 = codelist_from_csv(
    "codelists/opensafely-type-1-diabetes-secondary-care.csv",
    column="icd10_code"
)

# Type 2 diabetes secondary care
diabetes_type2_icd10 = codelist_from_csv(
    "codelists/user-r_denholm-type-2-diabetes-secondary-care-bristol.csv",
    column="code"
)

# Hypertension 
hypertension_snomed_clinical = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-hyp_cod.csv",
    column="code"
)

hypertension_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-hypertension_icd10.csv",
    column="code"
)

hypertension_codes = codelist_from_csv(
    "codelists/opensafely-hypertension.csv",
    column="CTV3ID"
)

hypertension_drugs_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-hypertension_drugs_dmd.csv",
    column="dmd_id"
)

# COPD
copd_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-copd_snomed.csv",
    column="code"
)

copd_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-copd_icd10.csv",
    column="code"
)

copd_drugs_bnf = codelist_from_csv(
     "codelists/bristol-copd-medications-bnf.csv",
     column="code"
)

# Severe Mental Illness codes
# Serious mental illness
serious_mental_illness_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-severe-mental-illness.csv",
    column="code"
)

# Bipolar and other mood disorders ICD10
bipolar_other_mood_icd10 = codelist_from_csv(
    "codelists/user-kurttaylor-bipolar_and_mood_disorders_icd10.csv",
    column="code"
)
# Other Psychotic disorders ICD10
psychotic_disorders_other_icd10 = codelist_from_csv(
    "codelists/user-kurttaylor-other-psychotic_disorders_icd10.csv",
    column="code"
)
# Schizophrenia ICD10
schizophrenia_icd10 = codelist_from_csv(
    "codelists/user-kurttaylor-schizophrenia_icd10.csv",
    column="code"
)
# antipsychotics drugs
antipsychotics_dmd = codelist_from_csv(
    "codelists/opensafely-second-generation-antipsychotics-excluding-long-acting-injections.csv",
    column="dmd_id"
)
# prochlorperazine drugs
prochlorperazine_dmd = codelist_from_csv(
    "codelists/opensafely-prochlorperazine-dmd.csv",
    column="dmd_id"
)

# Self harm_snomed - aged >= 10 years
self_harm_10plus_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-intentional-self-harm-aged10-years.csv",
    column="code"
)

# Self harm_snomed - aged >= 15 years
self_harm_15plus_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-undetermined-intent-self-harm-aged15-years.csv",
    column="code"
)

# Self harm_snomed undetermined intent - combined
self_harm_15_10_combined_snomed = self_harm_10plus_snomed_clinical + self_harm_15plus_snomed_clinical

# Self harm_icd10 - aged >= 10 years
self_harm_10plus_icd10 = codelist_from_csv(
    "codelists/user-kurttaylor-self_harm_intentional_10_years_icd10.csv",
    column="code"
)

# Self harm_icd10 - aged >= 15 years
self_harm_15plus_icd10 = codelist_from_csv(
    "codelists/user-kurttaylor-self_harm_undetermined_intent_15_years_icd10.csv",
    column="code"
)

# Self harm_icd10 undetermined intent - combined
self_harm_15_10_combined_icd10 = self_harm_10plus_icd10 + self_harm_15plus_icd10
#################################################
############Outcomes##############################
#################################################


# Asthma Exacerbation
ast_exacerbation_icd10 = codelist_from_csv(
    "codelists/opensafely-asthma-exacerbation-secondary-care.csv",
    column="code"
)

# Pneumonia due to [asthma or COPD]
pneu_icd10 = codelist_from_csv(
    "codelists/opensafely-pneumonia-secondary-care.csv", 
    column="ICD code"
)

# Coronary Artery Disease due to [diabetes or hypertension]
cad_icd10 = codelist_from_csv(
    "codelists/bristol-coronary-artery-disease.csv", 
    column="code"
)

# Peripheral Artery Disease due to [diabetes or hypertension]
pad_icd10 = codelist_from_csv(
    "codelists/bristol-peripheral-artery-diseases.csv", 
    column="code"
)

# Acute Ischaemic Stroke due to [diabetes or hypertension]
stroke_icd10 = codelist_from_csv(
    "codelists/opensafely-stroke-secondary-care.csv", 
    column="icd"
)
stroke_isch_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-stroke_isch_icd10.csv",
    column="code",
)
# Chronic Kidney Disease due to [diabetes]
ckd_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-ckd_icd10.csv",
    column="code"
)

# Cardiovascular including MI and Heart failure
cardiovascular_icd10 = codelist_from_csv(
    "codelists/opensafely-cardiovascular-secondary-care.csv", 
    column="icd"
)

# Suicide
suicide_icd10 = codelist_from_csv(
    "codelists/user-hjforbes-suicide-icd-10.csv",
    column="code"
)

# Negative Control Outcome-Fractures
fracture_icd10 = codelist_from_csv(
    "codelists/bristol-fractures.csv",
    column="code"
)
# Negative Control Outcome-Concussion
concussion_icd10 = codelist_from_csv(
    "codelists/bristol-concussion.csv",
    column="code"
)


#################################################
############Confounders##########################
#################################################
#Ethnicity 
ethinicity_codelist = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    column="Code",
    category_column="Grouping_6"
)

# Smoking
smoking_clear = codelist_from_csv(
    "codelists/opensafely-smoking-clear.csv",
    column="CTV3Code",
    category_column="Category"
)
smoking_unclear = codelist_from_csv(
    "codelists/opensafely-smoking-unclear.csv",
    column="CTV3Code",
    category_column="Category"
)

# BMI
bmi_obesity_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-bmi_obesity_snomed.csv",
    column="code"
)
bmi_obesity_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-bmi_obesity_icd10.csv",
    column="code"
)
# Carer codes
carer_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-carer.csv",
    column="code"
)

# No longer a carer codes
notcarer_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-notcarer.csv",
    column="code"
)

# Wider Learning Disability
learndis_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-learndis.csv",
    column="code"
)

# Employed by Care Home codes
carehome_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-carehome.csv",
    column="code"
)

# Employed by nursing home codes
nursehome_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-nursehome.csv",
    column="code"
)

# Employed by domiciliary care provider codes
domcare_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-domcare.csv",
    column="code"
)

# Patients in long-stay nursing and residential care
longres_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-longres.csv",
    column="code"
)

# Dementia
dementia_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_snomed.csv",
    column="code"
)
dementia_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_icd10.csv",
    column="code"
)
dementia_codes = codelist_from_csv(
    "codelists/opensafely-dementia.csv",
    column="CTV3ID"
)
# Dementia vascular 
dementia_vascular_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_vascular_snomed.csv",
    column="code"
)

dementia_vascular_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_vascular_icd10.csv",
    column="code"
)

# Liver disease
liver_disease_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-liver_disease_snomed.csv",
    column="code"
)
liver_disease_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-liver_disease_icd10.csv",
    column="code"
)
# Chronic Liver disease codes
liver_disease_chronic_icd10 = codelist_from_csv(
    "codelists/opensafely-chronic-liver-disease.csv",
    column="CTV3ID"
)

# Cancer
cancer_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-cancer_snomed.csv",
    column="code"
)
cancer_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-cancer_icd10.csv",
    column="code"
)

# AMI
ami_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-ami_snomed.csv",
    column="code"
)
ami_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-ami_icd10.csv",
    column="code"
)
ami_prior_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-ami_prior_icd10.csv",
    column="code"
)

# Stroke 
stroke_isch_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-stroke_isch_snomed.csv",
    column="code"
)

stroke_codes = codelist_from_csv(
    "codelists/opensafely-stroke-updated.csv",
    column="CTV3ID"
)

# Other Arterial Embolism 
arterial_embolism_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-other_arterial_embolism_icd10.csv",
    column="code"
)
arterial_embolism_snomed = codelist_from_csv(
    "codelists/user-tomsrenin-other_art_embol.csv",
    column="code"
)

# Pulmonary Embolism
pulmonary_embolism_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-pe_snomed.csv",
    column="code"
)
pulmonary_embolism_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-pe_icd10.csv",
    column="code"
)

# Venous Thromboembolism Events
dvt_main_snomed = codelist_from_csv(
    "codelists/user-tomsrenin-dvt_main.csv",
    column="code"
)

dvt_preg_snomed = codelist_from_csv(
    "codelists/user-tomsrenin-dvt-preg.csv",
    column="code"
)

dvt_main_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-dvt_dvt_icd10.csv",
    column="code"
)

dvt_preg_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dvt_pregnancy_icd10.csv",
    column="code"
)

# Chronic Kidney Disease
ckd_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-ckd_snomed.csv",
    column="code"
)

# Depression 
depression_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-depression-symptoms-and-diagnoses.csv",
    column="code"
)

depression_icd10 = codelist_from_csv(
    "codelists/user-kurttaylor-depression_icd10.csv",
    column="code"
)

# COCP
cocp_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-cocp_dmd.csv",
    column="dmd_id"
)

# Heart medication 
hrt_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-hrt_dmd.csv",
    column="dmd_id"
)

