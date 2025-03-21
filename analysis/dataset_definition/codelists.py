# Setup
from ehrql import codelist_from_csv

def create_codelist_dict(dic: dict) -> dict:
    '''
    Create a dictionary of codelists, so that queries can be run iteratively on
    groups of codelists that are subject to the same ehrQL query.
    Args:
        dic: dictionary where key = name, value = codelist csv path
    Returns:
        Dictionary where key = name, value = codelist
    '''
    for name in dic:
        dic[name] = codelist_from_csv(dic[name], 
                                                column = "code")
    return dic

# Exposure(s)

# Ethnicity
opensafely_ethnicity_codes_6 = codelist_from_csv(
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

# BMI
bmi_obesity_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-bmi_obesity_snomed.csv",
    column="code"
)

bmi_obesity_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-bmi_obesity_icd10.csv",
    column="code"
)

bmi_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-bmi.csv",
    column="code"
)

# For JCVI groups

jcvi_dict = {
    ## High-Risk Conditions
    "learndis_primis": "codelists/primis-covid19-vacc-uptake-learndis.csv",  ## Learning Disability
    "longres_primis": "codelists/primis-covid19-vacc-uptake-longres.csv",  ## Long-Stay Nursing/Residential Care
    "shield_primis": "codelists/primis-covid19-vacc-uptake-shield.csv",  ## High Risk from COVID-19
    "nonshield_primis": "codelists/primis-covid19-vacc-uptake-nonshield.csv",  ## Lower Risk from COVID-19

    ## Pregnancy-Related Conditions
    "preg_primis": "codelists/primis-covid19-vacc-uptake-preg.csv",  ## Pregnancy Codes
    "pregdel_primis": "codelists/primis-covid19-vacc-uptake-pregdel.csv",  ## Pregnancy/Delivery Codes

    ## BMI & Obesity
    "bmi_stage_primis": "codelists/primis-covid19-vacc-uptake-bmi_stage.csv",  ## BMI Codes
    "sev_obesity_primis": "codelists/primis-covid19-vacc-uptake-sev_obesity.csv",  ## Severe Obesity
    "bmi_primis": "codelists/primis-covid19-vacc-uptake-bmi.csv",

    ## Respiratory Conditions
    "ast_primis": "codelists/primis-covid19-vacc-uptake-ast.csv",  ## Asthma Diagnosis
    "astadm_primis": "codelists/primis-covid19-vacc-uptake-astadm.csv",  ## Asthma Admission
    "astrx_primis": "codelists/primis-covid19-vacc-uptake-astrx.csv",  ## Asthma Steroid Prescription
    "resp_primis": "codelists/primis-covid19-vacc-uptake-resp_cov.csv",  ## Chronic Respiratory Disease

    ## Neurological Conditions
    "cns_primis": "codelists/primis-covid19-vacc-uptake-cns_cov.csv",  ## Neurological Disease & Learning Disorder

    ## Immunosuppression
    "immdx_primis": "codelists/primis-covid19-vacc-uptake-immdx_cov.csv",  ## Immunosuppression Diagnosis
    "immrx_primis": "codelists/primis-covid19-vacc-uptake-immrx.csv",  ## Immunosuppression Medication

    ## Organ & Metabolic Conditions
    "spln_primis": "codelists/primis-covid19-vacc-uptake-spln_cov.csv",  ## Spleen Dysfunction
    "diab_primis": "codelists/primis-covid19-vacc-uptake-diab.csv",  ## Diabetes Diagnosis
    "dmres_primis": "codelists/primis-covid19-vacc-uptake-dmres.csv",  ## Diabetes Resolved
    "cld_primis": "codelists/primis-covid19-vacc-uptake-cld.csv",  ## Chronic Liver Disease

    ## Mental Health
    "sev_mental_primis": "codelists/primis-covid19-vacc-uptake-sev_mental.csv",  ## Severe Mental Illness
    "smhres_primis": "codelists/primis-covid19-vacc-uptake-smhres.csv",  ## Remission of Severe Mental Illness

    ## Cardiovascular Conditions
    "chd_primis": "codelists/primis-covid19-vacc-uptake-chd_cov.csv",  ## Chronic Heart Disease
    "ckd_primis": "codelists/primis-covid19-vacc-uptake-ckd_cov.csv",  ## Chronic Kidney Disease
    "ckd15_primis": "codelists/primis-covid19-vacc-uptake-ckd15.csv",  ## CKD (All Stages)
    "ckd35_primis": "codelists/primis-covid19-vacc-uptake-ckd35.csv",  ## CKD (Stages 3-5)
}
jcvi_dict = create_codelist_dict(jcvi_dict)

# For multimorbidity groups

multimorbidity_dict = {
    ## Hypertension
    "MS_Hypertension_ctv3": "codelists/user-ZoeMZou-multimorbidity_hypertension.csv",  

    ## Anxiety/Depression
    "MS_AnxietyDepression_ctv3": "codelists/user-ZoeMZou-multimorbidity_anxietydepression.csv",  
    "MS_AnxietyDepression_snomed": "codelists/bristol-multimorbidity_anxietydepression.csv", 
    ## Hearing Loss
    "MS_HL_ctv3": "codelists/user-ZoeMZou-multimorbidity_hearing-loss.csv",  

    ## Irritable Bowel Syndrome
    "MS_IBS_ctv3": "codelists/user-ZoeMZou-multimorbidity_irritable-bowel-syndrome.csv",  
    "MS_IBS_snomed": "codelists/bristol-multimorbidity_irritable-bowel-syndrome.csv", 

    ## Asthma
    "MS_Asthma_ctv3": "codelists/user-ZoeMZou-multimorbidity_asthma.csv",  

    ## Diabetes Mellitus
    "MS_Diabetes_ctv3": "codelists/user-ZoeMZou-multimorbidity_diabetes.csv",  
    "MS_Diabetes_snomed": "codelists/bristol-multimorbidity_diabetes.csv",  

    ## Coronary Heart Disease
    "MS_CHD_ctv3": "codelists/user-ZoeMZou-multimorbidity_coronary-heart-disease.csv",  

    ## Chronic Kidney Disease
    "MS_CKD_ctv3": "codelists/user-ZoeMZou-multimorbidity_chronic-kidney-disease.csv",  
    "MS_CKD_snomed": "codelists/bristol-multimorbidity_chronic-kidney-disease.csv", 

    ## Atrial Fibrillation
    "MS_AF_ctv3": "codelists/user-ZoeMZou-multimorbidity_atrial-fibrillation.csv", 
    "MS_AF_snomed": "codelists/bristol-multimorbidity_atrial-fibrillation.csv",  

    ## Constipation-not found in CMS
    "MS_Constipation_snomed": "codelists/nhsd-primary-care-domain-refsets-chronconstip_cod.csv",  

    ## Stroke/Transient Ischemic Attack (TIA)
    "MS_StrokeTIA_ctv3": "codelists/user-ZoeMZou-multimorbidity_stroketransient-ischemic-attack.csv",  

    ## COPD
    "MS_COPD_ctv3": "codelists/user-ZoeMZou-multimorbidity_copd.csv",
    "MS_COPD_snomed": "codelists/bristol-multimorbidity_copd.csv",   

    ## Connective Tissue Disorder
    "MS_CTD_ctv3": "codelists/user-ZoeMZou-multimorbidity_connective-tissue-disorder.csv",  

    ## Cancer
    "MS_Cancer_ctv3": "codelists/user-ZoeMZou-multimorbidity_cancer.csv",  
    "MS_Cancer_snomed": "codelists/bristol-multimorbidity_cancer.csv", 

    ## Alcohol Problems
    "MS_AlcoholProblem_ctv3": "codelists/user-ZoeMZou-alcohol_problems.csv",
    "MS_AlcoholProblem_snomed": "codelists/bristol-multimorbidity_alcoholproblems.csv",

    ## Heart Failure
    "MS_HF_ctv3": "codelists/user-ZoeMZou-multimorbidity_heart-failure.csv", 
    "MS_HF_snomed": "codelists/bristol-multimorbidity_heart-failure.csv", 

    ## Dementia
    "MS_Dementia_ctv3": "codelists/user-ZoeMZou-multimorbidity_dementia.csv",  
    "MS_Dementia_snomed": "codelists/bristol-multimorbidity_dementia.csv",  

    ## Psychosis/Bipolar Disorder
    "MS_Psychosis_ctv3": "codelists/user-ZoeMZou-multimorbidity_psychosisbipolar-disorder.csv",  
    "MS_Psychosis_snomed": "codelists/bristol-multimorbidity_psychosisbipolar-disorder.csv",

    ## Epilepsy
    "MS_Epilepsy_ctv3": "codelists/user-ZoeMZou-multimorbidity_epilepsy.csv", 
    "MS_Epilepsy_snomed": "codelists/bristol-multimorbidity_epilepsy.csv", 
}
multimorbidity_dict = create_codelist_dict(multimorbidity_dict)

    ## Painful Condition (Osteoarthritis)-not found in CMS
MS_Osteoarthritis_ctv3 = codelist_from_csv(
    "codelists/opensafely-osteoarthritis.csv",
    column="CTV3ID"
) 

# ACSC conditions in secondary care 
    ## COPD
copd_ctv3 = codelist_from_csv(
    "codelists/opensafely-current-copd.csv",
    column="CTV3ID"
)

copd_icd10 = codelist_from_csv(
    "codelists/opensafely-copd-secondary-care.csv",
    column="code"
)
    ## Asthma
asthma_snomed = codelist_from_csv(
    "codelists/opensafely-asthma-diagnosis-snomed.csv",
    column="id"
)

asthma_icd10 = codelist_from_csv(
    "codelists/opensafely-asthma-exacerbation-secondary-care.csv",
    column="code"
)

    ## Hypertension
hypertension_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-hypertension_icd10.csv",
    column="code"
)
hypertension_drugs_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-hypertension_drugs_dmd.csv",
    column="dmd_id"
)
hypertension_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-hyp_cod.csv",
    column="code"
)

    ## Diabetes
diabetes_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-diabetes_icd10.csv",
    column="code"
)

diabetes_drugs_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-diabetes_drugs_dmd.csv",
    column="dmd_id"
)

diabetes_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-diabetes_snomed.csv",
    column="code"
) 

    ## Angina
angina_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-angina_icd10.csv",
    column="code"
)

angina_snomed = codelist_from_csv(
    "codelists/user-hjforbes-angina_snomed.csv",
    column="code"
)

# Alternative codes for multimorbidity codes
    ## Stroke Ischaemic (Ischaemic Stroke)
stroke_isch_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-stroke_isch_snomed.csv",
    column="code"
)


stroke_isch_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-stroke_isch_icd10.csv",
    column="code"
)

    ## Dementia
dementia_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_snomed.csv",
    column="code"
)

dementia_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_icd10.csv",
    column="code"
)

dementia_vascular_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_vascular_snomed.csv",
    column="code"
)

dementia_vascular_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_vascular_icd10.csv",
    column="code"
)

    ## Chronic Kidney disease
ckd_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-ckd_snomed.csv",
    column="code"
)

ckd_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-ckd_icd10.csv",
    column="code"
)

    ## Cancer
cancer_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-cancer_snomed.csv",
    column="code"
)

cancer_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-cancer_icd10.csv",
    column="code"
) 

    ## Depression
depression_snomed = codelist_from_csv(
    "codelists/user-hjforbes-depression-symptoms-and-diagnoses.csv",
    column="code"
)

depression_icd10 = codelist_from_csv(
    "codelists/user-kurttaylor-depression_icd10.csv",
    column="code",
)

    ## AMI (Acute Myocardial Infarction)
ami_snomed = codelist_from_csv(
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