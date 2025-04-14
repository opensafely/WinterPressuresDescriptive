from module_table_imports import *
from codelists import *
from datetime import date

# Call functions from variable_helper_functions
from variable_helper_functions import (
    last_matching_event_clinical_snomed_between,
    last_matching_event_clinical_snomed_before,
    last_matching_med_dmd_between,
)

# Define the study_dates dictionary 

import json
with open("output/dataset_definition/study_dates.json") as f:
  study_dates = json.load(f)

# Extracting all variables from the study_dates dictionary
ref_age_1 = study_dates["ref_age_1"]  # reference date for calculating age for phase 1 groups
ref_age_2 = study_dates["ref_age_2"]  # reference date for calculating age for phase 2 groups
ref_cev = study_dates["ref_cev"]  # reference date for calculating eligibility for phase 1 group 4 (CEV)
ref_ar = study_dates["ref_ar"]  # reference date for calculating eligibility for phase 1 group 5 (at-risk)
pandemic_start = study_dates["pandemic_start"]  # rough start date for pandemic in UK
vax1_earliest = study_dates["vax1_earliest"]  # earliest expectation date for first vaccination
vax2_earliest = study_dates["vax2_earliest"]  # earliest expectation date for 2nd vaccination
vax3_earliest = study_dates["vax3_earliest"]  # earliest expectation date for 3rd vaccination

# JCVI VARIABLES-------------------------------------------------------------------------------------------------------------------

# Age on phase 1 reference date
vax_jcvi_age_1 = patients.age_on(ref_age_1)

# Age on phase 2 reference date
vax_jcvi_age_2 = patients.age_on(ref_age_2)

# preg_group (ongoing pregnancy as of the ref_cev)------------------------------------------

    ## Derived variables

cov_cat_sex = patients.sex  # this is required for preg_group variables

    ## Date of last pregnancy code in 36 weeks before ref_cev
preg_36wks_date = last_matching_event_clinical_snomed_between(
    jcvi_dict["preg_primis"], ref_cev - days(252), ref_cev - days(1)
).date

    ## Date of last delivery code recorded in 36 weeks before elig_date
pregdel_pre_date = last_matching_event_clinical_snomed_between(
    jcvi_dict["pregdel_primis"], ref_cev - days(252), ref_cev - days(1)
).date

preg_group = (
    (preg_36wks_date.is_not_null()) & 
    (cov_cat_sex == "female") &
    (vax_jcvi_age_1 < 50) &
    (
        (pregdel_pre_date <= preg_36wks_date) | (pregdel_pre_date.is_null())
    )
)

# cev_group (clinically extremely vulnerable group variables)--------------------------------

    ## Derived variables

    ## SHIELDED GROUP - first flag all patients with "high risk" codes
severely_clinically_vulnerable = last_matching_event_clinical_snomed_before(
    jcvi_dict["shield_primis"], ref_cev
).exists_for_patient()

    ## Find date at which the high risk code was added
severely_clinically_vulnerable_date = last_matching_event_clinical_snomed_before(
    jcvi_dict["shield_primis"], ref_cev
).date

    ## NOT SHIELDED GROUP (medium and low risk) - only flag if later than 'shielded'
less_vulnerable = last_matching_event_clinical_snomed_between(
    jcvi_dict["nonshield_primis"], severely_clinically_vulnerable_date + days(1), ref_cev - days(1)
).exists_for_patient()

cev_group = (
    severely_clinically_vulnerable & (less_vulnerable == False)
)


# Derived variables for at risk group-----------------------------------------------------
# asthma_group
    ## Derived variables for asthma_group
    ## Asthma Diagnosis codes
astdx = last_matching_event_clinical_snomed_before(
    jcvi_dict["ast_primis"], ref_ar
).exists_for_patient()

    ## Asthma Admission codes
astadm = last_matching_event_clinical_snomed_before(
    jcvi_dict["astadm_primis"], ref_ar
).exists_for_patient()

    ## Asthma systemic steroid prescription code in month 1
astrxm1 = last_matching_med_dmd_between(
    jcvi_dict["astrx_primis"], ref_ar - days(31), ref_ar - days(1)
).exists_for_patient()

    ## Asthma systemic steroid prescription code in month 2
astrxm2 = last_matching_med_dmd_between(
    jcvi_dict["astrx_primis"], ref_ar - days(61), ref_ar - days(32)
).exists_for_patient()

    ## Asthma systemic steroid prescription code in month 3
astrxm3 = last_matching_med_dmd_between(
    jcvi_dict["astrx_primis"], ref_ar - days(91), ref_ar - days(62)
).exists_for_patient()

asthma_group = (
    (astadm) | (astdx & astrxm1 & astrxm2 & astrxm3)
)

# resp_group (Chronic Respiratory Disease other than asthma)
resp_group = last_matching_event_clinical_snomed_before(
    jcvi_dict["resp_primis"], ref_ar
).exists_for_patient()

# cns_group (Chronic Neurological Disease including Significant Learning Disorder)
cns_group = last_matching_event_clinical_snomed_before(
    jcvi_dict["cns_primis"], ref_ar
).exists_for_patient()

# Diabetes
diab_date = last_matching_event_clinical_snomed_before(
    jcvi_dict["diab_primis"], ref_ar
).date

dmres_date = last_matching_event_clinical_snomed_before(
    jcvi_dict["dmres_primis"], ref_ar
).date

diab_group = (
    (dmres_date.is_null() & diab_date.is_not_null()) | (dmres_date < diab_date)
)

# Severe Mental Illness
sev_mental_date = last_matching_event_clinical_snomed_before(
    jcvi_dict["sev_mental_primis"], ref_ar
).date

smhres_date = last_matching_event_clinical_snomed_before(
    jcvi_dict["smhres_primis"], ref_ar
).date

sevment_group = (
    (smhres_date.is_null() & sev_mental_date.is_not_null()) | (smhres_date < sev_mental_date)
)

# Chronic Heart Disease
chd_group = last_matching_event_clinical_snomed_before(
    jcvi_dict["chd_primis"], ref_ar
).exists_for_patient()

# Chronic Kidney Disease
ckd15_date = last_matching_event_clinical_snomed_before(
    jcvi_dict["ckd15_primis"], ref_ar
).date

ckd35_date = last_matching_event_clinical_snomed_before(
    jcvi_dict["ckd35_primis"], ref_ar
).date

ckd = last_matching_event_clinical_snomed_before(
    jcvi_dict["ckd_primis"], ref_ar
).exists_for_patient()

ckd_group = (
    ckd | 
    (ckd15_date.is_not_null() & (ckd35_date >= ckd15_date)) |
    (ckd35_date.is_not_null() & ckd15_date.is_null())
)

# Chronic Liver Disease
cld_group = last_matching_event_clinical_snomed_before(
    jcvi_dict["cld_primis"], ref_ar
).exists_for_patient()

# Immunosuppressed
immdx = last_matching_event_clinical_snomed_before(
    jcvi_dict["immdx_primis"], ref_ar
).exists_for_patient()

immrx = last_matching_med_dmd_between(
    jcvi_dict["immrx_primis"], ref_ar - days(180), ref_ar - days(1)
).exists_for_patient()

immuno_group = (immdx | immrx)

# Asplenia/Dysfunction of Spleen
spln_group = last_matching_event_clinical_snomed_before(
    jcvi_dict["spln_primis"], ref_ar
).exists_for_patient()

# Learning Disability
learndis_group = last_matching_event_clinical_snomed_before(
    jcvi_dict["learndis_primis"], ref_ar
).exists_for_patient()

# Severe Obesity
bmi_stage_date = last_matching_event_clinical_snomed_before(
    jcvi_dict["bmi_stage_primis"], ref_ar
).date

sev_obesity_date = last_matching_event_clinical_snomed_between(
    jcvi_dict["sev_obesity_primis"], bmi_stage_date, ref_ar - days(1)
).date

bmi_date = last_matching_event_clinical_snomed_before(
    jcvi_dict["bmi_primis"], ref_ar
).date

bmi_value_temp = last_matching_event_clinical_snomed_before(
    jcvi_dict["bmi_primis"], ref_ar
).numeric_value

sevobese_group = (
    (sev_obesity_date.is_not_null() & bmi_date.is_null()) |
    (sev_obesity_date > bmi_date) |
    (bmi_value_temp >= 40)
)

# atrisk_group (at risk group) (??why the previous studies exlcuding asthma group)
atrisk_group = (
    asthma_group |
    resp_group |
    cns_group |
    diab_group |
    sevment_group |
    chd_group |
    ckd_group |
    cld_group |
    immuno_group |
    spln_group |
    learndis_group |
    sevobese_group
)

# longres_group (Patients in long-stay nursing and residential care)----------------------------
longres_group = last_matching_event_clinical_snomed_before(
    jcvi_dict["longres_primis"], vax1_earliest
).exists_for_patient()

# jcvi_group
vax_cat_jcvi_group = case(
    when((longres_group) & (vax_jcvi_age_1 > 65)).then("01"),
    when(vax_jcvi_age_1 >= 80).then("02"),
    when(vax_jcvi_age_1 >= 75).then("03"),
    when((vax_jcvi_age_1 >= 70) | 
         (cev_group & (vax_jcvi_age_1 >= 16) & (~preg_group))).then("04"),
    when(vax_jcvi_age_1 >= 65).then("05"),
    when((atrisk_group) & (vax_jcvi_age_1 >= 16)).then("06"),
    when(vax_jcvi_age_1 >= 60).then("07"),
    when(vax_jcvi_age_1 >= 55).then("08"),
    when(vax_jcvi_age_1 >= 50).then("09"),
    when(vax_jcvi_age_2 >= 40).then("10"),
    when(vax_jcvi_age_2 >= 30).then("11"),
    when(vax_jcvi_age_2 >= 18).then("12"),
    otherwise="99"  # Default group if no other criteria are met
)

# vaccination eligible date according to jcvi
vax_date_eligible = case(
    when((vax_cat_jcvi_group == "01") | (vax_cat_jcvi_group == "02")).then(date(2020, 12, 8)),
    when((vax_cat_jcvi_group == "03") | (vax_cat_jcvi_group == "04")).then(date(2021, 1, 18)),
    when((vax_cat_jcvi_group == "05") | (vax_cat_jcvi_group == "06")).then(date(2021, 2, 15)),
    when((vax_cat_jcvi_group == "07") & (vax_jcvi_age_1 >= 64) & (vax_jcvi_age_1 < 65)).then(date(2021, 2, 22)),
    when((vax_cat_jcvi_group == "07") & (vax_jcvi_age_1 >= 60) & (vax_jcvi_age_1 < 64)).then(date(2021, 3, 1)),
    when((vax_cat_jcvi_group == "08") & (vax_jcvi_age_1 >= 56) & (vax_jcvi_age_1 < 60)).then(date(2021, 3, 8)),
    when((vax_cat_jcvi_group == "08") & (vax_jcvi_age_1 >= 55) & (vax_jcvi_age_1 < 56)).then(date(2021, 3, 9)),
    when((vax_cat_jcvi_group == "09") & (vax_jcvi_age_1 >= 50) & (vax_jcvi_age_1 < 55)).then(date(2021, 3, 19)),
    when((vax_cat_jcvi_group == "10") & (vax_jcvi_age_2 >= 45) & (vax_jcvi_age_1 < 50)).then(date(2021, 4, 13)),
    when((vax_cat_jcvi_group == "10") & (vax_jcvi_age_2 >= 44) & (vax_jcvi_age_1 < 45)).then(date(2021, 4, 26)),
    when((vax_cat_jcvi_group == "10") & (vax_jcvi_age_2 >= 42) & (vax_jcvi_age_1 < 44)).then(date(2021, 4, 27)),
    when((vax_cat_jcvi_group == "10") & (vax_jcvi_age_2 >= 40) & (vax_jcvi_age_1 < 42)).then(date(2021, 4, 30)),
    when((vax_cat_jcvi_group == "11") & (vax_jcvi_age_2 >= 38) & (vax_jcvi_age_2 < 40)).then(date(2021, 5, 13)),
    when((vax_cat_jcvi_group == "11") & (vax_jcvi_age_2 >= 36) & (vax_jcvi_age_2 < 38)).then(date(2021, 5, 19)),
    when((vax_cat_jcvi_group == "11") & (vax_jcvi_age_2 >= 34) & (vax_jcvi_age_2 < 36)).then(date(2021, 5, 21)),
    when((vax_cat_jcvi_group == "11") & (vax_jcvi_age_2 >= 32) & (vax_jcvi_age_2 < 34)).then(date(2021, 5, 25)),
    when((vax_cat_jcvi_group == "11") & (vax_jcvi_age_2 >= 30) & (vax_jcvi_age_2 < 32)).then(date(2021, 5, 26)),
    when((vax_cat_jcvi_group == "12") & (vax_jcvi_age_2 >= 25) & (vax_jcvi_age_2 < 30)).then(date(2021, 6, 8)),
    when((vax_cat_jcvi_group == "12") & (vax_jcvi_age_2 >= 23) & (vax_jcvi_age_2 < 25)).then(date(2021, 6, 15)),
    when((vax_cat_jcvi_group == "12") & (vax_jcvi_age_2 >= 21) & (vax_jcvi_age_2 < 23)).then(date(2021, 6, 16)),
    when((vax_cat_jcvi_group == "12") & (vax_jcvi_age_2 >= 18) & (vax_jcvi_age_2 < 21)).then(date(2021, 6, 18)),
    otherwise=date(2100, 12, 31)  # Default if no other criteria are met
)

# Define a dictionary of JCVI variables created above 
jcvi_variables = dict(
    vax_cat_jcvi_group=vax_cat_jcvi_group, # jcvi_group
    vax_date_eligible=vax_date_eligible, # Vaccination eligible date according to jcvi
)

# PRELIMINARY DATE VARIABLES------------------------------------------------------------------------------------------------------------------------------------

# Add death date----------------------------------------------------------------------------------

    ## Primary care
primary_care_death_date = case(
    when(patients.date_of_death.is_on_or_after(pandemic_start)).then(patients.date_of_death)
)

    ## ONS
ons_died_from_any_cause_date = case(
    when(ons_deaths.date.is_on_or_after(pandemic_start)).then(ons_deaths.date)
)

death_date = minimum_of(primary_care_death_date, ons_died_from_any_cause_date)

# add vaccination dates----------------------------------------------------------------------------

# COVID-19 Vaccination (identified by target diseases of the vaccination)

vax_date_covid_1 = (
    vaccinations.where(
    vaccinations.target_disease.contains("SARS-2 CORONAVIRUS"))
    .where(vaccinations.date.is_on_or_after(vax1_earliest))
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date
)

vax_date_covid_2 = (
    vaccinations.where(
    vaccinations.target_disease.contains("SARS-2 CORONAVIRUS"))
    .where(vaccinations.date > vax_date_covid_1)  # Exclude the first date
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date  # Now this will be the second date
)

vax_date_covid_3 = (
    vaccinations.where(
    vaccinations.target_disease.contains("SARS-2 CORONAVIRUS"))
    .where(vaccinations.date > vax_date_covid_2)  # Exclude the first and second date
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date  # Now this will be the third date
)

vax_bin_covid = (
    vaccinations.where(
    vaccinations.target_disease.contains("SARS-2 CORONAVIRUS"))
    .exists_for_patient()
)

# Pfizer BioNTech Vaccination (identified by vaccination_id.product_name: 28.COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer))
vax_date_Pfizer_1 = (
    vaccinations.where(
    (vaccinations.product_name == "COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer)"))
    .where(vaccinations.date.is_on_or_after(vax1_earliest))
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date
)

vax_date_Pfizer_2 = (
    vaccinations.where(
    (vaccinations.product_name == "COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer)"))
    .where(vaccinations.date > vax_date_Pfizer_1)  # Exclude the first date
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date  # Now this will be the second date
)

vax_date_Pfizer_3 = (
    vaccinations.where(
    (vaccinations.product_name == "COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer)"))
    .where(vaccinations.date > vax_date_Pfizer_2)  # Exclude the first and second date
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date  # Now this will be the third date
)

vax_num_Pfizer = (
    vaccinations.where(
    (vaccinations.product_name == "COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer)"))
    .sort_by(vaccinations.date)
    .count_for_patient()
)

# Oxford AZ Vaccination (identified by vaccination_id.product_name: 49.COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca))

vax_date_AstraZeneca_1 = (
    vaccinations.where(
    (vaccinations.product_name == "COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca)"))
    .where(vaccinations.date.is_on_or_after(vax1_earliest))
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date
)

vax_date_AstraZeneca_2 = (
    vaccinations.where(
    (vaccinations.product_name == "COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca)"))
    .where(vaccinations.date > vax_date_AstraZeneca_1)  # Exclude the first date
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date  # Now this will be the second date
)

vax_date_AstraZeneca_3 = (
    vaccinations.where(
    (vaccinations.product_name == "COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca)"))
    .where(vaccinations.date > vax_date_AstraZeneca_2)  # Exclude the first and second date
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date  # Now this will be the third date
)

vax_num_AstraZeneca = (
    vaccinations.where(
    (vaccinations.product_name == "COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca)"))
    .sort_by(vaccinations.date)
    .count_for_patient()
)

# Moderna Vaccination (identified by vaccination_id.product_name: 30.COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna))

vax_date_Moderna_1 = (
    vaccinations.where(
    (vaccinations.product_name == "COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna)"))
    .where(vaccinations.date.is_on_or_after(vax1_earliest))
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date
)

vax_date_Moderna_2 = (
    vaccinations.where(
    (vaccinations.product_name == "COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna)"))
    .where(vaccinations.date > vax_date_Moderna_1)  # Exclude the first date
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date  # Now this will be the second date
)

vax_date_Moderna_3 = (
    vaccinations.where(
    (vaccinations.product_name == "COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna)"))
    .where(vaccinations.date > vax_date_Moderna_2)  # Exclude the first and second date
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date  # Now this will be the third date
)

vax_num_Moderna = (
    vaccinations.where(
    (vaccinations.product_name == "COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna)"))
    .sort_by(vaccinations.date)
    .count_for_patient()
)

# Define a dictionary of preliminary date variables (Death, Vaccination) created above 
prelim_date_variables = dict(
    cens_date_death=death_date,
    vax_date_covid_1=vax_date_covid_1,
    vax_date_covid_2=vax_date_covid_2,
    vax_date_covid_3=vax_date_covid_3,
    vax_bin_covid = vax_bin_covid,
    vax_date_Pfizer_1=vax_date_Pfizer_1,
    vax_date_Pfizer_2=vax_date_Pfizer_2,
    vax_date_Pfizer_3=vax_date_Pfizer_3,
    vax_date_AstraZeneca_1=vax_date_AstraZeneca_1,
    vax_date_AstraZeneca_2=vax_date_AstraZeneca_2,
    vax_date_AstraZeneca_3=vax_date_AstraZeneca_3,
    vax_date_Moderna_1=vax_date_Moderna_1,
    vax_date_Moderna_2=vax_date_Moderna_2,
    vax_date_Moderna_3=vax_date_Moderna_3,
)