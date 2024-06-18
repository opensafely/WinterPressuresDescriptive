from ehrql import (codelist_from_csv, create_dataset, days, minimum_of, case, when,)
# Bring table definitions from the TPP backend 
from ehrql.tables.tpp import (patients, practice_registrations, addresses, apcs, ec, opa, clinical_events, medications, ons_deaths,)
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

# Outcomes- date of apc, opc, ec, and death
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


