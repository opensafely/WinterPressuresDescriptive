from ehrql import codelist_from_csv, create_dataset, days, minimum_of, case, when
#bring table definitions from the TPP backend 
from ehrql.tables.tpp import patients, practice_registrations, addresses, apcs, ec, opa, clinical_events, medications, ons_deaths
## Codelists from codelists.py (which pulls all variables from the codelist folder)
from codelists import *
# Define study start and end dates
study_start_date = "2017-09-30"
study_end_date = "2023-11-28"
# Define a dataset 
dataset = create_dataset()
# Define population criteria 
# Define population criteria
dataset.define_population(
    # All individuals in OpenSAFELY-TPP starting from 30th Sep 2017 to 28th November 2023
    start_date=study_start_date,
    end_date=study_end_date,

    # Known age between 0 and 110 inclusive on the study start date
    population=(
        (patients.age_on(study_start_date).between(0, 110)) &
        # Alive on the study start date
        (patients.is_alive_on(study_start_date)) &
        # Registered for a minimum of 365 days prior to the study start date for individuals aged 1 year old or older
        (
            (patients.age_on(study_start_date) >= 1) &
            (practice_registrations.start_date <= (days(study_start_date) - 365))
        ) |
        # Registered since birth for those aged less than 1 year old
        (
            (patients.age_on(study_start_date) < 1) &
            (practice_registrations.start_date == patients.date_of_birth)
        ) &
        # Known sex, deprivation, and region
        (patients.sex.is_not_null()) &
        (patients.deprivation.is_not_null()) &
        (patients.region.is_not_null())
    )
)
# Add additional columns to the dataset if needed
dataset = dataset.add_columns(
    patient_id=patients.patient_id,

# Outcomes- date of apc, opc, ec, and death
    out_date_apcadm=apcs.admission_date,
    out_date_apcdis=apcs.discharge_date,
    out_date_opcadm=opa.appointment_date,
    out_date_ecadm=ec.arrival_date,
    out_date_death_tpp=patients.date_of_death,
    out_date_death_ons=ons_deaths.date,
    out_date_death_min=minimum_of(patients.date_of_death, ons_deaths.date),
# Covariates
## Age
    cov_date_of_birth=patients.date_of_birth,
    cov_num_age=patients.age_on(study_start_date),
## Sex
    cov_cat_sex=patients.sex,
## Ethnicity 
    cov_cat_ethnicity = ethinicity_codelist,
## Region
    cov_cat_region=patients.region,
## IMD
    cov_num_imd = addresses.for_patient_on("2023-01-01").imd_rounded
    dov_cat_imd = case(
        when((cov_num_imd >=0) & (cov_num_imd < int(32844 * 1 / 5))).then("1 (most deprived)"),
        when(cov_num_imd < int(32844 * 2 / 5)).then("2"),
        when(cov_num_imd < int(32844 * 3 / 5)).then("3"),
        when(cov_num_imd < int(32844 * 4 / 5)).then("4"),
        when(cov_num_imd < int(32844 * 5 / 5)).then("5 (least deprived)"),
        otherwise="unknown",
    )
)
results = dataset.execute()