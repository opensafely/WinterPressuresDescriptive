import operator
from functools import reduce # for function building, e.g. any_of
from module_table_imports import *
from datetime import date
from codelists import *

def ever_matching_event_clinical_ctv3_before(codelist, start_date, where=True):
    return(
        clinical_events.where(where)
        .where(clinical_events.ctv3_code.is_in(codelist))
        .where(clinical_events.date.is_before(start_date))
    )

def ever_matching_event_clinical_snomed_before(codelist, start_date, where=True):
    return(
        clinical_events.where(where)
        .where(clinical_events.snomedct_code.is_in(codelist))
        .where(clinical_events.date.is_before(start_date))
    )

def ever_matching_med_dmd_before(codelist, start_date, where=True):
    return(
        medications.where(where)
        .where(medications.dmd_code.is_in(codelist))
        .where(medications.date.is_before(start_date))
    )

def last_matching_event_clinical_ctv3_before(codelist, start_date, where=True):
    return(
        clinical_events.where(where)
        .where(clinical_events.ctv3_code.is_in(codelist))
        .where(clinical_events.date.is_before(start_date))
        .sort_by(clinical_events.date)
        .last_for_patient()
    )

def last_matching_event_clinical_snomed_before(codelist, start_date, where=True):
    return(
        clinical_events.where(where)
        .where(clinical_events.snomedct_code.is_in(codelist))
        .where(clinical_events.date.is_before(start_date))
        .sort_by(clinical_events.date)
        .last_for_patient()
    )

def last_matching_event_clinical_snomed_on_or_before(codelist, start_date, where=True):
    return(
        clinical_events.where(where)
        .where(clinical_events.snomedct_code.is_in(codelist))
        .where(clinical_events.date.is_on_or_before(start_date))
        .sort_by(clinical_events.date)
        .last_for_patient()
    )

def last_matching_med_dmd_before(codelist, start_date, where=True):
    return(
        medications.where(where)
        .where(medications.dmd_code.is_in(codelist))
        .where(medications.date.is_before(start_date))
        .sort_by(medications.date)
        .last_for_patient()
    )

def last_matching_event_apc_before(codelist, start_date, only_prim_diagnoses=False, where=True):
    query = apcs.where(where).where(apcs.admission_date.is_before(start_date))
    if only_prim_diagnoses:
        query = query.where(
            apcs.primary_diagnosis.is_in(codelist)
        )
    else:
        query = query.where(apcs.all_diagnoses.contains_any_of(codelist))
    return query.sort_by(apcs.admission_date).last_for_patient()

# helper function
def any_of(conditions):
    return reduce(operator.or_, conditions)

def last_matching_event_ec_snomed_before(codelist, start_date, where=True):
    conditions = [
        getattr(emergency_care_attendances, column_name).is_in(codelist)
        for column_name in ([f"diagnosis_{i:02d}" for i in range(1, 25)])
    ]
    return(
        emergency_care_attendances.where(where)
        .where(any_of(conditions))
        .where(emergency_care_attendances.arrival_date.is_before(start_date))
        .sort_by(emergency_care_attendances.arrival_date)
        .last_for_patient()
    )

def matching_death_before(codelist, start_date, where=True):
    conditions = [
        getattr(ons_deaths, column_name).is_in(codelist)
        for column_name in (["underlying_cause_of_death"] + [f"cause_of_death_{i:02d}" for i in range(1, 16)])
    ]
    return any_of(conditions) & ons_deaths.date.is_before(start_date)

def last_matching_event_clinical_snomed_between(codelist, start_date, end_date, where=True):
    return(
        clinical_events.where(where)
        .where(clinical_events.snomedct_code.is_in(codelist))
        .where(clinical_events.date.is_on_or_between(start_date, end_date))
        .sort_by(clinical_events.date)
        .last_for_patient()
    )

def last_matching_med_dmd_between(codelist, start_date, end_date, where=True):
    return(
        medications.where(where)
        .where(medications.dmd_code.is_in(codelist))
        .where(medications.date.is_on_or_between(start_date, end_date))
        .sort_by(medications.date)
        .last_for_patient()
    )

def first_matching_event_clinical_ctv3_between(codelist, start_date, end_date, where=True):
    return(
        clinical_events.where(where)
        .where(clinical_events.ctv3_code.is_in(codelist))
        .where(clinical_events.date.is_on_or_between(start_date, end_date))
        .sort_by(clinical_events.date)
        .first_for_patient()
    )

def first_matching_event_clinical_snomed_between(codelist, start_date, end_date, where=True):
    return(
        clinical_events.where(where)
        .where(clinical_events.snomedct_code.is_in(codelist))
        .where(clinical_events.date.is_on_or_between(start_date, end_date))
        .sort_by(clinical_events.date)
        .first_for_patient()
    )

def first_matching_med_dmd_between(codelist, start_date, end_date, where=True):
    return(
        medications.where(where)
        .where(medications.dmd_code.is_in(codelist))
        .where(medications.date.is_on_or_between(start_date, end_date))
        .sort_by(medications.date)
        .first_for_patient()
    )

def ever_matching_event_apc_between(codelist, start_date, end_date, only_prim_diagnoses=False, where=True):
    query = apcs.where(where).where(apcs.admission_date.is_on_or_between(start_date, end_date))
    if only_prim_diagnoses:
        query = query.where(
            apcs.primary_diagnosis.is_in(codelist)
        )
    else:
        query = query.where(apcs.all_diagnoses.contains_any_of(codelist))
    return query.sort_by(apcs.admission_date)

def ever_matching_event_ec_snomed_between(codelist, start_date, end_date, where=True):
    conditions = [
        getattr(emergency_care_attendances, column_name).is_in(codelist)
        for column_name in ([f"diagnosis_{i:02d}" for i in range(1, 25)])
    ]
    return(
        emergency_care_attendances.where(where)
        .where(any_of(conditions))
        .where(emergency_care_attendances.arrival_date.is_on_or_between(start_date, end_date))
        .sort_by(emergency_care_attendances.arrival_date)
    )

def matching_death_between(codelist, start_date, end_date, where=True):
    conditions = [
        getattr(ons_deaths, column_name).is_in(codelist)
        for column_name in (["underlying_cause_of_death"] + [f"cause_of_death_{i:02d}" for i in range(1, 16)])
    ]
    return any_of(conditions) & ons_deaths.date.is_on_or_between(start_date, end_date)

# filter a codelist based on whether its values included a specified set of allowed values (include)
def filter_codes_by_category(codelist, include):
    return {k:v for k,v in codelist.items() if v in include}

# look up the most recent BMI value for each patient (>16 years old) before a specified date, ignoring out-of-range values
def most_recent_bmi(start_date, where=True):
    return(
        clinical_events.where(where)
        .where(clinical_events.snomedct_code.is_in(["60621009", "846931000000101"]) # BMI codes
        # Ignore out-of-range values
        & (clinical_events.numeric_value > 4)
        & (clinical_events.numeric_value < 200))
        .where(clinical_events.date.is_before(start_date))
        .sort_by(clinical_events.date)
        .last_for_patient()
    )

# Function to obtain Cambridge multimorbidity score
# Function to check dates are valid (i.e., not before or after death)

def check_date_validity(
    date_to_check,
    death_date,
    check_not_before_dob=True,
    check_not_after_death=True
):

    conditions = []

    ## Base requirement: must not be null
    conditions.append(date_to_check.is_not_null())

    ## Check not before DOB
    if check_not_before_dob:
        conditions.append(
            patients.date_of_birth.is_null()
            | (date_to_check >= patients.date_of_birth)
        )

    ## Check not after death
    if check_not_after_death:
        conditions.append(
            death_date.is_null()
            | (date_to_check <= death_date)
        )

    ## Combine all validity conditions
    is_valid = conditions[0]
    for cond in conditions[1:]:
        is_valid = is_valid & cond

    return case(
        when(is_valid).then(date_to_check),
        otherwise=None
    )

def has_condition_before(
    index_date,
    death_date,
    snomed=None,
    dmd1=None,
    dmd2=None,
    where=True,
):
    conditions = []

    if snomed is not None:
        filtered = ever_matching_event_clinical_snomed_before(
            snomed,
            index_date,
            where=where,
        )

        conditions.append(
            filtered.where(
                check_date_validity(
                    filtered.date,
                    death_date=death_date,
                ).is_not_null()
            ).exists_for_patient()
        )

    for dmd in [dmd1, dmd2]:
        if dmd is not None:
            filtered = ever_matching_med_dmd_before(
                dmd,
                index_date,
                where=where,
            )

            conditions.append(
                filtered.where(
                    check_date_validity(
                        filtered.date,
                        death_date=death_date,
                    ).is_not_null()
                ).exists_for_patient()
            )

    if not conditions:
        raise ValueError(
            "Must provide at least one SNOMED or DMD codelist."
        )

    return any_of(conditions)

# Function to obtain Cambridge multimorbidity score (https://github.com/opensafely/NeurodegenerativeDiseaseBurden/blob/6ce9d827aa16356c76b3680b8d7ef58122eb2305/analysis/dataset_definition/variable_helper_functions.py#L45-L320)

def get_cms_on_date(input_date, death_date, return_components=False):

    cms = clinical_events.exists_for_patient().as_int().as_float() * 0
    components = {}

    # Any diagnosis ever before input_date
    for name, codelist, weight in [
        ("alcohol", multimorbidity_dict["MS_AlcoholProblem_snomed"], 0.792243),
        ("af", multimorbidity_dict["MS_AF_snomed"], 0.334891),
        ("copd", multimorbidity_dict["MS_COPD_snomed"], 0.702181),
        ("dementia", multimorbidity_dict["MS_Dementia_snomed"], 0.938001),
        ("diabetes", multimorbidity_dict["MS_Diabetes_snomed"], 0.29467),
        ("hf", multimorbidity_dict["MS_HF_snomed"], 0.505245),
        ("cld", multimorbidity_dict["MS_CLD_snomed"], 0.68621),
        ("prostate", multimorbidity_dict["MS_Prostate_snomed"], -0.18781),
        ("learning", multimorbidity_dict["MS_LD_snomed"], 0.637273),
        ("sclerosis", multimorbidity_dict["MS_Sclerosis_snomed"], 0.761606),
        ("parkinsonism", multimorbidity_dict["MS_Parkinson_snomed"], 0.546194),
        (
            "perivascular_leg",
            multimorbidity_dict["MS_Perivascular_snomed"],
            0.334558,
        ),
        (
            "psychosub_misuse",
            multimorbidity_dict["MS_Psychoactive_snomed"],
            0.449321,
        ),
    ]:
        filtered = clinical_events.where(
            clinical_events.snomedct_code.is_in(codelist)
        ).where(
            clinical_events.date.is_before(input_date)
        )

        binary = filtered.where(
            check_date_validity(
                filtered.date, death_date=death_date
            ).is_not_null()
        ).exists_for_patient().as_int()

        cms += binary.as_float() * weight

        if return_components:
            components[name] = binary

    # First recorded cancer diagnosis in the last 5 years
    for name, codelist, weight in [
        ("cancer", multimorbidity_dict["MS_Cancer_snomed"], 1.202615),
    ]:
        if isinstance(input_date, str):
            d = date.fromisoformat(input_date)
            earliest = f"{d.year-5}-{d.month:02d}-{d.day:02d}"
        else:
            earliest = input_date - years(5)

        filtered = clinical_events.where(
            clinical_events.snomedct_code.is_in(codelist)
        ).where(
            clinical_events.date.is_before(input_date)
        )

        filtered2 = filtered.where(
            check_date_validity(
                filtered.date, death_date=death_date
            ).is_not_null()
        ).date.minimum_for_patient()

        binary = (
            filtered2.is_not_null() & (filtered2 >= earliest)
        ).as_int()

        cms += binary.as_float() * weight

        if return_components:
            components[name] = binary

    # Larger eGFR value from the two most recent measurement dates < 60
    for name, codelist, weight in [
        ("ckd", multimorbidity_dict["MS_CKD_snomed"], 0.213652),
    ]:
        filtered = clinical_events.where(
            clinical_events.snomedct_code.is_in(codelist)
        ).where(
            clinical_events.date.is_before(input_date)
        )

        filtered2 = filtered.where(
            check_date_validity(
                filtered.date, death_date=death_date
            ).is_not_null()
        )

        recent1 = filtered2.date.maximum_for_patient()
        recent2 = filtered2.where(
            filtered2.date < recent1
        ).date.maximum_for_patient()

        value1 = filtered2.where(
            filtered2.date == recent1
        ).numeric_value.minimum_for_patient()

        value2 = filtered2.where(
            filtered2.date == recent2
        ).numeric_value.minimum_for_patient()

        value = maximum_of(value1, value2)
        binary = (
            value1.is_not_null()
            & value2.is_not_null()
            & (value < 60)
        ).as_int()

        cms += binary.as_float() * weight

        if return_components:
            components[name] = binary

    # Epilepsy: any diagnosis ever AND at least one prescription
    # in the last 12 months
    for name, codelist, medlist, weight in [
        (
            "epilepsy",
            multimorbidity_dict["MS_Epilepsy_snomed"],
            multimorbidity_dict["MS_Epilepsy_dmd"],
            0.477465,
        ),
    ]:
        if isinstance(input_date, str):
            d = date.fromisoformat(input_date)
            earliest = f"{d.year-1}-{d.month:02d}-{d.day:02d}"
        else:
            earliest = input_date - years(1)

        filtered = clinical_events.where(
            clinical_events.snomedct_code.is_in(codelist)
        ).where(
            clinical_events.date.is_before(input_date)
        )

        diag = filtered.where(
            check_date_validity(
                filtered.date, death_date=death_date
            ).is_not_null()
        ).exists_for_patient()

        medfiltered = medications.where(
            medications.dmd_code.is_in(medlist)
        ).where(
            medications.date.is_before(input_date)
        ).where(
            medications.date.is_on_or_after(earliest)
        )

        med = medfiltered.where(
            check_date_validity(
                medfiltered.date, death_date=death_date
            ).is_not_null()
        ).exists_for_patient()

        binary = (diag & med).as_int()
        cms += binary.as_float() * weight

        if return_components:
            components[name] = binary

    # IBS: any diagnosis ever OR prescriptions on at least
    # four distinct dates in the last 12 months
    for name, codelist, medlist, weight in [
        (
            "bowel",
            multimorbidity_dict["MS_IBS_snomed"],
            multimorbidity_dict["MS_IBS_dmd"],
            -0.20368,
        ),
    ]:
        if isinstance(input_date, str):
            d = date.fromisoformat(input_date)
            earliest = f"{d.year-1}-{d.month:02d}-{d.day:02d}"
        else:
            earliest = input_date - years(1)

        filtered = clinical_events.where(
            clinical_events.snomedct_code.is_in(codelist)
        ).where(
            clinical_events.date.is_before(input_date)
        )

        diag = filtered.where(
            check_date_validity(
                filtered.date, death_date=death_date
            ).is_not_null()
        ).exists_for_patient()

        medfiltered = medications.where(
            medications.dmd_code.is_in(medlist)
        ).where(
            medications.date.is_before(input_date)
        ).where(
            medications.date.is_on_or_after(earliest)
        )

        med = medfiltered.where(
            check_date_validity(
                medfiltered.date, death_date=death_date
            ).is_not_null()
        ).date.count_distinct_for_patient()

        binary = (diag | (med >= 4)).as_int()
        cms += binary.as_float() * weight

        if return_components:
            components[name] = binary

    # Psychosis/bipolar disorder: any diagnosis OR prescription ever
    for name, codelist, medlist, weight in [
        (
            "psychosis",
            multimorbidity_dict["MS_Psychosis_snomed"],
            multimorbidity_dict["MS_Psychosis_dmd"],
            0.482469,
        ),
    ]:
        filtered = clinical_events.where(
            clinical_events.snomedct_code.is_in(codelist)
        ).where(
            clinical_events.date.is_before(input_date)
        )

        diag = filtered.where(
            check_date_validity(
                filtered.date, death_date=death_date
            ).is_not_null()
        ).exists_for_patient()

        medfiltered = medications.where(
            medications.dmd_code.is_in(medlist)
        ).where(
            medications.date.is_before(input_date)
        )

        med = medfiltered.where(
            check_date_validity(
                medfiltered.date, death_date=death_date
            ).is_not_null()
        ).exists_for_patient()

        binary = (diag | med).as_int()
        cms += binary.as_float() * weight

        if return_components:
            components[name] = binary

    # Constipation: prescriptions on at least four distinct dates
    # in the last 12 months; no diagnosis required
    for name, medlist, weight in [
        (
            "constipation",
            multimorbidity_dict["MS_Constipation_dmd"],
            0.383006,
        ),
    ]:
        if isinstance(input_date, str):
            d = date.fromisoformat(input_date)
            earliest = f"{d.year-1}-{d.month:02d}-{d.day:02d}"
        else:
            earliest = input_date - years(1)

        medfiltered = medications.where(
            medications.dmd_code.is_in(medlist)
        ).where(
            medications.date.is_before(input_date)
        ).where(
            medications.date.is_on_or_after(earliest)
        )

        med = medfiltered.where(
            check_date_validity(
                medfiltered.date, death_date=death_date
            ).is_not_null()
        ).date.count_distinct_for_patient()

        binary = (med >= 4).as_int()
        cms += binary.as_float() * weight

        if return_components:
            components[name] = binary

    # Anxiety/depression: diagnosis in the last 12 months OR
    # prescriptions on at least four distinct dates in that period
    for name, codelist, medlist, weight in [
        (
            "anxiety",
            multimorbidity_dict["MS_AnxietyDepression_snomed"],
            multimorbidity_dict["MS_AnxietyDepression_dmd"],
            0.324207,
        ),
    ]:
        if isinstance(input_date, str):
            d = date.fromisoformat(input_date)
            earliest = f"{d.year-1}-{d.month:02d}-{d.day:02d}"
        else:
            earliest = input_date - years(1)

        filtered = clinical_events.where(
            clinical_events.snomedct_code.is_in(codelist)
        ).where(
            clinical_events.date.is_before(input_date)
        ).where(
            clinical_events.date.is_on_or_after(earliest)
        )

        diag = filtered.where(
            check_date_validity(
                filtered.date, death_date=death_date
            ).is_not_null()
        ).exists_for_patient()

        medfiltered = medications.where(
            medications.dmd_code.is_in(medlist)
        ).where(
            medications.date.is_before(input_date)
        ).where(
            medications.date.is_on_or_after(earliest)
        )

        med = medfiltered.where(
            check_date_validity(
                medfiltered.date, death_date=death_date
            ).is_not_null()
        ).date.count_distinct_for_patient()

        binary = (diag | (med >= 4)).as_int()
        cms += binary.as_float() * weight

        if return_components:
            components[name] = binary

    # Painful condition:
    # analgesics on >=4 distinct dates in the last 12 months OR
    # antiepileptics for pain on >=4 distinct dates in that period
    # AND no previous epilepsy diagnosis
    for name, codelist, medlist1, medlist2, weight in [
        (
            "pain",
            multimorbidity_dict["MS_Epilepsy_snomed"],
            multimorbidity_dict["MS_PC_Analgesics_dmd"],
            multimorbidity_dict["MS_PC_Antiepileptic_dmd"],
            0.445521,
        ),
    ]:
        if isinstance(input_date, str):
            d = date.fromisoformat(input_date)
            earliest = f"{d.year-1}-{d.month:02d}-{d.day:02d}"
        else:
            earliest = input_date - years(1)

        filtered = clinical_events.where(
            clinical_events.snomedct_code.is_in(codelist)
        ).where(
            clinical_events.date.is_before(input_date)
        )

        diag = filtered.where(
            check_date_validity(
                filtered.date, death_date=death_date
            ).is_not_null()
        ).exists_for_patient()

        medfiltered1 = medications.where(
            medications.dmd_code.is_in(medlist1)
        ).where(
            medications.date.is_before(input_date)
        ).where(
            medications.date.is_on_or_after(earliest)
        )

        med1 = medfiltered1.where(
            check_date_validity(
                medfiltered1.date, death_date=death_date
            ).is_not_null()
        ).date.count_distinct_for_patient()

        medfiltered2 = medications.where(
            medications.dmd_code.is_in(medlist2)
        ).where(
            medications.date.is_before(input_date)
        ).where(
            medications.date.is_on_or_after(earliest)
        )

        med2 = medfiltered2.where(
            check_date_validity(
                medfiltered2.date, death_date=death_date
            ).is_not_null()
        ).date.count_distinct_for_patient()

        binary = (
            ((~diag) & (med2 >= 4)) | (med1 >= 4)
        ).as_int()

        cms += binary.as_float() * weight

        if return_components:
            components[name] = binary

    if return_components:
        components["cms"] = cms
        return components

    return cms