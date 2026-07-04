import operator
from functools import reduce # for function building, e.g. any_of
from module_table_imports import *
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

def get_cms_on_date(index_date, death_date, return_components=False):

    cms = clinical_events.exists_for_patient().as_int().as_float() * 0
    components = {}

    for name, weight, snomed, dmd1, dmd2 in [
        ("alcohol",      0.65, multimorbidity_dict["MS_AlcoholProblem_snomed"], None, None),
        ("anxiety",      0.50, multimorbidity_dict["MS_AnxietyDepression_snomed"], multimorbidity_dict["MS_AnxietyDepression_dmd"], None),
        ("asthma",       0.19, multimorbidity_dict["MS_Asthma_snomed"], multimorbidity_dict["MS_Asthma_dmd"], None),
        ("af",           1.34, multimorbidity_dict["MS_AF_snomed"], None, None),
        ("cancer",       1.53, multimorbidity_dict["MS_Cancer_snomed"], None, None),
        ("ckd",          0.53, multimorbidity_dict["MS_CKD_snomed"], None, None),
        ("constipation", 1.12, None, multimorbidity_dict["MS_Constipation_dmd"], None),
        ("copd",         1.46, multimorbidity_dict["MS_COPD_snomed"], None, None),
        ("ctd",          0.43, multimorbidity_dict["MS_CTD_snomed"], None, None),
        ("chd",          0.49, multimorbidity_dict["MS_CHD_snomed"], None, None),
        ("dementia",     2.50, multimorbidity_dict["MS_Dementia_snomed"], None, None),
        ("diabetes",     0.75, multimorbidity_dict["MS_Diabetes_snomed"], None, None),
        ("epilepsy",     0.92, multimorbidity_dict["MS_Epilepsy_snomed"], multimorbidity_dict["MS_Epilepsy_dmd"], None),
        ("hearing_loss", 0.09, multimorbidity_dict["MS_HL_snomed"], None, None),
        ("hf",           1.18, multimorbidity_dict["MS_HF_snomed"], None, None),
        ("hypertension", 0.08, multimorbidity_dict["MS_Hypertension_snomed"], None, None),
        ("ibs",          0.21, multimorbidity_dict["MS_IBS_snomed"], multimorbidity_dict["MS_IBS_dmd"], None),
        ("psychosis",    0.64, multimorbidity_dict["MS_Psychosis_snomed"], multimorbidity_dict["MS_Psychosis_dmd"], None),
        ("stroke",       0.80, multimorbidity_dict["MS_StrokeTIA_snomed"], None, None),
        ("pain",         0.92, None,
                        multimorbidity_dict["MS_PC_Analgesics_dmd"],
                        multimorbidity_dict["MS_PC_Antiepileptic_dmd"]),
    ]:

        binary = has_condition_before(
            index_date=index_date,
            death_date=death_date,
            snomed=snomed,
            dmd1=dmd1,
            dmd2=dmd2,
        ).as_int()

        cms += binary.as_float() * weight

        if return_components:
            components[name] = binary

    if return_components:
        components["cms"] = cms
        return components

    return cms