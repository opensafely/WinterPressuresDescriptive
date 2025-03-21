from module_table_imports import *

# Codelists from codelists.py (which pulls all variables from the codelist folder)
from codelists import *

# Call functions from variable_helper_functions
from variable_helper_functions import (
    ever_matching_event_clinical_ctv3_before,
    ever_matching_event_apc_between,
    ever_matching_event_ec_snomed_between,
    last_matching_event_clinical_ctv3_before,
    last_matching_event_clinical_snomed_before,
    last_matching_med_dmd_before,
    last_matching_event_apc_before,
    filter_codes_by_category,

)

# Define generate variables function
def generate_variables(interval_start, interval_end):  
    ## Inclusion/exclusion criteria-------------------------------------------------------------------------

    ### Registered throughout the study period and 90 days before for each cohort (for longitudinal measures, i.e. consultation rate/hospital admission)
    inex_bin_reg_long = (practice_registrations.spanning(
        interval_start - days(90), interval_end
    )).exists_for_patient()

    ### Consultation rate during follow-up of exposure
    tmp_exp_num_consrate = appointments.where(
        appointments.status.is_in([
            "Arrived",
            "In Progress",
            "Finished",
            "Visit",
            "Waiting",
            "Patient Walked Out",
            ]) & appointments.start_date.is_on_or_between(interval_start, interval_end)
            ).count_for_patient()    

    exp_num_consrate = case(
        when(tmp_exp_num_consrate <= 365).then(tmp_exp_num_consrate),
        otherwise=365,
    )

    ### Multimorbidity conditions (n=20)

    exp_bin_hypertension = (
        (last_matching_event_clinical_snomed_before(
            hypertension_snomed, interval_start
        ).exists_for_patient()) |
        (last_matching_med_dmd_before(
            hypertension_drugs_dmd, interval_start
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            hypertension_icd10, interval_start
        ).exists_for_patient())
    )

    exp_bin_anxdep = (
        (last_matching_event_clinical_snomed_before(
            multimorbidity_dict["MS_AnxietyDepression_snomed"], interval_start
        ).exists_for_patient())
    )

    exp_bin_asthma = (
        (last_matching_event_clinical_ctv3_before(
            multimorbidity_dict["MS_Asthma_ctv3"], interval_start
        ).exists_for_patient())
    )    

    ## Outcomes---------------------------------------------------------------------------------
    ### A&E attendance 
    out_num_ec = (
        ec.where(
            ec.arrival_date.is_on_or_between(interval_start, interval_end)
        ).count_for_patient()
    )
    ### Hospital admission
    out_num_apc = (
        apcs.where(
            apcs.admission_date.is_on_or_between(interval_start, interval_end)
        ).count_for_patient()
    )

    ### ACSC 
    out_num_asthma_ec = ever_matching_event_ec_snomed_between(
        asthma_snomed, interval_start, interval_end
    ).count_for_patient()

    out_num_asthma_apc = ever_matching_event_apc_between(
        asthma_icd10, interval_start, interval_end
    ).count_for_patient()

    dynamic_variables = dict(
        inex_bin_reg_long = inex_bin_reg_long,
        inex_bin_reg_cs = inex_bin_reg_cs,
        inex_bin_alive = inex_bin_alive,
        inex_bin_age = inex_bin_age,
        inex_bin_sex = inex_bin_sex,
        inex_bin_ethinicity = inex_bin_ethinicity,
        inex_bin_imd = inex_bin_imd,
        inex_bin_region = inex_bin_region,
        practice_id = practice_id,
        exp_bin_female = exp_bin_female,
        exp_bin_under_5y = exp_bin_under_5y,
        exp_bin_5_16y = exp_bin_5_16y,
        exp_bin_65_74y = exp_bin_65_74y,
        exp_bin_75_84y = exp_bin_75_84y,
        exp_bin_85y_plus = exp_bin_85y_plus,
        exp_num_consrate = exp_num_consrate,
        out_num_apc =out_num_apc,
        out_num_asthma_apc = out_num_asthma_apc,
    )
    return dynamic_variables

