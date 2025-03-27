from module_table_imports import *

# Codelists from codelists.py (which pulls all variables from the codelist folder)
from codelists import *

# Call functions from variable_helper_functions
from variable_helper_functions import (
    ever_matching_event_apc_between,
    ever_matching_event_ec_snomed_between,
)

# Define generate variables function
def generate_variables(interval_start, interval_end):  
    ## Inclusion/exclusion criteria-------------------------------------------------------------------------

    ### Registered throughout the study period (for longitudinal measures, i.e. consultation rate/hospital admission)
    inex_bin_reg_long = (practice_registrations.spanning(
        interval_start, interval_end
    )).exists_for_patient()

    ## Exposure---------------------------------------------------------------------------------------------

    ###  Consultation rate during follow-up of exposure
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

    ## Outcomes----------------------------------------------------------------------------------------------
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
    ### COPD
    out_num_copd_ec = ever_matching_event_ec_snomed_between(
        multimorbidity_dict["MS_COPD_snomed"], interval_start, interval_end
    ).count_for_patient()

    out_num_copd_apc = ever_matching_event_apc_between(
        copd_icd10, interval_start, interval_end
    ).count_for_patient()

    ### Asthma
    out_num_asthma_ec = ever_matching_event_ec_snomed_between(
        asthma_snomed, interval_start, interval_end
    ).count_for_patient()

    out_num_asthma_apc = ever_matching_event_apc_between(
        asthma_icd10, interval_start, interval_end
    ).count_for_patient()

    ### Hypertension
    out_num_hypertension_ec = ever_matching_event_ec_snomed_between(
        hypertension_snomed, interval_start, interval_end
    ).count_for_patient()

    out_num_hypertension_apc = ever_matching_event_apc_between(
        hypertension_icd10, interval_start, interval_end
    ).count_for_patient()

    ### Diabetes
    out_num_diabetes_ec = ever_matching_event_ec_snomed_between(
        diabetes_snomed, interval_start, interval_end
    ).count_for_patient()

    out_num_diabetes_apc = ever_matching_event_apc_between(
        diabetes_icd10, interval_start, interval_end
    ).count_for_patient()

    ### Angina
    out_num_angina_ec = ever_matching_event_ec_snomed_between(
        angina_snomed, interval_start, interval_end
    ).count_for_patient()

    out_num_angina_apc = ever_matching_event_apc_between(
        angina_icd10, interval_start, interval_end
    ).count_for_patient()

    dynamic_variables = dict(
        inex_bin_reg_long = inex_bin_reg_long,
        exp_num_consrate = exp_num_consrate,
        out_num_ec =out_num_ec,
        out_num_apc =out_num_apc,
        out_num_copd_ec = out_num_copd_ec,                   # COPD (EC)
        out_num_copd_apc = out_num_copd_apc,                 # COPD (APC)
        out_num_asthma_ec = out_num_asthma_ec,               # Asthma (EC)
        out_num_asthma_apc = out_num_asthma_apc,             # Asthma (APC)
        out_num_hypertension_ec = out_num_hypertension_ec,   # Hypertension (EC)
        out_num_hypertension_apc = out_num_hypertension_apc, # Hypertension (APC)
        out_num_diabetes_ec = out_num_diabetes_ec,           # Diabetes (EC)
        out_num_diabetes_apc = out_num_diabetes_apc,         # Diabetes (APC)
        out_num_angina_ec = out_num_angina_ec,               # Angina (EC)
        out_num_angina_apc = out_num_angina_apc              # Angina (APC)
    )
    return dynamic_variables

