from module_table_imports import *
from codelists import *

# Call functions from variable_helper_functions
from variable_helper_functions import (
    ever_matching_event_apc_between,
    ever_matching_event_ec_snomed_between,
)

# Define generate variables function
def generate_variables(interval_start, interval_end, start_cohort):  
    ## Inclusion/exclusion criteria-------------------------------------------------------------------------

    ### Registered throughout the study period (for longitudinal measures, i.e. consultation rate/hospital admission)
    inex_bin_reg_long = (
        practice_registrations.spanning_with_systmone(
        interval_start, interval_end
        ).where(
            practice_registrations.practice_systmone_go_live_date <= start_cohort
            )
            .exists_for_patient()
    )
    
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

    ### Vaccination against flu in the last 12 months
    exp_bin_vax = {}
    for disease in ['INFLUENZA', 'SARS-2 CORONAVIRUS', 'PNEUMOCOCCAL']:
        exp_bin_vax[disease] = (vaccinations.where((vaccinations
                                            .target_disease
                                            .is_in([disease])) &
                                            vaccinations
                                            .date
                                            .is_on_or_between(interval_start, interval_end))
                                            .exists_for_patient())

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
    ### Unplanned hospital admission
    out_num_apc_unplanned = (
        apcs.where(
            (apcs.admission_date.is_on_or_between(interval_start, interval_end)) &
            (apcs.admission_method.is_in(["21", "22", "23", "24", "25", "2A", "2B", "2D", "28"]))
        ).count_for_patient()
    )
    ### Planned hospital admission
    out_num_apc_planned = (
        apcs.where(
            (apcs.admission_date.is_on_or_between(interval_start, interval_end)) &
            (apcs.admission_method.is_in(["11", "12", "13", "81", "2C", "82", "83", "31", "32"]))
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

    ### Hospital admissions  - all ACSC conditions defined above
    out_num_acsc_apc = ever_matching_event_apc_between(
        copd_icd10 + asthma_icd10 + hypertension_icd10 + diabetes_icd10 + angina_icd10, interval_start, interval_end
    ).count_for_patient()

    ### Unplanned hospital admissions - all ACSC conditions defined above
    out_num_acsc_apc_unplanned = ever_matching_event_apc_between(
        copd_icd10 + asthma_icd10 + hypertension_icd10 + diabetes_icd10 + angina_icd10, interval_start, interval_end
    ).where(apcs.admission_method.is_in(["21", "22", "23", "24", "25", "2A", "2B", "2D", "28"])).count_for_patient()
    ### Planned hospital admissions - all ACSC conditions defined above
    out_num_acsc_apc_planned = ever_matching_event_apc_between(
        copd_icd10 + asthma_icd10 + hypertension_icd10 + diabetes_icd10 + angina_icd10, interval_start, interval_end
    ).where(apcs.admission_method.is_in(["11", "12", "13", "81", "2C", "82", "83", "31", "32"])).count_for_patient()

    ### A&E attendances - all ACSC conditions defined above
    out_num_acsc_ec = ever_matching_event_ec_snomed_between(
        multimorbidity_dict["MS_COPD_snomed"] + asthma_snomed + hypertension_snomed + diabetes_snomed + angina_snomed, interval_start, interval_end
    ).count_for_patient()

    dynamic_variables = dict(
        inex_bin_reg_long = inex_bin_reg_long,
        exp_num_consrate = exp_num_consrate,
        exp_bin_vax_flu = exp_bin_vax["INFLUENZA"],
        exp_bin_vax_covid = exp_bin_vax["SARS-2 CORONAVIRUS"],
        exp_bin_vax_pneumo = exp_bin_vax["PNEUMOCOCCAL"],
        out_num_ec =out_num_ec,
        out_num_apc =out_num_apc,
        out_num_apc_unplanned = out_num_apc_unplanned,
        out_num_apc_planned = out_num_apc_planned,
        out_num_acsc_apc = out_num_acsc_apc,                     # ACSC (APC)
        out_num_acsc_apc_unplanned = out_num_acsc_apc_unplanned, # ACSC unplanned (APC)
        out_num_acsc_apc_planned = out_num_acsc_apc_planned,     # ACSC planned (APC)
        out_num_acsc_ec = out_num_acsc_ec,                       # ACSC (EC)
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

