####################################################################
##Importing key modules, TPP tables
####################################################################

from ehrql import (
    create_dataset, 
    create_measures,
    codelist_from_csv,
    when, 
    years,
    months,
    weeks,
    days, 
    minimum_of, 
    case, 
    show,
    INTERVAL, 
    )

##Importing key TPP tables
from ehrql.tables.tpp import (
    patients, 
    practice_registrations,
    addresses, 
    appointments, 
    occupation_on_covid_vaccine_record,
    sgss_covid_all_tests,
    vaccinations,
    apcs, 
    ec, 
    clinical_events, 
    ons_deaths,
    emergency_care_attendances,
)

from ehrql.tables.core import medications