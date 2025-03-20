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
    clinical_events, 
    ons_deaths,
    ec,
    addresses, 
    appointments, 
    occupation_on_covid_vaccine_record,
    sgss_covid_all_tests,
    apcs, 
)

from ehrql.tables.core import medications