cd "C:\Users\mz16609\OpenSAFELY_IWP\ImpactWinterPressures\local_data"
use workforce_master, clear
sort prac_code date_collect
duplicates drop
gen start_date = date("30sep2017", "DMY")
keep if date_collect>=start_date
gen month = month(date_collect)
gen season=3 if month==3
replace season=6 if month==6
replace season=9 if month==9
replace season=12 if month==12
drop if season==.
drop month
save workforce_season, replace

use workforce_season, clear 
gen gp_patient=total_gp_fte*100000/total_patients
collapse (mean) gp_patient, by(prac_code season)
twoway line gp_patient season


use workforce_season, clear 
gen nurse_patient=total_nurses_fte*100000/total_patients
collapse (mean) nurse_patient, by(prac_code season)
twoway line nurse_patient season


use workforce_season, clear 
gen dpc_patient=total_dpc_fte*100000/total_patients
collapse (mean) dpc_patient, by(prac_code season)
twoway line dpc_patient season