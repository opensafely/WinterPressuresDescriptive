cd "C:\Users\mz16609\OpenSAFELY_IWP\ImpactWinterPressures\local_data"
use workforce_master, clear
gen gp_patient=total_gp_fte*100000/total_patients
gen nurse_patient=total_nurses_fte*100000/total_patients
gen dpc_patient=total_dpc_fte*100000/total_patients
drop if gp_patient==.
drop if nurse_patient==.
drop if dpc_patient==.
sort prac_code date_collect
duplicates drop
gen start_date = date("30sep2017", "DMY")
keep if date_collect>=start_date
gen month = month(date_collect)
keep if month==3 | month==6 |month==9 |month==12
* Generate a variable to hold the corresponding number
gen time_num = .
* Replace the dates with corresponding numbers
replace time_num = 1 if date_collect == date("30sep2017", "DMY")
replace time_num = 2 if date_collect == date("31dec2017", "DMY")
replace time_num = 3 if date_collect == date("31mar2018", "DMY")
replace time_num = 4 if date_collect == date("30jun2018", "DMY")
replace time_num = 5 if date_collect == date("30sep2018", "DMY")
replace time_num = 6 if date_collect == date("31dec2018", "DMY")
replace time_num = 7 if date_collect == date("31mar2019", "DMY")
replace time_num = 8 if date_collect == date("30jun2019", "DMY")
replace time_num = 9 if date_collect == date("30sep2019", "DMY")
replace time_num = 10 if date_collect == date("31dec2019", "DMY")
replace time_num = 11 if date_collect == date("31mar2020", "DMY")
replace time_num = 12 if date_collect == date("30jun2020", "DMY")
replace time_num = 13 if date_collect == date("30sep2020", "DMY")
replace time_num = 14 if date_collect == date("31dec2020", "DMY")
replace time_num = 15 if date_collect == date("31mar2021", "DMY")
replace time_num = 16 if date_collect == date("30jun2021", "DMY")
replace time_num = 17 if date_collect == date("30sep2021", "DMY")
replace time_num = 18 if date_collect == date("31dec2021", "DMY")
replace time_num = 19 if date_collect == date("31mar2022", "DMY")
replace time_num = 20 if date_collect == date("30jun2022", "DMY")
replace time_num = 21 if date_collect == date("30sep2022", "DMY")
replace time_num = 22 if date_collect == date("31dec2022", "DMY")
replace time_num = 23 if date_collect == date("31mar2023", "DMY")
replace time_num = 24 if date_collect == date("30jun2023", "DMY")
replace time_num = 25 if date_collect == date("30sep2023", "DMY")
replace time_num = 26 if date_collect == date("31dec2023", "DMY")
replace time_num = 27 if date_collect == date("31mar2024", "DMY")
* Define labels for the time_num values
label define time_num_label ///
    1 "30sep2017" ///
    2 "31dec2017" ///
    3 "31mar2018" ///
    4 "30jun2018" ///
    5 "30sep2018" ///
    6 "31dec2018" ///
    7 "31mar2019" ///
    8 "30jun2019" ///
    9 "30sep2019" ///
    10 "31dec2019" ///
    11 "31mar2020" ///
    12 "30jun2020" ///
    13 "30sep2020" ///
    14 "31dec2020" ///
    15 "31mar2021" ///
    16 "30jun2021" ///
    17 "30sep2021" ///
    18 "31dec2021" ///
    19 "31mar2022" ///
    20 "30jun2022" ///
    21 "30sep2022" ///
    22 "31dec2022" ///
    23 "31mar2023" ///
    24 "30jun2023" ///
    25 "30sep2023" ///
    26 "31dec2023" ///
    27 "31mar2024"

* Apply the labels to the time_num variable
label values time_num time_num_label
drop month start_date
save workforce_quar, replace

use workforce_quar, clear 
*Generate a unique identifier for each practice
bysort prac_code: gen unique_id = _n == 1
*Create a list of unique IDs
levelsof prac_code if unique_id, local(id_list)
* Determine the number of IDs to sample (e.g., o.5% of the IDs, n=37; 5% of the IDs, n=362)
local num_ids = ceil(0.05 * `: word count of `id_list'')
* Randomly sample IDs
tempfile sampled_ids
preserve
keep if unique_id
bsample `num_ids'
keep prac_code
save `sampled_ids', replace
restore
* Keep only the selected IDs
joinby prac_code using `sampled_ids'
save workforce_subsample, replace


* Plot all selected IDs' trajectories in the same graph
**# Plot GP**
*Create the plot command
use workforce_subsample, clear
drop if gp_patient>200
* Extract unique time points
preserve
keep date_collect
duplicates drop
sort date_collect
list date_collect
local timepoints ""
foreach t in `r(timepoints)' {
    local timepoints `timepoints' `t'
}
restore


local plotcmd = ""
* Loop through each selected ID to add its line to the plot
levelsof prac_code, local(selected_ids)
foreach id of local selected_ids {
	local plotcmd `plotcmd' (line gp_patient time_num if prac_code == "`id'")
	}
* Execute the plot command
twoway `plotcmd', title("Subsample of Practices' Trajectories") xtitle("Time") ytitle("FTE(GP) per 100,000 patients") legend(off) xlabel(1 "09/17" 3 "03/18" 5 "09/18" 7 "03/19" 9 "09/19" 11 "03/20" 13 "09/20" 15 "03/21" 17 "09/21" 19 "03/22" 21 "09/22" 23 "03/23" 25 "09/23" 27 "03/24" )


**# Plot Nurse**
*Create the plot command
use workforce_subsample, clear
drop if nurse_patient>200
local plotcmd = ""
* Loop through each selected ID to add its line to the plot
levelsof prac_code, local(selected_ids)
foreach id of local selected_ids {
	local plotcmd `plotcmd' (line nurse_patient time_num if prac_code == "`id'")
	}
* Execute the plot command
twoway `plotcmd', title("Subsample of Practices' Trajectories") xtitle("Time") ytitle("FTE(Nurse) per 100,000 patients") legend(off) xlabel(1 "09/17" 3 "03/18" 5 "09/18" 7 "03/19" 9 "09/19" 11 "03/20" 13 "09/20" 15 "03/21" 17 "09/21" 19 "03/22" 21 "09/22" 23 "03/23" 25 "09/23" 27 "03/24" )

**# Plot DPC**
*Create the plot command
use workforce_subsample, clear
drop if dpc_patient>200
local plotcmd = ""
* Loop through each selected ID to add its line to the plot
levelsof prac_code, local(selected_ids)
foreach id of local selected_ids {
	local plotcmd `plotcmd' (line dpc_patient time_num if prac_code == "`id'")
	}
* Execute the plot command
twoway `plotcmd', title("Subsample of Practices' Trajectories") xtitle("Time") ytitle("FTE(DPC) per 100,000 patients") legend(off) xlabel(1 "09/17" 3 "03/18" 5 "09/18" 7 "03/19" 9 "09/19" 11 "03/20" 13 "09/20" 15 "03/21" 17 "09/21" 19 "03/22" 21 "09/22" 23 "03/23" 25 "09/23" 27 "03/24" )



**Use R to visulise all the trajectories 
use workforce_quar, clear 
export delimited "workforce_quar.csv", replace
