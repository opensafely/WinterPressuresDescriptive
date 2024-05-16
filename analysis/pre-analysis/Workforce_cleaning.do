cd "C:\Users\mz16609\OneDrive - University of Bristol\Documents - grp-EHR\Projects\ImpactWinterPressures\GP workforce_NHS dig"
local csvfiles : dir . files "*.csv"
foreach file in `csvfiles' {
	import delimited "`file'", varnames(1) clear
	local noextension=subinstr("`file'",".csv","",.)
	keep prac_code total_patients total_gp_hc total_gp_fte total_nurses_hc total_nurses_fte total_dpc_hc total_dpc_fte
	save "`noextension'", replace
}

local dtafiles : dir "C:\Users\mz16609\OneDrive - University of Bristol\Documents - grp-EHR\Projects\ImpactWinterPressures\GP workforce_NHS dig" files "2*.dta"
qui foreach file in `dtafiles' {
	use "`file'", clear
	local date=subinstr("`file'",".dta","",.)
	gen date="`date'"
	destring total_patients total_gp_hc total_gp_fte total_nurses_hc total_nurses_fte total_dpc_hc total_dpc_fte, replace ignore("NAND")
	format %15.0f total_patients total_gp_hc total_nurses_hc total_dpc_hc 
	format %15.2f total_gp_fte total_nurses_fte total_dpc_fte
	save "`date'_date", replace 
}

local files: dir "C:\Users\mz16609\OneDrive - University of Bristol\Documents - grp-EHR\Projects\ImpactWinterPressures\GP workforce_NHS dig" files "*date.dta"
	qui foreach i in `files' {
		append using "`i'"
		rm "`i'"
	}
	gen date_collect=date(date, "YMD")
	format date_collect %td
	drop date
	save workforce_master, replace
	
	
use workforce_master, clear