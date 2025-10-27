/*==============================================================================
FILENAME: tab_supp.do

PROJECT: Enoch PhD Study 1 (Comparing survival extrapolation)
	
AUTHOR: Enoch Yi-Tung Chen (enoch.yitung.chen@ki.se)

CREATED: Enoch Yi-Tung Chen	
UPDATED: 

STATA VERSION: 17.0
==============================================================================*/

// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global path `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global path `"P:"'

// Set subpath
global subpath `"SweCan/Site_specific/Enoch_Study1"'

// Set the working directory as where the do files are
cd "${path}/${subpath}/Data"

// Log file
local todaydate: di %tdCYND date(c(current_date),"DMY")
global dofilename = "tab_supp" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace


/*========Code begins here========*/
clear all

use results, clear

de

// Make survival into %
replace s = 100*s
replace km = 100*km

// format the number
local varlist s km rmst10 rmst10_km le40 le40_km diff_rmst10 diff_le40 diff_surv10 diff_surv40
foreach var of local varlist{
	format `var' %9.2f
}

frame copy default rmst10
// RMST 10 
frame rmst10 {
	keep if _t == 10
	drop method le40 diff_surv40 diff_le40 _t out

	keep exyr agegroup fw cancer dist_nu rmst10_km diff_rmst10 
	sort exyr fw cancer agegroup dist_nu
	order exyr agegroup fw cancer dist_nu rmst10_km diff_rmst10 
	export excel using "../Output/plots_supp/tab_rmst10.xls", firstrow(variables) replace
}

frame copy default surv10
// Surv10
frame surv10{
	keep if _t == 10
	drop method le40 diff_surv40 diff_le40 _t out
	
	keep exyr agegroup fw cancer dist_nu km diff_surv10 
	sort exyr fw cancer agegroup dist_nu
	order exyr agegroup fw cancer dist_nu km diff_surv10 
	export excel using "../Output/plots_supp/tab_surv10.xls", firstrow(variables) replace
}

frame copy default le40, replace
// LE40
frame le40{
	keep if _t == 40
	drop method rmst10 diff_surv10 diff_rmst10 _t out
	
	keep exyr agegroup fw cancer dist_nu le40_km diff_le40
	sort exyr fw cancer agegroup dist_nu
	order exyr agegroup fw cancer dist_nu le40_km diff_le40
	export excel using "../Output/plots_supp/tab_le40.xls", firstrow(variables) replace
}

frame copy default surv40
// Surv40
frame surv40{
	keep if _t == 40
	drop method rmst10 diff_surv10 diff_rmst10 _t out
	
	keep exyr agegroup fw cancer dist_nu km diff_surv40
	sort exyr fw cancer agegroup dist_nu
	order exyr agegroup fw cancer dist_nu km diff_surv40
	export excel using "../Output/plots_supp/tab_surv40.xls", firstrow(variables) replace
}
