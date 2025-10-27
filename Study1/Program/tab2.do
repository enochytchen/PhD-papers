/*==============================================================================
FILENAME: tab2.do

PROJECT: Enoch PhD Study 1 (Comparing survival extrapolation)
	
AUTHOR: Enoch Yi-Tung Chen (enoch.yitung.chen@ki.se)

CREATED: Enoch Yi-Tung Chen	
UPDATED: 20230819 Enoch Chen: Updated analysis for revised submission
		 20230124 Enoch Chen: Updated due to refitting stpm2, scale(normal)

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
global dofilename = "tab2" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace

/*========Code begins here========*/
clear all

use results, clear

keep if outcome == "rmst10"
// rmst10
// Make the difference into absolute value
replace diff_rmst10 = abs(diff_rmst10)

// Flag if diff_rmst10 <= 0.1 years
egen flag_diff_rmst10 = cut(diff_rmst10), at(0 0.1) icodes
lab def flag_diff_rmst10 0 "diff_rmst10 <= 0.1" 
lab value flag_diff_rmst10 flag_diff_rmst10

bysort dist fw exyr: tabulate flag_diff_rmst10

// surv10
// Make the difference into absolute value
replace diff_surv10 = abs(diff_surv10)

// Flag if diff_surv10 <= 1 (1%)
egen flag_diff_surv10 = cut(diff_surv10), at(0 1) icodes
lab def flag_diff_surv10 0 "diff_surv10 <= 1 %" 
lab value flag_diff_surv10 flag_diff_surv10

bysort dist fw exyr: tabulate flag_diff_surv10


use results, clear

keep if outcome == "le40"

// le40
// Make the difference into absolute value
replace diff_le40 = abs(diff_le40)

// Flag if le40 <= =1 years
egen flag_diff_le40 = cut(diff_le40), at(0 1) icodes
lab def flag_diff_le40 0 "diff_le40 <= 1" 
lab value flag_diff_le40 flag_diff_le40

bysort dist fw exyr: tabulate flag_diff_le40

// surv40
// Make the difference into absolute value
replace diff_surv40 = abs(diff_surv40)

// Flag if diff_surv40 <= 1 (1%)
egen flag_diff_surv40 = cut(diff_surv40), at(0 1) icodes
lab def flag_diff_surv40 0 "diff_surv40 <= 1 %" 
lab value flag_diff_surv40 flag_diff_surv40

bysort dist fw exyr: tabulate flag_diff_surv40

log close
