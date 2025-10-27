/*==============================================================================
FILENAME: breast_desc_analysis.do

PROJECT: Enoch PhD Study 1 (Comparing survival extrapolation)
	
AUTHOR: Enoch Yi-Tung Chen (enoch.yitung.chen@ki.se)

CREATED: Enoch Yi-Tung Chen	
UPDATED: 20230821 Enoch Chen: updated descriptive analysis based on reviewer'comments

STATA VERSION: 17.0
==============================================================================*/

// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global path `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global path `"P:"'

// Set subpath
global subpath `"SweCan/Site_specific/Enoch_Study1"'

// Set the working directory as where the do files are
cd "${path}/${subpath}/Program"

// Log file
local todaydate: di %tdCYND date(c(current_date),"DMY")
global dofilename = "breast_desc_analysis" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace

/*========Code begins here========*/
clear all

// Read breast dx 1981-1990
use "${path}/${subpath}/Data/breast.dta", clear

// Caculate censoring proportion at year 2 3 5 10
local yearlist 2 3 5 10 

foreach y of local yearlist{
	stset stime, failure(dead==1) exit(time `y') id(lpnr)
	bysort agegroup: egen cen_`y' = mean(100*(_d == 0))
}

// Survival proportion at year 10
stset stime, failure(dead==1) exit(time 10) id(lpnr)
sts list, by(agegroup) tmax(10) compare

local surv_10_age0 = 0.6744
local surv_10_age1 = 0.6044
local surv_10_age2 = 0.2878

// In percentage
gen surv_10 = . 
forvalues i = 0/2{
replace surv_10 = 100*`surv_10_age`i'' if agegroup == `i'
}

// RMST at year 10
stset stime, failure(dead==1) id(lpnr)
gen rmst = .
strmst2 agegroup, tau(10) reference(0)
forvalues i = 0/2{
	// Display matrix
	matrix list r(rmstarm`i')
	// Local values
	local rmst`i' r(rmstarm`i')[1,1] // rmst is stored at the matrix 
	di `rmst`i''
	replace rmst = `rmst`i'' if agegroup == `i'
}

// Survival proportion at year 40
stset stime, failure(dead==1) exit(time 40) id(lpnr)
sts list, by(agegroup) tmax(40) risktable(2 3 5 10 20 30 37 38 39 40)

local surv_40_age0 = 0.1936
local surv_40_age1 = 0
local surv_40_age2 = 0

// In percentage
gen surv_40 = . 
forvalues i = 0/2{
replace surv_40 = 100*`surv_40_age`i'' if agegroup == `i'
}

// Plot K-M curve first to see whether all achieve LE or not
sts graph, by(agegroup)

// Life expectancy at year 40
// Estimated by AUC under the KM survival curve
// mean survival time restricted to longest fu time
gen le = .
forvalues i =0/2{
	stci if agegroup == `i', rmean
	local le`i' r(rmean)
	di `le`i''
	replace le = `le`i'' if agegroup == `i'
}

// Table 1
table (agegroup) (),     ///
stat(frequency)       	 /// 
stat(mean cen_2 cen_3 cen_5 cen_10 surv_10 surv_40) ///
stat(median agediag) ///
nformat(%4.2f) 

table (agegroup) (),     ///
stat(mean rmst) ///
stat(mean le) ///
nformat(%4.2f) 
/***************************** FILE ENDS HERE**************************/
log close
exit
