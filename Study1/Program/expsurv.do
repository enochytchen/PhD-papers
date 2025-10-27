/*==============================================================================
FILENAME: expsurv.do

PROJECT: Enoch PhD Study 1 (Comparing survival extrapolation)
	
AUTHOR: Enoch Yi-Tung Chen (enoch.yitung.chen@ki.se)

CREATED: Enoch Yi-Tung Chen	
UPDATED: 20230818 Enoch Chen: make expsurv by agegroup
	     20230228 Enoch Chen: Fixed the expected survival estimator from Ederer II to I

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
global dofilename = "expsurv" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace

/*========Code begins here========*/
clear all

// Loop over all cancer types
local cancerlist "colon breast melanoma prostate cml"

foreach cancertype of local cancerlist {
	
//Use study pop dx during 1981-1990
use "${path}/${subpath}/Data/`cancertype'.dta", clear

// Make individual to be alive for 40 years
replace stime=40
replace dead=0

// All-cause death as outcome
stset stime, failure(dead==1) exit(time 40) id(lpnr)  

// Check the popmort file 
preserve 
	use ${path}/SweCan/Popmort/popmort2020, clear
restore

// Use strs to predict expected survival within each age group
// Using Ederer 1 
strs using "${path}/SweCan/Popmort/popmort2020", ///
	breaks(0(`=1/12')40) ///
	diagage(agediag) diagyear(yeardiag) ///
	mergeby(_year  sex  _age) by(agegroup) ///
	ederer1 ///
	savgroup(${path}/${subpath}/Data/`cancertype'_expected, replace)

use "${path}/${subpath}/Data/`cancertype'_expected", clear

// Add observations for time 0
expand 2 if start==0, generate(expandflag)
replace end=0 if expandflag==1
replace cp_e1=1 if expandflag==1

// Change variable name to merge with model-based predictions later
rename end _t 
keep cp_e1 agegroup _t
sort agegroup _t
compress
save `cancertype'_expsurv, replace

// Plot to check expected survival
twoway (line cp_e1 _t if agegroup==0, sort lcolor(brown)) ///
	   (line cp_e1 _t if agegroup==1, sort lcolor(blue)) ///
       (line cp_e1 _t if agegroup==2, sort lcolor(black)), ///
	    ylabel(0(0.2)1, format(%5.1f)) name(cp_e1, replace) ///
	    legend(order(1 "Age 18-59" 2 "Age 60-69" 3 "Age 70-99") ring(0) pos(1) col(1)) ///
	   ytitle("Expected survival (Ederer I)", size(*1.0)) ///
	   xtitle("Time since diagnosis (years)", size(*1.0))
}	   
/***************************** FILE ENDS HERE**************************/
log close
exit	   

	  	   