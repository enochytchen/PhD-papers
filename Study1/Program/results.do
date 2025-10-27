/*==============================================================================
FILENAME: results.do

PROJECT: Enoch PhD Study 1 (Comparing survival extrapolation)
	
AUTHOR: Enoch Yi-Tung Chen (enoch.yitung.chen@ki.se)

CREATED: Enoch Yi-Tung Chen	
UPDATED: 20230819 Enoch Chen: Updated analysis for revised submission

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
global dofilename = "results" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace

/*========Code begins here========*/
clear all

// Merge all cancer sites in one single dataframe
clear
append using `: dir . files "*_diff.dta"'

// Generate distribution order for organising the boxplots (Figure)
gen dist_nu = 1 if dist == "exp"
replace  dist_nu = 2 if dist == "weib"
replace  dist_nu = 3 if dist == "gomp"
replace  dist_nu = 4 if dist == "loglog"
replace  dist_nu = 5 if dist == "lognorm"
replace  dist_nu = 6 if dist == "ggamma"
replace  dist_nu = 7 if dist == "splhaz"
replace  dist_nu = 8 if dist == "splodds"
replace  dist_nu = 9 if dist == "splnorm"

lab def dist_nu 1 "Exponential" 2 "Weibull" 3 "Gompertz" 4 "Log-logistic" 5 "Log-normal" 6 "Generalized gamma" 7 "Spline hazard" 8 "Spline odds" 9 "Spline normal" 
lab val dist_nu dist_nu

tab dist_nu

// Generate cohort order for organising the dotplots (Figure)
/*---------------rmst10---------------*/
gen cohort_nu = 1 if cancer == "colon" & agegroup == 0 
replace cohort_nu = 2 if cancer == "breast" & agegroup == 0 
replace cohort_nu = 3 if cancer == "melanoma" & agegroup == 0
replace cohort_nu = 4 if cancer == "prostate" & agegroup == 0 
replace cohort_nu = 5 if cancer == "cml" & agegroup == 0 

replace cohort_nu = 6 if cancer == "colon" & agegroup == 1 
replace cohort_nu = 7 if cancer == "breast" & agegroup == 1
replace cohort_nu = 8 if cancer == "melanoma" & agegroup == 1
replace cohort_nu = 9 if cancer == "prostate" & agegroup == 1 
replace cohort_nu = 10 if cancer == "cml" & agegroup == 1 

replace cohort_nu = 11 if cancer == "colon" & agegroup == 2 
replace cohort_nu = 12 if cancer == "breast" & agegroup == 2 
replace cohort_nu = 13 if cancer == "melanoma" & agegroup == 2
replace cohort_nu = 14 if cancer == "prostate" & agegroup == 2 
replace cohort_nu = 15 if cancer == "cml" & agegroup == 2 


lab def cohort_nu 1 "Colon" 2 "Breast" 3 "Melanoma" 4 "Prostate" 5 "CML" 6 "Colon" 7 "Breast" 8 "Melanoma" 9 "Prostate" 10 "CML" 11 "Colon" 12 "Breast" 13 "Melanoma" 14 "Prostate" 15 "CML"

lab val cohort_nu cohort_nu

tab cohort_nu

save results, replace
/***************************** FILE ENDS HERE**************************/
log close 
exit
