/*==============================================================================
FILENAME: diff.do

PROJECT: Enoch PhD Study 1 (Comparing survival extrapolation)
	
AUTHOR: Enoch Yi-Tung Chen (enoch.yitung.chen@ki.se)

CREATED: Enoch Yi-Tung Chen	
UPDATED: 20230819 Enoch Chen: Updated analysis for revised submission
		 20230228 Enoch Chen: Rerun the analysis with corrected expsurv (Ederer I)

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
global dofilename = "diff" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace

/*========Code begins here========*/
clear all

/*========For the empirical K-M curves,======================
==========look up the survival closest to t10 and t40========*/
/*========Define programs for reading the file========*/
cap prog drop caldiff_1
program define caldiff_1 `1' `2' `3'
	// Read the survival data extrapolated by parametric mods
	// For RMST at 10 years
	if "`3'" == "rmst10"{
		// Read the file
		use `1'_`2'_`3', clear
		keep if _t == 10
		// Keep certain vars
		local varlist method agegroup _t rmst10 s
		keep `varlist' 
		order `varlist'
	} 
	// For LE at 40 years
	else if "`3'" == "le40"{
		// Read the file
		use `1'_`2'_`3', clear
		keep if _t == 40
		// Keep certain vars
		local varlist method agegroup _t le40 s
		keep `varlist' 
		order `varlist'
	}
end

cap prog drop caldiff_2
program define caldiff_2 `1' `2' `3'
// For RMST at 10 years
	if "`3'" == "rmst10"{
		// calculate difference of RMST and surv at 10 years 
		bysort agegroup: gen diff_rmst10 = rmst10 - rmst10_km 
		bysort agegroup: gen diff_surv10 = 100 *(s - km)
		
		lab var diff_rmst10 "Difference (years) of RMST at 10 years (extrapolation-empirical)"
	    lab var diff_surv10 "Difference (%) of survival at 10 years (extrapolation-empirical)"
		save `1'_diff_`2'_`3', replace
	}
	// For LE at 40 years
	else if "`3'" == "le40"{
		// calculate difference of LE40 and surv at 40 years 
		bysort agegroup: gen diff_surv40 = 100 *(s - km) 
		bysort agegroup: gen diff_le40 = le40 - le40_km 
		
		lab var diff_le40 "Difference (years) of life expectancy at 40 years (extrapolation-empirical)"
	    lab var diff_surv40 "Difference (%) of survival at 40 years (extrapolation-empirical)"
		save `1'_diff_`2'_`3', replace
	}
end

/*-----------------------------------------------------*/
// Loop over all cancer types
local cancerlist "colon breast melanoma prostate cml"

foreach cancertype of local cancerlist {
	use `cancertype'_km, clear
	gen t10_diff = abs(_t - 10)
	gen t40_diff = abs(_t - 40)
	bysort agegroup: egen t10_min = min(t10_diff) if _t <= 10
	bysort agegroup: egen t40_min = min(t40_diff) if _t <= 40
	keep if t10_min == t10_diff | t40_min == t40_diff
	duplicates drop agegroup _t, force
	replace _t = 10 if t10_min != .
	replace _t = 40 if t10_min == .
	list agegroup _t km le40 rmst10

	// Keep and order selected variables
	local varlist agegroup _t le40 rmst10 km
	keep `varlist' 
	order `varlist'
	rename (le40 rmst10) (le40_km rmst10_km)
	sort agegroup _t 

	save "`cancertype'_km_new.dta", replace
}

foreach cancertype of local cancerlist {
	// Macro: framework distribution exit_year outcome
	// Standard parametric models and spline models
	local fwlist "ac rel" 
	local distlist "exp weib gomp loglog lognorm ggamma splhaz splnorm splodds" 
	local exyrlist 2 3 5 10

	foreach fw of local fwlist{                             // all-cause vs. rel
		foreach dist of local distlist{                     // param distributions
			foreach exyr of local exyrlist{                 // extrapolation year
					if `exyr' == 2 | `exyr' == 3 | `exyr' == 5 { 
						local outcome rmst10
						local i "`fw'_`dist'_exyr`exyr'"
						caldiff_1 `cancertype' "`i'" "`outcome'"
						// merge with KM
						merge 1:1 agegroup _t using "`cancertype'_km_new.dta", nogen
						keep if _t == 10
						caldiff_2 `cancertype' "`i'" "`outcome'"

						local outcome le40
						local i "`fw'_`dist'_exyr`exyr'"
						caldiff_1 `cancertype' "`i'" "`outcome'"
						// merge with KM
						merge 1:1 agegroup _t using "`cancertype'_km_new.dta", nogen
						keep if _t == 40
						caldiff_2 `cancertype' "`i'" "`outcome'"
					}
					else if `exyr' == 10 {
						local outcome le40
						local i "`fw'_`dist'_exyr`exyr'"
						caldiff_1 `cancertype' "`i'" "`outcome'"
						// merge with KM
						merge 1:1 agegroup _t using "`cancertype'_km_new.dta", nogen
						keep if _t == 40
						caldiff_2 `cancertype' "`i'" "`outcome'"
					}

				}
			}
		}

	// Merge all files in one single dataframe
	clear
	append using `: dir . files "`cancertype'_diff_*.dta"'

	// Generate variables entitled (fw dist exyr) to distinguish methods
	split method, parse(_)
	rename (method1 method2 method3 method4 method5)(ag fw dist exyr outcome)
	replace exyr = substr(exyr,5,.)
	gen cancer = "`cancertype'"

	// Label variables
	lab var agegroup "0 20-59; 1 60-69; 2 70-99"
	lab var le40 "Life expectancy or RMST at 40 years"
	lab var s "survival proportion at time _t"
	lab var le40_km "Empirical life expectancy or RMST at 40 years"
	lab var rmst10_km "Empirical RMST at 10 years"
	lab var km "Survival propostion of K-M curve at time _t"
	lab var rmst10 "RMST at 10 years"
	lab var fw "Framework (all-cause or relative survival)"
	lab var dist "Parametric distribution"
	lab var exyr "Amount of timne (years) used for extrapolation"
	lab var outcome "rmst10 or le40"
	lab var cancer "Cancer type"

	save "`cancertype'_diff.dta", replace
}
/***************************** FILE ENDS HERE**************************/
log close
exit


					  

