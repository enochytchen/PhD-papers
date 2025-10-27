// Updated: 20230127 Enoch Chen

// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global path `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global path `"P:"'

// Set subpath
global subpath `"SweCan/Site_specific/Enoch_Study1"'

// Set the working directory as where the do files are
cd "${path}/${subpath}/Data"

// Log file
local todaydate: di %tdCYND date(c(current_date),"DMY")
global dofilename = "example_surv_breast" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace

/*========Code begins here========*/
clear all
global cancervar "breast"
global agegroupvar 1
local outcomevar "rmst10"

// Merge in one single dataframe
clear
dir  "${cancervar}8190_ac_*_rmst10.dta"
dir  "${cancervar}8190_rel_*_rmst10.dta"

append using `: dir . files "${cancervar}8190_ac_*_rmst10.dta"'
append using `: dir . files "${cancervar}8190_rel_*_rmst10.dta"'

// Multiply all-cause survival by 100 to show percentage
replace s = s * 100
replace s_lci = s_lci * 100
replace s_uci = s_uci * 100 

// Generate variables entitled (fw dist exyr) to distinguish methods
split method, parse(_)
rename (method1 method2 method3 method4)(fw dist exyr outcome)
replace exyr = substr(exyr,5,.)

// Label variables
lab var agegroup "0 20-59; 1 60-69; 2 70-99"
lab var rmst10 "10-year RMST"
lab var s "survival proportion at time _t"
lab var fw "Framework (all-cause or relative survival)"
lab var dist "Parametric distribution"
lab var exyr "Amount of timne (years) used for extrapolation"
lab var outcome "rmst10"
lab var cancer "Cancer type"

save ${dofilename}, replace

/*============Extrapolation from 3 years===========*/
di "${dofilename}"
cd "${path}/${subpath}/Data"

use ${dofilename}, clear
local exyr 3 
keep if exyr  == "`exyr'"

append using ${cancervar}8190_km.dta
keep if agegroup == ${agegroupvar} 
keep if _t <= 10

gen s0 = 0
/*============Draw one all-cause hazard to determind yaxis scale=========*/
// All-cause azard
	local exyr 3
	   
/*============All-cause survival===========*/
// Save temp graphs
cd "${path}/${subpath}/Output/plots_supp/surv_haz/temp/"

// Flexible parametric models
	// Survival   
	 local exyr 3
	 tw (rarea km_100 s0 _t, sort col(gs12%20)) ///
	 (rarea s s0 _t if fw == "rel" & dist == "loglog", sort col(dkorange%20)) ///
	 (line s _t if fw == "rel" & dist == "loglog" & _t <= 3,   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) ///
	  (line s _t if fw == "rel" & dist == "loglog" & _t > 3,   sort lp(dash) lcol(dkorange%80) lwidth(*1.5)) ///
	 (line km_100 _t , sort lcol(black) lp(solid) lwidth(*1.5)), ///
	 xline(`exyr', lcol(black) lp(dash)) ///
	 xline(10, lcol(black) lp(dash)) ///
	    xscale(range(0 40)) xlab(`exyr' 10 20 30 40, labsize(medium)) xtick(0(5)40) xmtick(0(1)40) ///
		ylab(0(20)100, nogrid labsize(medium)) ///
		ytitle("Survival proportion (%)") ///
	    xtitle("Time since diagnosis (years)") /// 
		title("10-year survial") ///
		legend(order(3 "Fitted" 4 "Extrapolated" 5 "Observed K-M") position(0) bplacement(neast) rows(3)) scheme(s1mono) 
    
graph export "example_surv_breast.png", as(png) replace width(5000)

log close
exit
