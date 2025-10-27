// Updated: 20230120 Enoch Chen

// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global path `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global path `"P:"'

// Set subpath
global subpath `"SweCan/Site_specific/Enoch_Study1"'

// Set the working directory as where the do files are
cd "${path}/${subpath}/Data"

// Log file
local todaydate: di %tdCYND date(c(current_date),"DMY")
global dofilename = "legend_rel" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace

/*========Code begins here========*/
clear all
global cancervar "colon"
global agegroupvar 0
local outcomevar "le40"

// Merge in one single dataframe
clear
dir  "${cancervar}_ac_*_le40.dta"
dir  "${cancervar}_rel_*_le40.dta"

append using `: dir . files "${cancervar}_ac_*_le40.dta"'
append using `: dir . files "${cancervar}_rel_*_le40.dta"'

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
lab var le40 "Life expectancy or RMST at 40 years"
lab var s "survival proportion at time _t"
lab var fw "Framework (all-cause or relative survival)"
lab var dist "Parametric distribution"
lab var exyr "Amount of timne (years) used for extrapolation"
lab var outcome "le40"
lab var cancer "Cancer type"

local exyr 5 
keep if exyr  == "`exyr'"

append using ${cancervar}_km.dta
keep if agegroup == ${agegroupvar} 

/*============Draw one all-cause hazard to determind yaxis scale=========*/
// All-cause azard
// Standard parametric models   
	// Survival
tw     (line km _t , sort lcol(black) lp(solid) lwidth(*1.5)) ///
	   (rarea ub lb _t , sort color(gray%20)) ///
	   (line s _t if fw == "ac" & dist == "exp" ,   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line s _t if fw == "ac" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line s _t if fw == "ac" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line s _t if fw == "ac" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line s _t if fw == "ac" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line s _t if fw == "ac" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line s _t if fw == "ac" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) ///
		(line s _t if fw == "ac" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
		(line s _t if fw == "ac" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
		(line s _t if method != "km" , sort lp(solid) lcol(purple%80) lwidth(*1.5)) , ///
	   legend(order(1 "K-M survival/smoothed hazard" 2 "95% CIs for K-M/smoothed hazard" 3 "Exponential" 4 "Weibull" 5 "Gompertz" 6 "Log-logistic" 7 "Log-normal" 8 "Generalized gamma" 9 "Spline hazard" 10 "Spline odds" 11 "Spline normal" 12 "Expected hazard (Ederer I)") rows(3) cols(4)) ///
	   scheme(s1mono) graphregion(color(white))
		
graph display, xsize(30) ysize(10)       

cd "${path}/${subpath}/Output/plots_supp/surv_haz/"

graph export "${dofilename}.png", as(png) replace width(5000)

log close
exit
