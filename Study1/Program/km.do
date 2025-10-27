/*==============================================================================
FILENAME: km.do

PROJECT: Enoch PhD Study 1 (Comparing survival extrapolation)
	
AUTHOR: Enoch Yi-Tung Chen (enoch.yitung.chen@ki.se)

CREATED: Enoch Yi-Tung Chen	
UPDATED: 20230819 Enoch Chen: Updated analysis for revised submission
    	 20230307 Enoch Chen: Obtain empirical expected hazard from all-cause minus excess hazard

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
global dofilename = "km" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace

/*========Code begins here========*/
local cancerlist "colon breast melanoma prostate cml"

foreach cancertype of local cancerlist {
	
 // Just for the title later
 if "`cancertype'" == "colon"{
	local cancertype_title "Colon"
 }
 else if "`cancertype'" == "breast"{
	local cancertype_title "Breast"
 } 
 else if "`cancertype'" == "melanoma"{
	local cancertype_title "Melanoma"
 }
 else if "`cancertype'" == "prostate"{
	local cancertype_title "Prostate"
 }
 else if "`cancertype'" == "cml"{
	local cancertype_title "CML"
 }
 

clear all

// Read study population dx 1981-1990
use "`cancertype'.dta", clear

//stset using full follow-up
stset stime, failure(dead==1) exit(time 40) id(lpnr)

sts graph, survival risktable(0 2 3 5 10 20 30 40) by(agegroup)
graph display, xsize(30) ysize(10)

cd "${path}/${subpath}/Output/plots_km"
graph export "`cancertype'_km_risktab.png", as(png) replace width(5000)

// generate K-M and  95% CI
sts generate km=s, by(agegroup)
sts gen ub=ub(s), by(agegroup)
sts gen lb=lb(s), by(agegroup)

// Mutiply by 100 for showing percentage
gen km_100 = km * 100
gen ub_100 = ub * 100
gen lb_100 = lb * 100

// Plot legend
tw (line km _t if agegroup == 0, sort lcol(navy) lp(solid)) ///
   (rarea ub lb _t if agegroup == 0, sort color(gray%20)) ///
   (line km _t if agegroup == 1, sort lcol(maroon) lp(longdash)) ///
   (rarea ub lb _t if agegroup == 1, sort color(gray%20)) ///
   (line km _t if agegroup == 2, sort lcol(dkgreen) lp(shortdash)) ///
   (rarea ub lb _t if agegroup == 2, sort color(gray%20)), ///
   legend(order(1 "Age 18-59" 3 "Age 60-69" 5 "Age 70-99") col(1)) ///
   xline(2 3 5 10, lcol(gs14)) ///
   xtitle(Time since diagnosis (years)) ytitle(Survival proportion (%)) ///
   title(`cancertype_title') ///
   xscale(range(0 40)) xlab(0 2 3 5 10 20 30 40) xtick(0(5)40) xmtick(0(1)40) ///
   ylab(0(20)100, nogrid labsize(medium)) ytick(0(20)100) ymtick(0(5)100) ///
   scheme(s1mono)

graph export "`cancertype'_km_legend.png", as(png) replace width(5000)
   
// Plot tw
tw (line km_100 _t if agegroup == 0, sort lcol(navy) lp(solid) lwidth(*1.5)) ///
   (rarea ub_100 lb_100 _t if agegroup == 0, sort color(gray%20)) ///
   (line km_100 _t if agegroup == 1, sort lcol(maroon) lp(longdash) lwidth(*1.5)) ///
   (rarea ub_100 lb_100 _t if agegroup == 1, sort color(gray%20)) ///
   (line km_100 _t if agegroup == 2, sort lcol(dkgreen) lp(shortdash) lwidth(*1.5)) ///
   (rarea ub_100 lb_100 _t if agegroup == 2, sort color(gray%20)), ///
   legend(off) ///
   xline(2 3 5 10 20 30, lcol(gs13) lp(dash)) ///
   xtitle(Time since diagnosis (years)) ytitle(Survival proportion (%)) ///
   title(`cancertype_title') ///
   xscale(range(0 40)) xlab(0 2 3 5 10 20 30 40) xtick(0(5)40) xmtick(0(1)40) ///
   ylab(0(20)100, nogrid labsize(medium)) ytick(0(20)100) ymtick(0(5)100) ///
   scheme(s1mono)

graph export "`cancertype'_km.png", as(png) replace width(5000)

// RMST at 10
strmst2 agegroup, tau(10) reference(0)

gen rmst10 = .
forvalues i = 0/2{
	local rmst`i' r(rmstarm`i')[1,1] // rmst is stored at the matrix 
	di `rmst`i''
	replace rmst10 = `rmst`i'' if agegroup == `i'
}

tab agegroup rmst10

// Life expectancy at year 40
// (Mean survival time restricted to 40 years)
gen le40 = .
forvalues i =0/2{
	stci if agegroup == `i', rmean
	local le`i' r(rmean)
	di `le`i''
	replace le40 = `le`i'' if agegroup == `i'
}

tab agegroup le40

// Create an empty dataset
tempfile km_data
save "`km_data'", replace

/*------------Plot smoothed functions-------------*/
cd "${path}/${subpath}/Data"

// Creat list of datasets
local datasets "`cancertype'_ag0 `cancertype'_ag1 `cancertype'_ag2"

foreach dataset of local datasets {
	use `dataset', clear
	
	stset stime, failure(dead==1) exit(time 40) id(lpnr)

	// Fit a FPM to obtain smoothed hazard function
	stpm2 , scale(h) df(5) 

	// Predict all-cause hazard
	predict h, haz ci

	// Fit a FPRSM to obtain smoothed excess hazard function
		// Merge data with life table
			gen _year= floor(min(yeardiag + _t, 2020)) //popmort goes to year 2020
			gen _age=floor(min(agediag + _t, 107)) // the popmort only goes to age 107

			// merge with the popmort file to get hazard rate by age sex and year*/
			sort _year sex _age 
			merge m:1 _year sex _age /// 
				using "${path}/SweCan/Popmort/popmort2020.dta", ///
				nolabel keep(match master) keepusing(rate) 
				
			drop _age _year _merge
		// Model
			stpm2 ,scale(haz) df(5) bhazard(rate) 
	// Predict excess hazard		
	predict exh, haz ci
	replace exh = 0 if exh == .					
	// Obtain empirical expected hazard (exph) by all-cause hazard (h) minus excess hazard (exh)
	gen exph = h - exh
	
	 // Append the results to the combined dataset
    save "`dataset'_haz", replace
}

// Use the combined dataset
use `cancertype'_ag0_haz, clear
append using `cancertype'_ag1_haz
append using `cancertype'_ag2_haz

// Merge with the km data
sort lpnr
merge 1:1 lpnr using `km_data', nolab
					
// Only keep relevant variables
keep agegroup _t km* ub* lb* rmst10 le40 h* exh* exph
sort agegroup _t 
drop if _t == .

gen method = "km"
gen cancer = "`cancertype'"

pwd
cd "${path}/${subpath}/Data"
save `cancertype'_km, replace
}
/***************************** FILE ENDS HERE**************************/
log close
exit 
