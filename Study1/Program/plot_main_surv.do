/*==============================================================================
FILENAME: plot_main_surv.do

PROJECT: Enoch PhD Study 1 (Comparing survival extrapolation)
	
AUTHOR: Enoch Yi-Tung Chen (enoch.yitung.chen@ki.se)

CREATED: Enoch Yi-Tung Chen	
UPDATED: 20230819 Enoch Chen: Updated analysis for revised submission
         20230417 Enoch Chen: Remain only two rows

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
global dofilename = "plot_main_surv" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace

/*========Code begins here========*/

/*==============================================
==========Breast 60-69, 3 years, RMST10==========
================================================*/
clear all
global cancervar "breast"
global agegroupvar 1
local outcomevar "rmst10"

// Merge in one single dataframe
clear
dir  "${cancervar}_ac_*_rmst10.dta"
dir  "${cancervar}_rel_*_rmst10.dta"

append using `: dir . files "${cancervar}_ac_*_rmst10.dta"'
append using `: dir . files "${cancervar}_rel_*_rmst10.dta"'

// Multiply all-cause survival by 100 to show percentage
replace s = s * 100
replace s_lci = s_lci * 100
replace s_uci = s_uci * 100 

// Generate variables entitled (fw dist exyr) to distinguish methods
split method, parse(_)
rename (method1 method2 method3 method4 method5)(ag fw dist exyr outcome)
replace exyr = substr(exyr,5,.)

// Label variables
lab var agegroup "0 18-59; 1 60-69; 2 70-99"
lab var rmst10 "10-year RMST"
lab var s "survival proportion at time _t"
lab var fw "Framework (all-cause or relative survival)"
lab var dist "Parametric distribution"
lab var exyr "Amount of timne (years) used for extrapolation"
lab var outcome "rmst10 or le40"
lab var cancer "Cancer type"

local exyr 3 
keep if exyr  == "`exyr'"

append using ${cancervar}_km.dta
keep if agegroup == ${agegroupvar} 
keep if _t <= 10

/*============Legend============*/
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
	   legend(order(1 "K-M survival" 2 "95% CIs for K-M" 3 "Exponential" 4 "Weibull" 5 "Gompertz" 6 "Log-logistic" 7 "Log-normal" 8 "Generalized gamma" 9 "Spline Hazard" 10 "Spline Odds" 11 "Spline Normal") rows(3) cols(4)) ///
	   scheme(s1mono) graphregion(color(white))
graph display, xsize(30) ysize(10)       

cd "${path}/${subpath}/Output/plots_main"

graph export "plot_main_surv_legend.png", as(png) replace width(5000)


/*=================================================
============Breast 60-69, 3 years, LE40===========
==================================================*/
clear all

cd "${path}/${subpath}/Data"

global cancervar "breast"
global agegroupvar 1
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
rename (method1 method2 method3 method4 method5)(ag fw dist exyr outcome)
replace exyr = substr(exyr,5,.)

// Label variables
lab var agegroup "0 18-59; 1 60-69; 2 70-99"
lab var le40 "Life expectancy or RMST at 40 years"
lab var s "survival proportion at time _t"
lab var fw "Framework (all-cause or relative survival)"
lab var dist "Parametric distribution"
lab var exyr "Amount of timne (years) used for extrapolation"
lab var outcome "le40"
lab var cancer "Cancer type"

local exyr 3 
 
keep if exyr  == "`exyr'"

append using ${cancervar}_km.dta
keep if agegroup == ${agegroupvar} 

/*============All-cause survival===========*/
// Save temp graphs
cd "${path}/${subpath}/Output/plots_main/temp/"

// Standard parametric models   
	// Survival
	local exyr 3 

	tw (line s _t if fw == "ac" & dist == "exp" ,   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line s _t if fw == "ac" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line s _t if fw == "ac" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line s _t if fw == "ac" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line s _t if fw == "ac" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line s _t if fw == "ac" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line km_100 _t , sort lcol(black) lp(solid) lwidth(*1.5)) ///
	   (rarea ub_100 lb_100 _t , sort color(gray%20)), ///
	   xline(`exyr', lcol(gs13) lp(dash)) ///
	   xline(10) ///	   
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   ylab(0(20)100, nogrid labsize(medium)) ytick(0(20)100) ymtick(0(5)100) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_ac_spm_surv, replace)

// Flexible parametric models
	// Survival   
	 local exyr 3    
	 tw (line s _t if fw == "ac" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
		(line s _t if fw == "ac" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
		(line s _t if fw == "ac" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
    	(line km_100 _t , sort lcol(black) lp(solid) lwidth(*1.5)) ///
		(rarea ub_100 lb_100 _t , sort color(gray%20)), ///
		xline(`exyr', lcol(gs13) lp(dash)) ///
		xline(10) ///	   
	    xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	    yscale(range(0 100)) ylab(none) ytick(0(20)100) ymtick(0(5)100) ///
	    xtitle("") /// 
		legend(off) scheme(s1mono) ///
	    saving(exyr`exyr'_ac_fpm_surv, replace)

/*============Relative survival===========*/

// Standard parametric models   
	// Survival
	local exyr 3
	tw (line s _t if fw == "rel" & dist == "exp" ,   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line s _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line km_100 _t , sort lcol(black) lp(solid) lwidth(*1.5)) ///
	   (rarea ub_100 lb_100 _t , sort color(gray%20)), ///
	   xline(`exyr', lcol(gs13) lp(dash)) ///
	   xline(10) ///	   	   
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 100)) ylab(none) ytick(0(20)100) ymtick(0(5)100) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_spm_surv, replace)
	   
// Flexible parametric models
	// Survival   
	 local exyr 3    
	 tw (line s _t if fw == "rel" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
		(line s _t if fw == "rel" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
		(line s _t if fw == "rel" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
     	(line km_100 _t , sort lcol(black) lp(solid) lwidth(*1.5)) ///
		(rarea ub_100 lb_100 _t , sort color(gray%20)), ///
		xline(`exyr', lcol(gs13) lp(dash)) ///
	    xline(10) ///	   		
	    xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	    yscale(range(0 100)) ylab(none) ytick(0(20)100) ymtick(0(5)100) ///
	    xtitle("") /// 
		legend(off) scheme(s1mono) ///
	    saving(exyr`exyr'_rel_fpm_surv, replace)
	   
local exyr 3
	      
graph combine exyr`exyr'_ac_spm_surv.gph exyr`exyr'_ac_fpm_surv.gph ///
			  exyr`exyr'_rel_spm_surv.gph exyr`exyr'_rel_fpm_surv.gph, ///
			  cols(4) rows(1) ///
			  scheme(s1mono) graphregion(color(white)) ///
			  l2title("(A) 3 years to 40 years", size(medium)) imargin(tiny) ///
			  l1title("Survival proportion (%)", size(medium)) ///
			  saving(plot_main_surv_haz_temp1, replace)

graph display, xsize(30) ysize(10) 

graph save "plot_main_surv_temp1", replace

/*=================================================
============Breast 60-69, 10 years, LE40===========
==================================================*/
clear all

cd "${path}/${subpath}/Data"

global cancervar "breast"
global agegroupvar 1
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
rename (method1 method2 method3 method4 method5)(ag fw dist exyr outcome)
replace exyr = substr(exyr,5,.)

// Label variables
lab var agegroup "0 18-59; 1 60-69; 2 70-99"
lab var le40 "Life expectancy or RMST at 40 years"
lab var s "survival proportion at time _t"
lab var fw "Framework (all-cause or relative survival)"
lab var dist "Parametric distribution"
lab var exyr "Amount of timne (years) used for extrapolation"
lab var outcome "le40"
lab var cancer "Cancer type"

local exyr 10 
keep if exyr  == "`exyr'"

append using ${cancervar}_km.dta
keep if agegroup == ${agegroupvar} 

/*============All-cause survival===========*/
// Save temp graphs
cd "${path}/${subpath}/Output/plots_main/temp/"

// Standard parametric models   
	// Survival
	local exyr 10
	tw (line s _t if fw == "ac" & dist == "exp" ,   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line s _t if fw == "ac" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line s _t if fw == "ac" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line s _t if fw == "ac" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line s _t if fw == "ac" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line s _t if fw == "ac" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line km_100 _t , sort lcol(black) lp(solid) lwidth(*1.5)) ///
	   (rarea ub_100 lb_100 _t , sort color(gray%20)), ///
	   xline(`exyr', lcol(gs13) lp(dash)) ///
	   xscale(range(0 40)) xlab(0(10)40, labsize(medium)) xtick(0(5)40) xmtick(0(1)40) ///
	   ylab(0(20)100, nogrid labsize(medium)) ytick(0(20)100) ymtick(0(5)100) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_ac_spm_surv, replace)

// Flexible parametric models
	// Survival   
	 local exyr 10    
	 tw (line s _t if fw == "ac" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
		(line s _t if fw == "ac" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
		(line s _t if fw == "ac" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
		(line km_100 _t , sort lcol(black) lp(solid) lwidth(*1.5)) ///
		(rarea ub_100 lb_100 _t , sort color(gray%20)), ///
		xline(`exyr', lcol(gs13) lp(dash)) ///
	    xscale(range(0 40)) xlab(0(10)40, labsize(medium)) xtick(0(5)40) xmtick(0(1)40) ///
	    yscale(range(0 100)) ylab(none) ytick(0(20)100) ymtick(0(5)100) ///
	    xtitle("") /// 
		legend(off) scheme(s1mono) ///
	    saving(exyr`exyr'_ac_fpm_surv, replace)

/*============Relative survival===========*/

// Standard parametric models   
	// Survival
	local exyr 10
	tw (line s _t if fw == "rel" & dist == "exp" ,   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line s _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line km_100 _t , sort lcol(black) lp(solid) lwidth(*1.5)) ///
	   (rarea ub_100 lb_100 _t , sort color(gray%20)), ///
	   xline(`exyr', lcol(gs13) lp(dash)) ///
	   xscale(range(0 40)) xlab(0(10)40, labsize(medium)) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 100)) ylab(none) ytick(0(20)100) ymtick(0(5)100) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_spm_surv, replace)
	   
// Flexible parametric models
	// Survival   
	 local exyr 10    
	 tw (line s _t if fw == "rel" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
		(line s _t if fw == "rel" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
		(line s _t if fw == "rel" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
		(line km_100 _t , sort lcol(black) lp(solid) lwidth(*1.5)) ///
		(rarea ub_100 lb_100 _t , sort color(gray%20)), ///
		xline(`exyr', lcol(gs13) lp(dash)) ///
	    xscale(range(0 40)) xlab(0(10)40, labsize(medium)) xtick(0(5)40) xmtick(0(1)40) ///
	    yscale(range(0 100)) ylab(none) ytick(0(20)100) ymtick(0(5)100) ///
	    xtitle("") /// 
		legend(off) scheme(s1mono) ///
	    saving(exyr`exyr'_rel_fpm_surv, replace)
	   
local exyr 10
	     
graph combine exyr`exyr'_ac_spm_surv.gph exyr`exyr'_ac_fpm_surv.gph ///
			  exyr`exyr'_rel_spm_surv.gph exyr`exyr'_rel_fpm_surv.gph, ///
			  cols(4) rows(1) ///
			  scheme(s1mono) graphregion(color(white)) ///
			  l2title("(B) 10 years to 40 years", size(medium)) imargin(tiny) ///
			  l1title("Survival proportion (%)", size(medium)) ///
			  saving(plot_main_surv_haz_temp2, replace)

graph display, xsize(30) ysize(10) 

graph save "plot_main_surv_temp2", replace

// Combine all graphs
cd "${path}/${subpath}/Output/plots_main/temp"
	
graph combine plot_main_surv_haz_temp1.gph plot_main_surv_haz_temp2.gph, ///
			  rows(2) scheme(s1mono) ///
			  graphregion(color(white)) imargin(0 0 0 0) ///
			  b1title("Time since diagnosis (years)", size(medium))

graph display, xsize(40) ysize(20)       

cd "${path}/${subpath}/Output/plots_main/"

graph export "plot_main_surv.png", as(png) replace width(5000)

log close
exit
