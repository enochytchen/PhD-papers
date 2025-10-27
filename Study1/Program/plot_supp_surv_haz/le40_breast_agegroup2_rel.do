// Updated: 20230819 Enoch Chen: Updated analysis for revised submission
//			20230307 Enoch Chen: Separate the figures into all-cause survival vs. relative survival
//	  		20230120 Enoch Chen

// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global path `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global path `"P:"'

// Set subpath
global subpath `"SweCan/Site_specific/Enoch_Study1"'

// Set the working directory as where the do files are
cd "${path}/${subpath}/Data"

// Log file
local todaydate: di %tdCYND date(c(current_date),"DMY")
global dofilename = "le40_breast_agegroup2_rel" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace

/*========Code begins here========*/
clear all
global cancervar "breast"
global agegroupvar 2
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
lab var agegroup "0 20-59; 1 60-69; 2 70-99"
lab var le40 "LE or 40-year RMST"
lab var s "survival proportion at time _t"
lab var fw "Framework (all-cause or relative survival)"
lab var dist "Parametric distribution"
lab var exyr "Amount of timne (years) used for extrapolation"
lab var outcome "le40"
lab var cancer "Cancer type"

save ${dofilename}, replace

/*============Extrapolation from 2 years===========*/
di "${dofilename}"

cd "${path}/${subpath}/Data"
use ${dofilename}, clear
local exyr 2 
keep if exyr  == "`exyr'"

append using ${cancervar}_km.dta
keep if agegroup == ${agegroupvar} 

/*============Draw one all-cause hazard to determind yaxis scale=========*/
// All-cause azard
	local exyr 2
	tw (line ach _t if fw == "rel" & dist == "exp",   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line ach _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line h _t , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea h_lci h_uci _t , sort color(gray%20)), /// 
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) 
	   
/*============Relative survival===========*/
cd "${path}/${subpath}/Output/plots_supp/surv_haz/temp/"
// Standard parametric models   
	// Survival
	local exyr 2
	tw (line s _t if fw == "rel" & dist == "exp" ,   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line s _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line km_100 _t , sort lcol(black) lp(solid) lwidth(*3)) ///
	   (rarea ub_100 lb_100 _t , sort color(gray%20)), ///
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   ylab(0(20)100, nogrid labsize(medium)) ytick(0(10)100) ///
	   xtitle("")  /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_spm_surv, replace)
	   
   	// All-cause hazard
	local exyr 2
	tw (line ach _t if fw == "rel" & dist == "exp",   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line ach _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "ggamma" & ach <=0.5, sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line h _t , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea h_lci h_uci _t , sort color(gray%20)), ///  
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_spm_haz, replace)
	   
	// Excess hazard   
	local exyr 2
	tw (line exh _t if fw == "rel" & dist == "exp",   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line exh _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "ggamma" & exh <=0.5, sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line exh _t if method == "km" , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea exh_lci exh_uci _t if method == "km", sort color(gray%20)), /// 
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_spm_exh, replace)
	
	// Expected hazard 
	tw (line exph _t if method != "km" , sort lp(solid) lcol(purple%80) lwidth(*1.5)) /// 
	   (line exph _t if method == "km" , sort lcol(black) lp(solid) lwidth(*3)), /// 
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") ytitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(rel_exph, replace)
	   
// Flexible parametric models
	// Survival   
	 local exyr 2    
	 tw (line s _t if fw == "rel" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
		(line s _t if fw == "rel" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
		(line s _t if fw == "rel" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
		(line km_100 _t , sort lcol(black) lp(solid) lwidth(*3)) ///
		(rarea ub_100 lb_100 _t , sort color(gray%20)), ///
		xline(`exyr', lcol(gs5) lp(dash)) ///
	    xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
		ylab(0(20)100, nogrid labsize(medium)) ytick(0(10)100) ///
	    xtitle("") /// 
		legend(off) scheme(s1mono) ///
	    saving(exyr`exyr'_rel_fpm_surv, replace)
    
	// All-cause hazard
	local exyr 2
	tw (line ach _t if fw == "rel" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
	   (line ach _t if fw == "rel" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
	   (line h _t , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea h_lci h_uci _t , sort color(gray%20)), ///
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") ///   
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_fpm_haz, replace)
	   
	// Excess hazard 
	local exyr 2
	tw (line exh _t if fw == "rel" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
	   (line exh _t if fw == "rel" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
	   (line exh _t if method == "km" , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea exh_lci exh_uci _t if method == "km", sort color(gray%20)), ///
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_fpm_exh, replace)
	   
local exyr 2
	   
graph combine exyr`exyr'_rel_spm_surv.gph exyr`exyr'_rel_spm_haz.gph exyr`exyr'_rel_spm_exh.gph rel_exph.gph ///
			  exyr`exyr'_rel_fpm_surv.gph exyr`exyr'_rel_fpm_haz.gph exyr`exyr'_rel_fpm_exh.gph rel_exph.gph, ///
			  cols(8) scheme(s1mono) graphregion(color(white)) ///
			  l1title("`exyr' years") imargin(tiny) ///
			  saving(exyr`exyr', replace)

graph display, xsize(50) ysize(10) 

graph save "exyr`exyr'", replace

/*============Extrapolation from 3 years===========*/
di "${dofilename}"

cd "${path}/${subpath}/Data"
use ${dofilename}, clear
local exyr 3 
keep if exyr  == "`exyr'"

append using ${cancervar}_km.dta
keep if agegroup == ${agegroupvar} 

/*============Relative survival===========*/
cd "${path}/${subpath}/Output/plots_supp/surv_haz/temp/"
// Standard parametric models   
	// Survival
	local exyr 3
	tw (line s _t if fw == "rel" & dist == "exp" ,   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line s _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line km_100 _t , sort lcol(black) lp(solid) lwidth(*3)) ///
	   (rarea ub_100 lb_100 _t , sort color(gray%20)), ///
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   ylab(0(20)100, nogrid labsize(medium)) ytick(0(10)100) ///
	   xtitle("")  /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_spm_surv, replace)
	   
   	// All-cause hazard
	local exyr 3
	tw (line ach _t if fw == "rel" & dist == "exp",   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line ach _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line h _t , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea h_lci h_uci _t , sort color(gray%20)), ///  
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_spm_haz, replace)
	   
	// Excess hazard   
	local exyr 3
	tw (line exh _t if fw == "rel" & dist == "exp",   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line exh _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "ggamma" & exh <=0.5, sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line exh _t if method == "km" , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea exh_lci exh_uci _t if method == "km", sort color(gray%20)), /// 
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_spm_exh, replace)
	
	// Expected hazard 
	tw (line exph _t if method != "km" , sort lp(solid) lcol(purple%80) lwidth(*1.5)) /// 
	   (line exph _t if method == "km" , sort lcol(black) lp(solid) lwidth(*3)), /// 
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") ytitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(rel_exph, replace)
	   
// Flexible parametric models
	// Survival   
	local exyr 3
	 tw (line s _t if fw == "rel" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
		(line s _t if fw == "rel" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
		(line s _t if fw == "rel" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
		(line km_100 _t , sort lcol(black) lp(solid) lwidth(*3)) ///
		(rarea ub_100 lb_100 _t , sort color(gray%20)), ///
		xline(`exyr', lcol(gs5) lp(dash)) ///
	    xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
		ylab(0(20)100, nogrid labsize(medium)) ytick(0(10)100) ///
	    xtitle("") /// 
		legend(off) scheme(s1mono) ///
	    saving(exyr`exyr'_rel_fpm_surv, replace)
    
	// All-cause hazard
	local exyr 3
	tw (line ach _t if fw == "rel" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
	   (line ach _t if fw == "rel" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
	   (line h _t , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea h_lci h_uci _t , sort color(gray%20)), ///
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") ///   
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_fpm_haz, replace)
	   
	// Excess hazard 
	local exyr 3
	tw (line exh _t if fw == "rel" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
	   (line exh _t if fw == "rel" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
	   (line exh _t if method == "km" , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea exh_lci exh_uci _t if method == "km", sort color(gray%20)), ///
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_fpm_exh, replace)
	   
local exyr 3
	   
graph combine exyr`exyr'_rel_spm_surv.gph exyr`exyr'_rel_spm_haz.gph exyr`exyr'_rel_spm_exh.gph rel_exph.gph ///
			  exyr`exyr'_rel_fpm_surv.gph exyr`exyr'_rel_fpm_haz.gph exyr`exyr'_rel_fpm_exh.gph rel_exph.gph, ///
			  cols(8) scheme(s1mono) graphregion(color(white)) ///
			  l1title("`exyr' years") imargin(tiny) ///
			  saving(exyr`exyr', replace)

graph display, xsize(50) ysize(10) 

graph save "exyr`exyr'", replace
/*============Extrapolation from 5 years===========*/
di "${dofilename}"

cd "${path}/${subpath}/Data"
use ${dofilename}, clear
local exyr 5 
keep if exyr  == "`exyr'"

append using ${cancervar}_km.dta
keep if agegroup == ${agegroupvar} 

/*============Relative survival===========*/
cd "${path}/${subpath}/Output/plots_supp/surv_haz/temp/"
// Standard parametric models   
	// Survival
	local exyr 5
	tw (line s _t if fw == "rel" & dist == "exp" ,   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line s _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line km_100 _t , sort lcol(black) lp(solid) lwidth(*3)) ///
	   (rarea ub_100 lb_100 _t , sort color(gray%20)), ///
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   ylab(0(20)100, nogrid labsize(medium)) ytick(0(10)100) ///
	   xtitle("")  /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_spm_surv, replace)
	   
   	// All-cause hazard
	local exyr 5
	tw (line ach _t if fw == "rel" & dist == "exp",   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line ach _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line h _t , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea h_lci h_uci _t , sort color(gray%20)), ///  
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_spm_haz, replace)
	   
	// Excess hazard   
	local exyr 5
	tw (line exh _t if fw == "rel" & dist == "exp",   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line exh _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line exh _t if method == "km" , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea exh_lci exh_uci _t if method == "km", sort color(gray%20)), /// 
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_spm_exh, replace)
	
	// Expected hazard 
	tw (line exph _t if method != "km" , sort lp(solid) lcol(purple%80) lwidth(*1.5)) /// 
	   (line exph _t if method == "km" , sort lcol(black) lp(solid) lwidth(*3)), /// 
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") ytitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(rel_exph, replace)
	   
// Flexible parametric models
	// Survival   
	local exyr 5
	 tw (line s _t if fw == "rel" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
		(line s _t if fw == "rel" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
		(line s _t if fw == "rel" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
		(line km_100 _t , sort lcol(black) lp(solid) lwidth(*3)) ///
		(rarea ub_100 lb_100 _t , sort color(gray%20)), ///
		xline(`exyr', lcol(gs5) lp(dash)) ///
	    xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
		ylab(0(20)100, nogrid labsize(medium)) ytick(0(10)100) ///
	    xtitle("") /// 
		legend(off) scheme(s1mono) ///
	    saving(exyr`exyr'_rel_fpm_surv, replace)
    
	// All-cause hazard
	local exyr 5
	tw (line ach _t if fw == "rel" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
	   (line ach _t if fw == "rel" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
	   (line h _t , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea h_lci h_uci _t , sort color(gray%20)), ///
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") ///   
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_fpm_haz, replace)
	   
	// Excess hazard 
	local exyr 5
	tw (line exh _t if fw == "rel" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
	   (line exh _t if fw == "rel" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
	   (line exh _t if method == "km" , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea exh_lci exh_uci _t if method == "km", sort color(gray%20)), ///
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(none) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_fpm_exh, replace)
	   
local exyr 5
	   
graph combine exyr`exyr'_rel_spm_surv.gph exyr`exyr'_rel_spm_haz.gph exyr`exyr'_rel_spm_exh.gph rel_exph.gph ///
			  exyr`exyr'_rel_fpm_surv.gph exyr`exyr'_rel_fpm_haz.gph exyr`exyr'_rel_fpm_exh.gph rel_exph.gph, ///
			  cols(8) scheme(s1mono) graphregion(color(white)) ///
			  l1title("`exyr' years") imargin(tiny) ///
			  saving(exyr`exyr', replace)

graph display, xsize(50) ysize(10) 

graph save "exyr`exyr'", replace

/*============Extrapolation from 10 years===========*/
di "${dofilename}"

cd "${path}/${subpath}/Data"
use ${dofilename}, clear
local exyr 10 
keep if exyr  == "`exyr'"

append using ${cancervar}_km.dta
keep if agegroup == ${agegroupvar} 

/*============Relative survival===========*/
cd "${path}/${subpath}/Output/plots_supp/surv_haz/temp/"
// Standard parametric models   
	// Survival
	local exyr 10 
	tw (line s _t if fw == "rel" & dist == "exp" ,   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line s _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line s _t if fw == "rel" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line km_100 _t , sort lcol(black) lp(solid) lwidth(*3)) ///
	   (rarea ub_100 lb_100 _t , sort color(gray%20)), ///
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(0(10)40, labsize(medium)) xtick(0(5)40) xmtick(0(1)40) ///
	   ylab(0(20)100, nogrid labsize(medium)) ytick(0(10)100) ///
	   xtitle("")  /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_spm_surv, replace)
	   
   	// All-cause hazard
	local exyr 10 
	tw (line ach _t if fw == "rel" & dist == "exp",   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line ach _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line h _t , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea h_lci h_uci _t , sort color(gray%20)), ///  
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(0(10)40, labsize(medium)) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_spm_haz, replace)
	   
	// Excess hazard   
	local exyr 10 
	tw (line exh _t if fw == "rel" & dist == "exp",   sort lp(solid) lcol(eltblue%80) lwidth(*1.5)) /// 
	   (line exh _t if fw == "rel" & dist == "weib",  sort lp(solid) lcol(midblue%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "gomp",  sort lp(solid) lcol(gold%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "loglog", sort lp(solid) lcol(midgreen%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "lognorm", sort lp(solid) lcol(cranberry%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "ggamma", sort lp(solid) lcol(orange_red%80) lwidth(*1.5)) ///
	   (line exh _t if method == "km" , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea exh_lci exh_uci _t if method == "km", sort color(gray%20)), /// 
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(0(10)40, labsize(medium)) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_spm_exh, replace)
	
	// Expected hazard 
	tw (line exph _t if method != "km" , sort lp(solid) lcol(purple%80) lwidth(*1.5)) /// 
	   (line exph _t if method == "km" , sort lcol(black) lp(solid) lwidth(*3)), /// 
	   xscale(range(0 40)) xlab(0(10)40, labsize(medium)) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") ytitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(rel_exph, replace)
	   
// Flexible parametric models
	// Survival   
	local exyr 10 
	 tw (line s _t if fw == "rel" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
		(line s _t if fw == "rel" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
		(line s _t if fw == "rel" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
		(line km_100 _t , sort lcol(black) lp(solid) lwidth(*3)) ///
		(rarea ub_100 lb_100 _t , sort color(gray%20)), ///
		xline(`exyr', lcol(gs5) lp(dash)) ///
	    xscale(range(0 40)) xlab(0(10)40, labsize(medium)) xtick(0(5)40) xmtick(0(1)40) ///
		ylab(0(20)100, nogrid labsize(medium)) ytick(0(10)100) ///
	    xtitle("") /// 
		legend(off) scheme(s1mono) ///
	    saving(exyr`exyr'_rel_fpm_surv, replace)
    
	// All-cause hazard
	local exyr 10 
	tw (line ach _t if fw == "rel" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
	   (line ach _t if fw == "rel" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
	   (line ach _t if fw == "rel" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
	   (line h _t , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea h_lci h_uci _t , sort color(gray%20)), ///
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(0(10)40, labsize(medium)) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") ///   
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_fpm_haz, replace)
	   
	// Excess hazard 
	local exyr 10 
	tw (line exh _t if fw == "rel" & dist == "splhaz",   sort lp(solid) lcol(dkorange%80) lwidth(*1.5)) /// 
	   (line exh _t if fw == "rel" & dist == "splnorm",  sort lp(solid) lcol(blue%80) lwidth(*1.5)) ///
	   (line exh _t if fw == "rel" & dist == "splodds",  sort lp(solid) lcol(green%80) lwidth(*1.5)) ///
	   (line exh _t if method == "km" , sort lcol(black) lp(solid) lwidth(*3)) ///
       (rarea exh_lci exh_uci _t if method == "km", sort color(gray%20)), ///
	   xline(`exyr', lcol(gs5) lp(dash)) ///
	   xscale(range(0 40)) xlab(0(10)40, labsize(medium)) xtick(0(5)40) xmtick(0(1)40) ///
	   yscale(range(0 0.5)) ylab(0(0.1)0.5) ytick(0(0.1)0.5) ///
	   xtitle("") /// 
	   legend(off) scheme(s1mono) ///
	   saving(exyr`exyr'_rel_fpm_exh, replace)
	   
	local exyr 10 
	   
graph combine exyr`exyr'_rel_spm_surv.gph exyr`exyr'_rel_spm_haz.gph exyr`exyr'_rel_spm_exh.gph rel_exph.gph ///
			  exyr`exyr'_rel_fpm_surv.gph exyr`exyr'_rel_fpm_haz.gph exyr`exyr'_rel_fpm_exh.gph rel_exph.gph, ///
			  cols(8) scheme(s1mono) graphregion(color(white)) ///
			  l1title("`exyr' years") imargin(tiny) ///
			  saving(exyr`exyr', replace)

graph display, xsize(50) ysize(10) 

graph save "exyr`exyr'", replace

// Put all 4 graphs together

cd "${path}/${subpath}/Output/plots_supp/surv_haz/temp"
	
graph combine exyr2.gph exyr3.gph exyr5.gph exyr10.gph, rows(4) scheme(s1mono) ///
			  graphregion(color(white)) imargin(0 0 0 0) ///
			  b1title("Time since diagnosis (years)")

graph display, xsize(30) ysize(10)       

cd "${path}/${subpath}/Output/plots_supp/surv_haz/"

graph export "${dofilename}.png", as(png) replace width(5000)

log close
exit
