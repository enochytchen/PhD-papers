/*==============================================================================
FILENAME: plot_supp_surv10.do

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
global dofilename = "surv10" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace


/*========Code begins here========*/
clear all

use results, clear

// RMST10
// exyr 2
preserve
	cd "${path}/${subpath}/Data"
	use results, clear
	local i 2
	keep if exyr == "`i'" & outcome == "rmst10"
	replace exyr = "`i' years" if exyr == "`i'"

	graph dot diff_surv10, over(fw) over(cohort_nu, axis(off)) ///
		  ytit("") note("") asyvars ndots(0) ///
		  yscale(range(-70 70)) ylab(none) ytick(-70(10)70) ymtick(-70(5)70) ///
		  lintensity(.5) yline(0 , lc(black) lp(solid)) ///
		  marker(1, mc(navy%80) msize(medium)) ///
		  marker(2, mc(maroon%80) msymbol(T) msize(medlarge)) ///
		  by(dist_nu, cols(9) compact note("") legend(off)) ///
		  subtitle(,size(medium)) ///
	      scheme(s1mono) saving(exyr`i', replace)
		  
	graph display, xsize(30) ysize(10)	
restore

// exyr 3
preserve
	cd "${path}/${subpath}/Data"
	use results, clear
	local i 3
	keep if exyr == "`i'" & outcome == "rmst10"
	replace exyr = "`i' years" if exyr == "`i'"

	graph dot diff_surv10, over(fw) over(cohort_nu, axis(off)) ///
		  ytit("") note("") asyvars ndots(0) ///
		  yscale(range(-70 70)) ylab(none) ytick(-70(10)70) ymtick(-70(5)70) ///
		  lintensity(.5) yline(0 , lc(black) lp(solid)) ///
		  marker(1, mc(navy%80) msize(medium)) ///
		  marker(2, mc(maroon%80) msymbol(T) msize(medlarge)) ///
		  by(dist_nu, cols(9) compact note("") legend(off) imargin(0 0 0 0)) subtitle("") ///
	      scheme(s1mono) saving(exyr`i', replace)
		  
	graph display, xsize(30) ysize(10)	
restore

// exyr 5
preserve
	cd "${path}/${subpath}/Data"
	use results, clear
	local i 5
	keep if exyr == "`i'" & outcome == "rmst10"
	replace exyr = "`i' years" if exyr == "`i'"

	graph dot diff_surv10, over(fw) over(cohort_nu, axis(off)) ///
		  ytit("") note("") asyvars ndots(0) ///
		  yscale(range(-70 70)) ylab(-60(20)60, nogrid) ytick(-70(10)70) ymtick(-70(5)70) ///
		  lintensity(.5) yline(0 , lc(black) lp(solid)) ///
		  marker(1, mc(navy%80) msize(medium)) ///
		  marker(2, mc(maroon%80) msymbol(T) msize(medlarge)) ///
		  by(dist_nu, cols(9) compact note("") legend(off) imargin(0 0 0 0)) subtitle("") ///
	      scheme(s1mono) saving(exyr`i', replace)
		  
	graph display, xsize(30) ysize(10)	
restore


graph combine exyr2.gph exyr3.gph exyr5.gph, ////
			  rows(3) imargin(tiny) ///
			  b2title("Difference (%)", size(small)) /// 
			  scheme(s1mono) graphregion(color(white))

graph display, xsize(45) ysize(20)	

cd "${path}/${subpath}/Output/plots_supp"

graph export "surv10.png", replace width(10000)

