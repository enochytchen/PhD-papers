/*==============================================================================
FILENAME: plot_main_surv.do

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
global dofilename = "le40" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace


/*========Code begins here========*/
clear all

// LE40
// exyr 2
preserve
	cd "${path}/${subpath}/Data"
	use results, clear
	local i 2
	keep if exyr == "`i'" & outcome == "le40"
	replace exyr = "`i' years" if exyr == "`i'"

	graph dot diff_le40, over(fw) over(cohort_nu, axis(off)) ///
		  ytit("") note("") asyvars ndots(0) ///
		  yscale(range(-15 15)) ylab(none) ytick(-15(5)15) ymtick(-15(1)15) ///
		  lintensity(.5) yline(0 , lc(black) lp(solid)) ///
		  marker(1, mc(navy%80) msize(medsmall)) ///
		  marker(2, mc(maroon%80) msymbol(T) msize(medsmall)) ///
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
	keep if exyr == "`i'" & outcome == "le40"
	replace exyr = "`i' years" if exyr == "`i'"

	graph dot diff_le40, over(fw) over(cohort_nu, axis(off)) ///
		  ytit("") note("") asyvars ndots(0) ///
		  yscale(range(-15 15)) ylab(none) ytick(-15(5)15) ymtick(-15(1)15) ///
		  lintensity(.5) yline(0 , lc(black) lp(solid)) ///
		  marker(1, mc(navy%80) msize(medsmall)) ///
		  marker(2, mc(maroon%80) msymbol(T) msize(medsmall)) ///
		  by(dist_nu, cols(9) compact note("") legend(off) imargin(0 0 0 0)) subtitle("") ///
	      scheme(s1mono) saving(exyr`i', replace)
		  
	graph display, xsize(30) ysize(10)	
restore

// exyr 5
preserve
	cd "${path}/${subpath}/Data"
	use results, clear
	local i 5
	keep if exyr == "`i'" & outcome == "le40"
	replace exyr = "`i' years" if exyr == "`i'"

	graph dot diff_le40, over(fw) over(cohort_nu, axis(off)) ///
		  ytit("") note("") asyvars ndots(0) ///
		  yscale(range(-15 15)) ylab(none) ytick(-15(5)15) ymtick(-15(1)15) ///
		  lintensity(.5) yline(0 , lc(black) lp(solid)) ///
		  marker(1, mc(navy%80) msize(medsmall)) ///
		  marker(2, mc(maroon%80) msymbol(T) msize(medsmall)) ///
		  by(dist_nu, cols(9) compact note("") legend(off) imargin(0 0 0 0)) subtitle("") ///
	      scheme(s1mono) saving(exyr`i', replace)
		  
	graph display, xsize(30) ysize(10)	
restore

// exyr 10
preserve
	cd "${path}/${subpath}/Data"
	use results, clear
	local i 10
	keep if exyr == "`i'" & outcome == "le40"
	replace exyr = "`i' years" if exyr == "`i'"

	graph dot diff_le40, over(fw) over(cohort_nu, axis(off)) ///
		  ytit("") note("") asyvars ndots(0) ///
		  yscale(range(-15 15)) ylab(-15(5)15, nogrid) ytick(-15(5)15) ymtick(-15(1)15) ///
		  lintensity(.5) yline(0 , lc(black) lp(solid)) ///
		  marker(1, mc(navy%80) msize(medsmall)) ///
		  marker(2, mc(maroon%80) msymbol(T) msize(medsmall)) ///
		  by(dist_nu, cols(9) compact note("") legend(off) imargin(0 0 0 0)) subtitle("") ///
	      scheme(s1mono) saving(exyr`i', replace)
		  
	graph display, xsize(30) ysize(10)	
restore


graph combine exyr2.gph exyr3.gph exyr5.gph exyr10.gph, ////
			  rows(4) imargin(tiny) ///
			  b2title("Difference (years)", size(small)) /// 
			  scheme(s1mono) graphregion(color(white))

graph display, xsize(45) ysize(23)	

cd "${path}/${subpath}/Output/plots_supp"

graph export "le40.png", replace width(10000)

