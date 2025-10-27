/*==============================================================================
FILENAME: plot_main.do

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
global dofilename = "plots_main" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace

/*========Code begins here========*/
clear all
do "${path}/${subpath}/Program/results"
cd "${path}/${subpath}/Data"

// Just for y-axis (all distribution names)
preserve
use results, clear
local i 3
keep if exyr == "`i'" & outcome == "rmst10"
graph hbox diff_rmst10, over(fw) over(dist_nu) ///
		      ytit("") note("") asyvars ///
			  yscale(range(-2 2)) ylab(-2(1)2, nogrid) ///
			  lintensity(.5) yline(0 , lc(black) lp(solid)) ///
			  box(1, fc(white) lc(navy) lw(medthick)) marker(1, mc(navy) msize(vsmall)) ///
			  box(2, fc(white) lc(maroon) lw(medthick)) marker(2, mc(maroon) msize(vsmall)) ///
			  scheme(s1mono) graphregion(color(white)) ///
			  title("`i' years") saving(exyr_legend, replace) 
 
cd "${path}/${subpath}/Output/plots_main"

graph export "distname.png", replace width(10000)

twoway (line diff_rmst10 _t if fw == "ac", lcolor(navy) lwidth(medthick)) ///
	   (line diff_rmst10 _t if fw == "rel", lcolor(maroon) lwidth(medthick)), ///
	   legend(order(1 "All-cause survival" 2 "Relative survival"))

graph export "legend.png", replace width(10000)
	   
restore

// Using by() option to combine graph
// RMST10
cd "${path}/${subpath}/Data"

use results, clear

keep if (exyr == "2" |  exyr == "3" | exyr == "5") & outcome == "rmst10"

replace exyr = "2 years" if exyr == "2"
replace exyr = "3 years" if exyr == "3"
replace exyr = "5 years" if exyr == "5"

graph hbox diff_rmst10, over(fw) over(dist_nu, axis(off)) ///
		      ytit("Difference (years)") note("") asyvars ///
		      yscale(range(-4 4)) ylab(-4(1)4, nogrid labsize(small)) ytick(-4(1)4) ymtick(-4(0.5)4) ///
			  legend(off) ///
			  lintensity(.5) yline(0 , lc(black) lp(solid)) ///
			  box(1, fc(white) lc(navy) lw(medthick)) medtype(line) marker(1, mc(navy) msize(vsmall)) ///
			  box(2, fc(white) lc(maroon) lw(medthick)) marker(2, mc(maroon) msize(vsmall)) ///
			  by(exyr, cols(3) compact note("") legend(off)) ///
			  scheme(s1mono) saving(diff_rmst10, replace)

// Surv10			  
graph hbox diff_surv10, over(fw) over(dist_nu, axis(off)) ///
		      ytit("Difference (%)") note("") asyvars ///
			  yscale(range(-70 70)) ylab(-60(20)60, nogrid labsize(small)) ytick(-70(10)70) ymtick(-70(5)70) ///
			  legend(off) ///
			  lintensity(.5) yline(0 , lc(black) lp(solid)) ///
			  box(1, fc(white) lc(navy) lw(medthick)) medtype(line) marker(1, mc(navy) msize(vsmall)) ///
			  box(2, fc(white) lc(maroon) lw(medthick)) marker(2, mc(maroon) msize(vsmall)) ///
			  by(exyr, cols(3) compact note("") legend(off) imargin(0 0 0 0)) ///
			  scheme(s1mono) saving(diff_surv10, replace)
			  
graph combine diff_rmst10.gph diff_surv10.gph, rows(2) ///
			  scheme(s1mono) graphregion(color(white))

graph display, xsize(10) ysize(15)	

cd "${path}/${subpath}/Output/plots_main"

graph export "rmst10.png", replace width(5000)

// LE40
cd "${path}/${subpath}/Data"

use results, clear

keep if (exyr == "2" |  exyr == "3" | exyr == "5" | exyr == "10") & outcome == "le40"

destring exyr, replace

lab def exyr 2 "2 years" 3 "3 years" 5 "5 years" 10 "10 years"
lab value exyr exyr 

graph hbox diff_le40, over(fw) over(dist_nu, axis(off)) ///
		      ytit("Difference (years)") note("") asyvars ///
			  yscale(range(-15 15)) ylab(-15(5)15, nogrid labsize(small)) ytick(-15(5)15) ymtick(-15(1)15) ///
			  legend(off) ///
			  lintensity(.5) yline(0 , lc(black) lp(solid)) ///
			  box(1, fc(white) lc(navy) lw(medthick)) medtype(line) marker(1, mc(navy) msize(vsmall)) ///
			  box(2, fc(white) lc(maroon) lw(medthick)) marker(2, mc(maroon) msize(vsmall)) ///
			  by(exyr, cols(4) compact note("") legend(off)) ///
			  scheme(s1mono) saving(diff_le40, replace)

// Surv40			  
graph hbox diff_surv40, over(fw) over(dist_nu, axis(off)) ///
		      ytit("Difference (%)") note("") asyvars ///
			  yscale(range(-70 70)) ylab(-60(20)60, nogrid labsize(small)) ytick(-70(10)70) ymtick(-70(5)70) ///
			  legend(off) ///
			  lintensity(.5) yline(0 , lc(black) lp(solid)) ///
			  box(1, fc(white) lc(navy) lw(medthick)) medtype(line) marker(1, mc(navy) msize(vsmall)) ///
			  box(2, fc(white) lc(maroon) lw(medthick))  marker(2, mc(maroon) msize(vsmall)) ///
			  by(exyr, cols(4) compact note("") legend(off)) ///
			  scheme(s1mono) saving(diff_surv40, replace)
			  
graph combine diff_le40.gph diff_surv40.gph, rows(2) ///
			  scheme(s1mono) graphregion(color(white))

graph display, xsize(10) ysize(15)	

cd "${path}/${subpath}/Output/plots_main"

graph export "le40.png", replace width(5000)
/***************************** FILE ENDS HERE**************************/
log close 
exit
