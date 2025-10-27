// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global path `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global path `"P:"'

/****************************************************************************
Purpose: Fit Bower's flexible parametric relative survival models to the CML survival data

Created: 2025-02-10 by Enoch Chen
Updated: 
****************************************************************************/

clear all

// Data originally from SweCan/Data
use "../Data/PIAMOD_01_analysisdata_cml.dta", replace


****** Create new observations for all covariate combinations that don't exist ********************

/*add "missing" observations to use for predictions, but not for modeling*/
local added = 0
forvalues k=1/2 {
forvalues i=1973/2019 {
	forvalues j=50/99  {
			
			qui count if yeardiag==`i' & agediag==`j' & sex==`k'
			if r(N)==0 {
				local added=`added'+1
				local N=_N+1
				qui set obs `N'
				qui replace yeardiag=`i' if _n==_N
				qui replace agediag=`j' if _n==_N
				qui replace sex=`k' if _n==_N
			}
	}
}
}

display as text "Number of observations added: " `added'

*Create variables of interest for modeling and pedictions **
/*dummy variable for sex*/
gen female=(sex==2)

/*spline variable for age at diagnosis*/
rcsgen agediag, df(4) gen(agespl) orthog
matrix Rage = r(R)
global knotsage `r(knots)'

/*spline variable for year of diagnosis*/
rcsgen yeardiag, df(4) gen(yearspl) orthog
matrix Ryear = r(R)
global knotsyear `r(knots)'

/*variables for interactions*/
forvalues i = 1/4 {
	forvalues j = 1/4 {
		gen yearagespl`i'`j'=yearspl`i'*agespl`j'
	}
}
gen sexyearspl1=female*yearspl1
gen sexagespl1=female*agespl1

*Stset with exit at year 15, and merge on background rate*

stset stime, fail(dead==1) id(lpnr) exit(t 15)


/*create age and year variables at exit time, with the same names as in the popmort file*/
gen _year= floor(min(yeardiag + _t, 2022)) 
gen _age=floor(min(agediag + _t, 99))

/*merge with the popmort file to get hazard rate by age sex and year*/
merge m:1 _year sex _age ///
	using "../Data/PIAMOD_01_popmort2022_proj", nolabel keepusing(rate) ///
	assert(2 3) keep(3) noreport nogenerate 
drop _age _year


***********************STPM2 model *************************
stpm2 yearspl* agespl* female yearagespl11 sexyearspl1 sexagespl1, scale(h) df(5) bhazard(rate) difficult ///
	tvc(yearspl1 agespl1 female /*yearagespl11 sexyearspl1 sexagespl1*/) dftvc(2)
	
bysort yeardiag agediag sex: keep if _n==1
*keep if agediag==50 | agediag==55 | agediag==60 | agediag==65 | agediag==70 | agediag==75 | agediag==80 | agediag==85 | agediag==90 | agediag==95
count 

*********************Predicted relative survival *************
gen t1=1
gen t5=5
gen t10=10

predict rs1, s ci timevar(t1)
predict rs5, s ci timevar(t5)
predict rs10, s ci timevar(t10)

*******************Plot************************
// 1-year RS
twoway	    (line rs1 yeardiag if agediag==55 & female == 1, sort lpattern(solid) lwidth(medthick) lcolor(gs0)) ///
			(line rs1 yeardiag if agediag==65 & female == 1, sort lpattern(longdash) lwidth(medthick) lcolor(gs0)) ///
			(line rs1 yeardiag if agediag==75 & female == 1, sort lpattern(shortdash) lwidth(medthick) lcolor(gs0)) ///
			(line rs1 yeardiag if agediag==85 & female == 1, sort lpattern(dot) lwidth(medthick) lcolor(gs0)) ///
			, legend(label(1 "Age 55") label(2 "Age 65") label(3 "Age 75") label(4 "Age 85")) scheme(sj) 
			
// 5-year RS
twoway	    (line rs5 yeardiag if agediag==55 & female == 1, sort lpattern(solid) lwidth(medthick) lcolor(gs0)) ///
			(line rs5 yeardiag if agediag==65 & female == 1, sort lpattern(longdash) lwidth(medthick) lcolor(gs0)) ///
			(line rs5 yeardiag if agediag==75 & female == 1, sort lpattern(shortdash) lwidth(medthick) lcolor(gs0)) ///
			(line rs5 yeardiag if agediag==85 & female == 1, sort lpattern(dot) lwidth(medthick) lcolor(gs0)) ///
			, legend(label(1 "Age 55") label(2 "Age 65") label(3 "Age 75") label(4 "Age 85")) scheme(sj) 
			
			
// 10-year RS
twoway	    (line rs10 yeardiag if agediag==55 & female == 1, sort lpattern(solid) lwidth(medthick) lcolor(gs0)) ///
			(line rs10 yeardiag if agediag==65 & female == 1, sort lpattern(longdash) lwidth(medthick) lcolor(gs0)) ///
			(line rs10 yeardiag if agediag==75 & female == 1, sort lpattern(shortdash) lwidth(medthick) lcolor(gs0)) ///
			(line rs10 yeardiag if agediag==85 & female == 1, sort lpattern(dot) lwidth(medthick) lcolor(gs0)) ///
			, legend(label(1 "Age 55") label(2 "Age 65") label(3 "Age 75") label(4 "Age 85")) scheme(sj) 
			
**************************************************
** Predict outside of the range of data to 2030 **
**************************************************

clear

range agediag 50 99 50
range yeardiag 1973 2030 58
range female 0 1 2
range _t 1 40 40

fillin agediag yeardiag female _t
drop if missing(agediag, yeardiag, female, _t)
gen _d = .
sort yeardiag agediag female _t

rcsgen agediag, gen(agespl) rmatrix(Rage) knots($knotsage)
rcsgen yeardiag, gen(yearspl) rmatrix(Ryear) knots($knotsyear)

/*variables for interactions*/
forvalues i = 1/4 {
	forvalues j = 1/4 {
		gen yearagespl`i'`j'=yearspl`i'*agespl`j'
	}
}
gen sexyearspl1=female*yearspl1
gen sexagespl1=female*agespl1


predict rs, s timevar(_t)

*******************Plot************************
// 1-year RS
twoway	    (line rs yeardiag if _t == 1 & agediag==55 & female == 1, sort lpattern(solid) lwidth(medthick) lcolor(gs0)) ///
			(line rs yeardiag if _t == 1 & agediag==65 & female == 1, sort lpattern(longdash) lwidth(medthick) lcolor(gs0)) ///
			(line rs yeardiag if _t == 1 & agediag==75 & female == 1, sort lpattern(shortdash) lwidth(medthick) lcolor(gs0)) ///
			(line rs yeardiag if _t == 1 & agediag==85 & female == 1, sort lpattern(dot) lwidth(medthick) lcolor(gs0)) ///
			, legend(label(1 "Age 55") label(2 "Age 65") label(3 "Age 75") label(4 "Age 85")) scheme(sj) ///
			  yscale(range(0 1)) ///
			  xlabel(1970(5)2030) ///
			  ylabel(0(0.2)1)
			
// 5-year RS
twoway	    (line rs yeardiag if _t == 5 & agediag==55 & female == 1, sort lpattern(solid) lwidth(medthick) lcolor(gs0)) ///
			(line rs yeardiag if _t == 5 & agediag==65 & female == 1, sort lpattern(longdash) lwidth(medthick) lcolor(gs0)) ///
			(line rs yeardiag if _t == 5 & agediag==75 & female == 1, sort lpattern(shortdash) lwidth(medthick) lcolor(gs0)) ///
			(line rs yeardiag if _t == 5 & agediag==85 & female == 1, sort lpattern(dot) lwidth(medthick) lcolor(gs0)) ///
			, legend(label(1 "Age 55") label(2 "Age 65") label(3 "Age 75") label(4 "Age 85")) scheme(sj) ///
			  yscale(range(0 1)) ///
			  xlabel(1970(5)2030) ///
			  ylabel(0(0.2)1)
			
			
// 10-year RS
twoway	    (line rs yeardiag if _t == 10 & agediag==55 & female == 1, sort lpattern(solid) lwidth(medthick) lcolor(gs0)) ///
			(line rs yeardiag if _t == 10 & agediag==65 & female == 1, sort lpattern(longdash) lwidth(medthick) lcolor(gs0)) ///
			(line rs yeardiag if _t == 10 & agediag==75 & female == 1, sort lpattern(shortdash) lwidth(medthick) lcolor(gs0)) ///
			(line rs yeardiag if _t == 10 & agediag==85 & female == 1, sort lpattern(dot) lwidth(medthick) lcolor(gs0)) ///
			, legend(label(1 "Age 55") label(2 "Age 65") label(3 "Age 75") label(4 "Age 85")) scheme(sj) ///
			  yscale(range(0 1)) ///
			  xlabel(1970(5)2030) ///
			  ylabel(0(0.2)1)
