// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global sys `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global sys `"P:"'

/*
Created: 20240111 Enoch Chen: fit bower's model but with the CML QALY's study pop
							  variable yeardiag was removed. It caused convergence issue.
							  The data only have patients dx during 2007 to 2017.

max yeardiag 2017
max yeardeath 2018
max popmort 2018
*/

/*clear the memory, change woking directory and read in the data*/
clear all 
pwd
cd "../Data"

use 00_studypop, clear


//
replace date_exit = deathdate if deathdate != .
gen stime = (date_exit - date_tki1)/365.241

/*add "missing" observations to use for predictions, but not for modeling*/
local added = 0
forvalues k=1/2 {
	forvalues j=45/99  {
			qui count if dxage ==`j' & sex==`k'
			if r(N)==0 {
				local added=`added'+1
				local N=_N+1
				qui set obs `N'
				qui replace dxage=`j' if _n==_N
				qui replace sex=`k' if _n==_N
			}
	}
}


display as text "Number of observations added: " `added'

*Create variables of interest for modeling and pedictions **
/*dummy variable for sex*/
gen female=(sex==2)

gen sexage =female*dxage

*Stset with exit at year 15, and merge on background rate*
stset stime, fail(death==1) id(idnr)

/*create age and year variables at exit time, with the same names as in the popmort file*/
gen _year= floor(min(dxyear + _t, 2018))  // popmort goes to year 2020
gen _age=floor(min(dxage + _t, 99))		// the popmort only goes to age 99

/*merge with the popmort file to get hazard rate by age sex and year*/
capture noi drop rate
merge m:1 _year sex _age ///
	using "01_popmort2022_proj", nolabel keepusing(rate) ///
	assert(2 3) keep(3) noreport nogenerate 
drop _age _year

***********************STPM2 model *************************
stpm2 dxage female sexage, scale(h) df(5) bhazard(rate) difficult noorth ///
	tvc(female) dftvc(2)


*create new dataset with one of each variable combination*

bysort dxage sex: keep if _n==1

count 


*do the predictions*

/*loss in expectation of life*/
predict llind , lifelost mergeby(_year sex _age) diagage(dxage) diagyear(dxyear) nodes(40) tinf(80) ///
				using("01_popmort2022_proj") stub(surv) ci maxyear(2018)

***************************************
keep surv* llind* sex dxage
sort sex dxage
rename (survexp survobs survobs_lci survobs_uci llind llind_lci llind_uci) (leexp leobs leobs_lci leobs_uci lle lle_lci lle_uci)
// Proportion
gen plle =  lle/leexp
gen plle_lci = lle_lci/leexp
gen plle_uci = lle_uci/leexp

local varlist "leexp leobs leobs_lci leobs_uci lle lle_lci lle_uci"

foreach var in `varlist'{
	format `var' %9.1f
}

local varlist2 "plle plle_lci plle_uci"

foreach var in `varlist2'{
	format `var' %9.2f
}

list if (dxage == 45 | dxage == 55 | dxage == 65 | dxage == 75 | dxage == 85)

save 13e_bower_SweCanCML_linear, replace	

*END OF FILE*


