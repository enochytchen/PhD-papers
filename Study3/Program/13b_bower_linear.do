// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global sys `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global sys `"P:"'

/*
Created: Enoch Chen
Original file: /SweCan/Site_specific/CML/LEL_2015/Programs/predictions.do
*/

/*clear the memory, change woking directory and read in the data*/
clear all 
cd "${sys}/SweCan_CML/Enoch_PhD/Study3_QALY/Data"

use 13a_bower_analysisdata_cml if yeardiag <= 2013, clear

****** Create new observations for all covariate combinations that don't exist ********************

/*add "missing" observations to use for predictions, but not for modeling*/
local added = 0
forvalues k=1/2 {
forvalues i=1973/2013 {
	forvalues j=45/99  {
			
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
// Linear age, so we don't run this line
// rcsgen agediag, df(4) gen(agespl) orthog

/*spline variable for year of diagnosis*/
rcsgen yeardiag, df(4) gen(yearspl) orthog

/*variables for interactions*/
forvalues i = 1/4 {
		gen yearsplage`i'=yearspl`i'*agediag
	}

gen sexyearspl1=female*yearspl1
gen sexage=female*agediag
***********************Stset with exit at year 10, and merge on background rate************

stset stime, fail(dead==1) id(lpnr) exit(t 15)

/*create age and year variables at exit time, with the same names as in the popmort file*/
gen _year= floor(min(yeardiag + _t, 2013)) //popmort goes to year 2013
gen _age=floor(min(agediag + _t, 99))		// the popmort only goes to age 99

/*merge with the popmort file to get hazard rate by age sex and year*/
capture noi drop rate
merge m:1 _year sex _age ///
	using "${sys}/SweCan/Popmort/popmort2014", nolabel keepusing(rate) ///
	assert(2 3) keep(3) noreport nogenerate 
drop _age _year


***********************STPM2 model *************************
stpm2 yearspl* agediag female yearsplage1 sexyearspl1 sexage, scale(h) df(5) bhazard(rate) difficult ///
	tvc(yearspl1 agediag female) dftvc(2)

*create new dataset with one of each variable combination*
bysort yeardiag agediag sex: keep if _n==1

count 


*do the predictions*

/*loss in expectation of life*/
predict llind , lifelost mergeby(_year sex _age) diagage(agediag) diagyear(yeardiag) nodes(40) tinf(80) ///
				using("${sys}/SweCan/Popmort/SCB_projection/popmort_projection") stub(surv) ci maxyear(2070)

***************************************
keep surv* llind* sex yeardiag agediag
sort sex yeardiag agediag
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

// The results are slightly different because the prediction data has age lower than 45.
// This has a subtle impact on the results, so it looks a little different from Bower2016
list if (agediag == 45 | agediag == 55 | agediag == 65 | agediag == 75 | agediag == 85) & yeardiag == 2010


pwd

save 13b_bower_linear, replace	
