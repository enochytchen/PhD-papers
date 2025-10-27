// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global sys `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global sys `"P:"'

/*
Created: 20221114 Enoch Chen
Updated: 
Purpose: reproduce Bower2016's results using her syntax at SweCan/Site_specific/CML/LEL_2015/Programs/timetrends_analysis.do

Updated data
max yeardiag 2019
max yeardeath 2020
max popmort 2021
popmort projection: remain the same as popmort2021
*/

/*clear the memory, change woking directory and read in the data*/
clear all 
cd "${sys}/SweCan_CML/Enoch_PhD/Study2_QALY/Data"

use analysisdata_cml

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

/*spline variable for year of diagnosis*/
rcsgen yeardiag, df(4) gen(yearspl) orthog

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
gen _year= floor(min(yeardiag + _t, 2021))  //popmort goes to year 2021
gen _age=floor(min(agediag + _t, 99))		// the popmort only goes to age 99

/*merge with the popmort file to get hazard rate by age sex and year*/
capture noi drop rate
merge m:1 _year sex _age ///
	using "${sys}/SweCan/Popmort/popmort2021", nolabel keepusing(rate) ///
	assert(2 3) keep(3) noreport nogenerate 
drop _age _year

***********************STPM2 model *************************
stpm2 yearspl* agespl* female yearagespl11 sexyearspl1 sexagespl1, scale(h) df(5) bhazard(rate) difficult ///
	tvc(yearspl1 agespl1 female) dftvc(2)


*create new dataset with one of each variable combination*

bysort yeardiag agediag sex: keep if _n==1
*keep if agediag==50 | agediag==55 | agediag==60 | agediag==65 | agediag==70 | agediag==75 | agediag==80 | agediag==85 | agediag==90 | agediag==95
count 


*do the predictions*

/*loss in expectation of life*/
predict llind , lifelost mergeby(_year sex _age) diagage(agediag) diagyear(yeardiag) nodes(40) tinf(80) ///
				using("${sys}/SweCan/Popmort/popmort2021") stub(surv) ci maxyear(2050)

***************************************
keep surv* llind* sex yeardiag agediag
sort sex yeardiag agediag
rename (survexp survobs survobs_lci survobs_uci llind llind_lci llind_uci) (leexp leobs leobs_lci leobs_uci lle lle_lci lle_uci)

list if agediag == 55 & yeardiag == 2010

cd "${sys}/SweCan_CML/Enoch_PhD/Study2_QALY/Data"

save update_bower2016_mod3, replace	
*END OF FILE*


