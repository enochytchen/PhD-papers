// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global sys `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global sys `"P:"'

set more off
***************************************************************************
* 02_tabRS.do
*
* Created: 20241011 Enoch Chen
* Updated: 20241114 Enoch Chen
* Purpose: Estimate relative survival for CML
* Reference: Italy course q201 to q204
*			 MIAMOD_PIAMOD_USER_MANUAL_Rel3.0.pdf
*		
* Input data: cml_20211101.dta 
*
* Output data: analysisdata_cml.dta
*
****************************************************************************
// Read Swedish Cancer Register CML data
cd ../Data
use PIAMOD_01_analysisdata_cml.dta, replace

// stset 
stset stime, fail(dead == 1) id(lpnr)

// Prepare to merge popmort file 
gen _age=int(min(100, agediag+_t))
gen _year=int(min(2020, year(diagdate + _t*365.24)))

capture drop rate
merge m:1 _age sex _year using "PIAMOD_01_popmort2022_proj.dta", keepusing(rate) assert(2 3) keep(3)
drop _age _year _merge

// Lifetable estimates, 6-mon intervals
strs using PIAMOD_01_popmort2022_proj.dta, br(0(0.5)20) mergeby(_year _age sex) ///
					diagage(agediag) diagyear(yeardiag) pohar ///
					by(sex agegrp yeargrp) ///
					save(replace)

// Read the results of strs
use grouped.dta, clear
keep start end cns_pp sex agegrp yeargrp

// Label indicating the population/site/sex in exam
gen filterfield = ""
replace filterfield = "SECML_M" if sex == 1
replace filterfield = "SECML_F" if sex == 2  

// PERIOD OF DIAGNOSIS - FIRST YEAR
gen firstyear = .
replace firstyear = 1973 if yeargrp == 0
replace firstyear = 1976 if yeargrp == 1
replace firstyear = 1979 if yeargrp == 2
replace firstyear = 1982 if yeargrp == 3
replace firstyear = 1985 if yeargrp == 4
replace firstyear = 1988 if yeargrp == 5
replace firstyear = 1991 if yeargrp == 6
replace firstyear = 1994 if yeargrp == 7
replace firstyear = 1997 if yeargrp == 8
replace firstyear = 2000 if yeargrp == 9
replace firstyear = 2003 if yeargrp == 10
replace firstyear = 2006 if yeargrp == 11
replace firstyear = 2009 if yeargrp == 12
replace firstyear = 2012 if yeargrp == 13
replace firstyear = 2015 if yeargrp == 14


// PERIOD OF DIAGNOSIS - LAST YEAR
gen lastyear = .
replace lastyear = 1975 if yeargrp == 0
replace lastyear = 1978 if yeargrp == 1
replace lastyear = 1981 if yeargrp == 2
replace lastyear = 1984 if yeargrp == 3
replace lastyear = 1987 if yeargrp == 4
replace lastyear = 1990 if yeargrp == 5
replace lastyear = 1993 if yeargrp == 6
replace lastyear = 1996 if yeargrp == 7
replace lastyear = 1999 if yeargrp == 8
replace lastyear = 2002 if yeargrp == 9
replace lastyear = 2005 if yeargrp == 10
replace lastyear = 2008 if yeargrp == 11
replace lastyear = 2011 if yeargrp == 12
replace lastyear = 2014 if yeargrp == 13
replace lastyear = 2019 if yeargrp == 14


// AGE AT DIAGNOSIS - YOUNGEST
gen youngestage = .
replace youngestage = 18 if agegrp == 0
replace youngestage = 45 if agegrp == 1
replace youngestage = 55 if agegrp == 2
replace youngestage = 65 if agegrp == 3
replace youngestage = 75 if agegrp == 4


// AGE AT DIAGNOSIS - OLDEST
gen oldestage = .
replace oldestage = 44 if agegrp == 0
replace oldestage = 54 if agegrp == 1
replace oldestage = 64 if agegrp == 2
replace oldestage = 74 if agegrp == 3
replace oldestage = 99 if agegrp == 4

// Caution: PIAMOD requires follow up duration in six month intervals from date of diagnosis, expressed as integer, e.g., 1=6 months, 2=12 months, 3=18 months.
gen interval = end*2
gen year = end

// Keep essential variables
order filterfield sex firstyear lastyear youngestage oldestage interval cns_pp year
keep filterfield sex firstyear lastyear youngestage oldestage interval cns_pp year
sort sex firstyear youngestage interval

export delimited using "PIAMOD_02_tabRS_strs.txt", delimiter("tab")  novarnames replace
