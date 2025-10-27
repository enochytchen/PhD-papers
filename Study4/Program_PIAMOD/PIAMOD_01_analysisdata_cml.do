// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global sys `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global sys `"P:"'

set more off
*********************************************************************************
* PIAMOD_01_analysisdata_cml.do
*
* Created: 20241010 Enoch Chen
* Updated: 20241114 Enoch Chen: Change age group definition
* Purpose: Obtain cleaned data from SweCan's CML data
* Reference: SweCan/Site_specific/CML/LEL_2015/Programs/analysisdata_cml.do
*		
* Input data: cml_20211101.dta 
*
* Output data: analysisdata_cml.dta
*
*******************************************************************************
clear
clear matrix
set mem 200m

cd "../Data"
use "${sys}/SweCan/Data/cml_20211101", clear
count
codebook yeardiag yeardeath

/**Only keep from 1973*/
codebook yeardiag
keep if yeardiag>1972


/*Only keep if not autopsy finding and not benign*/
tab obd1, miss
drop if obd1=="1"
tab ben, miss
drop if ben!=.

/*Only keep histologically verified and correct pad code*/
tab digr, miss
keep if digr=="3" | digr=="4" | digr=="5"
tab pad, miss
keep if pad=="226" 


/*Drop if negative survival time*/
drop if stime<0


/*Select first cml cancer for each individual*/
sort lpnr tnrmal
duplicates tag lpnr, gen(dup)
tab dup
duplicates drop lpnr, force


/*Give the region variable new labels*/
label define regionlabel 1 "Stockholm" 2 "Uppsala/orebro" 3 "Linkoing" 4 "Lund/Malmo" 5 "Goteborg" 7 "Umea" 
label values region regionlabel 

/*put date format on exitdate*/
format exitdate %td

/* Drop those who had diagnosis date after death*/
list lpnr diagdate deathdate if diagdate >= deathdate & deathdate != . 
drop if diagdate >= deathdate & deathdate != . 

/* Only age at diagnosis between 18 and 99 years */
keep if agediag >= 18 & agediag <= 99

/* Create a new variable with age in categories */
egen agegrp=cut(agediag), at(0, 45, 55, 65, 75, 130) icodes
cap label drop agegrp
label define agegrp 0 "18-44" 1 "45-54" 2 "55-64" 3 "65-74" 4 "75-99"
label values agegrp agegrp
label variable agegrp "Age at diagnosis categorical"
tab agediag agegrp, missing


/* Create a new variable for period of diagnosis */
egen yeargrp=cut(yeardiag), at(1973(3)2017 2020) icodes
cap label drop yeargrp
label define yeargrp 0 "1973-1975" 1 "1976-1978" 2 "1979-1981" 3 "1982-1984" 4 "1985-1987" 5 "1988-1990" 6 "1991-1993" 7 "1994-1996" 8 "1997-1999" 9 "2000-2002" 10 "2003-2005" 11 "2006-2008" 12 "2009-2011" 13 "2012-2014" 14 "2015-2019"

label values yeargrp yeargrp
label variable yeargrp "Year at diagnosis categorical"
tab yeardiag yeargrp, missing


/*drop variables that are not of interest*/
drop dup ulorsak tnmgrund obd1 digr ben snomedo10 snomed3 figo m n t sida ///
	 icd9 icdo10 icdo3 ben_char dbgrund1 tnr tnrmal imputed_deathdate ///
	 monthdiag yeardeath birthdate first_emigration

	 
/*Save the data*/
count	 
compress
label data "CML in Sweden dx 1973-2019, with follow-up until 2020"
save PIAMOD_01_analysisdata_cml, replace

*********************END OF FILE************************************;

