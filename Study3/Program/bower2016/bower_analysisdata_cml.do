// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global sys `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global sys `"P:"'

capture log close
set more off
*********************************************************************************
* analysisdata_cml.do
*
* Created: 20221114 Enoch Chen
* Updated: 
* Purpose: Update Bower2016's results using her syntax at SweCan/Site_specific/CML/LEL_2015/Programs/analysisdata_cml.do
*		
* Input data: cml_20211101.dta 
*
* Output data: analysisdata_cml.dta
*
*******************************************************************************
clear
clear matrix
set mem 200m
cd "${sys}/SweCan_CML/Enoch_PhD/Study2_QALY/Data"
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



/*drop if agediag<50, previously drop if <20- check how many*/
codebook agediag
count if agediag<20
count if agediag<50
drop if agediag<50


/*Give the region variable new labels*/
label define regionlabel 1 "Stockholm" 2 "Uppsala/orebro" 3 "Linkoing" 4 "Lund/Malmo" 5 "Goteborg" 7 "Umea" 
label values region regionlabel 


/*put date format on exitdate*/
format exitdate %td

  
/* Create a new variable with age in categories */
egen agecat=cut(agediag), at(0,50,60,70,80,120) icodes
label define agecat 0 "20-49" 1 "50-59" 2 "60-69" 3 "70-79" 4 "80+"
label values agecat agecat
label variable agecat "Age at diagnosis categorical"
*tab age agecat, missing


/* Create a new variable for period of diagnosis */
egen period=cut(yeardiag), at(1972,1983,1993,2003,2020) icodes
label define period 0 "1973-1982" 1 "1983-1992" 2 "1993-2002" 3 "2003-2019"
label values period period
label variable period "Period of diagnosis categorical"
*tab yeardiag period, missing



/*drop variables that are not of interest*/
drop dup ulorsak tnmgrund obd1 digr ben snomedo10 snomed3 figo m n t sida ///
	 icd9 icdo10 icdo3 ben_char dbgrund1 tnr tnrmal imputed_deathdate ///
	 monthdiag yeardeath birthdate first_emigration

	 
/*Save the data*/
count	 
compress
label data "cml in Sweden dx 1973-2019, with follow-up until 2020"
save analysisdata_cml, replace


log close
******************************************END OF FILE************************************;

