/*==============================================================================
FILENAME: cml_studypop.do

PROJECT: Enoch PhD Study 1 (Comparing survival extrapolation)
	
AUTHOR: Enoch Yi-Tung Chen (enoch.yitung.chen@ki.se)

CREATED: Enoch Yi-Tung Chen	
UPDATED: 20220818 Enoch Chen: added CML as one of the cancer types

STATA VERSION: 17.0
==============================================================================*/

// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global path `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global path `"P:"'

// Set subpath
global subpath `"SweCan/Site_specific/Enoch_Study1"'

// Set the working directory as where the do files are
cd "${path}/${subpath}/Program"

// Log file
local todaydate: di %tdCYND date(c(current_date),"DMY")
global dofilename = "cml_studypop" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace

/*=======================================================================*/
// Refer to 
// P:\SweCan\Site_specific\CML\LEL_2015\Programs\analysisdata_cml

// *Take the data from P drive
use "${path}/SweCan/Data/cml_20211101.dta", clear

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
label define regionlabel 1 "Stockholm" 2 "Uppsala/Örebro" 3 "Linköing" 4 "Lund/Malmö" 5 "Göteborg" 7 "Umeå" 
label values region regionlabel 


/*put date format on exitdate*/
format exitdate %td

/*drop variables that are not of interest*/
drop dup ulorsak tnmgrund obd1 digr ben snomedo10 snomed3 figo m n t sida ///
	 icd9 icdo10 icdo3 ben_char dbgrund1 tnr tnrmal imputed_deathdate ///
	 monthdiag yeardeath birthdate first_emigration
	 
// Only keep the first CML for each individual
sort lpnr diagdate
duplicates drop lpnr, force

// Only include 18 <= age <= 99
keep if age>=18 & age <100

// age groups 
egen agegroup=cut(age), at(0 60 70 100) icodes
label variable agegroup "Age group"
label define agegroup 0 "18-59" 1 "60-69" 2 "70-99" 
label values agegroup agegroup

// Create dummy varaiables for age categories
quietly tab agegroup, gen(agegroup)

// generate variable censor
gen censor = 1 if dead == 0
replace censor = 0 if dead == 1

// Only dx 1981-90 
keep if yeardiag>=1981 &  yeardiag<=1990

save "${path}/${subpath}/Data/cml.dta",replace

/*-----Split by age group------*/
foreach i in 0 1 2{
	use "${path}/${subpath}/Data/cml.dta",clear
	keep if agegroup == `i'
	
	label data "CML patients diagnosed during 1981 to 1990"
	save "${path}/${subpath}/Data/cml_ag`i'.dta", replace
}
	
/***************************** FILE ENDS HERE**************************/
exit
log close

