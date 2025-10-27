/*==============================================================================
FILENAME: breast_studypop.do

PROJECT: Enoch PhD Study 1 (Comparing survival extrapolation)
	
AUTHOR: Enoch Yi-Tung Chen (enoch.yitung.chen@ki.se)

CREATED: Enoch Yi-Tung Chen	
UPDATED: 20220818 Enoch Chen: subset the study population by age groups

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
global dofilename = "breast_studypop" // Rename this one every time

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace
/*-------------------------------------------------------------------*/
*Take the data from P drive
use "${path}/SweCan/Data/breast_20211101.dta", clear
tab icd7
keep if     icd7 == "170" | ///    /*Malignant neoplasm of breast */
            icd7 == "1701" | ///    /* */
			icd7 == "1702" | ///    /* */
			icd7 == "1707" | ///    /* */
			icd7 == "1708" | ///    /* */
			icd7 == "1709" 

/*Convert string (except icd/snomed/pad) to numeric variables*/
destring (digr), replace
destring (obd1), replace
destring (dbgrund1), replace
destring (tnmgrund), replace

/*Correct/translate variable names and labels*/
label variable lpnr "ID"

label variable sex "Sex"
label define sex 1 "male" 2 "female" /*Define Sex*/
lab values sex sex

generate female=(sex==2) // Create an indicator variable for sex

label variable agediag "Age at diagnosis"

label variable diagdate "Date of diagnosis"

label variable digr "Basis of diagnose"

/*Define the value label, Bilaga_variabellistan p 15*/
lab def digr 1 "Clinical", add
lab def digr 2 "Radiological", add
lab def digr 3 "Histopahological", add
lab def digr 4 "Autopsy w PAD", add
lab def digr 5 "Cytological", add
lab def digr 6 "Surgical wo PAD", add
lab def digr 7 "Autopsy wo PAD", add
lab def digr 8 "Other lab", add
lab values digr digr

rename icdo10 icdo2
label variable icdo2 "ICD-O/2"

label variable pad "C24/hist"

rename snomedo10 snomed2
label variable snomed2 "SNOMED-O/2"

/*MEB Guideline SwedishCancerRegister p 5*/
label variable ben "Benign"
lab def ben 3 "benign", add
lab values ben ben

label variable deathdate "Date of death"

/*MEB Guideline SwedishCancerRegister p 15*/
label variable obd1 "Autopsy finding"
lab def obd1 1 "Unexpected", add
lab def obd1 2 "Expected", add
lab def obd1 3 "?", add
lab values obd1 obd1

label variable ulorsak "Cause of death"

/*MEB Guideline SwedishCancerRegister p 19*/
label variable dbgrund1 "Cause of death base"

label variable tnmgrund "TNM base"
label variable m "M-stage"
label variable n "N-stage"
label variable t "T-stage"

rename m mstage
rename n nstage
rename t tstage

label variable tnrmal "Tumor count"

********************************************************************************
/*Drop benign cancers*/
drop if ben == 3 	
********************************************************************************

********************************************************************************
/* Identify and drop cases found incidentally at autopsy*/
/* First classify patients with zero survival time and autopsy as the
   basis of diagnosis as incidental */ 
tab digr obd1, missing 

replace obd1=1 if (diagdate == deathdate) & digr == 4

tab digr obd1, missing
	
drop if obd1 == 1 
********************************************************************************

/*******************************************************************************
Drop cancers that have not been microscopically verified (keeping only 3,4,5)
*******************************************************************************/
drop if digr == 1 | digr == 2 | digr == 6 | digr == 7 | digr == 8 | digr == .
/*******************************************************************************
RESTRICT TO ADENOCARCINOMA
196 & 996 is unknown but a majority of these patients are likely to have adenocarcinoma
*******************************************************************************/
keep if pad == "096" | pad == "996" | pad == "196" 

// Only keep the first breast cancer for each individual
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

save "${path}/${subpath}/Data/breast.dta",replace

/*-----Split by age group------*/
foreach i in 0 1 2{
	use "${path}/${subpath}/Data/breast.dta",clear
	keep if agegroup == `i'
	
	label data "breast cancer patients diagnosed during 1981 to 1990"
	save "${path}/${subpath}/Data/breast_ag`i'.dta", replace
}
	
/***************************** FILE ENDS HERE**************************/
exit
log close
