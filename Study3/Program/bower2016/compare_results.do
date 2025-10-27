// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global sys `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global sys `"P:"'

/*
Created: 20230130 Enoch Chen
Updated: 20230214 Enoch Chen
Purpose: reproduce Bower2016's results with longer follow-up

Bower2016 
max yeardiag 2013
max yeardeath 2013
max popmort 2014
popmort projection: use SCB projection for year beyond 2014

update_bower2016_mod0 
max yeardiag 2013
max yeardeath 2020
max popmort 2014
popmort projection: use SCB projection for year beyond 2014

update_bower2016_mod1
max yeardiag 2013
max yeardeath 2013
max popmort 2014
popmort projection: remain the same as popmort2014

update_bower2016_mod2
max yeardiag 2019
max yeardeath 2020
max popmort 2021
popmort projection: use SCB projection for year beyond 2021

update_bower2016_mod3
max yeardiag 2019
max yeardeath 2020
max popmort 2021
popmort projection: remain the same as popmort2021

CML_rel_microsim_approach1&3,CML_ac_microsim
max yeardiag 2012
max yeardeath 2018
max popmort 2021
popmort projection: remain the same as popmort2021

*/
clear all 

use "${sys}/SweCan/Site_specific/CML/LEL_2015/Data/timetrendests", clear 
keep surv* llind* sex yeardiag agediag
sort sex yeardiag agediag
rename (survexp survobs survobs_lci survobs_uci llind llind_lci llind_uci) (leexp leobs leobs_lci leobs_uci lle lle_lci lle_uci)

drop surv5*
gen mod = "Bower2016"

cd "${sys}/SweCan_CML/Enoch_PhD/Study2_QALY/Data"

append using "update_bower2016_mod0", generate(source)
replace mod = "update_bower2016_mod0" if source == 1
drop source
append using "update_bower2016_mod1", generate(source)
replace mod = "update_bower2016_mod1" if source == 1
drop source
append using "update_bower2016_mod2", generate(source)
replace mod = "update_bower2016_mod2" if source == 1
drop source
append using "update_bower2016_mod3", generate(source)
replace mod = "update_bower2016_mod3" if source == 1
drop source

preserve
	// Read CML_rel_microsim
	import delimited CML_rel_microsim.csv, clear
	// Rename sex
	gen sex_temp = .
	replace sex_temp = 1 if sex == "male"
	replace sex_temp = 2 if sex == "female"
	drop sex
	rename sex_temp sex
	tempfile CML_rel_microsim
	save `CML_rel_microsim'
	
	// Read CML_ac_microsim
	import delimited CML_ac_microsim.csv, clear
	// Rename sex
	gen sex_temp = .
	replace sex_temp = 1 if sex == "male"
	replace sex_temp = 2 if sex == "female"
	drop sex
	rename sex_temp sex
	tempfile CML_ac_microsim
	save `CML_ac_microsim'
restore

append using `CML_rel_microsim', generate(source)
replace mod = "CML_rel_microsim" if source == 1
drop source
append using `CML_ac_microsim', generate(source)
replace mod = "CML_ac_microsim" if source == 1
drop source

replace yeardiag = 2010 if mod == "CML_rel_microsim" | mod == "CML_ac_microsim"

// Change working directory to Output
cd ../Output
pwd

// Compare LE estimates
// Compare different scenarios of Bower2016's model
// Male
tw (line leobs agediag if sex == 1 & yeardiag == 2010 & mod == "Bower2016", lcol(black)) ///
   (line leobs agediag if sex == 1 & yeardiag == 2010 & mod == "update_bower2016_mod0", lcol(green)) ///
   (line leobs agediag if sex == 1 & yeardiag == 2010 & mod == "update_bower2016_mod1", lcol(red)) ///
   (line leobs agediag if sex == 1 & yeardiag == 2010 & mod == "update_bower2016_mod2", lcol(blue)) ///
   (line leobs agediag if sex == 1 & yeardiag == 2010 & mod == "update_bower2016_mod3", lcol(dkorange)), ///
   legend(order(1 "Bower2016" 2 "Senario 0" 3 "Senario 1" 4 "Senario 2" 5 "Senario 3*")) ///
   ytitle("Life expectancy (years)") yscale(range(0 35)) ylab(0(5)35) ///
   title("CML patients, males, diagnosis in 2010")

graph export compare_results_Bower2016_m.png, as(png) replace   

// Female 
tw (line leobs agediag if sex == 2 & yeardiag == 2010 & mod == "Bower2016", lcol(black)) ///
   (line leobs agediag if sex == 2 & yeardiag == 2010 & mod == "update_bower2016_mod0", lcol(green)) ///
   (line leobs agediag if sex == 2 & yeardiag == 2010 & mod == "update_bower2016_mod1", lcol(red)) ///
   (line leobs agediag if sex == 2 & yeardiag == 2010 & mod == "update_bower2016_mod2", lcol(blue)) ///
   (line leobs agediag if sex == 2 & yeardiag == 2010 & mod == "update_bower2016_mod3", lcol(dkorange)), ///
   legend(order(1 "Bower2016" 2 "Senario 0" 3 "Senario 1" 4 "Senario 2" 5 "Senario 3*")) ///
   ytitle("Life expectancy (years)") yscale(range(0 35)) ylab(0(5)35) ///
   title("CML patients, females, diagnosis in 2010")

graph export compare_results_Bower2016_f.png, as(png) replace   
   
// Compare selected Bower2016's model vs. 
// CML_rel_microsim_approach 1 vs. CML_rel_microsim_approach 3 vs.
// CML_ac_microsim
// Male
tw (line leobs agediag if sex == 1 & yeardiag == 2010 & mod == "update_bower2016_mod3", lcol(green)) ///
   (line le agediag if sex == 1 & yeardiag == 2010 & mod == "CML_rel_microsim", lcol(red)) ///
   (line le agediag if sex == 1 & yeardiag == 2010 & mod == "CML_rel_microsim_approach3", lcol(yellow)) ///
   (line le agediag if sex == 1 & yeardiag == 2010 & mod == "CML_ac_microsim", lcol(blue)), ///
   legend(order(1 "scenario 3* (Bower2016_updated)" 2 "CML multi-state approach 1 (RS)" 3 "CML multi-state approach 3 (RS)" 4 "CML multi-state (AC)")) ///
   ytitle("Life expectancy (years)") yscale(range(0 35)) ylab(0(5)35) ///
   title("CML patients, males, diagnosis in 2010 or 2007-2012")
graph export compare_results_CML_microsim_m.png, as(png) replace   

// Female
tw (line leobs agediag if sex == 2 & yeardiag == 2010 & mod == "update_bower2016_mod3", lcol(green)) ///
   (line le agediag if sex == 2 & yeardiag == 2010 & mod == "CML_rel_microsim_approach1", lcol(red)) ///
   (line le agediag if sex == 2 & yeardiag == 2010 & mod == "CML_rel_microsim_approach3", lcol(yellow)) ///
   (line le agediag if sex == 2 & yeardiag == 2010 & mod == "CML_ac_microsim", lcol(blue)), ///
   legend(order(1 "scenario 3* (Bower2016_updated)" 2 "CML multi-state approach 1 (RS)" 3 "CML multi-state approach 3 (RS)" 4 "CML multi-state (AC)")) ///
   ytitle("Life expectancy (years)") yscale(range(0 35)) ylab(0(5)35) ///
   title("CML patients, females, diagnosis in 2010 or 2007-2012")   
graph export compare_results_CML_microsim_f.png, as(png) replace   

// Re-read datasets with only age 55 65 75 85
drop if mod == "CML_rel_microsim" | mod == "CML_ac_microsim"

// Change working directory to Data
cd ../Data
pwd

preserve
	// Read CML_rel_microsim55_85
	import delimited CML_rel_microsim55_85.csv, clear
	// Rename sex
	gen sex_temp = .
	replace sex_temp = 1 if sex == "male"
	replace sex_temp = 2 if sex == "female"
	drop sex
	rename sex_temp sex
	// Rename 
	rename(le_exp le_mean le_lower le_upper lel_mean lel_lower lel_upper) (leexp leobs leobs_lci leobs_uci lle lle_lci lle_uci)
	rename(qale_exp qale_mean qale_lower qale_upper lqale_mean lqale_lower lqale_upper)(qaleexp qaleobs qaleobs_lci qaleobs_uci lqale lqale_lci lqale_uci)
	rename(qale_ad_mean qale_ad_lower qale_ad_upper lqale_ad_mean lqale_ad_lower lqale_ad_upper)(qaleobs_ad qaleobs_ad_lci qaleobs_ad_uci lqale_ad lqale_ad_lci lqale_ad_uci)
	tempfile CML_rel_microsim55_85
	save `CML_rel_microsim55_85', replace
	
	// Read CML_ac_microsim55_85
	import delimited CML_ac_microsim55_85.csv, clear
	// Rename sex
	gen sex_temp = .
	replace sex_temp = 1 if sex == "male"
	replace sex_temp = 2 if sex == "female"
	drop sex
	rename sex_temp sex
	// Rename 
	rename(le_exp le_mean le_lower le_upper lel_mean lel_lower lel_upper) (leexp leobs leobs_lci leobs_uci lle lle_lci lle_uci)
	rename(qale_exp qale_mean qale_lower qale_upper lqale_mean lqale_lower lqale_upper)(qaleexp qaleobs qaleobs_lci qaleobs_uci lqale lqale_lci lqale_uci)
	rename(qale_ad_mean qale_ad_lower qale_ad_upper lqale_ad_mean lqale_ad_lower lqale_ad_upper)(qaleobs_ad qaleobs_ad_lci qaleobs_ad_uci lqale_ad lqale_ad_lci lqale_ad_uci)
	tempfile CML_ac_microsim55_85
	save `CML_ac_microsim55_85', replace
restore

append using `CML_rel_microsim55_85', generate(source)
replace mod = "CML_rel_microsim" if source == 1
drop source

append using `CML_ac_microsim55_85', generate(source)
replace mod = "CML_ac_microsim" if source == 1
drop source

replace yeardiag = 2010 if mod == "CML_rel_microsim" | mod == "CML_ac_microsim"

// LE
foreach i in 55 65 75 85{
	foreach j in 1 2{
	list yeardiag age sex leexp leobs leobs_lci leobs_uci  lle lle_lci lle_uci mod if agediag == `i' & yeardiag == 2010 & sex == `j' & mod != "update_bower2016_mod1" & mod !=  "update_bower2016_mod2"
}
}

// QALE
	// Without adjusting background pop value 
foreach i in 55 65 75 85{
	foreach j in 1 2{
	list yeardiag age sex qaleexp qaleobs qaleobs_lci qaleobs_uci lqale lqale_lci lqale_uci mod if agediag == `i' & yeardiag == 2010 & sex == `j' & mod == "CML_rel_microsim"
}
}
	// With adjusting background pop value 
foreach i in 55 65 75 85{
	foreach j in 1 2{
	list yeardiag age sex qaleexp qaleobs_ad qaleobs_ad_lci qaleobs_ad_uci  lqale_ad lqale_ad_lci lqale_ad_uci mod if agediag == `i' & yeardiag == 2010 & sex == `j' & mod == "CML_rel_microsim"
}
}
