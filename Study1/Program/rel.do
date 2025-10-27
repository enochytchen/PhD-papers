/*==============================================================================
FILENAME: rel.do

PROJECT: Enoch PhD Study 1 (Comparing survival extrapolation)
	
AUTHOR: Enoch Yi-Tung Chen (enoch.yitung.chen@ki.se)

CREATED: Enoch Yi-Tung Chen	
UPDATED: 20230817 Enoch Chen: updated follow-up time used for extrapolation
         20230228 Enoch Chen: Rerun the analysis with corrected expsurv (Ederer I)
         20230119 Enoch Chen: Paul L updated stpm2 scale(normal)

STATA VERSION: 17.0
==============================================================================*/

// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global path `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global path `"P:"'

// Set subpath
global subpath `"SweCan/Site_specific/Enoch_Study1"'

// Set the working directory as where the do files are
cd "${path}/${subpath}/Data"

// Log file
local todaydate: di %tdCYND date(c(current_date),"DMY")
global dofilename = "rel" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace

/*========Code begins here========*/
clear all
/*========Define programs for reading the file========*/
program define mergerate
	// Merge data with life table
	gen _year= floor(min(yeardiag + _t, 2020)) //popmort goes to year 2020
	gen _age=floor(min(agediag + _t, 107)) // the popmort only goes to age 107

	// merge with the popmort file to get hazard rate by age sex and year*/
	sort _year sex _age 
	merge m:1 _year sex _age /// 
		using "${path}/SweCan/Popmort/popmort2020.dta", ///
		nolabel keep(match master) keepusing(rate) 
		
	drop _age _year _merge
end

// Program: Predict RMST at year 10
// Break the program to two parts in order to generate a var indicating agegroup
cap program drop make_rmst10
program define make_rmst10
	clear
	range _t 0 10 121  // time in months: (12*10)+1=121

	// predict relative survival with 95% CIs
	gen _d = .
	gen rate = .
	predict rs, survival ci timevar(_t) 
	predict exh, haz ci timevar(_t)

end

cap program drop predict_rmst10
program define predict_rmst10	
	//cp=cp_e1*cr_e2
	gen s=cp_e1*rs
	gen s_lci = cp_e1 * rs_lci
	gen s_uci = cp_e1 * rs_uci
	replace s=1 if _t==0
	replace s = 0 if s == . // make surv 0 if the prediction shows .
	sort  _t
	
	// Integrate to obtain RMST10
	integ s _t , gen(rmst10)	
	
	// Calculate expected hazard from expected survival
	gen cum_exph = -ln(cp_e1)
	gen exph = (cum_exph - cum_exph[_n-1] ) / (`=1/12')
	replace exph = . if rs == 1 

	// Calculate all-cause hazard = expected haz + excess hazard
	gen ach = exph + exh
	gen ach_uci = exph + exh_uci
	gen ach_lci = exph + exh_lci
	
	replace ach = . if rs == 1 
   	replace ach_uci = . if rs == 1 
	replace ach_lci = . if rs == 1 
	
	// Generate a variable for cancer type
	gen cancer = "`cancertype'"
	gen method = "`1'"	
end

// Program: Predict LE at year 40
cap program drop make_le40
program define make_le40
	clear
	range _t 0 40 481  // time in months: (12*40)+1=481

	// predict relative survival with 95% CIs
	gen _d = .
	gen rate = .
	predict rs, survival ci timevar(_t) 
	predict exh, haz ci timevar(_t)

end

cap program drop predict_le40
program define predict_le40
	//cp=cp_e1*cr_e2
	gen s=cp_e1*rs
	gen s_lci = cp_e1 * rs_lci
	gen s_uci = cp_e1 * rs_uci
	replace s=1 if _t==0
	replace s = 0 if s == . // make surv 0 if the prediction shows .
	sort _t
	
	// Integrate to obtain LE
	integ s _t , gen(le40)	

	// Calculate expected hazard from expected survival
	gen cum_exph = -ln(cp_e1)
	gen exph = (cum_exph - cum_exph[_n-1] ) / (`=1/12')
	replace exph = . if rs == 1 

	// Calculate all-cause hazard = expected haz + excess hazard
	gen ach = exph + exh
	gen ach_uci = exph + exh_uci
	gen ach_lci = exph + exh_lci
	
	replace ach = . if rs == 1 
   	replace ach_uci = . if rs == 1 
	replace ach_lci = . if rs == 1 
	
	// Generate a variable for cancer type
	gen cancer = "`cancertype'"
	gen method = "`1'"	
end

// Loop over all cancer types
local cancerlist "colon breast melanoma prostate cml"

foreach cancertype of local cancerlist {
	
/*========Use the defined programs above and feed different mods========*/
// Standard parametric models
local distlist "exp weib gomp loglog lognorm ggamma"

// For censoring at 2 3 5 years (extrap to 10 years); 
//					2 3 5 10 years (extrap to 40 years)
local yrlist 2 3 5 10
// For age group 0 1 2 
local aglist 0 1 2

foreach dist of local distlist{
	foreach yr of local yrlist{
		foreach j of local aglist{
			if `yr' == 10 { // 10 years only for lifetime
				use `cancertype'_ag`j', clear
				stset stime, failure(dead==1) id(lpnr) exit(time `yr')
				mergerate
				merlin (_t , family(`dist', failure(_d) bhaz(rate)))
				local i le40
				make_`i'
				gen agegroup = `j'
				sort _t
				local expsurv "`cancertype'_expsurv"
				merge 1:m agegroup _t  ///
					using `expsurv'.dta, ///
					nolabel keep(match master)  nogen			
				predict_`i' ag`j'_rel_`dist'_exyr`yr'_`i'
				save "`cancertype'_ag`j'_rel_`dist'_exyr`yr'_`i'.dta", replace	
			}
			else if (`yr' == 5 & "`cancertype'" == "melanoma" & `j' == 0 & "`dist'" == "ggamma") {
				di "skip"
			}
			else {				
				use `cancertype'_ag`j', clear
				stset stime, failure(dead==1) id(lpnr) exit(time `yr')
				mergerate
				merlin (_t, family(`dist', failure(_d) bhaz(rate)))
				
				local i rmst10
				make_`i'
				gen agegroup = `j'
				sort _t
				local expsurv "`cancertype'_expsurv"
				merge 1:m agegroup _t  ///
					using `expsurv'.dta, ///
					nolabel keep(match master)  nogen
				predict_`i' ag`j'_rel_`dist'_exyr`yr'_`i'
				save "`cancertype'_ag`j'_rel_`dist'_exyr`yr'_`i'.dta", replace	
				
				local i le40
				make_`i'
				gen agegroup = `j'
				sort _t
				local expsurv "`cancertype'_expsurv"
				merge 1:m agegroup _t  ///
					using `expsurv'.dta, ///
					nolabel keep(match master)  nogen
				predict_`i' ag`j'_rel_`dist'_exyr`yr'_`i'
				save "`cancertype'_ag`j'_rel_`dist'_exyr`yr'_`i'.dta", replace	
				}
			}
		}
	}

	 // melanoma agegroup 0, exyr 5, ggamma has convergence issue. So use 5.15 instead
	local cancertype "melanoma"
	local yr 5.15
	local j 0
	local dist "ggamma"

	use `cancertype'_ag`j', clear
	stset stime, failure(dead==1) id(lpnr) exit(time `yr')
	mergerate
	merlin (_t, family(`dist', failure(_d) bhaz(rate)))
				
	local i rmst10
	make_`i'
	gen agegroup = `j'
    sort _t
	local expsurv "`cancertype'_expsurv"
	merge 1:m agegroup _t  ///
		  using `expsurv'.dta, ///
		  nolabel keep(match master)  nogen
	predict_`i' ag`j'_rel_`dist'_exyr`yr'_`i'
	save "`cancertype'_ag`j'_rel_`dist'_exyr5_`i'.dta", replace	
				
	local i le40
	make_`i'
	gen agegroup = `j'
	sort _t
	local expsurv "`cancertype'_expsurv"
	merge 1:m agegroup _t  ///
		  using `expsurv'.dta, ///
		  nolabel keep(match master)  nogen
	predict_`i' ag`j'_rel_`dist'_exyr`yr'_`i'
	save "`cancertype'_ag`j'_rel_`dist'_exyr5_`i'.dta", replace
}

// Append all the age groups into one file
foreach cancertype of local cancerlist {
	foreach dist of local distlist{	
		foreach yr of local yrlist{
			if `yr' == 10 {
			clear
			local i le40
			use `cancertype'_ag0_rel_`dist'_exyr`yr'_`i'.dta, clear
			append using `cancertype'_ag1_rel_`dist'_exyr`yr'_`i'.dta
			append using `cancertype'_ag2_rel_`dist'_exyr`yr'_`i'.dta
			save "`cancertype'_rel_`dist'_exyr`yr'_`i'.dta", replace
			}
			else if `yr' == 2 | `yr' == 3 | `yr' == 5{
			clear
			local i rmst10
			use `cancertype'_ag0_rel_`dist'_exyr`yr'_`i'.dta, clear
			append using `cancertype'_ag1_rel_`dist'_exyr`yr'_`i'.dta
			append using `cancertype'_ag2_rel_`dist'_exyr`yr'_`i'.dta
			save "`cancertype'_rel_`dist'_exyr`yr'_`i'.dta", replace
			
			clear
			local i le40
			use `cancertype'_ag0_rel_`dist'_exyr`yr'_`i'.dta, clear
			append using `cancertype'_ag1_rel_`dist'_exyr`yr'_`i'.dta
			append using `cancertype'_ag2_rel_`dist'_exyr`yr'_`i'.dta
			save "`cancertype'_rel_`dist'_exyr`yr'_`i'.dta", replace
			}	
		}
	}
}

// Spline models: scale(haz, odds, norm)
local scalelist "haz odds norm"
// For censoring at 2 3 5 years (extrap to 10 years); 
//					2 3 5 10 years (extrap to 40 years)
local yrlist 2 3 5 10
// For age group 0 1 2 
local aglist 0 1 2

foreach cancertype of local cancerlist {
	foreach s of local scalelist{	
		foreach yr of local yrlist{
			foreach j of local aglist{
				if `yr' == 10 { // 10 years only for lifetime
					use `cancertype'_ag`j', clear
					stset stime, failure(dead==1) id(lpnr) exit(time `yr') 
					mergerate				
					stpm2 , scale(`s') df(5) bhaz(rate)				
					local i le40
					make_`i'
					gen agegroup = `j'
					sort _t
					local expsurv "`cancertype'_expsurv"
					merge 1:m agegroup _t  ///
						using `expsurv'.dta, ///
						nolabel keep(match master)  nogen
					predict_`i' ag`j'_rel_spl`s'_exyr`yr'_`i'
					save "`cancertype'_ag`j'_rel_spl`s'_exyr`yr'_`i'.dta", replace	
				}
				else if `yr' == 2 | `yr' == 3 | `yr' == 5{
					use `cancertype'_ag`j', clear
					stset stime, failure(dead==1) id(lpnr) exit(time `yr') 
					mergerate				
					stpm2 , scale(`s') df(5) bhaz(rate)				
					local i rmst10
					make_`i'
					gen agegroup = `j'
					sort _t
					local expsurv "`cancertype'_expsurv"
					merge 1:m agegroup _t  ///
						using `expsurv'.dta, ///
						nolabel keep(match master)  nogen
					predict_`i' ag`j'_rel_spl`s'_exyr`yr'_`i'
					save "`cancertype'_ag`j'_rel_spl`s'_exyr`yr'_`i'.dta", replace	
					
					local i le40
					make_`i'
					gen agegroup = `j'
					sort _t
					local expsurv "`cancertype'_expsurv"
					merge 1:m agegroup _t  ///
						using `expsurv'.dta, ///
						nolabel keep(match master)  nogen
					predict_`i' ag`j'_rel_spl`s'_exyr`yr'_`i'
					save "`cancertype'_ag`j'_rel_spl`s'_exyr`yr'_`i'.dta", replace	
			}
		}
	}
}

// Append all the age groups into one file
foreach s of local scalelist{	
	foreach yr of local yrlist{
			if `yr' == 10 {
			clear
			local i le40
			use `cancertype'_ag0_rel_spl`s'_exyr`yr'_`i'.dta, clear
			append using `cancertype'_ag1_rel_spl`s'_exyr`yr'_`i'.dta
			append using `cancertype'_ag2_rel_spl`s'_exyr`yr'_`i'.dta
			save "`cancertype'_rel_spl`s'_exyr`yr'_`i'.dta", replace
			}
			else if `yr' == 2 | `yr' == 3 | `yr' == 5{
			clear
			local i rmst10
			use `cancertype'_ag0_rel_spl`s'_exyr`yr'_`i'.dta, clear
			append using `cancertype'_ag1_rel_spl`s'_exyr`yr'_`i'.dta
			append using `cancertype'_ag2_rel_spl`s'_exyr`yr'_`i'.dta
			save "`cancertype'_rel_spl`s'_exyr`yr'_`i'.dta", replace
			
			clear
			local i le40
			use `cancertype'_ag0_rel_spl`s'_exyr`yr'_`i'.dta, clear
			append using `cancertype'_ag1_rel_spl`s'_exyr`yr'_`i'.dta
			append using `cancertype'_ag2_rel_spl`s'_exyr`yr'_`i'.dta
			save "`cancertype'_rel_spl`s'_exyr`yr'_`i'.dta", replace
			}
		}
	}
}

/***************************** FILE ENDS HERE**************************/
log close
exit
