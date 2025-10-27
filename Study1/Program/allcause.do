/*==============================================================================
FILENAME: allcause.do

PROJECT: Enoch PhD Study 1 (Comparing survival extrapolation)
	
AUTHOR: Enoch Yi-Tung Chen (enoch.yitung.chen@ki.se)

CREATED: Enoch Yi-Tung Chen	
UPDATED: 20230817 Enoch Chen: updated follow-up time used for extrapolation

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
global dofilename = "allcause" 

capture log close
log using "${path}/${subpath}/Log/${dofilename}_`todaydate'.log", replace

/*========Code begins here========*/

clear all

/*========Define programs========*/
// Program: Predict RMST at year 10
// Break the program to two parts in order to generate a var indicating agegroup
cap program drop make_rmst10
program define make_rmst10
	clear
	range _t 0 10 121  // time in months: (12*10)+1=121

	// predict relative survival with 95% CIs
	gen _d = .
	predict s, survival ci timevar(_t) 
	replace s = 0 if s == . // make surv 0 if the prediction shows .
	predict ach, hazard ci timevar(_t)
end

cap program drop predict_rmst10
program define predict_rmst10
	integ s _t , gen(rmst10)	
	list  rmst10 if _t == 10
	gen method = "`1'"
end

// Program: Predict LE at year 40
cap program drop make_le40
program define make_le40
	clear
	// Prepare an empty dataset
	range _t 0 40 481  // time in months: (12*40)+1=48

	// predict surv and haz with 95% CIs
	gen _d = .
	predict s, survival ci timevar(_t)
	replace s = 0 if s == . // make surv 0 if the prediction shows .
	predict ach, hazard ci timevar(_t)
end

cap program drop predict_le40
program define predict_le40
	integ s _t , gen(le40)	
	list le if _t == 40
	
	// Generate a variable for cancer type
	gen method = "`1'"
end

// Standard parametric models

local cancerlist "colon breast melanoma prostate cml"
local distlist "exp weib gomp loglog lognorm ggamma"
local yrlist 2 3 5 10
local aglist 0 1 2

foreach cancertype of local cancerlist {
	foreach dist of local distlist {
		foreach yr of local yrlist {
			foreach j of local aglist {
					if `yr' == 10 { // 10 years only for lifetime
						use `cancertype'_ag`j', clear
						stset stime, failure(dead==1) id(lpnr) exit(time `yr') 		
						merlin (_t , family(`dist', failure(_d)))
						local i le40
						make_`i'
						gen agegroup = `j'
						predict_`i' ag`j'_ac_`dist'_exyr`yr'_`i'
						gen cancer = "`cancertype'"
						save "`cancertype'_ag`j'_ac_`dist'_exyr`yr'_`i'.dta", replace
					}
					else if `yr' == 2 & "`cancertype'" == "melanoma" & `j' == 2 & "`dist'" == "ggamma"		 { 	  di "skip"	
					}
					else {
						use `cancertype'_ag`j', clear
						stset stime, failure(dead==1) id(lpnr) exit(time `yr') 				  
						merlin (_t, family(`dist', failure(_d)))
						
						local i rmst10
						make_`i'
						gen agegroup = `j'
						predict_`i' ag`j'_ac_`dist'_exyr`yr'_`i'
						gen cancer = "`cancertype'"					
						save "`cancertype'_ag`j'_ac_`dist'_exyr`yr'_`i'.dta", replace
						
						local i le40
						make_`i'
						gen agegroup = `j'
						predict_`i' ag`j'_ac_`dist'_exyr`yr'_`i'
						gen cancer = "`cancertype'"
						save "`cancertype'_ag`j'_ac_`dist'_exyr`yr'_`i'.dta", replace
					}	
				}
			}
		}
}

// melanoma agegroup 2, exyr 2, ggamma has convergence issue. So use 1.8 instead
	local cancertype "melanoma"
	local yr 1.8
	local j 2
	local dist "ggamma"

	use `cancertype'_ag`j', clear
	stset stime, failure(dead==1) id(lpnr) exit(time `yr') 		
	merlin (_t , family(`dist', failure(_d)))
	local i rmst10
	make_`i'
	gen agegroup = `j'
	predict_`i' ag`j'_ac_`dist'_exyr2_`i'
	gen cancer = "`cancertype'"	
	save "`cancertype'_ag`j'_ac_`dist'_exyr2_`i'.dta", replace
	
	local i le40
	make_`i'
	gen agegroup = `j'
	predict_`i' ag`j'_ac_`dist'_exyr2_`i'
	gen cancer = "`cancertype'"	
	save "`cancertype'_ag`j'_ac_`dist'_exyr2_`i'.dta", replace
	
/*-----------------------------------------------------------------------------*/
// Append all the age groups into one file

local cancerlist "colon breast melanoma prostate cml"
local distlist "exp weib gomp loglog lognorm ggamma"
local yrlist 2 3 5 10

foreach cancertype of local cancerlist{
	foreach dist of local distlist{	
		foreach yr of local yrlist{
			if `yr' == 10 {
			clear
			local i le40
			use `cancertype'_ag0_ac_`dist'_exyr`yr'_`i'.dta, clear
			append using `cancertype'_ag1_ac_`dist'_exyr`yr'_`i'.dta
			append using `cancertype'_ag2_ac_`dist'_exyr`yr'_`i'.dta
			save "`cancertype'_ac_`dist'_exyr`yr'_`i'.dta", replace
			}
			else if `yr' == 2 | `yr' == 3 | `yr' == 5{
			clear
			local i rmst10
			use `cancertype'_ag0_ac_`dist'_exyr`yr'_`i'.dta, clear
			append using `cancertype'_ag1_ac_`dist'_exyr`yr'_`i'.dta
			append using `cancertype'_ag2_ac_`dist'_exyr`yr'_`i'.dta
			save "`cancertype'_ac_`dist'_exyr`yr'_`i'.dta", replace
			
			clear
			local i le40
			use `cancertype'_ag0_ac_`dist'_exyr`yr'_`i'.dta, clear
			append using `cancertype'_ag1_ac_`dist'_exyr`yr'_`i'.dta
			append using `cancertype'_ag2_ac_`dist'_exyr`yr'_`i'.dta
			save "`cancertype'_ac_`dist'_exyr`yr'_`i'.dta", replace
			}
		}
	}
}

// Spline models: scale(haz, odds, norm)
local cancerlist "colon breast melanoma prostate cml"
local scalelist "haz odds norm"
local yrlist 2 3 5 10
local aglist 0 1 2

foreach cancertype of local cancerlist {
	foreach s of local scalelist{	
		foreach yr of local yrlist{
			foreach j of local aglist{
				if `yr' == 10 { // 10 years only for lifetime
					use `cancertype'_ag`j', clear
					stset stime, failure(dead==1) id(lpnr) exit(time `yr') 		
					stpm2 , scale(`s') df(5)				
					local i le40
					make_`i'
					gen agegroup = `j'
					predict_`i' ag`j'_ac_spl`s'_exyr`yr'_`i'
					gen cancer = "`cancertype'"	
					save "`cancertype'_ag`j'_ac_spl`s'_exyr`yr'_`i'.dta", replace
				}
				else if `yr' == 2 | `yr' == 3 | `yr' == 5{
					use `cancertype'_ag`j', clear
					stset stime, failure(dead==1) id(lpnr) exit(time `yr') 
					stpm2 , scale(`s') df(5)	
					
					local i rmst10
					make_`i'
					gen agegroup = `j'
					predict_`i' ag`j'_ac_spl`s'_exyr`yr'_`i'
					gen cancer = "`cancertype'"	
					save "`cancertype'_ag`j'_ac_spl`s'_exyr`yr'_`i'.dta", replace
					
					local i le40
					make_`i'
					gen agegroup = `j'
					predict_`i' ag`j'_ac_spl`s'_exyr`yr'_`i'
					gen cancer = "`cancertype'"	
					save "`cancertype'_ag`j'_ac_spl`s'_exyr`yr'_`i'.dta", replace
			}
		}
	 }
  }
}

local cancerlist "colon breast melanoma prostate cml"
local scalelist "haz odds norm"
local yrlist 2 3 5 10

foreach cancertype of local cancerlist {
	foreach s of local scalelist{	
		foreach yr of local yrlist{
				if `yr' == 10 {
				clear
				local i le40
				use `cancertype'_ag0_ac_spl`s'_exyr`yr'_`i'.dta, clear
				append using `cancertype'_ag1_ac_spl`s'_exyr`yr'_`i'.dta
				append using `cancertype'_ag2_ac_spl`s'_exyr`yr'_`i'.dta
				save "`cancertype'_ac_spl`s'_exyr`yr'_`i'.dta", replace
				}
				else if `yr' == 2 | `yr' == 3 | `yr' == 5{
				clear
				local i rmst10
				use `cancertype'_ag0_ac_spl`s'_exyr`yr'_`i'.dta, clear
				append using `cancertype'_ag1_ac_spl`s'_exyr`yr'_`i'.dta
				append using `cancertype'_ag2_ac_spl`s'_exyr`yr'_`i'.dta
				save "`cancertype'_ac_spl`s'_exyr`yr'_`i'.dta", replace
				
				clear
				local i le40
				use `cancertype'_ag0_ac_spl`s'_exyr`yr'_`i'.dta, clear
				append using `cancertype'_ag1_ac_spl`s'_exyr`yr'_`i'.dta
				append using `cancertype'_ag2_ac_spl`s'_exyr`yr'_`i'.dta
				save "`cancertype'_ac_spl`s'_exyr`yr'_`i'.dta", replace
			}
		}
	}
}

/***************************** FILE ENDS HERE**************************/
log close
exit
