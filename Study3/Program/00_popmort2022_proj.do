// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global path `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global path `"P:"'

/****************************************************************************
Purpose: Make population mortality projection file
Note: 
1. Import the raw data dowloaded from 
https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__BE__BE0401__BE0401D/BefProgDodstalNb/
2. Append using empirical popmort~2022
 
Created data: 2024-01-29 by Enoch Chen
Updated: 2024-07-07 by Enoch Chen: rename file name
****************************************************************************/

clear all

// Downlaod the data from Statistics Sweden (SCB)
import delimited "${path}/SweCan/Popmort/SCB_projection/popmort_projection_2023_2120.csv", varnames(2) clear 

drop regionofbirth

// Relabel sex
gen nsex=1 if sex=="men"
replace nsex=2 if sex=="women"
drop sex
rename nsex sex 
label define sex 1 "male" 2 "female" /*Define Sex*/
lab values sex sex

// Get rid of "years" in the variable age
program define extrnum
version 7
syntax varlist(max=1) , gen(str)
local maxlen: type `varlist'
local maxlen=substr("`maxlen'",4,.)
tempvar work
qui gen str1 `work'=""
forvalues i=1/`maxlen' {
 qui replace `work'=`work'+substr(`varlist',`i',1) if real(substr(`varlist',`i',1))<.
}
gen `gen'=real(`work')
end

extrnum age, gen(nage)
drop age
rename nage _age

// Put column v4(2023)-v101(2120) into one column
gen hhid = _n 
reshape long v, i(hhid) j(pid)

// Rename the variable names
gen _year=pid+2019
rename v rate
gen prob=exp(-rate)
drop hhid pid

order _year _age sex rate prob
sort _year sex _age 
compress

// Append popmort2022 (life tables 1950-2022)
// Only keep age<=100
append using ${path}/SweCan/Popmort/popmort2022, gen(source)
drop  if source == 1 & _year > 2022
drop source

drop if _age>100
order _year _age sex  
sort _year sex _age  

// Generate mortality rates after age 101 as 100
forvalues i = 101/130{
	expand 2 if _age == 100, gen(dup)
	replace _age = `i' if dup == 1 & _age == 100
	drop dup 
}

sort _year sex _age  

compress

save 00_popmort2022_proj, replace

exit
