/*==============================================================================
FILENAME: master.do

PROJECT: Enoch PhD Study 1 (Comparing survival extrapolation)
	
AUTHOR: Enoch Yi-Tung Chen (enoch.yitung.chen@ki.se)

CREATED: Enoch Yi-Tung Chen	
UPDATED: 

STATA VERSION: 17.0
==============================================================================*/

// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global path `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global path `"P:"'

// Set subpath
global subpath `"SweCan/Site_specific/Enoch_Study1"'

// Set the working directory
cd "${path}/${subpath}/Program"

/*========Code begins here========*/
// Study populations for each cancer site
do colon_studypop
do breast_studypop
do melanoma_studypop
do prostate_studypop
do cml_studypop

// Descriptive analysis
do colon_desc_analysis
do breast_desc_analysis
do melanoma_desc_analysis
do prostate_desc_analysis
do colon_desc_analysis

// The following syntaxes loop over cancer types
do km 
do allcause
do expsurv
do rel
do diff
do results
do plots_main     
do plot_main_surv 
do tab2           

/*Supplementary materials*/
cd "${path}/${subpath}/Program"
do plot_supp_rmst10  
cd "${path}/${subpath}/Program"
do plot_supp_surv10  
cd "${path}/${subpath}/Program"
do plot_supp_le40    
cd "${path}/${subpath}/Program"
do plot_supp_surv40  
cd "${path}/${subpath}/Program"
do tab_supp

plot_supp_surv_haz 
plots_presentation

exit
