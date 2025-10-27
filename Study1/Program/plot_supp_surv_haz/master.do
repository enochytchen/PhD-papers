// Updated: 20230819 Enoch Chen: Updated analysis for revised submission

// Set the OSX/Windows system
if `"`c(os)'"' == "MacOSX"   global path `"/Volumes/projects"'
if `"`c(os)'"' == "Windows"  global path `"P:"'

// Set subpath
global subpath `"SweCan/Site_specific/Enoch_Study1"'

// RMST10--all-cause survival framework
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_breast_agegroup0_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_breast_agegroup1_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_breast_agegroup2_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_colon_agegroup0_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_colon_agegroup1_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_colon_agegroup2_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_melanoma_agegroup0_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_melanoma_agegroup1_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_melanoma_agegroup2_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_prostate_agegroup0_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_prostate_agegroup1_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_prostate_agegroup2_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_cml_agegroup0_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_cml_agegroup1_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_cml_agegroup2_ac.do

// RMST10--relative survival framework
// RUN HERE
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_breast_agegroup0_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_breast_agegroup1_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_breast_agegroup2_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_colon_agegroup0_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_colon_agegroup1_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_colon_agegroup2_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_melanoma_agegroup0_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_melanoma_agegroup1_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_melanoma_agegroup2_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_prostate_agegroup0_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_prostate_agegroup1_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_prostate_agegroup2_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_cml_agegroup0_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_cml_agegroup1_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do rmst10_cml_agegroup2_rel.do

// LE40--all-cause survival framework
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_breast_agegroup0_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_breast_agegroup1_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_breast_agegroup2_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_colon_agegroup0_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_colon_agegroup1_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_colon_agegroup2_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_melanoma_agegroup0_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_melanoma_agegroup1_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_melanoma_agegroup2_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_prostate_agegroup0_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_prostate_agegroup1_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_prostate_agegroup2_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_cml_agegroup0_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_cml_agegroup1_ac.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_cml_agegroup2_ac.do

// LE40--relative survival framework
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_breast_agegroup0_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_breast_agegroup1_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_breast_agegroup2_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_colon_agegroup0_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_colon_agegroup1_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_colon_agegroup2_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_melanoma_agegroup0_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_melanoma_agegroup1_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_melanoma_agegroup2_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_prostate_agegroup0_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_prostate_agegroup1_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_prostate_agegroup2_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_cml_agegroup0_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_cml_agegroup1_rel.do
cd "${path}/${subpath}/Program/plot_supp_surv_haz/"
do le40_cml_agegroup2_rel.do
exit
