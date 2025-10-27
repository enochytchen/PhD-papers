# Study population
- studypop.do: make study population
- tab1.do: make descriptive analysis for table 1
- studypop_msset.do: claim the individual-level data into multi-state modelling structure
- popmort2022.do: make Swedish popmort file


# CML microsimulation models 
- survmod.R: survival models for multi-states
- expsurv.R: estimate LE and QALE of the general population for studypop
- expsurv_curve.R: Plot expected survival curves
- microsim_cml.R: microsimulation model for CML in a relative survival framework using approach 3 to estimate expected hazard
- stack.R: stacked probabilities/QALYs plot
- results_lle_qale.R


# Compare with Bower2016
# bower2016: to compare bower 2016’s LE results
	- analysisdata_cml.do
	- update_bower2016_mod1
	- update_bower2016_mod2
	- update_bower2016_mod3
	- bower2016_mod.R
	- compare_bower2016
	- compare_results

# Microsimulation essentials in R
- DES.R: discrete event simulation algorithm in R written by Mark Clements


# Old
- survmod_reduced.R: CML_mod reduce to only two states
- expected_haz_approach.R: three different approaches to estimate expected hazard within multi-state setting- micro_rel_example_r: an illness-death model using microsimulation in R
- micro_rel_example_cpp: reproduce micro_rel_example_r in C++
- microsim_ac_reduced.R: 2-state microsimulation model in all-cause survival framework
- microsim_rel_reduced.R: 2-state microsimulation model in relative survival framework using approach 3
- microsim_ac.R: CML model in all-cause survival framework

