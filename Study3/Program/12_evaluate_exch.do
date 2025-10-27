// Created: 2023-12-15 Enoch Chen: investigate the underlying reason of high upper bound of the CIs of the excess hazard of "Transition 1L-TKI -> Death" and "Transition TFR -> Death"

## Transition 5
### strs
~~~~
<<dd_do>>
/*==============================
 Transition 5: 1L-TKI -> Death
==============================*/
// Read data
cd ../Data
use 02_studypop_msset.dta if trans == 5, clear

// To avoid variable names same as strs
rename start start1
rename stop stop1

// stset
stset stop1, origin(start1) failure(status == 1) scale(365.24) id(idnr)
tab _d
summarize _t

// strs to estimate 10-year relative survival and related estimates
strs using popmort2018.dta, break(0(`=1/12')10) mergeby (_year sex _age) ///
	 diagyear(dxyear) diagage(dxage) notab ///
	 save(replace)
<</dd_do>>
~~~~~

~~~~~
<<dd_do>>
// Read the results from strs
use grouped, clear
list start end n d d_star p p_star r cp cr_e2 if end <=2
<</dd_do>>
~~~~~
Some **d (deaths during the interval)** was very close to lower than **d_star(expected number of deahths)**.

### stpm2
~~~~~
<<dd_do>>
// And now we prepare to fit an stpm2 model for the same data	 
use 02_studypop_msset.dta if trans == 5, clear
stset stop, origin(start) failure(status == 1) scale(365.24) id(idnr)
<</dd_do>>
~~~~~

~~~~~
<<dd_do>>	
// Fit an stpm2 excess hazard model
stpm2 ,df(3) bhazard(rate) scale(haz) // knot centile positions (33 67)
<</dd_do>>
~~~~~

~~~~~
<<dd_do>>	
// Predict and plot
clear 
range _t 0 10 121 // First 10 years
gen _d = .

predict exh, haz ci timevar(_t)

tw (line exh _t, lcol(black)) ///
   (rarea exh_uci exh_lci _t if exh_uci < 1, color(blue%10)), ///
   ylab(0 1) 
<</dd_do>>
~~~~~

<<dd_graph: saving(graph1.png) replace height(400)>>

The model converged, but the upper bound of the confidence intervals was substantially
high, especially in the beginning. 

### stpm2--change knot position
~~~~~
<<dd_do>>
// And now we prepare to fit an stpm2 model for the same data	 
use 02_studypop_msset.dta if trans == 5, clear
stset stop, origin(start) failure(status == 1) scale(365.24) id(idnr)
<</dd_do>>
~~~~~

~~~~~
<<dd_do>>	
// Fit an stpm2 excess hazard model
stpm2 , bhazard(rate) scale(haz) knots(5 95) knscale(centile)
<</dd_do>>
~~~~~

~~~~~
<<dd_do>>	
// Predict and plot
clear 
range _t 0 10 121 // First 10 years
gen _d = .

predict exh, haz ci timevar(_t)

tw (line exh _t, lcol(black)) ///
   (rarea exh_uci exh_lci _t if exh_uci < 1, color(blue%10)), ///
   ylab(0 1) 
<</dd_do>>
~~~~~

<<dd_graph: saving(graph1.png) replace height(400)>>

Changing the knot position would induce the hazard shape to change, but it does not solve the issue that CIs may be extremely wide.


## Transition 14
### strs
~~~~
<<dd_do>>
/*==============================
 Transition 14: TFR -> Death
==============================*/
// Read data
cd ../Data

use 02_studypop_msset.dta if trans == 14, clear

rename start start1
rename stop stop1

// stset
stset stop1, origin(start1) failure(status == 1) scale(365.24) id(idnr)
tab _d
summarize _t

strs using popmort2018.dta, break(0(`=1/12')10) mergeby (_year sex _age) ///
	 diagyear(dxyear) diagage(dxage) notab ///
	 save(replace)
<</dd_do>>
~~~~~

~~~~
<<dd_do>>	 
use grouped, clear
list start end n d d_star p p_star r cp cr_e2 if end <=2
<</dd_do>>
~~~~~
Some **d (deaths during the interval)** was very close to zero and lower than **d_star(expected number of deahths)**.

### stpm2
~~~~
<<dd_do>>
// And now we prepare to fit an stpm2 model for the same data	 
cd ../Data 
use 02_studypop_msset.dta if trans == 14, clear
stset stop, origin(start) failure(status == 1) scale(365.24) id(idnr)

stpm2 ,df(3) bhazard(rate) scale(haz)
<</dd_do>>
~~~~~

~~~~
<<dd_do>>
clear 
range _t 0 10 121 // First 10 years
gen _d = .

predict exh, haz ci timevar(_t)
<</dd_do>>
~~~~~

~~~~~
<<dd_do>>	
tw (line exh _t, lcol(black)) ///
   (rarea exh_uci exh_lci _t if exh_uci < 1, color(blue%10)), ///
   ylab(0 1)
<</dd_do>>
~~~~~
<<dd_graph: saving(graph2.png) replace height(400)>>
~~~~~

// Export as a markdown
cd ../Program

dyndoc 12_evaluate_exch.do, replace
