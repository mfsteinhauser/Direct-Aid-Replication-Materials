/***************************************************************************

	DESCRIPTION: 	This do file analyzes participants' preferences for 
					cash out options versus HesabPay balance based on a 
					hypothetical scenario presented during the fourth 
					survey round. 
	
	INPUTS:			- surveysDataset.dta
	
	OUTPUTS:		- figure_a3.pdf

***************************************************************************/
 
********************************************************************************
* 0. PREAMBLE
********************************************************************************

clear
clear matrix
clear mata
set more off
estimates clear

********************************************************************************
* 1. CASH OUT OPTION
********************************************************************************
use "${data}/surveysDataset.dta", clear

mat cashOut = J(9, 3, .)

*** All Sample
sum prefer_100_fee
mat cashOut[1, 1] = round(r(mean), 0.001)
mat cashOut[1, 2] = round(r(sd), 0.001)
mat cashOut[1, 3] = 1
sum prefer_300_fee 
mat cashOut[2, 1] = round(r(mean), 0.001)
mat cashOut[2, 2] = round(r(sd), 0.001)
mat cashOut[2, 3] = 1
sum prefer_500_fee 
mat cashOut[3, 1] = round(r(mean), 0.001)
mat cashOut[3, 2] = round(r(sd), 0.001)
mat cashOut[3, 3] = 1

*** Treatment Group 
sum prefer_100_fee if treatment == 1
mat cashOut[4, 1] = round(r(mean), 0.001)
mat cashOut[4, 2] = round(r(sd), 0.001)
mat cashOut[4, 3] = 2
sum prefer_300_fee if treatment == 1
mat cashOut[5, 1] = round(r(mean), 0.001)
mat cashOut[5, 2] = round(r(sd), 0.001)
mat cashOut[5, 3] = 2
sum prefer_500_fee if treatment == 1 
mat cashOut[6, 1] = round(r(mean), 0.001)
mat cashOut[6, 2] = round(r(sd), 0.001)
mat cashOut[6, 3] = 2

*** Control Group 
sum prefer_100_fee if treatment == 0
mat cashOut[7, 1] = round(r(mean), 0.001)
mat cashOut[7, 2] = round(r(sd), 0.001)
mat cashOut[7, 3] = 3
sum prefer_300_fee if treatment == 0
mat cashOut[8, 1] = round(r(mean), 0.001)
mat cashOut[8, 2] = round(r(sd), 0.001)
mat cashOut[8, 3] = 3
sum prefer_500_fee if treatment == 0
mat cashOut[9, 1] = round(r(mean), 0.001)
mat cashOut[9, 2] = round(r(sd), 0.001)
mat cashOut[9, 3] = 3

**** CREATING FIGURE

clear 
svmat cashOut

replace cashOut1 = cashOut1 * 100

gen n = _n
replace n = n + 1 if n > 3
replace n = n + 1 if n > 7
gen cost = 1
replace cost = 2 if n == 2 | n == 6 | n == 10
replace cost = 3 if n == 3 | n == 7 | n == 11

gen cashOut1_temp = round(cashOut1, 0.01)

twoway (bar cashOut1 n if cost == 1, fcolor(black) lcolor(black)) ///
       (bar cashOut1 n if cost == 2, fcolor(gs5) lcolor(gs5)) ///
       (bar cashOut1 n if cost == 3, fcolor(gs10) lcolor(gs10)) ///
       (scatter cashOut1_temp n, mlab(cashOut1_temp) msym(none) mlabpos(12) mlabcolor(black) mlabsize(large)) ///
       , graphregion(color(white)) ytitle("Percent that Prefer 4000 AFA in HP Credit") ylabel(0(20)100) ///
       legend(label(1 "100 AFA Cut") label(2 "300 AFA Cut") label(3 "500 AFA Cut") rows(1) order(1 2 3) ///
       position(6)) ///
       xlabel(2 "All Sample" 6 "Early Group" 10 "Late Group") xtitle("")

graph export "${output}/figure_a3.pdf", replace

	
