/***************************************************************************

	DESCRIPTION: 	This do file produces timeseries scatterplots that 
					plot mean values for key variables (e.g., skipped 
					meals, lifesatisfaction) for both treatment and 
					control groups, segmented into two-week bins. It 
					includes data from all survey rounds, encompassing the 
					period after the control group began receiving payments.
	
	INPUTS:			- surveysDataset.dta
	
	OUTPUTS:		- figure_2_timeSeriesBins_skippedMeals 
					- figure_2_timeSeriesBins_totalBreadTea 
					- figure_2_timeSeriesBins_happy 
					- figure_2_timeSeriesBins_lifeSatisfaction
					
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
* 1. TIMESERIES SCATTERPLOTS
********************************************************************************
use "${data}/surveysDataset.dta", clear

* Days since they received the FIRST payment (both the assignment and the actual days)
local payment1_early = td(06Nov2022)
gen daysSince_firstPayment_actual = date_actualSurvey - `payment1_early'
label var daysSince_firstPayment_actual "Days between 1st payment and follow-up survey, in practice"

* Dividing days since first payment into weekly bins
gen survey_bin = 0
local bin_lb = 0
forvalues bin = 1(1)24{
	
	local bin_up = `bin_lb' + 6
	
	replace survey_bin = `bin' if inrange(daysSince_firstPayment_actual, `bin_lb', `bin_up') == 1
	
	local bin_lb = `bin_up' + 1
	
}

gen n = 1
collapse (mean) avr_skippedMeals = skippedMeals avr_totalBreadTea = totalBreadTea avr_happy = happy avr_lifeSatisfaction = lifeSatisfaction (sd) sd_skippedMeals = skippedMeals sd_totalBreadTea = totalBreadTea sd_happy = happy sd_lifeSatisfaction = lifeSatisfaction (sum) n, by(survey_bin treatment)

drop if treatment == .

* Replace SD for SEs

foreach var in skippedMeals totalBreadTea happy lifeSatisfaction{
	replace sd_`var' = sd_`var' / sqrt(n)
	gen `var'_ub = avr_`var' + (1.96 * sd_`var')
	gen `var'_lb = avr_`var' - (1.96 * sd_`var')
}

replace survey_bin = survey_bin + 0.2 if treatment == 1

* Generating plots

foreach var in skippedMeals totalBreadTea happy lifeSatisfaction{
	
	preserve 
	
	* Baseline levels
	sum avr_`var' if survey_bin == 0
	local mean_base = r(mean)
	
	local baseline = "yline(`mean_base')"
		
	drop if survey_bin < 1
	
	if inlist("`var'", "happy") == 1{
		local scale = "ylabel(, format(%03.1f) labsize(large))"
		local ytitle = "Happy (=1)"
	}
	else if inlist("`var'", "lifeSatisfaction") == 1{
		local ytitle = "Life Satisfaction (0 to 10)"
	}
	else if inlist("`var'", "skippedMeals") == 1{
		local scale = "ylabel(, format(%3.0f) labsize(large))"
		local ytitle = "Days Skipping Meals in Past Week"
	}
	else if inlist("`var'", "totalBreadTea") == 1{
		local ytitle = "Total Bread & Tea Meals"
	}
	
	twoway (rcap `var'_ub `var'_lb survey_bin if treatment == 1, lc(gs5)) ///
		(rcap `var'_ub `var'_lb survey_bin if treatment == 0, lc(gs5)) ///
		(scatter avr_`var' survey_bin if treatment == 1, color(ebblue) msymbol(diamond)) ///
		(scatter avr_`var' survey_bin if treatment == 0, color(cranberry) msymbol(square)) ///
		, graphregion(color(white)) legend(label(3 "Treatment") label(4 "Control") order(3 4)) ///
		`scale' ylabel(, angle(0)) xline(0.8, lcolor(ebblue%50) lpattern(dash)) xline(2.8, lcolor(ebblue%50) lpattern(dash)) xline(4.8, lcolor(ebblue%50) lpattern(dash)) xline(6.8, lcolor(ebblue%50) lpattern(dash)) xline(8.8, lcolor(red%50) lpattern(dash)) xline(10.8, lcolor(red%50) lpattern(dash)) xline(12.8, lcolor(red%50) lpattern(dash)) xline(14.8, lcolor(red%50) lpattern(dash)) ytitle("`ytitle'") ///
		xlabel(-0.5 " " 0.8 "November 6" 4.8 "December 4" 8.8 "January 1" 12.8 "January 29",  labsize(medsmall) tlength(*0.5) notick) xtitle("") `baseline'
		
	graph export "${output}/figure_2_`var'.pdf", replace
	
	restore 
	
}
