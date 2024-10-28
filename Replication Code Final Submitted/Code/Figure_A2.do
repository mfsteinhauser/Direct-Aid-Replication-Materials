/***************************************************************************

	DESCRIPTION: 	This do file conducts heterogeneity analysis by 
					running regressions with interaction terms for various 
					subgroup characteristics and storing the results, and 
					generates plots displaying the baseline and 
					heterogeneity estimates with 95% confidence intervals.
	
	INPUTS:			- surveysDataset.dta
	
	OUTPUTS:		- figure_a2_nutrition_index
					- figure_a2_econ_index
					- figure_a2_tax_index

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
* 1. PREPARING DATA
********************************************************************************
use "${data}/surveysDataset.dta", clear

drop if round == 3 | round == 4
* Creating variables needed for analysis

gen aboveMedTBT = aboveMedianTotalBreadTea

gen round_temp = round - 1

* Creating household head variable 
gen is_hh_head_aux = hh_head == 1 if hh_head != . & round == 2
egen is_hh_head = max(is_hh_head_aux), by(uniqueId)
egen married_base = max(married), by(uniqueId)

* Generating strata variable 
egen strata = group(nahia_city_comb aboveMedianTotalBreadTea)

********************************************************************************
* 2. HETEROGENEITY ANALYSIS
********************************************************************************

mat hetAnalysis = J(33, 4, .)

local row = 1
local dep_var = 1
foreach var in nutrition_index tax_index econ_index{
	
	local heter_var = 0
	
	* Baseline estimate 
	reg `var' treatment i.nahia_city_comb aboveMedianTotalBreadTea round_temp if t != 0, vce(cluster uniqueId)
	
	mat hetAnalysis[`row', 1] = round(_b[treatment], 0.0001)
	mat hetAnalysis[`row', 2] = round(_se[treatment], 0.0001)
	mat hetAnalysis[`row', 3] = `dep_var'
	mat hetAnalysis[`row', 4] = `heter_var'
	local ++heter_var
	local ++row
	
	foreach het_var in aboveMedTBT totalBreadTea_base kabul ableLeaveHouse_base married_base pashtun someEduc aboveMedianAge finDecisionMaker_base aboveMedianHHSize{
	
		tempfile res_`het_var'
		local n = 1
		
		cap gen int_`het_var'_treat = `het_var' * treatment
				
		* Loop for variables with baseline information
		if inlist("`var'", "income", "employed", "totalBreadTea", "lifeSatisfaction", "happy", "medicinePurchase", "finDecisionMaker", "skippedMeals") == 1{
		
			reg `var' int_`het_var'_treat treatment `het_var' round_temp i.strata `var'_base if t != 0, vce(cluster uniqueId)
			
		}
		else{ // Variables without baseline value
			
			reg `var' int_`het_var'_treat treatment `het_var' round_temp i.strata if t != 0, vce(cluster uniqueId)
			
		}
		
		lincom int_`het_var'_treat + treatment + `het_var'
		mat hetAnalysis[`row', 1] = round(r(estimate), 0.00001)
		mat hetAnalysis[`row', 2] = round(r(se), 0.00001)
		mat hetAnalysis[`row', 3] = `dep_var'
		mat hetAnalysis[`row', 4] = `heter_var'
		
		local ++row
		local ++heter_var
		
	}

	local ++dep_var

}

*** B. Plot

clear
svmat hetAnalysis

gen lb = hetAnalysis1 - (1.96 * hetAnalysis2)
gen ub = hetAnalysis1 + (1.96 * hetAnalysis2)

local n = 1
foreach var in nutrition_index tax_index econ_index{
	
	preserve 
	
	keep if hetAnalysis3 == `n'
	
	* Getting baseline estimate
	qui sum hetAnalysis1 if hetAnalysis4 == 0 & hetAnalysis3 == `n'
	local base_est = r(mean)
	
	gen n = hetAnalysis4 * (-1)
	
	twoway (rcap lb ub n, horizontal lcolor(black)) ///
		(scatter n hetAnalysis1 if hetAnalysis4 == 0, color(ebblue)) ///
		(scatter n hetAnalysis1 if hetAnalysis4 != 0, color(cranberry)) ///
		, graphregion(color(white)) legend(label(2 "Baseline") label(3 "Heterogeneity") order(2 3) ring(1) position(6) row(1) region(lstyle(none))) ///
		ylabel(0 "Baseline" -1 "Above Med. Bread & Tea" -2 "Total Bread & Tea" -3 "Kabul" -4 "Able To Leave House" -5 "Married" -6 "Pashtun" -7 "Some Education" -8 "Above Med. Age" -9 "Fin. Decision Maker" -10 "Above Med. HH Size", angle(0)) xline(0, lcolor(red%50) lpattern(dash)) ytitle("") xline(`base_est', lcolor(gs8%50) lpattern(dash)) ///
		xlabel(, nogrid)
	graph export "${output}/figure_a2_`var'.pdf", replace
	
	restore 
	
	local ++n
	
}
