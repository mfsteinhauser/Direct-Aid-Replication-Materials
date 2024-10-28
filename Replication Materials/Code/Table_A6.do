/***************************************************************************

	DESCRIPTION: 	This do file generates several balance tables and
					summary statistics. It computes summary statistics for 
					baseline variables collected during onboarding 
					sessions, including means, standard deviations, and 
					respondent counts.
	
	INPUTS:			- cleanedBaselineData.dta
	
	OUTPUTS:		- table_a6.tex

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
* 1. CREATING SUMMARY STATISTICS
********************************************************************************

use "${data}/cleanedBaselineData.dta", clear

* Creating matrix with the different elements 
mat sumStats = J(20 ,3,.)

local row = 1
foreach var in breadTea_breakfast breadTea_lunch breadTea_dinner totalBreadTea medicinePurchase income lifeSatisfaction skippedMeals happy employed married someEduc finDecisionMaker age numFamilyMembers bankAccount ableLeaveHouse transAirtime transMoney creditConstrained_yes{
	
	* Overall mean, SD and N for C1
	qui sum `var'
	mat sumStats[`row', 1] = round(r(mean), 0.01)
	mat sumStats[`row', 2] = round(r(sd), 0.01)
	mat sumStats[`row', 3] = round(r(N), 0.01)
	
	local ++row
	
}

preserve
*** Generating table
clear
svmat sumStats

* Labels
forvalues value = 1/3{
	rename sumStats`value' sumStats`value'_aux
	gen sumStats`value' = strofreal(sumStats`value'_aux)
	label var sumStats`value' "(`value')"
	replace sumStats`value' = "0" + sumStats`value' if regexm(sumStats`value', "^\.") == 1
	drop sumStats`value'_aux
	replace sumStats`value' = "" if sumStats`value' == "0."
}

* Variables 
gen var = ""
order var 
insobs 1, before(1)
replace var = "Panel A. Outcome Vars" if sumStats1 == ""
insobs 1, after(11)
replace var = "Panel B. Demographic Vars" if sumStats1 == "" & var == ""
insobs 1, after(11)
label var var "Variable"

replace var = "1. Bread-tea breakfast" in 2
replace var = "2. Bread-tea lunch" in 3
replace var = "3. Bread-tea dinner" in 4
replace var = "4. Total bread-tea meals" in 5
replace var = "5. Afford Medicine" in 6

replace var = "6. Income" in 7
replace var = "7. Life satisfaction" in 8
replace var = "8. Skipped meals" in 9
replace var = "9. Happy" in 10
replace var = "10. Employed" in 11

replace var = "1. Married" in 14
replace var = "2. Some education" in 15
replace var = "3. Fin. decision maker" in 16
replace var = "4. Age" in 17
replace var = "5. Number family members" in 18
replace var = "6. Has had bank account" in 19
replace var = "7. Able to leave house" in 20
replace var = "8. Has transf. airtime" in 21
replace var = "9. Has transf. money" in 22
replace var = "10. Credit constrained" in 23

texsave using "${output}/table_a6_temp.tex", ///
	title(Summary Statistics) align(l|ccc) ///
	hlines(-0) frag varlabels label(baseBalComparison) italics("Panel") ///
	headerlines("& Mean & SD & N") replace ///
	footnote("\justifying \noindent \(Notes \): Table shows the value of different variables at baseline, collected during the onboarding sessions, for the whole sample. Column 1 shows the mean of the variable, column 2 the standard deviation, and column 3 the number of respondents of this question at baseline.", size(scriptsize))
	
filefilter "${output}/table_a6_temp.tex" "${output}/table_a6.tex", ///
	from("\BSbegin{table}[tbp]") to("\BSbegin{table}[h!]") replace
filefilter "${output}/table_a6.tex" "${output}/table_a6_temp.tex", ///
	from("\BSbegin{tabularx}{\BSlinewidth}") to("\BSbegin{adjustbox}{max width=\BSlinewidth, max height=\BStextwidth}\BSbegin{tabular}") replace
filefilter "${output}/table_a6_temp.tex" "${output}/table_a6.tex", ///
	from("\BSend{tabularx}") to("\BSend{tabular} \BSend{adjustbox}") replace
erase "${output}/table_a6_temp.tex"

restore
