/***************************************************************************

	DESCRIPTION: 	This do file generates several balance tables and
					summary statistics. It performs the following tasks:
					- Creates a balance table with overall sample statistics
	
	INPUTS:			- cleanedBaselineData.dta (cleaned baseline data)
	
	OUTPUTS:		- Table A2

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
* CREATING BALANCE TABLE
********************************************************************************

use "${data}/cleanedBaselineData.dta", clear

* Creating matrix with the different elements 
mat sumStats = J(19 ,5,.)

local row = 1
foreach var in totalBreadTea skippedMeals infTax_gov_others infTax_leader_others infTax_gov_you infTax_leader_you lifeSatisfaction happy married pashtun someEduc balkh herat kabul aboveMedianAge finDecisionMaker aboveMedianHHSize ableLeaveHouse{
	
	if regexm("`var'", "infTax") == 1 local decimals = 0.001
	else local decimals = 0.01
	
	* Overall mean and SD
	qui sum `var'
	mat sumStats[`row', 1] = round(r(mean), `decimals')
	mat sumStats[`row', 2] = round(r(sd), `decimals')
	
	* Mean of treatment group
	qui sum `var' if treatment == 1
	mat sumStats[`row', 3] = round(r(mean), `decimals')
	
	* Mean of control group
	qui sum `var' if treatment == 0
	mat sumStats[`row', 4] = round(r(mean), `decimals')
	
	* p-value of difference
	reghdfe `var' treatment, vce(robust)
	test _b[treatment] == 0
	mat sumStats[`row', 5] = round(r(p), `decimals')
	
	local ++row
	
}

* Number of individuals
qui count if treatment != .
mat sumStats[`row', 1] = r(N)

* Mean of treatment group
qui count if treatment == 1
mat sumStats[`row', 3] = r(N)

* Mean of control group
qui count if treatment == 0
mat sumStats[`row', 4] = r(N)

*** Generating table
preserve
clear
svmat sumStats

* Labels
forvalues value = 1/5{
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
insobs 1, after(9)
replace var = "Panel B. Heterogeneity Vars" if sumStats1 == "" & var == ""
insobs 1, after(9)
label var var "Variable"

replace var = "1. Total bread-tea meals" in 2
replace var = "2. Skipped meals" in 3
replace var = "3. Inf tax gov (others)" in 4
replace var = "4. Inf tax leaders (others)" in 5
replace var = "5. Inf tax gov (you)" in 6
replace var = "6. Inf tax leaders (you)" in 7
replace var = "7. Life satisfaction" in 8
replace var = "8. Happy" in 9

replace var = "1. Married" in 12
replace var = "2. Pashtun" in 13
replace var = "3. Some education" in 14
replace var = "4. Balkh" in 15
replace var = "5. Herat" in 16
replace var = "6. Kabul" in 17
replace var = "7. Above median age" in 18
replace var = "8. Fin. decision maker" in 19
replace var = "9. Above median HH size" in 20
replace var = "10. Able to leave house" in 21

replace sumStats5 = "1" if sumStats5 == "" & var != "" & regexm(var, "(Panel|Number of individuals)") != 1

replace var = "Number of individuals" in 22

texsave using "${output}/table_a2_temp.tex", ///
	title(Baseline Balance Check) align(l|ccccc) ///
	hlines(-0) frag varlabels label(baseBalCheck) bold("Panel") ///
	headerlines("  & \multicolumn{2}{c}{Whole Sample} & Treatment & Control & p-value" "& Mean & SD & Mean & Mean & Difference") ///
	footnote("\justifying \noindent \(Notes \): The table shows, for different pre-specified variables at baseline, the overall mean and standard deviation (columns 1 and 2), the mean in the treatment group (column 3) and the mean in the control group (column 4). Column 5 shows the p-value of the difference between the means in the treatment and control group, adjusting for robust standard errors.", size(scriptsize)) replace
	
filefilter "${output}/table_a2_temp.tex" "${output}/table_a2.tex", ///
	from("\BSbegin{table}[tbp]") to("\BSbegin{table}[h!]") replace
filefilter "${output}/table_a2.tex" "${output}/table_a2_temp.tex", ///
	from("\BSbegin{tabularx}{\BSlinewidth}") to("\BSbegin{adjustbox}{max width=\BSlinewidth, max height=\BStextwidth}\BSbegin{tabular}") replace
filefilter "${output}/table_a2_temp.tex" "${output}/table_a2.tex", ///
	from("\BSend{tabularx}") to("\BSend{tabular} \BSend{adjustbox}") replace
erase "${output}/table_a2_temp.tex"
