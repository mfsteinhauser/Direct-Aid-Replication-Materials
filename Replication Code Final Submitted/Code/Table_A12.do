/***************************************************************************

	DESCRIPTION: 	This do file generates a balance table to examine 
					attrition between two survey rounds, and to estimate 
					the impact of treatment and survey round on attrition 
					rates.
	
	INPUTS:			- cleanedBaselineData.dta
					- cleanedFollowUpData.dta
					
	OUTPUTS:		- table_a12

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
* 1. CREATING BALANCE TABLE
********************************************************************************
* Generating attrition dummies
* Round 1
use "${data}/cleanedFollowUpData.dta", clear 
keep if round == 1
merge 1:1 uniqueId using "${data}/cleanedBaselineData.dta"
gen missing = _merge == 2
replace round = 1 if round == .

tempfile att_round1
save `att_round1'

* Round 2
use "${data}/cleanedFollowUpData.dta", clear 
keep if round == 2
merge 1:1 uniqueId using "${data}/cleanedBaselineData.dta"
gen missing = _merge == 2
replace round = 2 if round == .

append using `att_round1'

* Creating interactions
replace round = round - 1
gen inter_treat = treatment * round

count if missing == 1 & round == 0
local first_round = r(N)

count if missing == 1 & round == 1
local second_round = r(N)

* Generating strata variable 
egen strata = group(nahia_city_comb aboveMedianTotalBreadTea)

* Estimating regressions
tempfile prelim_results
reghdfe missing treatment, absorb(strata) vce(cluster uniqueId)
regsave using `prelim_results', replace table(attrition, parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addlabel(first_round, `first_round', second_round, `second_round')

reghdfe missing treatment round inter_treat, ///
	absorb(strata) vce(cluster uniqueId)
regsave using `prelim_results', append table(inter, parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addlabel(first_round, `first_round', second_round, `second_round')

preserve
use `prelim_results', clear
 
drop if regexm(var, "(nahia_city_comb|aboveMedian|_cons|^[0-9])") == 1

replace var = "" if regexm(var, "_stderr") == 1
replace var = "Treated" if regexm(var, "^treatment") == 1
replace var = "Round 2" if regexm(var, "^round_coef") == 1
replace var = "Treated \( \times \) Round 2" if regexm(var, "^inter_treat") == 1
replace var = "R Squared" if var == "r2"
replace var = "Observations" if var == "N"
replace var = "Round 1 Attrited" if var == "first_round"
replace var = "Round 2 Attrited" if var == "second_round"

local n = 1
foreach var in attrition inter{
	
	label var `var' "(`n')"
	local ++n
	
}

drop if regexm(var, "Attrited") == 1

texsave using "${output}/table_a12_temp.tex", ///
	title(Attrition) nofix align(l|cc) ///
	headerlines("& Attrited & By Survey Round") hlines(0 -0 -2) frag varlabels  ///
	footnote("\justifying \noindent \(Notes \): Controls for strata fixed effects are included. Standard errors are clustered at the individual level. In the first survey round, 29 interviews could not be completed (17 treatment, 12 control). In the second round, 26 interviews could not be completed (10 treatment, 16 control). Households were surveyed once per month, over two months. Each of these months constitutes a survey round. \\   \textit{Levels of significance}: *$ p<0.1$ , **$ p<0.05$ , ***$ p<0.01$.", width(0.6\linewidth)) replace
	
filefilter "${output}/table_a12_temp.tex" "${output}/table_a12.tex", ///
	from("\BSbegin{table}[tbp]") to("\BSbegin{table}[h!]") replace
filefilter "${output}/table_a12.tex" "${output}/table_a12_temp.tex", ///
	from("\BSbegin{tabularx}{\BSlinewidth}") to("\BSbegin{adjustbox}{max width=\BSlinewidth, max height=\BStextwidth}\BSbegin{tabular}") replace
filefilter "${output}/table_a12_temp.tex" "${output}/table_a12.tex", ///
	from("\BSend{tabularx}") to("\BSend{tabular} \BSend{adjustbox}") replace
erase "${output}/table_a12_temp.tex"

restore
