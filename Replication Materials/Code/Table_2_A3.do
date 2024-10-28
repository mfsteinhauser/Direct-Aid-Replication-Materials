/***************************************************************************

	DESCRIPTION: 	This do file runs regressions to prepare tables. It 
					estimates treatment effects for nutrition and wellbeing 
					outcomes across two survey rounds. It also focuses on 
					other aspects, including types of food and economic 
					outcomes, with similar analysis steps.
	
	INPUTS:			- surveysDataset.dta
	
	OUTPUTS:		- table_2.tex
					- table_a3.tex

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

replace round = round - 1
gen inter_timeTreat = round * treatment

* Generating strata variable 
egen strata = group(nahia_city_comb aboveMedianTotalBreadTea)

********************************************************************************
* 1. TABLE 2 PAPER - NUTRITION + WELLBEING OUTCOMES
********************************************************************************
*** A. Estimation
mat treatEffects = J(10, 4, .)
tempfile prelim_results
local n = 1
foreach var in skippedMeals childSkippedMeals eatTwiceADay totalBreadTea nutrition_index econSituationImproved finSatisfied happy lifeSatisfaction econ_index{
	
	qui sum `var' if treatment == 0
	local mean_control = round(r(mean), 0.001)
	
	* Loop for variables with baseline information
	if inlist("`var'", "skippedMeals", "breadTea_lunch", "medicinePurchase", "happy", "lifeSat", "income", "employed", "finDecisionMaker") == 1 | regexm("`var'", "infTax") == 1{
		
		reghdfe `var' inter_timeTreat round treatment `var'_base if t != 0, absorb(strata) vce(cluster uniqueId)
		
	}
	else{ // Variables without baseline value
		
		reghdfe `var' inter_timeTreat round treatment if t != 0, ///
			absorb(strata) vce(cluster uniqueId)
		
	}

	lincom inter_timeTreat + round + treatment
	local ratio = round(r(estimate) / _b[treatment], 0.01)
	
	if "`n'" == "1"{
		regsave using `prelim_results', replace ///
			table(var_`n', parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addvar(control_mean, `mean_control', `mean_control') addlabel(ratio, `ratio')
	}
	else{
		regsave using `prelim_results', append ///
			table(var_`n', parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addvar(control_mean, `mean_control', `mean_control') addlabel(ratio, `ratio')
	}
	
	mat treatEffects[`n', 3] = round(r(estimate), 0.00001)
	mat treatEffects[`n', 4] = round(r(se), 0.00001)

	local ++n
	
}

*** B. Table
preserve
use `prelim_results', clear
 
drop if regexm(var, "(^o.|aboveMedian|_base_|_cons|^[0-9]|control_mean_stderr)") == 1
replace var = "Control Mean" if var == "control_mean_coef"

* Creating two panels
keep var var_6 var_7 var_8 var_9 var_10
rename (var_6 var_7 var_8 var_9 var_10) (var_1 var_2 var_3 var_4 var_5)

tempfile panel2
save `panel2'

use `prelim_results', clear
 
drop if regexm(var, "(^o.|aboveMedian|_base_|_cons|^[0-9]|control_mean_stderr)") == 1
replace var = "Control Mean" if var == "control_mean_coef"

insobs 1, before(1)
replace var = "Panel A. Food Security" if var == ""
insobs 1, after(1)
keep var var_1 var_2 var_3 var_4 var_5 
insobs 1, after(_N)
replace var = "Panel B. Wellbeing" if var == "" & regexm(var[_n-1], "Panel") != 1
insobs 1, before(_N)
insobs 1, after(_N)
append using `panel2'

replace var = "" if regexm(var, "_stderr") == 1
replace var = "\( \beta_2 \): Round 2" if regexm(var, "^round") == 1
replace var = "\( \beta_3 \): Treated" if regexm(var, "^treatment") == 1
replace var = "\( \beta_1 \): Treated \( \times \) Round 2" if regexm(var, "^inter_timeTreat") == 1
replace var = "Observations" if var == "N"
replace var = "R Squared" if var == "r2"
replace var = "(\( \beta_1 \) + \( \beta_2 \) + \( \beta_3 \)) / \( \beta_3 \) " if var == "ratio"
drop if var == "R Squared"

replace var_1 = "Days Skipping" if regexm(var, "Panel A") == 1
replace var_1 = "Meals" if var_1[_n-1] == "Days Skipping"
replace var_2 = "Children Skipping" if regexm(var, "Panel A") == 1
replace var_2 = "Meals (=1)" if var_2[_n-1] == "Children Skipping"
replace var_3 = "Regularly Eat" if regexm(var, "Panel A") == 1
replace var_3 = "Twice a Day (=1)" if var_3[_n-1] == "Regularly Eat"
replace var_4 = "Total Bread" if regexm(var, "Panel A") == 1
replace var_4 = "\& Tea Meals" if var_4[_n-1] == "Total Bread"
replace var_5 = "KLK Index" if regexm(var, "Panel A") == 1

replace var_1 = "Better Economic" if regexm(var, "Panel B") == 1
replace var_1 = "Situation" if var_1[_n-1] == "Better Economic"
replace var_2 = "Satisfied with" if regexm(var, "Panel B") == 1
replace var_2 = "Financial Sit." if var_2[_n-1] == "Satisfied with"
replace var_3 = "Happy" if regexm(var, "Panel B") == 1
replace var_4 = "Life Satisfaction" if regexm(var, "Panel B") == 1
replace var_5 = "KLK Index" if regexm(var, "Panel B") == 1

local n = 1
foreach var in var_1 var_2 var_3 var_4 var_5{
	
	label var `var' "(`n')"
	local ++n
	
}

texsave using "${output}/table_2_temp.tex", ///
	title(Short vs. Long Treatment Effects -- Food Security \& Wellbeing Outcomes) ///
	nofix align(lccccc) bold("Panel")  ///
	hlines(-0 2 14 12) frag varlabels label(dynEffectsMain) ///
	footnote("\justifying \noindent \(Notes \): This table reports estimated impacts of treatment separately for the first and second survey round. Households were surveyed once per month for two months. Each of these months constitutes a survey round. All specifications control for stratum fixed effects and the baseline value of the dependent variable, if available. Standard errors are clustered at individual level. The outcome variables follow the primary outcomes shown in Table \ref{ITTsumTableAbridged}. \\   \textit{Levels of significance}: *$ p<0.1$ , **$ p<0.05$ , ***$ p<0.01$.", size(scriptsize)) replace
	
filefilter "${output}/table_2_temp.tex" "${output}/table_2.tex", ///
	from("\BSbegin{table}[tbp]") to("\BSbegin{table}[h!]") replace
filefilter "${output}/table_2.tex" "${output}/table_2_temp.tex", ///
	from("\BSbegin{tabularx}{\BSlinewidth}") to("\BSbegin{adjustbox}{max width=\BSlinewidth, max height=\BStextwidth}\BSbegin{tabular}") replace
filefilter "${output}/table_2_temp.tex" "${output}/table_2.tex", ///
	from("\BSend{tabularx}") to("\BSend{tabular} \BSend{adjustbox}") replace
erase "${output}/table_2_temp.tex"

restore

********************************************************************************
* 2. Short vs long run REGRESSION - OTHER OUTCOMES
********************************************************************************
*** A. Estimation
mat treatEffects = J(10, 4, .)
tempfile prelim_results
local n = 1
foreach var in ateRice ateBeans ateVegetables ateChicken ateDairy income employed finDecisionMaker medicinePurchase{
	
	qui sum `var' if treatment == 0
	local mean_control = round(r(mean), 0.001)
	
	* Loop for variables with baseline information
	if inlist("`var'", "income", "employed", "finDecisionMaker", "medicinePurchase") == 1{
		
		reghdfe `var' inter_timeTreat round treatment `var'_base if t != 0, absorb(strata) vce(cluster uniqueId)
		
	}
	else{ // Variables without baseline value
		
		reghdfe `var' inter_timeTreat round treatment if t != 0, ///
			absorb(strata) vce(cluster uniqueId)
		
	}
	
	mat treatEffects[`n', 1] = round(_b[treatment], 0.00001)
	mat treatEffects[`n', 2] = round(_se[treatment], 0.00001)
		
	if "`n'" == "1"{
		regsave using `prelim_results', replace ///
			table(var_`n', parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addvar(control_mean, `mean_control', `mean_control')
	}
	else{
		regsave using `prelim_results', append ///
			table(var_`n', parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addvar(control_mean, `mean_control', `mean_control')
	}
	
	lincom inter_timeTreat + round + treatment
	
	mat treatEffects[`n', 3] = round(r(estimate), 0.00001)
	mat treatEffects[`n', 4] = round(r(se), 0.00001)

	local ++n
	
}

*** B. Table
preserve
use `prelim_results', clear
 
drop if regexm(var, "(^o.|aboveMedian|_base_|_cons|^[0-9]|control_mean_stderr)") == 1
replace var = "Control Mean" if var == "control_mean_coef"

* Creating two panels
keep var var_6 var_7 var_8 var_9 
rename (var_6 var_7 var_8 var_9) (var_1 var_2 var_3 var_4)

tempfile panel2
save `panel2'

use `prelim_results', clear
 
drop if regexm(var, "(^o.|aboveMedian|_base_|_cons|^[0-9]|control_mean_stderr)") == 1
replace var = "Control Mean" if var == "control_mean_coef"

insobs 1, before(1)
replace var = "Panel A. Types of Food" if var == ""
keep var var_1 var_2 var_3 var_4 var_5 
insobs 1, after(_N)
replace var = "Panel B. Other Economic Outcomes" if var == ""
insobs 1, before(_N)
append using `panel2'

replace var = "" if regexm(var, "_stderr") == 1
replace var = "\( \beta_2 \): Round 2" if regexm(var, "^round") == 1
replace var = "\( \beta_3 \): Treated" if regexm(var, "^treatment") == 1
replace var = "\( \beta_1 \): Treated \( \times \) Round 2" if regexm(var, "^inter_timeTreat") == 1
replace var = "Observations" if var == "N"
replace var = "R Squared" if var == "r2"
replace var = "p-value \( \beta_1 \) + \( \beta_3 \) = 0 " if var == "p_val"

replace var_1 = "Rice" if regexm(var, "Panel A") == 1
replace var_2 = "Beans" if regexm(var, "Panel A") == 1
replace var_3 = "Vegetables" if regexm(var, "Panel A") == 1
replace var_4 = "Chicken" if regexm(var, "Panel A") == 1
replace var_5 = "Dairy" if regexm(var, "Panel A") == 1

replace var_1 = "Income" if regexm(var, "Panel B") == 1
replace var_2 = "Employed" if regexm(var, "Panel B") == 1
replace var_3 = "Fin. Decision-Maker" if regexm(var, "Panel B") == 1
replace var_4 = "Medicine Purchase" if regexm(var, "Panel B") == 1

local n = 1
foreach var in var_1 var_2 var_3 var_4 var_5{
	
	label var `var' "(`n')"
	local ++n
	
}

texsave using "${output}/table_a3_temp.tex", ///
	title(Treatment Effects by Survey Round -- Secondary Outcomes) nofix align(l|ccccc) bold("Panel") ///
	hlines(-0 1 12 11) frag varlabels label(SvLSecTEs) ///
	footnote("\justifying \noindent \(Notes \): This table reports estimated impacts of treatment separately for the first and second survey round. Households were surveyed once per month for two months. Each of these months constitutes a survey round. All specifications control for stratum fixed effects and the baseline value of the dependent variable, if available. Standard errors are clustered at individual level. \\   \textit{Levels of significance}: *$ p<0.1$ , **$ p<0.05$ , ***$ p<0.01$.", size(scriptsize)) replace
	
filefilter "${output}/table_a3_temp.tex" "${output}/table_a3.tex", ///
	from("\BSbegin{table}[tbp]") to("\BSbegin{table}[h!]") replace
filefilter "${output}/table_a3.tex" "${output}/table_a3_temp.tex", ///
	from("\BSbegin{tabularx}{\BSlinewidth}") to("\BSbegin{adjustbox}{max width=\BSlinewidth, max height=\BStextwidth}\BSbegin{tabular}") replace
filefilter "${output}/table_a3_temp.tex" "${output}/table_a3.tex", ///
	from("\BSend{tabularx}") to("\BSend{tabular} \BSend{adjustbox}") replace
erase "${output}/table_a3_temp.tex"

restore
