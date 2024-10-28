/***************************************************************************

	DESCRIPTION: 	This do file calculates baseline and long-term effects 
					using various tax indices and compares different 
					treatment groups. Additionally, it generates LaTeX 
					tables summarizing the results for baseline and 
					robustness analyses, including control means, 
					observations, and statistical significance.
	
	INPUTS:			- surveysDataset.dta
	
	OUTPUTS:		- table_3.tex
					- table_a7.tex
					
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
* 1. INFORMAL TAXATION RESULTS
********************************************************************************
use "${data}/surveysDataset.dta", clear

drop if round == 3 | round == 4 | round == . | treatment == . 
egen strata = group(nahia_city_comb aboveMedianTotalBreadTea)
tab strata, gen(stratum)
gen round_temp = round - 1

* Index without outlier 
gen tax_index_noOut = tax_index
qui sum tax_index
replace tax_index_noOut = . if tax_index_noOut == r(max)

*** A. Baseline results

estimates clear
tempfile prelim_results
local n = 1
foreach var in infTax_gov_others infTax_leader_others infTax_gov_you infTax_leader_you tax_index infTax_yesToAny{
	
	qui sum `var' if treatment == 0
	local mean_control = round(r(mean), 0.001)
	
	* Loop for variables with baseline information
	if regexm("`var'", "infTax") == 1{
		
		reghdfe `var' treatment `var'_base round_temp if t != 0, ///
			absorb(strata) vce(cluster uniqueId)
			
	}
	else{ // Variables without baseline value
		
		reghdfe `var' treatment round_temp if t != 0, ///
			absorb(strata) vce(cluster uniqueId)
		
	}
	
	estimates store coef_`n'
	
	if "`n'" == "1"{
		regsave using `prelim_results', replace ///
			table(var_`n', parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addvar(control_mean, `mean_control', `mean_control')
	}
	else{
		regsave using `prelim_results', append ///
			table(var_`n', parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addvar(control_mean, `mean_control', `mean_control')
	}
	
	local ++n
}

*** B. Long-term Effects
gen inter_timeTreat = round * treatment
foreach var in infTax_gov_others infTax_leader_others infTax_gov_you infTax_leader_you tax_index infTax_yesToAny{
		
	* Loop for variables with baseline information
	if regexm("`var'", "infTax") == 1{
		
		reghdfe `var' inter_timeTreat round treatment `var'_base if t != 0, ///
			absorb(strata) vce(cluster uniqueId)
		
	}
	else{ // Variables without baseline value
		
		reghdfe `var' inter_timeTreat round treatment if t != 0, ///
			absorb(strata) vce(cluster uniqueId)
		
	}
	
	test inter_timeTreat + treatment = 0
	local p = round(r(p), 0.001)	
	
	lincom inter_timeTreat + round + treatment
	local ratio = round(r(estimate) / _b[treatment], 0.01)
	
	regsave using `prelim_results', append ///
		table(var_`n', parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addlabel(ratio, `ratio', p_val, `p',)
		
	local ++n
}

*** C. List experiment
preserve
use "${data}/surveysDataset.dta", clear

drop if list_treatment != . & list_control != . // Some for some reason do both surveys

gen list_total = list_treatment
replace list_total = list_control if list_total == . & list_control != .
gen treated_list = 1 if list_treatment != .
replace treated_list = 0 if list_control != .

replace list_total = . if list_total > 20 & list_total != . // Dropping one outlier

qui sum list_total if round == 4
local mean = round(r(mean), 0.001)
reghdfe list_total treated_list if round == 4, vce(cluster uniqueId) absorb(enumerator_id)
regsave using `prelim_results', append table(var_`n', parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addlabel(mean, `mean')
local ++n

qui sum list_total if round == 4 & treatment == 0
local mean = round(r(mean), 0.001)
reghdfe list_total treated_list if treatment == 0 & round == 4, vce(cluster uniqueId) absorb(enumerator_id)
regsave using `prelim_results', append table(var_`n', parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addlabel(mean, `mean')
local ++n

qui sum list_total if round == 4 & treatment == 1
local mean = round(r(mean), 0.001)
reghdfe list_total treated_list if treatment == 1 & round == 4, vce(cluster uniqueId) absorb(enumerator_id)
regsave using `prelim_results', append table(var_`n', parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addlabel(mean, `mean')

****************
* 1B. CREATING TABLE
****************
* Panel A 
use `prelim_results', clear
drop if regexm(var, "(^o.|aboveMedian|_base_|_cons|^[0-9]|control_mean_stderr|^r2)") == 1

keep var var_1 var_2 var_3 var_4 var_5 var_6
drop if var_1 == ""
drop if regexm(var, "round_temp") == 1
gen temp = 2 if var != "control_mean_coef"
sort temp, stable
drop temp

insobs 1, before(1)
replace var = "Panel A. Baseline" if var == ""

replace var = "" if regexm(var, "_stderr") == 1
replace var = "Treated" if regexm(var, "^treatment") == 1
replace var = "Control Mean" if regexm(var, "^control_mean_coef") == 1
replace var = "Observations" if var == "N"

insobs 1, after(_N)

tempfile panelA
save `panelA'

* Panel B 
use `prelim_results', clear
drop if regexm(var, "(^o.|aboveMedian|_base_|_cons|^[0-9]|control_mean_stderr|^r2)") == 1

keep var var_7 var_8 var_9 var_10 var_11 var_12
drop if var_7 == ""
gen order = 1 if regexm(var, "inter_timeTreat") == 1
replace order = 2 if regexm(var, "round") == 1
replace order = 3 if regexm(var, "treatment") == 1
sort order, stable

insobs 1, before(1)
replace var = "Panel B. Long-Run" if var == ""

replace var = "" if regexm(var, "_stderr") == 1
replace var = "\( \beta_2 \): Round 2" if regexm(var, "^round") == 1
replace var = "\( \beta_3 \): Treated" if regexm(var, "^treatment") == 1
replace var = "\( \beta_1 \): Treated \( \times \) Round 2" if regexm(var, "^inter_timeTreat") == 1
replace var = "Observations" if var == "N"
replace var = "R Squared" if var == "r2"
replace var = "p-value \( \beta_1 \) + \( \beta_3 \) = 0 " if var == "p_val"
replace var = "(\( \beta_1 \) + \( \beta_2 \) + \( \beta_3 \)) / \( \beta_3 \) " if var == "ratio"

drop order 
insobs 1, after(_N)

rename (var_7 var_8 var_9 var_10 var_11 var_12) (var_1 var_2 var_3 var_4 var_5 var_6)

tempfile panelB
save `panelB'

* Panel C 
use `prelim_results', clear
drop if regexm(var, "(^o.|aboveMedian|_base_|_cons|^[0-9]|control_mean_stderr|^r2)") == 1
keep var var_13 var_14 var_15
drop if var_13 == ""

insobs 1, before(1)
replace var = "Panel C. List Experiment" if var == ""
 
replace var = "Treated List" if var == "treated_list_coef"
replace var = "" if regexm(var, "_stderr") == 1
replace var = "Observations" if var == "N"
replace var = "Mean Items Mentioned" if var == "mean"

replace var_13 = "All Sample" if var_13 == ""
replace var_14 = "Late Sample" if var_14 == ""
replace var_15 = "Early Sample" if var_15 == ""

rename (var_13 var_14 var_15) (var_1 var_2 var_3)

tempfile panelC
save `panelC'

* Combining them together
use `panelA', clear
append using `panelB'
append using `panelC'

local n = 1
foreach var in var_1 var_2 var_3 var_4 var_5 var_6{
	
	label var `var' "(`n')"
	local ++n
	
}

texsave using "${output}/table_3_temp.tex", ///
	title(Are Digital Payments Diverted?) nofix align(lcccccc) bold("Panel") ///
	headerlines("& Gov. Off. & Comm. Leader & Gov. Off. & Comm. Leader & KLK & Yes to"  "& Others & Others & You & You & Index & Any Question") ///
	hlines(-0 6 17) frag varlabels label(infTaxResults) ///
	footnote(`"\justifying \noindent \(Notes \): In Panels A and B, stratification fixed effects, survey round fixed effects, and baseline values of dependent variables, if available, are included. These are answers to questions of the type "Have you/someone in your community been asked to provide informal assistance (for example money or food) to local community leaders/government officials in the past month?". The outcome in column 6 was not pre-specified. Panel C shows the results of a list experiment where the treatment group received the following additional statement: "I have been approached by government officials or community leaders to provide them with any kind of assistance, like food or money, in the past month". This includes a control for surveyor fixed effects. Standard errors are clustered at the individual level. \\   \textit{Levels of significance}: *$ p<0.1$ , **$ p<0.05$ , ***$ p<0.01$."', size(scriptsize)) replace
	
filefilter "${output}/table_3_temp.tex" "${output}/table_3.tex", ///
	from("\BSbegin{table}[tbp]") to("\BSbegin{table}[h!]") replace
filefilter "${output}/table_3.tex" "${output}/table_3_temp.tex", ///
	from("\BSbegin{tabularx}{\BSlinewidth}") to("\BSbegin{adjustbox}{max width=\BSlinewidth, max height=\BStextwidth}\BSbegin{tabular}") replace
filefilter "${output}/table_3_temp.tex" "${output}/table_3.tex", ///
	from("\BSend{tabularx}") to("\BSend{tabular} \BSend{adjustbox}") replace
erase "${output}/table_3_temp.tex"

restore

***********************
* 2. ROBUSTNESS INDICES 
**********************

estimates clear
tempfile prelim_results
local n = 1
foreach var in tax_index_others tax_index_you tax_and_index tax_index_noOut{
	
	qui sum `var' if treatment == 0
	local mean_control = round(r(mean), 0.001)
	
	* Loop for variables with baseline information
	if regexm("`var'", "infTax") == 1{
		
		reghdfe `var' treatment `var'_base round_temp if t != 0, ///
			absorb(strata) vce(cluster uniqueId)
		
	}
	else{ // Variables without baseline value
		
		reghdfe `var' treatment round_temp if t != 0, ///
			absorb(strata) vce(cluster uniqueId)
		
	}
	
	estimates store coef_`n'
	
	if "`n'" == "1"{
		regsave using `prelim_results', replace ///
			table(var_`n', parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addvar(control_mean, `mean_control', `mean_control')
	}
	else{
		regsave using `prelim_results', append ///
			table(var_`n', parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addvar(control_mean, `mean_control', `mean_control')
	}
	
	local ++n
}

*** B. Long-term Effects
foreach var in tax_index_others tax_index_you tax_and_index tax_index_noOut{
		
	* Loop for variables with baseline information
	if regexm("`var'", "infTax") == 1{
		
		reghdfe `var' inter_timeTreat round treatment `var'_base if t != 0, ///
			absorb(strata) vce(cluster uniqueId)
		
	}
	else{ // Variables without baseline value
		
		reghdfe `var' inter_timeTreat round treatment if t != 0, ///
			absorb(strata) vce(cluster uniqueId)
		
	}
	
	test inter_timeTreat + treatment = 0
	local p = round(r(p), 0.001)	
	
	lincom inter_timeTreat + round + treatment
	local ratio = round(r(estimate) / _b[treatment], 0.01)
	
	regsave using `prelim_results', append ///
		table(var_`n', parentheses(stderr) asterisk(10 5 1) format(%7.3f)) addlabel(ratio, `ratio', p_val, `p',)
		
	local ++n
}

****************
* 2B. CREATING TABLE
****************
preserve
* Panel A 
use `prelim_results', clear
drop if regexm(var, "(^o.|aboveMedian|_base_|_cons|^[0-9]|control_mean_stderr|^r2)") == 1

keep var var_1 var_2 var_3 var_4
drop if var_1 == ""
drop if regexm(var, "round_temp") == 1
gen temp = 2 if var != "control_mean_coef"
sort temp, stable
drop temp

insobs 1, before(1)
replace var = "Panel A. Baseline" if var == ""

replace var = "" if regexm(var, "_stderr") == 1
replace var = "Treated" if regexm(var, "^treatment") == 1
replace var = "Control Mean" if regexm(var, "^control_mean_coef") == 1
replace var = "Observations" if var == "N"

insobs 1, after(_N)

tempfile panelA
save `panelA'

* Panel B 
use `prelim_results', clear
drop if regexm(var, "(^o.|aboveMedian|_base_|_cons|^[0-9]|control_mean_stderr|^r2)") == 1

keep var var_5 var_6 var_7 var_8
drop if var_5 == ""
gen order = 1 if regexm(var, "inter_timeTreat") == 1
replace order = 2 if regexm(var, "round") == 1
replace order = 3 if regexm(var, "treatment") == 1
sort order, stable

insobs 1, before(1)
replace var = "Panel B. Long-Run" if var == ""

replace var = "" if regexm(var, "_stderr") == 1
replace var = "\( \beta_2 \): Round 2" if regexm(var, "^round") == 1
replace var = "\( \beta_3 \): Treated" if regexm(var, "^treatment") == 1
replace var = "\( \beta_1 \): Treated \( \times \) Round 2" if regexm(var, "^inter_timeTreat") == 1
replace var = "Observations" if var == "N"
replace var = "R Squared" if var == "r2"
replace var = "p-value \( \beta_1 \) + \( \beta_3 \) = 0 " if var == "p_val"
replace var = "(\( \beta_1 \) + \( \beta_2 \) + \( \beta_3 \)) / \( \beta_3 \) " if var == "ratio"

drop order 
rename (var_5 var_6 var_7 var_8) (var_1 var_2 var_3 var_4)

tempfile panelB
save `panelB'

* Combining them together
use `panelA', clear
append using `panelB'

local n = 1
foreach var in var_1 var_2 var_3 var_4{
	
	label var `var' "(`n')"
	local ++n
	
}

texsave using "${output}/table_a7_temp.tex", ///
	title(Are Digital Payments Diverted? Indices) nofix align(lcccc) bold("Panel") ///
	headerlines("& KLK Index & KLK Index & Anderson Index & Dropping"  "& Others & You & All & Outlier") ///
	hlines(-0 6) frag varlabels label(infTaxResults_index) ///
	footnote(`"\justifying \noindent \(Notes \): In Panels A and B, controls for stratification fixed effects, survey round fixed effects, and baseline values of dependent variables, if available, are included. Columns 1-2 create indices following Kling, Liebman \& Katz (2007). Column 1 uses the two measures on whether others in their community have been asked to provide informal assistance, while column 2 uses the two measures on whether participants themselves have been asked to provide informal assistance by political actors. Column 3 includes the four measures, but creates the index following Anderson (2008). Column 4 uses the KLK Index composed of the four informal taxation questions (as in the baseline results), but dropping the one observation with the highest KLK Index value. Control for surveyor fixed effects are included. Standard errors are clustered at the individual level. \\   \textit{Levels of significance}: *$ p<0.1$ , **$ p<0.05$ , ***$ p<0.01$."', size(scriptsize)) replace
	
filefilter "${output}/table_a7_temp.tex" "${output}/table_a7.tex", ///
	from("\BSbegin{table}[tbp]") to("\BSbegin{table}[h!]") replace
filefilter "${output}/table_a7.tex" "${output}/table_a7_temp.tex", ///
	from("\BSbegin{tabularx}{\BSlinewidth}") to("\BSbegin{adjustbox}{max width=\BSlinewidth, max height=\BStextwidth}\BSbegin{tabular}") replace
filefilter "${output}/table_a7_temp.tex" "${output}/table_a7.tex", ///
	from("\BSend{tabularx}") to("\BSend{tabular} \BSend{adjustbox}") replace
erase "${output}/table_a7_temp.tex"

restore

