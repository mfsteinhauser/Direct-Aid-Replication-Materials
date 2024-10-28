/***************************************************************************

	DESCRIPTION: 	This do file performs analysis to compare actual 
					values from the survey with predictions made by experts.
	
	INPUTS:			- expertsSurvey.csv
					- cleanedBaselineData.dta
					- cleanedTransactionData.dta
					- surveysDataset.dta"
	
	OUTPUTS:		- table_4.tex

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
* 1. Cleaning the data
********************************************************************************
* Generating matrix to store results
mat expSurvey = J(8, 3, .)

import delimited "${data}/expertsSurvey.csv", varnames(1) clear

*drop if regexm(startdate, "[aA-zZ]") == 1
*drop ipaddress finished responseid externalreference locationlatitude locationlongitude distributionchannel userlanguage status progress recordeddate recipientlastname recipientfirstname recipientemail consent

*drop if q6 == "Tarek Ghani"

destring durationinseconds q1_1 q2_1 q3_1 q4_1, replace

rename (durationinseconds q1_1 q2_1 q3_1 q4_1) (duration percBuyGoods howManyBreadTea shareTaxed deliveryCost)
label var percBuyGoods "Expected % of participants able to use funds"
label var howManyBreadTea "Expected # of bread and tea meals"
label var shareTaxed "Expected % of participants asked for tax"
label var deliveryCost "Expected costs of delivering aid"

* Generating actual numbers
preserve 

use "${data}/surveysDataset.dta", clear

drop if round == 3 | round == 4

unique uniqueId if treatment == 1
local total_ind = r(unique)

qui sum totalBreadTea if treatment == 1 & t != 0
local howManyBreadTea_actual = round(r(mean), 0.01)
mat expSurvey[1, 1] = `howManyBreadTea_actual'

gen taxed_in_any_way = infTax_gov_you + infTax_leader_you
unique uniqueId if taxed_in_any_way != 0 & taxed_in_any_way != . & treatment == 1 & t != 0
local shareTaxed_actual = round(100*(r(unique) / `total_ind'), 0.01)
mat expSurvey[3, 1] = `shareTaxed_actual'

use "${data}/cleanedTransactionData.dta", clear

merge m:1 uniqueId using "${data}/cleanedBaselineData.dta", keepusing(treatment)
keep if _merge == 3 

qui sum no_transactions if treatment == 1  & trans_number == 1
local percBuyGoods_actual = round((1 - r(mean)) * 100, 0.01)
mat expSurvey[5, 1] = `percBuyGoods_actual'

restore 

local deliveryCost_actual = 6.7
mat expSurvey[7, 1] = `deliveryCost_actual'

local row = 1
local row_plus = `row' + 1
foreach var in howManyBreadTea shareTaxed percBuyGoods deliveryCost{
	
	* For table (estimates, SEs, and p-value of equality to actual value)
	qui reg `var'
	mat expSurvey[`row', 2] = round(_b[_cons], 0.01)
	mat expSurvey[`row_plus', 2] = round(_se[_cons], 0.01)
	
	test _b[_cons] = ``var'_actual'
	mat expSurvey[`row', 3] = round(r(p), 0.001)
	
	local row = `row' + 2
	local row_plus = `row_plus' + 2
}

* Creating table 
clear 
svmat expSurvey

* Add parentheses to SEs 
foreach var in expSurvey1 expSurvey2 expSurvey3{
	gen `var'_temp = strofreal(`var')
	drop `var'
	replace `var'_temp = "" if `var'_temp == "."
}

replace expSurvey2_temp = "0" + expSurvey2_temp if regexm(expSurvey2_temp, "^\.") == 1
replace expSurvey2_temp = "(" + expSurvey2 + ")" if expSurvey1_temp == ""
replace expSurvey3_temp = "\(<\) 0.001" if expSurvey3_temp == "0"

gen var = "How Many Bread and Tea Meals in Past Week?" in 1
replace var = "Share Reporting Diversion Attempts" in 3
replace var = "Share Able to Use Digital Payments" in 5
replace var = "Delivery Costs" in 7

order var

forvalues col = 1/3{
	label var expSurvey`col'_temp "(`col')"
}

texsave using "${output}/table_4_temp.tex", ///
	title("Actual Values vs. Experts' Predictions") align(lccc) ///
	hlines(-0) frag varlabels label(expPreds) ///
	headerlines("Variable & Actual Values & Predicted Values & p-value Predicted = Actual") ///
	footnote("\justifying \noindent \(Notes \): The first column shows the actual values of different elements of the intervention. The second column shows the mean predicted value by our sample of experts (with standard deviations in parentheses). The third column shows the p-value of a test of the mean predicted value being equal to the actual value (i.e. a test of equality of columns 1 and 2).", size(scriptsize)) replace
	
filefilter "${output}/table_4_temp.tex" "${output}/table_4.tex", ///
	from("\BSbegin{table}[tbp]") to("\BSbegin{table}[h!]") replace
filefilter "${output}/table_4.tex" "${output}/table_4_temp.tex", ///
	from("\BSbegin{tabularx}{\BSlinewidth}") to("\BSbegin{adjustbox}{max width=\BSlinewidth, max height=\BStextwidth}\BSbegin{tabular}") replace
filefilter "${output}/table_4_temp.tex" "${output}/table_4.tex", ///
	from("\BSend{tabularx}") to("\BSend{tabular} \BSend{adjustbox}") replace
erase "${output}/table_4_temp.tex"

