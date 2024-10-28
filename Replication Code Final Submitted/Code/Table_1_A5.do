/***************************************************************************

	DESCRIPTION: 	This do file runs pooled regressions and generates  
					summary tables with treatment effects.
	
	INPUTS:			- surveysDataset.dta
	
	OUTPUTS:		- table_1.tex
					- table_a5.tex

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
set seed 157621617

use "${data}/surveysDataset.dta", clear

drop if round == 3 | round == 4

* Generating strata variable 
egen strata = group(nahia_city_comb aboveMedianTotalBreadTea)
gen round_temp = round - 1

********************************************************************************
* 2. POOLED REGRESSION - CREATING SUMMARY TABLE
********************************************************************************

mat sumTable = J(27, 8, .)

rename (infTax_leader_others infTax_leader_others_base econSituationImproved lifeSatisfaction lifeSatisfaction_base) (infTax_l_others infTax_l_others_base econSitBetter lifeSat lifeSat_base)

*** A. Estimation
local row = 1
foreach var in skippedMeals childSkippedMeals eatTwiceADay totalBreadTea nutrition_index nut_and_index infTax_gov_others infTax_l_others infTax_gov_you infTax_leader_you tax_index tax_and_index econSitBetter finSatisfied happy lifeSat econ_index econ_and_index ateRice ateBeans ateVegetables ateChicken ateDairy medicinePurchase finDecisionMaker income employed{
	
	*** 1. Control mean
	qui sum `var' if treatment == 0
	mat sumTable[`row', 1] = round(r(mean), 0.001)
	mat sumTable[`row', 2] = round(r(sd), 0.001)
	
	*** 2. Romano-Wolf p-value
	if "`var'" == "skippedMeals"{
		rwolf2 (reg skippedMeals treatment i.strata round_temp skippedMeals_base if t != 0, vce(cluster uniqueId)) ///
			(reg childSkippedMeals treatment i.strata round_temp if t != 0, vce(cluster uniqueId)) ///
			(reg eatTwiceADay treatment i.strata round_temp if t != 0, vce(cluster uniqueId)) ///
			(reg totalBreadTea treatment i.strata round_temp totalBreadTea_base if t != 0, vce(cluster uniqueId)) ///
			, seed(2110) reps(5000) strata(strata) usevalid indepvars(treatment, treatment, treatment, treatment)
			
		local p_skippedMeals = round(e(rw_skippedMeals_treatment), 0.0001)
		local p_childSkippedMeals = round(e(rw_childSkippedMeals_treatment), 0.0001)
		local p_eatTwiceADay = round(e(rw_eatTwiceADay_treatment), 0.0001)
		local p_totalBreadTea = round(e(rw_totalBreadTea_treatment), 0.0001)
		
	}
	else if "`var'" == "infTax_gov_others"{
		rwolf2 (reg infTax_gov_others treatment i.strata round_temp infTax_gov_others_base if t != 0, vce(cluster uniqueId)) ///
			(reg infTax_l_others treatment i.strata round_temp infTax_l_others_base if t != 0, vce(cluster uniqueId)) ///
			(reg infTax_gov_you treatment i.strata round_temp infTax_gov_you_base if t != 0, vce(cluster uniqueId)) ///
			(reg infTax_leader_you treatment i.strata round_temp infTax_leader_you_base if t != 0, vce(cluster uniqueId)) ///
			, seed(2110) reps(5000) strata(strata) usevalid indepvars(treatment, treatment, treatment, treatment)
			
		local p_infTax_gov_others = round(e(rw_infTax_gov_others_treatment), 0.0001)
		local p_infTax_l_others = round(e(rw_infTax_l_others_treatment), 0.0001)
		local p_infTax_gov_you = round(e(rw_infTax_gov_you_treatment), 0.0001)
		local p_infTax_leader_you = round(e(rw_infTax_leader_you_treatment), 0.0001)
		
	}
	else if "`var'" == "econSitBetter"{
		rwolf2 (reg econSitBetter treatment i.strata round_temp if t != 0, vce(cluster uniqueId)) ///
			(reg finSatisfied treatment i.strata round_temp if t != 0, vce(cluster uniqueId)) ///
			(reg happy treatment i.strata round_temp happy_base if t != 0, vce(cluster uniqueId)) ///
			(reg lifeSat treatment i.strata round_temp lifeSat_base if t != 0, vce(cluster uniqueId)) ///
			, seed(2110) reps(5000) strata(strata) usevalid indepvars(treatment, treatment, treatment, treatment)
			
		local p_econSitBetter = round(e(rw_econSitBetter_treatment), 0.0001)
		local p_finSatisfied = round(e(rw_finSatisfied_treatment), 0.0001)
		local p_happy = round(e(rw_happy_treatment), 0.0001)
		local p_lifeSat = round(e(rw_lifeSat_treatment), 0.0001)
		
	}
	
	*** 3. Treatment effects
	* Loop for variables with baseline information
	if inlist("`var'", "skippedMeals", "breadTea_lunch", "medicinePurchase", "happy", "lifeSat", "income", "employed", "finDecisionMaker") == 1 | regexm("`var'", "infTax") == 1{
		
		reghdfe `var' treatment round_temp `var'_base if t != 0, ///
			absorb(strata) vce(cluster uniqueId)
		
	}
	else{ // Variables without baseline value
		
		reghdfe `var' treatment round_temp if t != 0, ///
			absorb(strata) vce(cluster uniqueId)
		
	}
	
	if inlist("`var'", "medicinePurchase", "finDecisionMaker", "income", "employed") == 1 | regexm("`var'", "^ate") == 1{
		local p_`var' = 10
	}
	else if regexm("`var'", "_index") == 1{
		local p_`var' = 100
	}
		
	mat sumTable[`row', 3] = round(_b[treatment], 0.001)
	mat sumTable[`row', 4] = round(_se[treatment], 0.001)
	mat sumTable[`row', 8] = e(N) // Number of observations in regression
	test treatment = 0
	mat sumTable[`row', 5] = round(r(p), 0.001)
	mat sumTable[`row', 6] = `p_`var''
	mat sumTable[`row', 7] = `row'
	
	local ++row
	
}

*** 4. Adding FDR p-values to secondary outcomes
cap frame create fdr
frame fdr{
	
	* Keeping original p-values
	clear
	svmat sumTable
	keep if sumTable6 == 10
	keep sumTable5 sumTable7
	rename sumTable5 pval
	
	quietly sum pval
	local totalpvals = r(N)

	rename sumTable7 original_sorting_order
	quietly sort pval
	quietly gen int rank = _n if pval~=.
	
	local qval = 1

	* Generate the variable that will contain the BKY (2006) sharpened q-values
	gen bky06_qval = 1 if pval~=.

	* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.

	while `qval' > 0 {
		* First Stage
		* Generate the adjusted first stage q level we are testing: q' = q/1+q
		local qval_adj = `qval'/(1+`qval')
		* Generate value q'*r/M
		gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q'*r/M
		gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank1 = reject_temp1*rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected1 = max(reject_rank1)

		* Second Stage
		* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
		local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
		* Generate value q_2st*r/M
		gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q_2st*r/M
		gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank2 = reject_temp2*rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected2 = max(reject_rank2)

		* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
		replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
		* Reduce q by 0.001 and repeat loop
		drop fdr_temp* reject_temp* reject_rank* total_rejected*
		local qval = `qval' - .001
	}
		
	quietly sort original_sorting_order
	
	tempfile fdr_pvalues
	save `fdr_pvalues'
	
}

*** B. Table
preserve 

clear
svmat sumTable

rename sumTable7 original_sorting_order
merge 1:1 original_sorting_order using `fdr_pvalues'
drop _merge rank pval 
replace sumTable6 = bky06_qval if sumTable6 == 10
replace sumTable6 = . if sumTable6 == 100

drop bky06_qval original_sorting_order 
 
* Assigning names to each row 
gen var = _n
tostring var, replace
local row = 1
replace var = "Days skipping meals (past week)" if var == "`row'"
local ++row
replace var = "Children skipping meals (=1)" if var == "`row'"
local ++row
replace var = "Regularly eat twice a day" if var == "`row'"
local ++row
replace var = "Total bread and tea meals (past week)" if var == "`row'"
local ++row
replace var = "Food Security - KLK Index" if var == "`row'"
local ++row
replace var = "Food Security - Anderson Index" if var == "`row'"
local ++row

replace var = "Inf. tax gov. off. (others)" if var == "`row'"
local ++row
replace var = "Inf. tax comm. leader (others)" if var == "`row'"
local ++row
replace var = "Inf. tax gov. off. (you)" if var == "`row'"
local ++row
replace var = "Inf. tax comm. leader (you)" if var == "`row'"
local ++row
replace var = "Informal tax. - KLK Index" if var == "`row'"
local ++row
replace var = "Informal tax. - Anderson Index" if var == "`row'"
local ++row

replace var = "Better economic situation" if var == "`row'"
local ++row
replace var = "Satisfied with fin. situation" if var == "`row'"
local ++row
replace var = "Happy" if var == "`row'"
local ++row
replace var = "Life satisfaction" if var == "`row'"
local ++row
replace var = "Economic/Wellbeing - KLK Index" if var == "`row'"
local ++row
replace var = "Economic/Wellbeing - Anderson Index" if var == "`row'"
local ++row

replace var = "Days eating rice (past week)" if var == "`row'"
local ++row
replace var = "Days eating beans (past week)" if var == "`row'"
local ++row
replace var = "Days eating vegetables (past week)" if var == "`row'"
local ++row
replace var = "Days eating chicken (past week)" if var == "`row'"
local ++row
replace var = "Days eating dairy (past week)" if var == "`row'"
local ++row

replace var = "Able to buy medicine" if var == "`row'"
local ++row
replace var = "Involved in fin. decisions" if var == "`row'"
local ++row
replace var = "Total household income (past month)" if var == "`row'"
local ++row
replace var = "Household's head employed (past month)" if var == "`row'"
local ++row

label var var ""

insobs 1, before(1)
replace var = "Panel A. Primary Outcomes" if var == ""

insobs 1, after(19)
replace var = "Panel B. Secondary Outcomes" if var == ""
insobs 1, after(19)

insobs 1, after(7)
insobs 1, after(14)

order var, first

* Fixing values
rename sumTable8 sumTable7
local value = 1
forvalues col = 1/7{
	rename sumTable`col' sumTable`col'_aux
	gen sumTable`col' = strofreal(sumTable`col'_aux)
	label var sumTable`col' "(`value')"
	replace sumTable`col' = "0" + sumTable`col' if regexm(sumTable`col', "^\.") == 1
	replace sumTable`col' = subinstr(sumTable`col', "-.", "-0.", 1) if regexm(sumTable`col', "^\-.") == 1
	drop sumTable`col'_aux
	replace sumTable`col' = "" if sumTable`col' == "0."
	local ++value
}

drop if regexm(var, "Anderson") == 1 
drop in 7/12

texsave using "${output}/table_1_temp.tex", ///
	title(Summary Table -- Treatment Effects) nofix align(lccccccc) ///
	headerlines("& Control & Control & Treatment & Standard & Naive & Adjusted & " "& Mean & SD & Effect & Error & \textit{p}-value & \textit{p}-value & N") ///
	hlines(-0) frag varlabels label(ITTsumTableAbridged) bold("Panel") italics("Index") ///
	footnote("\justifying \noindent \(Notes \): Stratification fixed effects, survey round fixed effects, and baseline values of dependent variables, if available, are included. Standard errors are clustered at the individual level. Primary outcomes show FWER-adjusted p-values within each family outcome (following Romano \& Wolf, 2005, using 5000 repetitions), while secondary outcomes show FDR-adjusted p-values (following Anderson, 2008). The KLK Index is created following Katz, Kling, \& Liebman (2007), and is the equally-weighted sum of the standardised component variables. Better economic situation is an index that equals 1 if the respondent answered that her economic situation compared to 30 days ago is slightly or much better, and 0 otherwise. Satisfied with financial situation is a dummy that equals 1 if the respondent answered that she agrees a lot or somewhat with the statement that she is highly satisfied with her current financial condition, and 0 otherwise. Happy is a dummy that equals 1 if respondent said that she was very happy or quite happy, and 0 otherwise. Life satisfaction is the score from 1 (dissatisfied) to 10 (satisfied) in terms of how satisfied the respondent is with her life as a whole these days. Total household income excludes the aid payments.", size(scriptsize)) replace
	
filefilter "${output}/table_1_temp.tex" "${output}/table_1.tex", ///
	from("\BSbegin{table}[tbp]") to("\BSbegin{table}[h!]") replace
filefilter "${output}/table_1.tex" "${output}/table_1_temp.tex", ///
	from("\BSbegin{tabularx}{\BSlinewidth}") to("\BSbegin{adjustbox}{max width=\BSlinewidth, max height=\BStextwidth}\BSbegin{tabular}") replace
filefilter "${output}/table_1_temp.tex" "${output}/table_1.tex", ///
	from("\BSend{tabularx}") to("\BSend{tabular} \BSend{adjustbox}") replace
erase "${output}/table_1_temp.tex"

restore

********************************************************************************
* 3. SUMMARY TABLE RESTRICTING TO UNIFORM SAMPLE
********************************************************************************

use "${data}/surveysDataset.dta", clear

drop if round == 3 | round == 4

* Generating strata variable 
egen strata = group(nahia_city_comb aboveMedianTotalBreadTea)
gen round_temp = round - 1

* Creating dummy that identifies individuals for which at least one variable is missing in one round to be excluded
gen restrictedSample = 1 if round != . 

foreach var in skippedMeals childSkippedMeals eatTwiceADay totalBreadTea infTax_gov_others infTax_leader_others infTax_gov_you infTax_leader_you econSituationImproved finSatisfied happy lifeSatisfaction_std ateRice ateBeans ateVegetables ateChicken ateDairy finDecisionMaker income employed{
	replace restrictedSample = 0 if `var' == .
	cap replace restrictedSample = 0 if `var'_base == .
}

* Which questions have missings?
foreach var in skippedMeals childSkippedMeals eatTwiceADay totalBreadTea infTax_gov_others infTax_leader_others infTax_gov_you infTax_leader_you econSituationImproved finSatisfied happy lifeSatisfaction_std ateRice ateBeans ateVegetables ateChicken ateDairy finDecisionMaker income employed{
	qui count if `var' == . & t != 0
	local obs = r(N)
	display("`var' `obs'")
}

* Keep only those observations for which we have answers to all questions in a given round
qui sum restrictedSample if round == 1
local total_round1 = r(N)
qui sum restrictedSample if round == 1 & restrictedSample == 0
local missing_round1 = r(N)
qui sum restrictedSample if round == 1 & restrictedSample == 1
local answered_round1 = r(N)

qui sum restrictedSample if round == 2
local total_round2 = r(N)
qui sum restrictedSample if round == 2 & restrictedSample == 0
local missing_round2 = r(N)
qui sum restrictedSample if round == 2 & restrictedSample == 1
local answered_round2 = r(N)

drop if restrictedSample == 0

mat sumTable = J(27, 8, .)

rename (infTax_leader_others infTax_leader_others_base econSituationImproved lifeSatisfaction lifeSatisfaction_base) (infTax_l_others infTax_l_others_base econSitBetter lifeSat lifeSat_base)

*** A. Estimation
local row = 1
foreach var in skippedMeals childSkippedMeals eatTwiceADay totalBreadTea nutrition_index nut_and_index infTax_gov_others infTax_l_others infTax_gov_you infTax_leader_you tax_index tax_and_index econSitBetter finSatisfied happy lifeSat econ_index econ_and_index ateRice ateBeans ateVegetables ateChicken ateDairy medicinePurchase finDecisionMaker income employed{
	
	*** 1. Control mean
	qui sum `var' if treatment == 0
	mat sumTable[`row', 1] = round(r(mean), 0.001)
	mat sumTable[`row', 2] = round(r(sd), 0.001)
	
	*** 2. Romano-Wolf p-value
	if "`var'" == "skippedMeals"{
		rwolf2 (reg skippedMeals treatment i.strata round_temp skippedMeals_base if t != 0, vce(cluster uniqueId)) ///
			(reg childSkippedMeals treatment i.strata round_temp if t != 0, vce(cluster uniqueId)) ///
			(reg eatTwiceADay treatment i.strata round_temp if t != 0, vce(cluster uniqueId)) ///
			(reg totalBreadTea treatment i.strata round_temp totalBreadTea_base if t != 0, vce(cluster uniqueId)) ///
			, seed(2110) reps(5000) strata(strata) usevalid indepvars(treatment, treatment, treatment, treatment)
			
		local p_skippedMeals = round(e(rw_skippedMeals_treatment), 0.0001)
		local p_childSkippedMeals = round(e(rw_childSkippedMeals_treatment), 0.0001)
		local p_eatTwiceADay = round(e(rw_eatTwiceADay_treatment), 0.0001)
		local p_totalBreadTea = round(e(rw_totalBreadTea_treatment), 0.0001)
		
	}
	else if "`var'" == "infTax_gov_others"{
		rwolf2 (reg infTax_gov_others treatment i.strata round_temp infTax_gov_others_base if t != 0, vce(cluster uniqueId)) ///
			(reg infTax_l_others treatment i.strata round_temp infTax_l_others_base if t != 0, vce(cluster uniqueId)) ///
			(reg infTax_gov_you treatment i.strata round_temp infTax_gov_you_base if t != 0, vce(cluster uniqueId)) ///
			(reg infTax_leader_you treatment i.strata round_temp infTax_leader_you_base if t != 0, vce(cluster uniqueId)) ///
			, seed(2110) reps(5000) strata(strata) usevalid indepvars(treatment, treatment, treatment, treatment)
			
		local p_infTax_gov_others = round(e(rw_infTax_gov_others_treatment), 0.0001)
		local p_infTax_l_others = round(e(rw_infTax_l_others_treatment), 0.0001)
		local p_infTax_gov_you = round(e(rw_infTax_gov_you_treatment), 0.0001)
		local p_infTax_leader_you = round(e(rw_infTax_leader_you_treatment), 0.0001)
		
	}
	else if "`var'" == "econSitBetter"{
		rwolf2 (reg econSitBetter treatment i.strata round_temp if t != 0, vce(cluster uniqueId)) ///
			(reg finSatisfied treatment i.strata round_temp if t != 0, vce(cluster uniqueId)) ///
			(reg happy treatment i.strata round_temp happy_base if t != 0, vce(cluster uniqueId)) ///
			(reg lifeSat treatment i.strata round_temp lifeSat_base if t != 0, vce(cluster uniqueId)) ///
			, seed(2110) reps(5000) strata(strata) usevalid indepvars(treatment, treatment, treatment, treatment)
			
		local p_econSitBetter = round(e(rw_econSitBetter_treatment), 0.0001)
		local p_finSatisfied = round(e(rw_finSatisfied_treatment), 0.0001)
		local p_happy = round(e(rw_happy_treatment), 0.0001)
		local p_lifeSat = round(e(rw_lifeSat_treatment), 0.0001)
		
	}
	
	*** 3. Treatment effects
	* Loop for variables with baseline information
	if inlist("`var'", "skippedMeals", "breadTea_lunch", "medicinePurchase", "happy", "lifeSat", "income", "employed", "finDecisionMaker") == 1 | regexm("`var'", "infTax") == 1{
		
		reghdfe `var' treatment round_temp `var'_base if t != 0, ///
		absorb(strata) vce(cluster uniqueId)
		
	}
	else{ // Variables without baseline value
		
		reghdfe `var' treatment round_temp if t != 0, /// 
			absorb(strata) vce(cluster uniqueId)
		
	}
	
	if inlist("`var'", "medicinePurchase", "finDecisionMaker", "income", "employed") == 1 | regexm("`var'", "^ate") == 1{
		local p_`var' = 10
	}
	else if regexm("`var'", "_index") == 1{
		local p_`var' = 100
	}
		
	mat sumTable[`row', 3] = round(_b[treatment], 0.001)
	mat sumTable[`row', 4] = round(_se[treatment], 0.001)
	mat sumTable[`row', 8] = e(N) // Number of observations
	test treatment = 0
	mat sumTable[`row', 5] = round(r(p), 0.001)
	mat sumTable[`row', 6] = `p_`var''
	mat sumTable[`row', 7] = `row'
	
	local ++row
	
}

*** 4. Adding FDR p-values to secondary outcomes
cap frame create fdr
frame fdr{
	
	* Keeping original p-values
	clear
	svmat sumTable
	keep if sumTable6 == 10
	keep sumTable5 sumTable7
	rename sumTable5 pval
	
	quietly sum pval
	local totalpvals = r(N)

	rename sumTable7 original_sorting_order
	quietly sort pval
	quietly gen int rank = _n if pval~=.
	
	local qval = 1

	* Generate the variable that will contain the BKY (2006) sharpened q-values
	gen bky06_qval = 1 if pval~=.

	* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.

	while `qval' > 0 {
		* First Stage
		* Generate the adjusted first stage q level we are testing: q' = q/1+q
		local qval_adj = `qval'/(1+`qval')
		* Generate value q'*r/M
		gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q'*r/M
		gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank1 = reject_temp1*rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected1 = max(reject_rank1)

		* Second Stage
		* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
		local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
		* Generate value q_2st*r/M
		gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q_2st*r/M
		gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank2 = reject_temp2*rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected2 = max(reject_rank2)

		* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
		replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
		* Reduce q by 0.001 and repeat loop
		drop fdr_temp* reject_temp* reject_rank* total_rejected*
		local qval = `qval' - .001
	}
		
	quietly sort original_sorting_order
	
	tempfile fdr_pvalues
	save `fdr_pvalues'
	
}


*** B. Table
preserve 

clear
svmat sumTable

rename sumTable7 original_sorting_order
merge 1:1 original_sorting_order using `fdr_pvalues'
drop _merge rank pval 
replace sumTable6 = bky06_qval if sumTable6 == 10
replace sumTable6 = . if sumTable6 == 100

drop bky06_qval original_sorting_order 
 
* Assigning names to each row 
gen var = _n
tostring var, replace
local row = 1
replace var = "Days skipping meals (past week)" if var == "`row'"
local ++row
replace var = "Children skipping meals (=1)" if var == "`row'"
local ++row
replace var = "Regularly eat twice a day" if var == "`row'"
local ++row
replace var = "Total bread and tea meals (past week)" if var == "`row'"
local ++row
replace var = "Food Security - KLK Index" if var == "`row'"
local ++row
replace var = "Food Security - Anderson Index" if var == "`row'"
local ++row

replace var = "Inf. tax gov. off. (others)" if var == "`row'"
local ++row
replace var = "Inf. tax comm. leader (others)" if var == "`row'"
local ++row
replace var = "Inf. tax gov. off. (you)" if var == "`row'"
local ++row
replace var = "Inf. tax comm. leader (you)" if var == "`row'"
local ++row
replace var = "Informal tax. - KLK Index" if var == "`row'"
local ++row
replace var = "Informal tax. - Anderson Index" if var == "`row'"
local ++row

replace var = "Better economic situation" if var == "`row'"
local ++row
replace var = "Satisfied with fin. situation" if var == "`row'"
local ++row
replace var = "Happy" if var == "`row'"
local ++row
replace var = "Life satisfaction" if var == "`row'"
local ++row
replace var = "Economic/Wellbeing - KLK Index" if var == "`row'"
local ++row
replace var = "Economic/Wellbeing - Anderson Index" if var == "`row'"
local ++row

replace var = "Days eating rice (past week)" if var == "`row'"
local ++row
replace var = "Days eating beans (past week)" if var == "`row'"
local ++row
replace var = "Days eating vegetables (past week)" if var == "`row'"
local ++row
replace var = "Days eating chicken (past week)" if var == "`row'"
local ++row
replace var = "Days eating dairy (past week)" if var == "`row'"
local ++row

replace var = "Able to buy medicine" if var == "`row'"
local ++row
replace var = "Involved in fin. decisions" if var == "`row'"
local ++row
replace var = "Total household income (past month)" if var == "`row'"
local ++row
replace var = "Household's head employed (past month)" if var == "`row'"
local ++row

label var var ""

insobs 1, before(1)
replace var = "Panel A. Primary Outcomes" if var == ""

insobs 1, after(19)
replace var = "Panel B. Secondary Outcomes" if var == ""
insobs 1, after(19)

insobs 1, after(7)
insobs 1, after(14)

order var, first

* Fixing values
rename sumTable8 sumTable7
local value = 1
forvalues col = 1/7{
	rename sumTable`col' sumTable`col'_aux
	gen sumTable`col' = strofreal(sumTable`col'_aux)
	label var sumTable`col' "(`value')"
	replace sumTable`col' = "0" + sumTable`col' if regexm(sumTable`col', "^\.") == 1
	replace sumTable`col' = subinstr(sumTable`col', "-.", "-0.", 1) if regexm(sumTable`col', "^\-.") == 1
	drop sumTable`col'_aux
	replace sumTable`col' = "" if sumTable`col' == "0."
	local ++value
}

texsave using "${output}/table_a5_temp.tex", ///
	title(Summary Table -- Treatment Effects, Restricted Sample) nofix align(lccccccc) ///
	headerlines("& Control & Control & Treatment & Standard & Naive & Adjusted &" "& Mean & SD & Effect & Error & \textit{p}-value & \textit{p}-value & N") ///
	hlines(-0) frag varlabels label(ITTsumTableRest) bold("Panel") italics("Index") ///
	footnote("\justifying \noindent \(Notes \): Control for stratification fixed effects, survey round fixed effects, and baseline value of dependent variable, if available. Standard errors clustered at individual level. Primary outcomes show FWER-adjusted p-values within each family outcome (following Romano \& Wolf, 2005, using 5000 repetitions), while secondary outcomes show FDR-adjusted p-values (following Anderson, 2008). The KLK Index is created following Katz, Kling, \& Liebman (2007), and is the equally-weighted sum of the standardised component variables. The Anderson Index is created following Anderson (2008), and weights the component variables by the inverse of their variance-covariance matrix. Better economic situation is an index that equals 1 if the respondent answered that her economic situation compared to 30 days ago is slightly or much better, and 0 otherwise. Satisfied with financial situation is a dummy that equals 1 if the respondent answered that she agrees a lot or somewhat with the statement that she is highly satisfied with her current financial condition, and 0 otherwise. Happy is a dummy that equals 1 if respondent said that she was very happy or quite happy, and 0 otherwise. Life satisfaction is the score from 1 (dissatisfied) to 10 (satisfied) in terms of how satisfied the respondent is with her life as a whole these days. Total household income excludes the aid payments. In Round 1, there are `missing_round1' women who did not respond to every single question (out of `total_round1' respondents), while in Round 2 there are `missing_round2' (out of `total_round2' respondents).", size(scriptsize)) replace
	
filefilter "${output}/table_a5_temp.tex" "${output}/table_a5.tex", ///
	from("\BSbegin{table}[tbp]") to("\BSbegin{table}[h!]") replace
filefilter "${output}/table_a5.tex" "${output}/table_a5_temp.tex", ///
	from("\BSbegin{tabularx}{\BSlinewidth}") to("\BSbegin{adjustbox}{max width=\BSlinewidth, max height=\BStextwidth}\BSbegin{tabular}") replace
filefilter "${output}/table_a5_temp.tex" "${output}/table_a5.tex", ///
	from("\BSend{tabularx}") to("\BSend{tabular} \BSend{adjustbox}") replace
erase "${output}/table_a5_temp.tex"
