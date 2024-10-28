/***************************************************************************

	DESCRIPTION: 	This do file estimates experimenter demand effects, 
					and generates a summary table.
	
	INPUTS:			- surveysDataset.dta
	
	OUTPUTS:		- table_a4.tex

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

* Generating strata variable 
egen strata = group(nahia_city_comb aboveMedianTotalBreadTea)
egen strata_demand = group(nahia_city_comb aboveMedianTotalBreadTea enumerator_id)
gen round_temp = round - 1
gen treatPrimed = treatment * treatPrimeReceived

********************************************************************************
* 2. ESTIMATION
********************************************************************************
mat sumTable = J(24, 10, .)

rename (infTax_leader_others infTax_leader_others_base econSituationImproved lifeSatisfaction_std lifeSatisfaction_std_base treatPrimeReceived) (infTax_l_others infTax_l_others_base econSitBetter lifeSat lifeSat_base primed)

*** A. Romano-Wolf p-value
rwolf2 (reghdfe skippedMeals primed skippedMeals_base if round == 2, absorb(strata_demand) vce(cluster uniqueId)) /// 
	(reghdfe childSkippedMeals primed if round == 2, absorb(strata_demand) vce(cluster uniqueId)) ///
	(reghdfe eatTwiceADay primed if round == 2, absorb(strata_demand) vce(cluster uniqueId)) ///
	(reghdfe totalBreadTea primed totalBreadTea_base if round == 2, absorb(strata_demand) vce(cluster uniqueId)) ///
	, seed(55356284) reps(5000) strata(strata_demand) usevalid indepvars(primed, primed, primed, primed)
	
local p_skippedMeals = round(e(rw_skippedMeals_primed), 0.0001)
local p_childSkippedMeals = round(e(rw_childSkippedMeals_primed), 0.0001)
local p_eatTwiceADay = round(e(rw_eatTwiceADay_primed), 0.0001)
local p_totalBreadTea = round(e(rw_totalBreadTea_primed), 0.0001)

rwolf2 (reghdfe infTax_gov_others primed infTax_gov_others_base if round == 2, absorb(strata_demand) vce(cluster uniqueId)) ///
	(reghdfe infTax_l_others primed infTax_l_others_base if round == 2, absorb(strata_demand) vce(cluster uniqueId)) ///
	(reghdfe infTax_gov_you primed infTax_gov_you_base if round == 2, absorb(strata_demand) vce(cluster uniqueId)) ///
	(reghdfe infTax_leader_you primed infTax_leader_you_base if round == 2, absorb(strata_demand) vce(cluster uniqueId)) ///
	, seed(64197665) reps(5000) strata(strata_demand) usevalid indepvars(primed, primed, primed, primed)
	
local p_infTax_gov_others = round(e(rw_infTax_gov_others_primed), 0.0001)
local p_infTax_l_others = round(e(rw_infTax_l_others_primed), 0.0001)
local p_infTax_gov_you = round(e(rw_infTax_gov_you_primed), 0.0001)
local p_infTax_leader_you = round(e(rw_infTax_leader_you_primed), 0.0001)

rwolf2 (reghdfe econSitBetter primed if round == 2, absorb(strata_demand) vce(cluster uniqueId)) ///
	(reghdfe finSatisfied primed if round == 2, absorb(strata_demand) vce(cluster uniqueId)) ///
	(reghdfe happy primed happy_base if round == 2, absorb(strata_demand) vce(cluster uniqueId)) ///
	(reghdfe lifeSat primed lifeSat_base if round == 2, absorb(strata_demand) vce(cluster uniqueId)) ///
	, seed(46662015) reps(5000) strata(strata_demand) usevalid indepvars(primed, primed, primed, primed)
	
local p_econSitBetter = round(e(rw_econSitBetter_primed), 0.0001)
local p_finSatisfied = round(e(rw_finSatisfied_primed), 0.0001)
local p_happy = round(e(rw_happy_primed), 0.0001)
local p_lifeSat = round(e(rw_lifeSat_primed), 0.0001)

rename primed treatPrimeReceived

*** B. Estimation
local row = 1
foreach var in skippedMeals childSkippedMeals eatTwiceADay totalBreadTea nutrition_index infTax_gov_others infTax_l_others infTax_gov_you infTax_leader_you tax_index econSitBetter finSatisfied happy lifeSat econ_index ateRice ateBeans ateVegetables ateChicken ateDairy medicinePurchase finDecisionMaker income employed{
	
	*** A. Baseline Treatment effects
	* Loop for variables with baseline information
	if inlist("`var'", "skippedMeals", "breadTea_lunch", "medicinePurchase", "happy", "lifeSat", "income", "employed", "finDecisionMaker") == 1 | regexm("`var'", "infTax") == 1{
		
		qui sum `var' if treatment == 0 & t == 0
		mat sumTable[`row', 1] = round(r(mean), 0.001)
		
		reghdfe `var' treatment round_temp `var'_base if t != 0, ///
			absorb(strata) vce(cluster uniqueId)
		
	}
	else{ // Variables without baseline value
	
		qui sum `var' if treatment == 0
		mat sumTable[`row', 1] = round(r(mean), 0.001)
		
		reghdfe `var' treatment round_temp if t != 0, ///
			absorb(strata) vce(cluster uniqueId)
		
	}
		
	mat sumTable[`row', 2] = round(_b[treatment], 0.001)
	mat sumTable[`row', 3] = round(_se[treatment], 0.001)
	
	*** B. Primed Results
	
	if inlist("`var'", "skippedMeals", "breadTea_lunch", "medicinePurchase", "happy", "lifeSat", "income", "employed", "finDecisionMaker") == 1 | regexm("`var'", "infTax") == 1{
		
		reg `var' treatPrimeReceived i.strata_demand `var'_base if round == 2, vce(cluster uniqueId)
		
	}
	else{ // Variables without baseline value
		
		reg `var' treatPrimeReceived i.strata_demand if round == 2, vce(cluster uniqueId)
		
	}
	
	mat sumTable[`row', 4] = round(_b[treatPrimeReceived], 0.001)
	mat sumTable[`row', 5] = round(_se[treatPrimeReceived], 0.001)
	
	*** C. Interacted Primed Results
	
	if inlist("`var'", "skippedMeals", "breadTea_lunch", "medicinePurchase", "happy", "lifeSat", "income", "employed", "finDecisionMaker") == 1 | regexm("`var'", "infTax") == 1{
		
		reg `var' treatPrimed treatment treatPrimeReceived i.strata_demand `var'_base if round == 2, vce(cluster uniqueId)
		
	}
	else{ // Variables without baseline value
		
		reg `var' treatPrimed treatment treatPrimeReceived i.strata_demand if round == 2, vce(cluster uniqueId)
		
	}
	
	mat sumTable[`row', 6] = round(_b[treatPrimeReceived], 0.001)
	mat sumTable[`row', 7] = round(_se[treatPrimeReceived], 0.001)
	
	lincom treatPrimed + treatPrimeReceived
	
	mat sumTable[`row', 8] = round(r(estimate), 0.001)
	mat sumTable[`row', 9] = round(r(se), 0.001)
	
	if regexm("`var'", "(^ate|medicinePurchase|finDecisionMaker|income|employed|_index$)") != 1{
		mat sumTable[`row', 10] = `p_`var''
	}
	
	local ++row
	
}


********************************************************************************
* Table A4
********************************************************************************
preserve 

clear
svmat sumTable

* Drop panel B's rows
drop in 16/24

* Placing SEs underneath estimates 
forvalues row = 15(-1)1{
	
	insobs 1, after(`row')
	
	forvalues cols = 3(2)9{
		
		local previous_col = `cols' - 1
		replace sumTable`cols' = sumTable`cols'[_n-1] if sumTable`cols'[_n-1] != . & sumTable`cols' == .
		replace sumTable`previous_col' = sumTable`cols' if sumTable`cols' != . & sumTable`previous_col' == . 
		
	}
	
}

drop sumTable3 sumTable5 sumTable7 sumTable9

* Assigning names to each row 
gen var = _n
tostring var, replace
local row = 1
replace var = "Days skipping meals (past week)" if var == "`row'"
local ++row
local ++row
replace var = "Children skipping meals (=1)" if var == "`row'"
local ++row
local ++row
replace var = "Regularly eat twice a day" if var == "`row'"
local ++row
local ++row
replace var = "Total bread and tea meals (past week)" if var == "`row'"
local ++row
local ++row
replace var = "Food Security - KLK Index" if var == "`row'"
local ++row
local ++row

replace var = "Inf. tax gov. off. (others)" if var == "`row'"
local ++row
local ++row
replace var = "Inf. tax comm. leader (others)" if var == "`row'"
local ++row
local ++row
replace var = "Inf. tax gov. off. (you)" if var == "`row'"
local ++row
local ++row
replace var = "Inf. tax comm. leader (you)" if var == "`row'"
local ++row
local ++row
replace var = "Informal tax. - KLK Index" if var == "`row'"
local ++row
local ++row

replace var = "Better economic situation" if var == "`row'"
local ++row
local ++row
replace var = "Satisfied with fin. situation" if var == "`row'"
local ++row
local ++row
replace var = "Happy" if var == "`row'"
local ++row
local ++row
replace var = "Life satisfaction (std)" if var == "`row'"
local ++row
local ++row
replace var = "Economic/Wellbeing - KLK Index" if var == "`row'"
local ++row
local ++row

replace var = "" if regexm(var, "[aA-zZ]") != 1

label var var "Variable"

order var sumTable1 sumTable2 sumTable4 sumTable10 sumTable6 sumTable8, first

rename (sumTable1 sumTable2 sumTable4 sumTable10 sumTable6 sumTable8) (sumTable0 sumTable2 sumTable4 sumTable6 sumTable8 sumTable10)

* Adding panels 
insobs 1, before(1)
replace var = "Panel A. Food Security" in 1

insobs 1, before(12)
replace var = "Panel B. Informal Taxation" in 12
insobs 1, before(12)

insobs 1, before(24)
replace var = "Panel C. Wellbeing" in 24
insobs 1, before(24)

* Fixing values
local value = 1
forvalues col = 0(2)10{
	rename sumTable`col' sumTable`col'_aux
	gen sumTable`col' = strofreal(sumTable`col'_aux)
	label var sumTable`col' "(`value')"
	replace sumTable`col' = "0" + sumTable`col' if regexm(sumTable`col', "^\.") == 1
	replace sumTable`col' = subinstr(sumTable`col', "-.", "-0.", 1) if regexm(sumTable`col', "^\-.") == 1
	replace sumTable`col' = "(" + sumTable`col' + ")" if var == ""
	replace sumTable`col' = "" if sumTable`col' == "0."
	
	* Adding stars
	replace sumTable`col' = sumTable`col' + "*" if abs(sumTable`col'_aux[_n] / sumTable`col'_aux[_n+1]) >= 1.645 & var != "" & var[_n+1] == "" & sumTable`col'_aux[_n+1] != . & sumTable`col'_aux[_n] != 0
	replace sumTable`col' = sumTable`col' + "*" if abs(sumTable`col'_aux[_n] / sumTable`col'_aux[_n+1]) >= 1.96 & var != "" & var[_n+1] == "" & sumTable`col'_aux[_n+1] != . & sumTable`col'_aux[_n] != 0
	replace sumTable`col' = sumTable`col' + "*" if abs(sumTable`col'_aux[_n] / sumTable`col'_aux[_n+1]) >= 2.576 & var != "" & var[_n+1] == "" & sumTable`col'_aux[_n+1] != . & sumTable`col'_aux[_n] != 0
	
	replace sumTable`col' = "" if regexm(var[_n+1], "Panel.") == 1
	
	drop sumTable`col'_aux
	
	local ++value
}

replace sumTable0 = "" if var == ""
replace sumTable6 = "" if var == ""

texsave using "${output}/table_a4_temp.tex", ///
	title(Summary Table -- Experimenter Demand Effects) nofix align(lccccccccc) ///
	headerlines(" &  &  & \multicolumn{4}{c}{\textbf{Experimenter Demand}}" " & Control & Baseline & Overall & FWER & Control & Treatment" " & Mean & Estimate & Estimate & \(p\)-value & Estimate & Estimate") ///
	hlines(-0) frag varlabels label(ExpDemsumTableMain) italics("Index") bold("Panel") ///
	footnote("\justifying \noindent \(Notes \): Control for stratification fixed effects, survey round fixed effects (baseline estimates), and baseline value of dependent variable, if available. Standard errors clustered at individual level in parenthesis. Control mean is the mean in the baseline if available, or across follow up rounds otherwise, for the control group. The KLK Index is created following Katz, Kling, \& Liebman (2007), and is the equally-weighted sum of the standardised component variables. The baseline effect is the (pooled) ITT effect of the main treatment (receiving the aid payments). Primary outcomes show FWER-adjusted p-values within each family outcome (following Romano & Wolf, 2005, using 5000 repetitions). The overall effect is the coefficient on the prime treatment. The control effect is the coefficient on the prime treatment in a regression where the prime treatment and the main treatment are interacted, while the effect on the treated is the sum of the prime treatment and the interaction term between the two treatments from the same regression. Better economic situation is an index that equals 1 if the respondent answered that her economic situation compared to 30 days ago is slightly or much better, and 0 otherwise. Satisfied with financial situation is a dummy that equals 1 if the respondent answered that she agrees a lot or somewhat with the statement that she is highly satisfied with her current financial condition, and 0 otherwise. Happy is a dummy that equals 1 if respondent said that she was very happy or quite happy, and 0 otherwise. Life satisfaction is the score from 1 (dissatisfied) to 10 (satisfied) in terms of how satisfied the respondent is with her life as a whole these days (standardised). Total household income excludes the aid payments. \\   \textit{Levels of significance}: *$ p<0.1$ , **$ p<0.05$ , ***$ p<0.01$.", size(scriptsize)) replace
	
filefilter "${output}/table_a4_temp.tex" "${output}/table_a4.tex", ///
	from("\BSbegin{table}[tbp]") to("\BSbegin{table}[h!]") replace
filefilter "${output}/table_a4.tex" "${output}/table_a4_temp.tex", ///
	from("\BSbegin{tabularx}{\BSlinewidth}") to("\BSbegin{adjustbox}{max width=\BSlinewidth, max height=\BStextwidth}\BSbegin{tabular}") replace
filefilter "${output}/table_a4_temp.tex" "${output}/table_a4.tex", ///
	from("\BSend{tabularx}") to("\BSend{tabular} \BSend{adjustbox}") replace
erase "${output}/table_a4_temp.tex"

restore


