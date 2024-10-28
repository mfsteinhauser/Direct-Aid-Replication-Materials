/***************************************************************************

	DESCRIPTION: 	This do file generates graphs to analyze mobile 
					wallet acount uage over time. Graphs show how the 
					participants spent their funds and extent of the 
					unused funds.
	
	INPUTS:			- cleanedBaselineData.dta
					- cleanedTransactionData.dta
					- surveysDataset.dta 
	
	OUTPUTS:		- figure_3_shareUnused.pdf
					- figure_3_onlyFlow.pdf

***************************************************************************/
 
********************************************************************************
* 0. PREAMBLE
********************************************************************************

clear all
clear matrix
clear mata
set more off
estimates clear
capture log close

********************************************************************************
* 1. TIME SERIES PLOT
********************************************************************************
* Getting response rates
use "${data}/surveysDataset.dta", clear
keep if treatment == 1
qui sum round if round == 1
local respRate_r1 = r(N)/1208
qui sum round if round == 2
local respRate_r2 = r(N)/1208

use "${data}/cleanedTransactionData.dta", clear

merge m:1 uniqueId using "${data}/cleanedBaselineData.dta"
keep if _merge == 3
drop _merge

keep if treatment == 1
keep if beforeProg == 0

drop if uniqueId == 0500 | uniqueId == 2570 | uniqueId == 1756 | uniqueId == 1348

* Calculating numbers for Tarek
* Number of outgoing transactions since 2nd/3rd payment in the treatment group 
egen out_2 = sum(outgoingTransaction) if day >= td(22nov2022), by(uniqueId)
tab out_2 if totalOverallTrans == trans_number

gen dummy = out_2 == 0

reg dummy kabul balkh herat if totalOverallTrans == trans_number, rob nocons

tab account_balanceSinceStart if totalOverallTrans == trans_number & out_2 == 0

egen out_3 = sum(outgoingTransaction) if day >= td(04dec2022), by(uniqueId)
tab out_3 if totalOverallTrans == trans_number

sum account_balanceSinceStart if out_2 == 0 & totalOverallTrans == trans_number
sum account_balanceSinceStart if out_3 == 0 & totalOverallTrans == trans_number
sum account_balanceSinceStart if out_3 == 0 & totalOverallTrans == trans_number & out_2 != 0

sum account_balanceSinceStart if out_3 != 0 & out_3 != . & totalOverallTrans == trans_number
tab account_balanceSinceStart if out_3 != 0 & out_3 != . & totalOverallTrans == trans_number

* Completing the panel, so that I have data for each day for each participant
fillin uniqueId trans_year trans_month trans_day
drop if trans_year == 2022 & trans_day == 31 & (trans_month == 9 | trans_month == 11)
replace trans_time = 0 if trans_time == .

* Identifying last transaction of the day, to identify end of day balance
egen lastTrans_aux = rank(trans_number), by(uniqueId trans_year trans_month trans_day)
replace lastTrans_aux = 1 if lastTrans_aux == . & _fillin == 1
egen lastTrans = max(lastTrans_aux), by(uniqueId trans_year trans_month trans_day)
replace lastTrans = 0 if lastTrans != lastTrans_aux
replace lastTrans = 1 if lastTrans != 0 & lastTrans != .
drop lastTrans_aux

gen valueTestMerch = transactionamount if testMerch == 1 & transactiontype == "Outgoing" & testTrans != 2
gen valueTestOrigMerch = transactionamount if first_testMerch == 1 & transactiontype == "Outgoing" & testTrans != 2
gen valueTestOther = transactionamount if testMerch == 0 & transactiontype == "Outgoing" & testTrans != 2

foreach var in valueTestMerch valueTestOrigMerch valueTestOther{
	replace `var' = 0 if `var' == . & _fillin == 1
}

* Filling in balance over time
replace account_balanceSinceStart = . if lastTrans != 1
replace account_balanceSinceStart = 0 if account_balanceSinceStart == . & _fillin == 1 & trans_year == 2022 & trans_month == 9 & trans_day == 1

sort uniqueId trans_year trans_month trans_day trans_time, stable
replace account_balanceSinceStart = account_balanceSinceStart[_n-1] if account_balanceSinceStart == . & uniqueId[_n] == uniqueId[_n-1]

tostring trans_year trans_month trans_day, replace
gen date_aux = trans_year + "/" + trans_month + "/" + trans_day
replace day = date(date_aux, "YMD") if _fillin == 1 & day == .

collapse (sum) account_balanceSinceStart valueTestMerch valueTestOrigMerch valueTestOther, by(day lastTrans)

preserve 

keep day lastTrans account_balanceSinceStart
keep if lastTrans == 1
drop lastTrans
tempfile temp 
save `temp'

restore 

collapse (sum) valueTestMerch valueTestOrigMerch valueTestOther, by(day)

merge 1:1 day using `temp'
drop _merge

replace valueTestOther = valueTestMerch + valueTestOther
replace valueTestOther = (-1) * valueTestOther
replace valueTestMerch = (-1) * valueTestMerch
replace valueTestOrigMerch = (-1) * valueTestOrigMerch

foreach var in account_balanceSinceStart valueTestMerch valueTestOrigMerch valueTestOther{
	replace `var' = `var' / 1000
} 

format day %td

keep if day >= td(06nov2022) & day < td(01jan2023)

display(td(05nov2022))

*** In two panels

* Share spent of money received
* Generating a variable that corresponds to the percent of money transferred that has been deposited
gen transferDay = 1 if day == 22955 | day == 22971 | day == 22983 | day == 22997
replace transferDay = 0 if transferDay == .
gen amountReceived = 4000 * 1208 if transferDay == 1
count if amountReceived == . & day < td(04dec2022) & transferDay != 1
replace amountReceived = 400 * `respRate_r1' * 1208 / r(N) if day < td(04dec2022) & transferDay != 1 & amountReceived == . // Average amount of incentive payments paid in a given day of round 1, based on the response rate
count if amountReceived == . & day >= td(04dec2022) & transferDay != 1
replace amountReceived = 400 * `respRate_r2' * 1208 / r(N) if day >= td(04dec2022) & transferDay != 1 & amountReceived == . // Average amount of incentive payments paid in a given day of round 2, based on the response rate

gen totalAmountPaid = amountReceived if _n == 1
replace totalAmountPaid = totalAmountPaid[_n-1] + amountReceived if totalAmountPaid == .
replace totalAmountPaid = totalAmountPaid / 1000

gen shareSpent = (account_balanceSinceStart / totalAmountPaid) * 100 

graph twoway area shareSpent day, lcolor(black) lwidth(*0.2) fcolor(black) ///
	, graphregion(color(white)) ytitle("Share of Money Transferred Unused (%)") ///
	ylabel(0(5)42 ,angle(0)) xline(22955, lcolor(red%70)) xline(22971, lcolor(red%70)) xline(22983, lcolor(red%70)) xline(22997, lcolor(red%70)) ///
	xlabel(22955 "November 6" 22971 "November 22" 22983 "December 4" 22997 "December 18") xtitle("")
graph export "${output}/figure_3_shareUnused.pdf", replace

* Flow of transactions
replace valueTestOther = valueTestOther * (-1)
replace valueTestMerch = valueTestMerch * (-1)
replace valueTestOrigMerch = valueTestOrigMerch * (-1)

graph twoway area valueTestOther day, lcolor(black) lwidth(*0.2) fcolor(black) || ///
    area valueTestMerch day, lcolor(black) lwidth(*0.2) fcolor(gs8) || ///
    area valueTestOrigMerch day, ///
    lcolor(black) lwidth(*0.2) graphregion(color(white)) fcolor(gs12) ///
    ytitle("Value (Thousands)") ///
    legend(label(3 "Original Merchant") label(2 "Other Merchant") label(1 "Others") order(3 2 1) rows(1) ///
    position(6)) ylabel(0(500)3400, angle(0)) ///
    xline(22955, lcolor(red%70)) xline(22971, lcolor(red%70)) xline(22983, lcolor(red%70)) xline(22997, lcolor(red%70)) ///
    xlabel(22955 "November 6" 22971 "November 22" 22983 "December 4" 22997 "December 18") xtitle("")


graph export "${output}/figure_3_onlyFlow.pdf", replace
