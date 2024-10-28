/***************************************************************************

	DESCRIPTION: 	This do file generates descriptive statistics on the 
					number of merchants and participants by nahia 
					(district) based on transaction and baseline data.
	
	INPUTS:			- cleanedBaselineData.dta
					- cleanedTransactionData.dta
	
	OUTPUTS:		- table_a1

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
* 1. CREATING DESCRIPTIVES BASED ON TRANSACTION DATA
********************************************************************************
* Obtaining number of people per nahia 
use "${data}/cleanedBaselineData.dta", clear
gen n = 1
collapse (sum) n, by(nahia_city_comb)
rename n numParticipants
tempfile temp 
save `temp'

* Transaction data
use "${data}/cleanedTransactionData.dta", clear

merge m:1 uniqueId using "${data}/cleanedBaselineData.dta", keepusing(treatment nahia onboardingSession_city nahia_city_comb onboardingSession)
keep if _merge == 3
drop _merge

keep if day <= 23010 // Keep data until up to Dec 31. 

keep if transactiontype == "Outgoing" & beforeProg == 0 & testTrans != 2

* Those that we can't identify as test or other merchant (note that very few participants trade with each other)
gen unknownType = (testMerch != 1 & otherMerch != 1)

* Keeping only relevant variables 
drop transactiontype totalOverallTrans trans_number account_balance date_firstPayment days_firstOutgoing totalValueIncoming totalValueOutgoing totalValueOutgoingTest totalValueOutgoingOrigTest totalValueOutgoingOther totalValueOutgoingOtherMerch account_balanceSinceStart temp_transAmount totalTransOutgoingTest totalTransOutgoingOrigTest totalTransOutgoingOtherMerch_aux totalTransOutgoingOtherMerch totalTransOutgoingOther totalTransIncoming totalTransOutgoing totalTrans totalTransPost no_transactions totalTransOutgoingAirtime totalValueOutgoingAirtime incomingTransaction outgoingTransaction type_store beforeProg testTrans otherParticipant

* To avoid double counting, not include own first merchant in test merchant 
replace testMerch = 0 if first_testMerch == 1

gen knownMerch = (testMerch == 1 | otherMerch == 1 | first_testMerch == 1)

* Unique number of merchants of different types by nahia
foreach var in first_testMerch testMerch otherMerch unknownType knownMerch{
	
	unique toaccount if `var' == 1, by(nahia_city_comb) gen(un_`var'_temp)
	egen un_`var' = max(un_`var'_temp), by(nahia_city_comb)
	drop un_`var'_temp
	
}

preserve

collapse (firstnm) nahia onboardingSession_city un_first_testMerch un_testMerch un_otherMerch un_unknownType un_knownMerch, by(nahia_city_comb)

merge 1:1 nahia_city_comb using `temp'
drop _merge 

gsort -onboardingSession_city nahia
drop nahia_city_comb
order onboardingSession_city nahia numParticipants

drop un_testMerch un_otherMerch un_unknownType

* EXPORTING TABLE
local value = 1
foreach var in numParticipants un_first_testMerch un_knownMerch{
	label var `var' "(`value')"
	local ++value
}
label var onboardingSession_city ""
label var nahia ""

tostring nahia, replace 
replace nahia = "22" if nahia == "99"

texsave using "${output}/table_a1_temp.tex", ///
	title(Number of Merchants by Nahia) nofix align(lcccc) ///
	headerlines("&  & \# & \# Onboarding & \# All" "City & Nahia & Participants & Merchants & Merchants") ///
	hlines(-0) frag varlabels label(MerchByNahia)  ///
	footnote("\justifying \noindent \(Notes \): The number of own test merchants reflects the merchants that participants visited during the onboarding session to conduct their test purchase. The number of all merchants includes all numbers that we can identify as merchants in the transaction data.", size(scriptsize)) replace
	
filefilter "${output}/table_a1_temp.tex" "${output}/table_a1.tex", ///
	from("\BSbegin{table}[tbp]") to("\BSbegin{table}[h!]") replace
filefilter "${output}/table_a1.tex" "${output}/table_a1_temp.tex", ///
	from("\BSbegin{tabularx}{\BSlinewidth}") to("\BSbegin{adjustbox}{max width=\BSlinewidth, max height=\BStextwidth}\BSbegin{tabular}") replace
filefilter "${output}/table_a1_temp.tex" "${output}/table_a1.tex", ///
	from("\BSend{tabularx}") to("\BSend{tabular} \BSend{adjustbox}") replace
erase "${output}/table_a1_temp.tex"

restore
