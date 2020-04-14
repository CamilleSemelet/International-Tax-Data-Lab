* 4/13/2020
* Author: Camille Semelet, csemelet@worldbank.org 

/******************************************************************************/
/*					SURVIVAL MODEL TEMPLATE  								  */
/******************************************************************************/

/* We use a construct a simple linear probability model to predict the exit rate of firms following a revenue shock. This do-file runs first before the 

The list of the variables used for this do-file are the following:
	"year": years
	"tax_id": firm's unique id, identifiable across years
    "turnover": annual firm's revenue
    "gross_tax_base": annual firm's profits(losses), after deductions of costs from turnover
	"section": sectors string variables (fixed effects)

*/
/******************************************************************************/
/*							 INITIALISATION   								  */
/******************************************************************************/

clear
set more off
 
* Output
global output " "  // <- FILL IN

/* CALL ONE COUNTRY */

import delimited using " ", clear // <- FILL IN

/******************************************************************************/
/*							 PREPARATION 									  */
/******************************************************************************/

*** Cleaning and sampling 
keep if turnover > 0 | !missing(turnover)
sum year
local max = r(max)
local beforelast = `max'-1

* Last Year firm appears in the dataset
bys tax_id: egen lastyear = max(year)
gen dum_lastyear = 0
replace dum_lastyear = 1 if lastyear == year
replace dum_lastyear = . if year == `max'
// Last year of the panel isn't an exit year 

* Dummy exit (implies to create an additional row for first year firm doesn't appear)
expand 2 if dum_lastyear == 1, gen(exit) 
replace exit = . if dum_lastyear == .
replace year = year+1 if exit == 1

* Growth rate of turnover and profit
sort tax_id year
bys tax_id: gen grthturnover = ((turnover-(turnover[_n-1]))/turnover[_n-1])*100 if year == year[_n-1]+1
bys tax_id: gen grthprofit = ((gross_tax_base-(gross_tax_base[_n-1]))/gross_tax_base[_n-1])*100 if year == year[_n-1]+1

* Profit margin
gen profit_margin = gross_tax_base/turnover 
winsor profit_margin, gen(profitmargin) p(0.02)

* arcsinh(Profit or losses)
gen arc_profit = asinh(gross_tax_base)
winsor arc_profit, gen(arcprofit) p(0.02)

* log turnover
gen log_turnover = log(turnover)
winsor log_turnover, gen(logturnover) p(0.02)

* Dummies for revenue shocks
gen shock1030 = 0
replace shock1030 = 1 if grthturnover < -10 & grthturnover >= -30

gen shock3050 = 0
replace shock3050 = 1 if grthturnover < -30 & grthturnover >= -50

gen shock50p = 0
replace shock50p = 1 if grthturnover < -50

* Lagged term 1
foreach var in grthprofit profitmargin arcprofit logturnover shock1030 shock3050 shock50p{
bys tax_id: gen l1_`var' = `var'[_n-1] if year == year[_n-1]+1
}

* Industry FE 
encode section, gen(ind)

/******************************************************************************/
/*							 REGRESSION  									  */
/******************************************************************************/

estimates clear
local country = country
* CHOOSE 
local fe1 ib(freq).ind  // industry fe
local fe2 ib(freq).year // year fe
*
local depvar "l1_shock1030 l1_shock3050 l1_shock50p"
*
local spec groupvar() bdec(3) sdec(3) // Options
cap drop predicted1 predicted2

* REGRESSION TABLES

	* One lag
	eststo s1: reghdfe exit `depvar' l1_arcprofit l1_logturnover, absorb(`fe1' `fe2')
	#delimit;
	outreg2 using "$output\lm_exit_`country'.xls", replace label keep(`depvar' l1_arcprofit l1_logturnover) ctitle("One lag") addtext(Sector FE, Yes, Year FE, Yes) `spec'
	title("Dependent variable is Exit (dummy)") ;
	#delimit cr 
	* Store and predict
		predict y_hat_1
		scalar b1_shock1030 = _b[l1_shock1030]
		scalar b1_shock3050 = _b[l1_shock3050]
		scalar b1_shock50p = _b[l1_shock50p]
		scalar se1_shock1030 = _se[l1_shock1030]
		scalar se1_shock3050 = _se[l1_shock3050]
		scalar se1_shock50p = _se[l1_shock50p]

/******************************************************************************/
/*							 ESTIMATE & PREDICTION 		      				  */
/******************************************************************************/

gen b1_shock1030 = b1_shock1030 
gen b1_shock3050 = b1_shock3050
gen b1_shock50p = b1_shock50p

bys year: egen predicted_1 = mean(y_hat_1)

gen proba_shock1030 = predicted_1+b1_shock1030
gen proba_shock3050 = predicted_1+b1_shock3050
gen proba_shock50p = predicted_1+b1_shock50p

gen se_shock1030 = se1_shock1030 
gen se_shock3050 = se1_shock3050
gen se_shock50p = se1_shock50p

/* Keep second to last year results */
keep if year == `beforelast'

collapse (mean) predicted_1 proba_shock1030 proba_shock3050 proba_shock50p se_*
gen country = "`country'"

** Read the file in R or continue on stata to produce the graph.
export delimited using "$output\survival_estimates.csv", replace 

