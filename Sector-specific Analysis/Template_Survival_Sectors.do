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
	"section": sectors string variables, we use ISIC4 codes
	
 MAKE SURE TO FILL IN INFORMATION ABOUT YOUR DATA UNTIL LINE 42 
 THEN RUN THE SCRIPT	
*/
clear
set more off

/******************************************************************************/
/*							 INITIALISATION   								  */
/******************************************************************************/

* PATHS 
global output " " // <- FILL IN

/* CALL ONE COUNTRY */
import delimited using " ", clear  // <- FILL IN

gen all_sectors = 1
gen high_risk = 0
gen medium_risk = 0
gen low_risk = 0

** CHOOSE IMPACT CATEGORIES FOR SECTORS
* Example below with ISIC4 codes
replace high_risk = 1 if section == "H" | section == "I" | section == "S" // <- FILL IN // Transport, Food and Accomodation, other service activities
replace medium_risk = 1 if section == "G" | section == "P" // <- FILL IN // Wholesale and Retail Trade
replace low_risk = 1 if section != "H" & section != "I" & section != "G" & section != "P" & section != "S" // <- FILL IN

/******************************************************************************/
/*							 VARIABLES  									  */
/******************************************************************************/

*** Cleaning and sampling 
keep if turnover > 0 & !missing(turnover)

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

* Industry FE 
egen ind = group(section)

* Set data to Panel for lagged variables 
xtset tax_id year

/******************************************************************************/
/*							 LOOP OVER SECTORS 								  */
/******************************************************************************/

local country = country
local sectors_name "All Sectors" "High Risk Sectors" "Medium Risk Sectors" "Low Risk Sectors"
local sectors all_sectors high_risk medium_risk low_risk
 
foreach sector of local sectors{
	preserve
	keep if `sector' == 1
/******************************************************************************/
/*							 REGRESSION  									  */
/******************************************************************************/

estimates clear

* CHOOSE 
local fe1 ib(freq).ind  // industry fe
local fe2 ib(freq).year // year fe

*
local depvar "L1.shock1030 L1.shock3050 L1.shock50p"
*
local spec groupvar() bdec(3) sdec(3) // Options
cap drop y_hat_1 y_hat_2
*
* REGRESSION TABLES
	* One lag
	eststo s1: reghdfe exit `depvar' L1.arcprofit L1.logturnover, absorb(`fe1' `fe2')
	#delimit;
	outreg2 using "$output\lm_exit_`sector'.xls", replace label keep(`depvar' L1.arcprofit L1.logturnover) ctitle("One lag") addtext(Sector FE, Yes, Year FE, Yes) `spec'
	title("Dependent variable is Exit (dummy)") ;
	#delimit cr 
	* Store and predict
		predict y_hat_1
		scalar b1_shock1030 = _b[L1.shock1030]
		scalar b1_shock3050 = _b[L1.shock3050]
		scalar b1_shock50p = _b[L1.shock50p]
		scalar se1_shock1030 = _se[L1.shock1030]
		scalar se1_shock3050 = _se[L1.shock3050]
		scalar se1_shock50p = _se[L1.shock50p]
	* One lag
	eststo s3: reghdfe exit `depvar'  L1.arcprofit L1.logturnover L1.grthprofit, absorb(`fe1' `fe2')
	outreg2 using "$output\lm_exit_`sector'.xls", append label keep(`depvar' L1.arcprofit L1.logturnover L1.grthprofit) ctitle("One lag") addtext(Sector FE, Yes, Year FE, Yes) `spec'
	* Store and predict
		predict y_hat_2
		scalar b2_shock1030 = _b[L1.shock1030]
		scalar b2_shock3050 = _b[L1.shock3050]
		scalar b2_shock50p = _b[L1.shock50p]
		scalar se2_shock1030 = _se[L1.shock1030]
		scalar se2_shock3050 = _se[L1.shock3050]
		scalar se2_shock50p = _se[L1.shock50p]

	
/******************************************************************************/
/*							 ESTIMATE & PREDICTION 		      				  */
/******************************************************************************/

* Estimates first model
gen proba1_shock1030 = cond(shock1030 == 1, y_hat_1, y_hat_1 + b1_shock1030)
gen proba1_shock3050 = cond(shock3050 == 1, y_hat_1, y_hat_1 + b1_shock3050)
gen proba1_shock50p = cond(shock50p == 1, y_hat_1, y_hat_1 + b1_shock50p)

gen se1_shock1030 = se1_shock1030 
gen se1_shock3050 = se1_shock3050
gen se1_shock50p = se1_shock50p

* Estimates second model
gen proba2_shock1030 = cond(shock1030 == 1, y_hat_2, y_hat_2 + b2_shock1030)
gen proba2_shock3050 = cond(shock3050 == 1, y_hat_2, y_hat_2 + b2_shock3050)
gen proba2_shock50p = cond(shock50p == 1, y_hat_2, y_hat_2 + b2_shock50p)

gen se2_shock1030 = se2_shock1030 
gen se2_shock3050 = se2_shock3050
gen se2_shock50p = se2_shock50p

keep if year == `beforelast'

collapse (mean) y_hat_1 y_hat_2 proba1_shock1030 proba1_shock3050 proba1_shock50p proba2_shock1030 proba2_shock3050 proba2_shock50p se1* se2*
gen sectors = "`sector'"

tempfile temp_`sector'
save `temp_`sector''

restore
}

use `temp_high_risk', clear
append using `temp_medium_risk' 
append using `temp_low_risk' 
append using `temp_all_sectors' 


export delimited using "$output\survival_1_sectors_`country'.csv", replace 

