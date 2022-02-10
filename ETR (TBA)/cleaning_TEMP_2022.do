/* 
 
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * * * * * * * * * * *
*
* 	CIT DATA CLEANING - HARMONIZATION TEMPLATE
* 
*	CREATED: 		09/02/2022 
*
* 	LAST MODIFIED: 	
*
* 	NOTE: This do-file provides an harmonized breakdown of variables and concepts to clean administrative corporate income data.
*             This harmonization work is based on observations of CIT returns in more than 15 low- and middle-income countries.
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 Do file notes:
 1. Metavariables can be found directly in the data, or will have to be built. 
 The two variables in the template are given, if the variable exists, then still create the built variable for sanity check. Eventually we only keep the variable called "variable" and the "variable_built" will be dropped. 
 If there is only one "variable_built", you need to change the name to "variable".
 2. If there exists different regimes in for the CIT tax (e.g. lump sum tax, minimum tax, etc.), you might want to adapt the cleaning for those. 
 Our template mainly aims to clean general CIT regime returns.

 Example: 	
 1) There is a variable (exemple_var_netprofit) in the data set which correspond to our definition of net profit (total_income - tot_cost)
gen net_profit = exemple_var_netprofit
gen net_profit_built = total_income - tot_cost
 2) There is no variable that equals net profit, so we built it (either rename net_profit_built to net_profit; or set net_profit equal to the built variable):
gen net_profit_built = total_income - tot_cost
gen net_profit = net_profit_built


 There is one output file: 
	* XXX_withvars.txt 
	
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

cls
clear
capture restore
set more off
cap log close

* Path github
gl GitHub "xxxxxx"

* Path directory
gl directory "xxxxxx"

*****************************************************************************
**************************** LEGAL ENTITIES *********************************
*****************************************************************************
* Directory 
cd "$directory"

* Load data and append together
import excel using "datasheet.xlsx", firstrow clear

/* Load data from csv or txt - make sure the delimiters match your datasheet
import delimited using "datasheet.csv", clear delim (tab)
*/

* Remove special characters, if any
replace variable_with_spec = subinstr(variable_with_spec2, "|", "",.) 
replace variable_with_spec2= subinstr(variable_with_spec, "ñ", "n",.)

*Adjust first and last periods for firm entrance and exit
local first_period	= x_minimum
local last_period 	= x_maximum

******** SELECT AND CREATE VARIABLES ********
* GROUP 0
	* Identification variables
rename x year
rename y tax_ID
generate country = "TEMP"
generate entity = "Legal"	

	* Check for duplicates and drop
duplicates report year tax_ID // No duplicates

* GROUP 1: INCOMES
	* Turnover and other income 
gen turnover = . 
* egen turnover_built = rowtotal(sub_turnover1 sub_turnover2 sub_turnover3 sub_turnover4), missing // use when turnover variable is not reported and sub-totals of turnover are
gen other_income = .
	* Exempt income
gen exempt_income = .

	* Total income (turnover + other_income)
gen total_income = .	
egen total_income_built = rowtotal(turnover other_income), missing
	* Taxable income (-exempt income)
gen taxable_income = total_income - cond(missing(exempt_income), 0, exempt_income)

* GROUP 2: COST ITEMS
	* Labor Inputs
gen labor_inp = . 
	* Material Inputs
gen material_inp = . 
	*Selling, admin and other operating costs
gen operating_inp = .
	* Capital Inputs
gen capital_inp = .
	* Financial Inputs
gen financial_inp = .
	* Depreciation 
gen depreciation = .	
	* Other Inputs
gen other_inp = . 	
	* Non deductible Inputs
gen non_deduc_inp = . 

 	* Total costs: add up all costs & expenditure
egen tot_cost_built = rowtotal(capital_inp material_inp operating_inp labor_inp financial_inp depreciation other_inp), missing
gen tot_cost_dataset = .

	*** which one to choose from? tot_cost = max{tot_cost_built, tot_cost_dataset}, should be the same though
egen tot_cost = rowmax(tot_cost_built tot_cost_dataset)

	* Deductible Costs: total costs - non-deductible inputs 
gen deductible_cost = tot_cost - cond(missing(non_deduc_inp), 0, non_deduc_inp)   

* GROUP 3: PROFITS
	* Net profit
gen net_profit = .
	* replace net_profit = -1 * net_loss if net_profit == 0
gen net_profit_built = total_income - tot_cost

	* Taxable profit
gen taxable_profit = . // after all adjustment
gen taxable_profit_built = taxable_income - cond(missing(deductible_cost), 0, deductible_cost) // after all adjustment

* GROUP 4: GROSS & NET TAX BASE
	* Investment Incentives  (From tax base)
gen investment_taxbase = .
	* Capital allowances  (From tax base)
gen capital_allowance = .
	* Losses (always from tax base)
gen loss_carryforward = . 
	* Other Deductions (always from tax base)
gen other_ded_taxbase = . 
	* Add up credits & deductions from tax base: 
egen tot_deduc_taxbase = rowtotal(investment_taxbase capital_allowance other_ded_taxbase), missing 

	* Gross tax base (taxable_profit - tot_deduc_taxbase)
gen gross_tax_base = .
gen gross_tax_base_built = taxable_profit - cond(missing(tot_deduc_taxbase), 0, tot_deduc_taxbase)

	* Net tax base (gross_tax_base - loss_carryforward)
gen net_tax_base = .
	* replace net_tax_base = -1 * tax_loss if net_tax_base == 0
gen net_tax_base_built = gross_tax_base - cond(missing(loss_carryforward), 0, loss_carryforward)

* GROUP 5: GROSS & NET TAX LIABILITY
	* Gross tax liability (net tax base*CIT rate)
gen gross_tax_liability = .

	* Credits for foreign taxes paid
gen cred_foreign_tax = .	
	* Investment credits from tax liability
gen investment_taxliab = .
	* Other credit and allowances from tax liability
gen other_cred_taxliab =.
	* Add up credits & deductions from tax liability 
egen tot_deduc_taxliab = rowtotal(cred_foreign_tax investment_taxliab other_cred_taxliab), missing

	* Net tax liability (gross_tax_liability - tot_deduc_taxliab)
gen net_tax_liability = .
gen net_tax_liability_built = gross_tax_liability - cond(missing(tot_deduc_taxliab), 0, tot_deduc_taxliab)


* GROUP 6: WITHHOLDING. ADVANCED/PARTIAL PAYMENTS & OTHER CREDITABLE TAX PAYMENTS
	* Add up partial/advance payments
gen partial_advanced_pay = .
	* Add withholding 
gen withholding = .
	* Other creditable tax payments
gen other_cred_tax_pay = .

	* Tax to remit
gen tax_to_remit = .

**** Treatment of missings and 0
foreach var of varlist gross_tax_base net_tax_base gross_tax_liability net_tax_liability tax_to_remit {
    replace `var' = 0 if missing(`var') & !missing(total_income)
}

*********** ABSOLUTE TAX BASE & TAX LIABILITY  **********************
* reintegrate all deductions happening at the gross tax liability level to the tax base 

* Rate faced: 
gen rate = (gross_tax_liability/net_tax_base)
replace rate = 0 if net_tax_base == 0 
 
* Move tax liability deductions up before gross tax base level
gen abs_deduc_taxbase = tot_deduc_taxliab*(1/rate)
replace abs_deduc_taxbase = 0 if missing(abs_deduc_taxbase)
gen abs_gross_tax_base = gross_tax_base - abs_deduc_taxbase
gen abs_net_tax_base = net_tax_base - abs_deduc_taxbase
gen abs_tax_liability = abs_net_tax_base*rate // should equal net_tax_liability, check this

*********** NET OF LOSSES (NOL) TAX BASE & TAX LIABILITY  **********************
* reintegrate losses carry forward to have the theoretical tax liability for the period. There is no difference anymore between gross and net tax base then, so we construct a unique tax base variable

gen nol_tax_base = abs_net_tax_base + cond(missing(loss_carryforward), 0, loss_carryforward)
gen losses_tax_liab = loss_carryforward*rate
gen nol_tax_liability = abs_tax_liability + cond(missing(losses_tax_liab), 0, losses_tax_liab)

********* Add labels and drop some variables ********
label variable tax_ID "Tax ID"
label variable year "Fiscal Year"
label variable turnover "Turnover"
label variable total_income "Total income"
label variable taxable_income "Taxable income"
label variable tot_cost "Total Costs"
label variable deductible_cost "Deductible Costs"
label variable net_profit "Net profit"
label variable taxable_income "Taxable profit"
label variable gross_tax_base "Gross Tax Base"
label variable abs_gross_tax_base "Gross Tax Base after all Deductions (Tax base + Reintegrated Tax Liab)"
label variable tot_deduc_taxbase "Total Credits & Deductions - Tax Base"
label variable abs_deduc_taxbase "Total Credits & Deductions Reintegrated from Tax Liability"
label variable net_tax_base "Net Tax Base"
label variable abs_net_tax_base "Net Tax Base net of all deductions"
label variable nol_tax_base "Tax Base Net of Losses Carryfoward + all Reintegrated Deductions"
label variable nol_tax_liability "Tax Liability Net of Losses Carryfoward"
label variable gross_tax_liability "Gross Tax Liability"
label variable net_tax_liability "Net Tax Liability"
label variable tot_deduc_taxliab "Total Credits & Deductions - Tax Liability"
label variable tax_to_remit "Tax to Remit"
label variable country "Country"

**********************************************************************

	* Add sectors DEPENDS ON THE COUNTRY
* Standardized NACE rev 2 code
* example with division
gen section = ""
replace section = "A" if division > 0 & division <= 3  
replace section = "B" if division > 3 & division <= 9  
replace section = "C" if division > 9 & division <= 33  
replace section = "D" if division > 33 & division <= 35  
replace section = "E" if division > 35 & division <= 39  
replace section = "F" if division > 39 & division <= 43  
replace section = "G" if division > 43 & division <= 47  
replace section = "H" if division > 47 & division <= 53  
replace section = "I" if division > 53 & division <= 56  
replace section = "J" if division > 56 & division <= 63  
replace section = "K" if division > 63 & division <= 66
replace section = "L" if division > 66 & division <= 68
replace section = "M" if division > 68 & division <= 75
replace section = "N" if division > 75 & division <= 82
replace section = "O" if division > 82 & division <= 84
replace section = "P" if division > 84 & division <= 85
replace section = "Q" if division > 85 & division <= 88
replace section = "R" if division > 88 & division <= 93
replace section = "S" if division > 93 & division <= 96
replace section = "T" if division > 96 & division <= 98
replace section = "U" if division > 98 & division <= 99 

	* Add sectors DEPENDS ON THE COUNTRY
* Standardized ISIC rev 4 code
* example with sector_names
replace section = "A" if sector_names = "Agricultura, ganaderia, caza y silvicultura"
replace section = "B" if sector_names = "Explotaction de minas y canteras"
replace section = "C" if sector_names = "Industrias"
replace section = "D" if sector_names = "Suministro de electricidad, gas y agua"
replace section = "E" if sector_names = "Suministro de agua; alcantarillado, gestion de residuos y actividades de remediacion"
replace section = "F" if sector_names = "Construccion"
replace section = "G" if sector_names = "Comercio al por mayor y menor"
replace section = "H" if sector_names = "Transporte, almacenamiento y comunicaciones"
replace section = "I" if sector_names = "Hoteles y restaurantes"
replace section = "J" if sector_names = "Informacion y comunicacion"
replace section = "K" if sector_names = "Intermediacion financiera"
replace section = "L" if sector_names = "Actividades inmobiliarias, empresariales y de alquileres"
replace section = "M" if sector_names = "Actividades profesionales, cientificas y tecnicas"
replace section = "N" if sector_names = "Actividades administrativas y de servicios de apoyo"
replace section = "O" if sector_names = "Administracion publica  defensa; planes de seguridad social"
replace section = "P" if sector_names = "Ensenanza"
replace section = "Q" if sector_names = "Servicios sociales y de salud"
replace section = "R" if sector_names = "Artes, entretenimiento y recreacion"
replace section = "S" if sector_names = "Otras actividades no especificadas anteriormente"
replace section = "T" if sector_names = "Hogares privados con servicio domestico"
replace section = "U" if sector_names = "Organizaciones y organos extraterritoriales"

gen descr_section = section
replace descr_section = "AGRICULTURE, FORESTRY AND FISHING" if section == "A"
replace descr_section = "MINING AND QUARRYING" if section == "B"
replace descr_section = "MANUFACTURING" if section == "C"
replace descr_section = "ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY" if section == "D"
replace descr_section = "WATER SUPPLY; SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES" if section == "E"
replace descr_section = "CONSTRUCTION" if section == "F"
replace descr_section = "WHOLESALE AND RETAIL TRADE; REPAIR OF MOTOR VEHICLES AND MOTORCYCLES" if section == "G"
replace descr_section = "TRANSPORTATION AND STORAGE" if section == "H"
replace descr_section = "ACCOMODATION AND FOOD SERVICE ACTIVITIES" if section == "I"
replace descr_section = "INFORMATION AND COMMUNICATION" if section == "J"
replace descr_section = "FINANCIAL AND INSURANCE ACTIVITIES" if section == "K"
replace descr_section = "REAL ESTATE ACTIVITIES" if section == "L"
replace descr_section = "PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES" if section == "M"
replace descr_section = "ADMNINISTRATIVE AND SUPPORT SERVICE ACTIVITIES" if section == "N"
replace descr_section = "PUBLIC ADMINISTRATION AND DEFENCE; COMPULSORY SOCIAL SECURITY" if section == "O"
replace descr_section = "EDUCATION" if section == "P"
replace descr_section = "HUMAN HEALTH AND SOCIAL WORK ACTIVITIES" if section == "Q"
replace descr_section = "ARTS, ENTERTAINMENT AND RECREATION" if section == "R"
replace descr_section = "OTHER SERVICE ACTIVITIES" if section == "S"
replace descr_section = "ACTIVITY OF HOUSEHOLDS AS EMPLOYERS; ACTIVITIES OF HOUSEHOLDS FOR OWN USE" if section == "T"
replace descr_section = "ACTIVITY OF EXTRATERRITORIAL ORGANISATIONS AND BODIES" if section == "U"


************* KEEP VARIABLES WE NEED *************
	* Create locals to keep variables
local group0 year country tax_ID entity class section division descr_section
local group1 turnover exempt_income other_income total_income taxable_income 
local group2 tot_cost *_inp deductible_cost depreciation
local group3 taxable_profit net_profit
local group4 investment_taxbase capital_allowance loss_carryforward other_ded_taxbase net_tax_base gross_tax_base tot_deduc_taxbase
local group5 gross_tax_liability net_tax_liability tot_deduc_taxliab cred_foreign_tax investment_taxliab other_cred_taxliab  abs_* nol_* rate
local group6 withholding partial_advanced_pay other_cred_tax_pay tax_to_remit

	* Keep selected variables
keep  `group0' `group1' `group2' `group3' `group4' `group5' `group6'

	* Keep only legal entities and save
keep if entity == "Legal"


********* MERGE WITH OTHER DATASETS ********* 

* We'll use legal entities and can combine them with WDI and OECD Tax data to have 
* Can be merged with VAT returns or social security information
	
* Final adjustments
sort tax_ID year, stable

export delimited using "D:\Crosscountrytax\proc_data\template\XXX_withvars.csv", replace 



