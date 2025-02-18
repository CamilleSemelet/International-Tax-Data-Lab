
/* 
There is one major output: 
	* MEX_clean.txt - Legal entities with additional variables.

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 Do file notes:
 1. Since Mexico's raw data has no cost breakdown, variables in GROUP 1: COST 
 ITEMS are all missing values. 
 
 2. Unlike all the other datasets of the project, Mexico's raw data came from
 publicly available micro data. You will notice in many instances, items don't 
 add up as they should. Reasons: 
 i. Quality isn't so good. 
 ii. The Mexican 
 government intoduced  some noise to the variables to avoid identifying firms. 
 
 3. No firm characteristic is available for Mexico. 
*/

cd "C:\Users\s551964\Cross-Country\ETR\public_repository\OpenSourceData\"



clear all
set more off
* First and last period (Fiscal Year)
local first_period = 2010								
local last_period = 2015

*****************************************************************************
**************************** LEGAL ENTITIES *********************************
*****************************************************************************
program breakdown_variables
******** SELECT AND CREATE VARIABLES ********
* GROUP 0
	* Indentification variables
rename rfc_anon tax_ID
rename ejercicio year
generate country = "MEX"
generate entity = "Legal"	

	* Check for duplicates and drop
duplicates report year tax_ID // No duplicates

* GROUP 1: INCOMES
	* Turnover and other income 
gen turnover = it_aa 
gen other_income = .
	* Exempt income
gen exempt_income = .

	* Total income (turnover + other_income)
gen total_income = it_aa	
egen total_income_built = rowtotal(turnover other_income), missing
	* Taxable income (-exempt income)
gen taxable_income = total_income 

* GROUP 2: COST ITEMS
	* Cost of good solds
gen cogs = . 
	* Labor Inputs
gen labor_inp = . 
	* Material Inputs
gen material_inp = . 
	* Selling, admin and other operating costs
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
egen tot_cost_built = rowtotal(capital_inp operating_inp material_inp labor_inp financial_inp depreciation other_inp), missing
gen tot_cost = td_aa

	* Deductible Costs: total costs - non-deductible inputs 
gen deductible_cost = tot_cost - cond(missing(non_deduc_inp), 0, non_deduc_inp)   

* GROUP 3: PROFITS
	* gross profit
gen gross_profit = .

	* Net profit
gen net_profit = upaptu_c_aa
gen net_profit_built = total_income - cond(missing(tot_cost), 0, tot_cost)
*replace net_profit = net_profit_built if missing(net_profit)

	* Taxable profit
gen taxable_profit = ufe_c_aa // after all adjustment
replace taxable_profit = -1*pfe_c_aa if ufe_c_aa ==0
*gen taxable_profit_built = taxable_income - cond(missing(deductible_cost), 0, deductible_cost) // after all adjustment

* GROUP 4: GROSS & NET TAX BASE
	* Investment Incentives  (From tax base)
gen investment_taxbase = dafpe_aa
	* Capital allowances  (From tax base)
gen capital_allowance = .
	* Losses (always from tax base)
gen loss_carryforward = pfea_aa 
	* Other Deductions (always from tax base)
gen other_ded_taxbase = orisr_aa 
	* Add up credits & deductions from tax base: 
egen tot_deduc_taxbase = rowtotal(investment_taxbase capital_allowance other_ded_taxbase), missing 

	* Gross tax base (taxable_profit - tot_deduc_taxbase)
gen gross_tax_base = taxable_profit - cond(missing(tot_deduc_taxbase), 0, tot_deduc_taxbase)

	* Net tax base (gross_tax_base - loss_carryforward)
*gen net_tax_base_built = gross_tax_base - cond(missing(loss_carryforward), 0, loss_carryforward)
gen net_tax_base = rfis_c_aa


* GROUP 5: GROSS & NET TAX LIABILITY
	* Gross tax liability (net tax base*CIT rate)
gen gross_tax_liability = isreje_c_aa

	* Credits for foreign taxes paid
gen cred_foreign_tax = .	
	* Investment credits from tax liability
egen investment_taxliab = rowtotal(estec_aa escine_aa estea_aa ), missing // rowtotal(cfdim_aa rpm_aa estec_aa escine_aa estea_aa otroes_aa cfietu_aa ), missing
	* Special firms credits from tax liability
egen special_taxliab = rowtotal(rpm_aa cfdim_aa), missing
	* International Trade credits 
gen trade_taxliab = .
	* Other credit and allowances from tax liability
egen other_cred_taxliab = rowtotal(orisr_aa otroes_aa), missing
	* Add up credits & deductions from tax liability 
egen tot_deduc_taxliab = rowtotal(cred_foreign_tax special_taxliab trade_taxliab investment_taxliab other_cred_taxliab), missing
	* Add up credits & deductions that are not necessarily real credits from tax liability 
egen tot_noelse_taxliab = rowtotal(cred_foreign_tax  other_cred_taxliab), missing

	* Net tax liability (gross_tax_liability - tot_deduc_taxliab)
gen net_tax_liability = gross_tax_liability - cond(missing(tot_deduc_taxliab), 0, tot_deduc_taxliab)

	* Net tax liability without foreign and other credits  
gen ntl_noelse = net_tax_liability + cond(missing(tot_noelse_taxliab), 0, tot_noelse_taxliab)


* GROUP 6: WITHHOLDING. ADVANCED/PARTIAL PAYMENTS & OTHER CREDITABLE TAX PAYMENTS
	* Add up partial/advance payments
egen partial_advanced_pay = rowtotal(ppcon_aa ppef_aa), missing
	* Add withholding 
gen withholding = imrc_aa
	* Other creditable tax payments
egen other_cred_tax_pay = rowtotal(imape_aa imad_aa imccfc_aa imide_aa impsun_aa), missing

	* Tax to remit
gen tax_to_remit = isrcareje_c_aa

**** Treatment of missings and 0
foreach var of varlist gross_tax_base net_tax_base gross_tax_liability net_tax_liability tax_to_remit {
    replace `var' = 0 if missing(`var') & !missing(total_income)
}



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
label variable tot_deduc_taxbase "Total Credits & Deductions - Tax Base"
label variable net_tax_base "Net Tax Base"
label variable gross_tax_liability "Gross Tax Liability"
label variable net_tax_liability "Net Tax Liability"
label variable tot_deduc_taxliab "Total Credits & Deductions - Tax Liability"
label variable tax_to_remit "Tax to Remit"
label variable country "Country"
end

	/* 
	Loop: It uses the raw data (Anuales_ISR_PM_`i') and keeps only selected
	variables. 
	*/
	
forvalues i=`first_period'/`last_period'{
insheet using "Persona Moral/Anuales_ISR_PM_`i'.csv", clear
	* Program clean variables
breakdown_variables
	*Save data
tempfile MEX_MORAL_`i'
save `MEX_MORAL_`i''
}

	/* Loop: It creates one dataset for all "Legal" entities in Mexico. */
clear
forvalues i=`first_period'/`last_period'{
cap append using `MEX_MORAL_`i''
cap sort tax_ID year, stable
}

	* Adjustmenet
foreach var of varlist net_profit gross_tax_base net_tax_base gross_tax_liability net_tax_liability tax_to_remit {
    replace `var' = . if net_profit>turnover
}
	
	* Save
save "temporary.dta", replace


** ADD SECTOR, REGION
import delimited "Persona Moral/Catalogo_RFCs_PM.txt", clear 

rename rfc_anon tax_ID

gen section = ""
* Standardize sectors. we are using CIIU 4
replace section = "A" if sector == "AGRICULTURA, GANADERÍA, APROVECHAMIENTO FORESTAL, PESCA Y CAZA"
replace section = "B" if sector == "MINERÍA"
replace section = "C" if sector == "INDUSTRIAS MANUFACTURERAS"
replace section = "D" if sector == "ELECTRICIDAD, AGUA Y SUMINISTRO DE GAS POR DUCTOS AL CONSUMIDOR FINAL"
replace section =  "E" if sector == "SERVICIOS DE APOYO A LOS NEGOCIOS Y MANEJO DE DESECHOS Y SERVICIOS DE REMEDIACIÓN"
replace section = "F" if sector == "CONSTRUCCIÓN"
replace section = "G" if sector == "COMERCIO AL POR MENOR"
replace section = "G" if sector == "COMERCIO AL POR MAYOR"
replace section = "H" if sector == "TRANSPORTES, CORREOS Y ALMACENAMIENTO"
replace section = "I" if sector == "SERVICIOS DE ALOJAMIENTO TEMPORAL Y DE PREPARACIÓN DE ALIMENTOS Y BEBIDAS"
replace section =  "J" if sector == "INFORMACIÓN EN MEDIOS MASIVOS"
replace section = "K" if sector == "SERVICIOS FINANCIEROS Y DE SEGUROS"
replace section = "L" if sector == "SERVICIOS INMOBILIARIOS Y DE ALQUILER DE BIENES MUEBLES E INTANGIBLES"
replace section =  "M" if sector == "SERVICIOS PROFESIONALES, CIENTÍFICOS Y TÉCNICOS" 
replace section =  "M" if sector =="DIRECCIÓN DE CORPORATIVOS Y EMPRESAS"
replace section = "P" if sector == "SERVICIOS EDUCATIVOS"
replace section = "Q" if sector == "SERVICIOS DE SALUD Y DE ASISTENCIA SOCIAL"
replace section =  "R" if sector == "SERVICIOS DE ESPARCIMIENTO CULTURALES Y DEPORTIVOS, Y OTROS SERVICIOS RECREATIVOS"
replace section = "S" if sector == "OTROS SERVICIOS EXCEPTO ACTIVIDADES DEL GOBIERNO"
replace section = "U" if sector == "ACTIVIDADES DEL GOBIERNO Y DE ORGANISMOS INTERNACIONALES Y EXTRATERRITORIALES"

rename sector descr_section 

* 100% matched
merge 1:m tax_ID using "temporary.dta"
drop _merge 

gen capital_city= .
rename zona_geo region

* Incorporation is a range, so age of the firms can be approximitated with upper/lower bound 
gen min_age = .
replace min_age = 1 if rango_fecha == "1 a 5     "
replace min_age = 6 if rango_fecha == "6 a 10    "
replace min_age = 11 if rango_fecha == "11 a 20   "
replace min_age = 21 if rango_fecha == "21 a 25   " 
replace min_age = 25 if rango_fecha == "Mayor a 25"

gen firm_age = min_age
gen incorporation = year - min_age


************* KEEP VARIABLES WE NEED *************
	* Create locals to keep variables
local group0 year country tax_ID entity  section descr_section 
local group1 turnover exempt_income other_income total_income taxable_income 
local group2 tot_cost *_inp deductible_cost depreciation cogs
local group3 taxable_profit net_profit gross_profit
local group4 investment_taxbase capital_allowance loss_carryforward net_tax_base gross_tax_base tot_deduc_taxbase
local group5 gross_tax_liability net_tax_liability tot_deduc_taxliab cred_foreign_tax investment_taxliab other_cred_taxliab 
local group6 withholding partial_advanced_pay other_cred_tax_pay tax_to_remit
local group7 ntl_noelse tot_noelse_taxliab special_taxliab trade_taxliab
local group8 capital_city region incorporation firm_age 


	* Keep selected variables
keep  `group0' `group1' `group2' `group3' `group4' `group5' `group6' `group7' `group8'


	* Save
sort tax_ID year, stable
export delimited using "MEX_clean.csv", replace 
erase "temporary.dta"


