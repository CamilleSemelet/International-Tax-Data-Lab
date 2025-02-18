* Effective Tax Rates and Firm Size
* Pierre Bachas, Anne Brockmeyer, Roel Dom and Camille Semelet
* Code last update: 04/02/2022


* DESCRIPTION:
* This files creates a set of cross country graphs on the relationship between effective tax rate and firm size
* using micro tax data from different countries
* The do is constructed to be ran on several countries at once, using globals  
* As such, there are functions or loops that can appear redundant if run on only one country.

* ORGANIZATION: 
* 1) Load data, create new variables
* 2) ETR and decile functions: build ETR variables depending on denominator choice and data types 
* 3) Subset functions: extract the variables we want for each subset 
* 4) Dataframes
* 5) Regressions
* 6) Extract meta-data (commented out here)
* 7) Graphs

* External file we merge: WDI data from the World Bank. We use GDP, GDP pc, CPI indices and exchange rates to convert from LCU to current USD.

** List of variables we have in the clean data:
*tax_ID        turnover      taxable_income  capital_inp   non_deduc_inp  taxable_profit  other_ded_taxbase  gross_tax_liability  tot_deduc_taxliab  other_cred_taxliab  descr_section
*year          other_income  labor_inp     financial_inp  tot_cost     investment_taxbase  tot_deduc_taxbase  cred_foreign_tax  net_tax_liab  tax_to_remit  
*country       exempt_income  material_inp  depreciation  deductible_cost  capital_allowance  gross_tax_base  investment_taxliab  partial_advanced_pay 
*total_income  operating_inp  other_inp     net_profit    loss_carryforward  net_tax_base  other_cred_taxliab  withholding   section  

******************************* 1. SET UP **********************************

* Paths & objects  we organized our outputs in many folders, you can have only one)

gl main_directory "xxxx"   
gl directory "xxxx" 
gl output "xxxx"
gl WDI_loc "xxxx" // Where the WDI data is located. 

gl country_name "Xxxx"  //  We use this in many parts of the code
gl country_code "XXX"  //  Country code is used to call the object 'files' (see below) & to save all graphs at once (end of script)

ssc install egenmore
************************ a. Directory and Functions  ********************************* 

*----------- Load data (we have different directories for different countries, here you can set it to proc_data only)
  cd $directory
  import delimited "XXXX.csv", clear case(preserve) // load cleaned data
  tempfile XXX_withvars
  save `XXX_withvars'

************************ b. Samples and new variables  ********************************* 

*----------- Merge World Development Index Data:
 cd $WDI_loc
  import delimited "WDI_vars.csv", clear case(preserve) // import cleaned WDI data
  gen log_GDP_pc = log(GDP_pc_currentUSD)
  tempfile WDI_loc
  save `WDI_loc'
  
* Get data to Convert Turnover to USD, and adjust for inflation
  keep if country == "USA"
  keep year cpi
  rename cpi cpi_usd
  sum cpi_usd if year==2019
  gen base_cpi = r(mean) 
  merge 1:m year using `WDI_loc'
  drop _merge
  tempfile WDI_loc
  save `WDI_loc'
   
  use `XXX_withvars', clear
  merge m:1 country year using `WDI_loc', keep (3)
  drop _merge
* Adjust and convert   
  gen turnover_usd = total_income/official_exchange
  gen index = cpi_usd/base_cpi
  gen turnover_usdadj = cond(total_income>0, turnover_usd/index, .)
  gen log_turn_usd = cond(total_income>0, log(turnover_usdadj), .)

*------------ Code STR 
gen STR = XXX // Need to aadapt here according to country's CIT, if there are size-dependant thresholds or if the rate changes accross years.
  
  * If sample restrictions are needed, make them there. For example, drop firms with a special regime
*keep if CIT_normal_regime == 1 

*----------- CREATE GROSS PROFIT CONCEPTS:
* Different cases: Material missing because its 0, or because it's unknown. 
* If cost structure available for material, consider NA is a 0.
           
    ** Replace missing to 0 to still calculate profits
gen mat = cond(material_inp!=., material_inp, 0)
gen fin = cond(financial_inp!=., financial_inp, 0)
gen lab = cond(labor_inp!=., labor_inp, 0)
gen op = cond(operating_inp!=., operating_inp, 0)
gen els = cond(other_inp!=., other_inp, 0)
gen cap = cond(other_inp!=., other_inp, 0)
gen dep = cond(depreciation!=., depreciation, 0)

    ** Create different profit variables (we use absoolute value because negative costs don't make sense. Could have been filled with a negative sign in front by mistake.)
        * Turnover - Material Costs
gen GP_m = total_income - abs(mat)
        * Turnover - Material Costs - Labor Costs (==Gross Profit)
gen GP_m_l = GP_m - abs(lab)
        * Turnover - Material Costs - Labor Costs - Operating costs (==Operating Profit)
gen GP_m_l_o = GP_m_l - abs(op)
        * Operating profit - capital costs
gen GP_m_l_o_c = GP_m_l_o - abs(cap) - abs(dep)
        * Operating profit - capital costs - financial costs
gen GP_m_l_o_c_f = GP_m_l_o_c - abs(fin)
        * Operating profit - capital costs - financial costs - other costs
gen NP = net_profit
                         
		** Create a var where losses are set to 0 (Used later in the code)
gen net_profit_pos = cond(net_profit <0, 0 , net_profit) 
                          
 /*       ** Dummy if variable exists in the country 
 * (sometimes one concept cannot be constructed if variable is missing, but will still appear as different than missing because we set those var to 0. Construct a dummy to store this information)
                          dum_GPm = cond(all(missing(c(temp$material_inp))) == "FALSE", 1, 0),
                          dum_GPml = cond(all(missing(c(temp$labor_inp))) == "FALSE" & dum_GPm==1, 1, 0),
                          dum_GPmlo = cond(all(missing(c(temp$operating_inp))) == "FALSE", 1, 0),
                          dum_GPmloc = cond(all(missing(c(temp$capital_inp))) == "FALSE" & dum_GPmlo==1, 1, 0),
                          dum_GPmlocf = cond(all(missing(c(temp$financial_inp))) == "FALSE" & dum_GPmloc==1, 1, 0))
  */

  ** Adapt sectors: create "Sector" (7 categories) and "Large Sector" (5 categories), based on section letters from ISIC classification
gen sector = "Other"
replace sector = cond(section=="C", "Manufacturing", sector)
replace sector = cond(section=="G", "Retail", sector)
replace sector = cond(section=="A" | section == "B" | section=="F" | ///
					  section=="D" | section=="E", "Primary/Secondary", sector)
replace sector = cond(section=="Q" |  section=="I" | section=="H" | ///
				      section=="D" |  section=="R" | section=="S" | /// 
					  section=="O" | section=="N", "Services", sector)
replace sector = cond(section=="J" | section == "M"| section == "P" , "Knowledge-Based", sector)
replace sector = cond(section=="K" | section == "L", "Financial & Estate", sector)
       
gen largesector = "Other"
replace largesector = cond(section=="A" | section == "B" , ///
					"Primary", largesector)
replace largesector = cond(section=="C" | section == "F" | section=="D" | ///
					 section=="E" , "Secondary", largesector)
replace largesector = cond(section=="G", "Retail", largesector)

replace largesector = cond(sector=="Services" | section=="K" | section == "J" | /// 
					 section == "M" | section == "P" | /// 
					 section == "L", "Services", largesector)


*----------- Sample restriction for all: 
* - keep only firms with turnover > 1. Some firms can report 0.1 of turnover. Limiting to 0 would still produce outlier ratios.
cd $output

* Panel version of the data.
keep if turnover > 1 & !missing(turnover)
save data, replace

* Keep last year available for each year. 
sum year
keep if year == r(max) & turnover > 1 & !missing(turnover)
save df_sample, replace

*----------- Descriptive statistics:
use df_sample, clear

gen pos_turn = cond(turnover > 0 & !missing(turnover), 1, 0)
gen pos_income = cond(total_income > 0 & !missing(total_income), 1, 0)
gen pos_taxliab = cond(net_tax_liability > 0 & !missing(net_tax_liability), 1, 0),
gen pos_netprofit = cond(net_profit > 0 & !missing(net_profit), 1, 0)
gen count = 1 
sum STR
local STR_min = r(min)    
local STR_max = r(max)    

collapse (sum)  count pos_taxliab pos_netprofit ///
         (mean) year turnover_usdadj log_GDP_pc GDP_pc_const2010 STR
gen STR_min = `STR_min'    
gen STR_max = `STR_max'    
gen pos_tax_liability = 100*pos_taxliab/count,
gen pos_net_profit = 100*pos_netprofit/count
drop pos_taxliab pos_netprofit
  
gen country = "$country_code"
gen country_name = "$country_name"
  
save "df.ETR.descr.dta" , replace

************************ 2. FUNCTION CONSTRUCT ********************************* 
program drop _all
*----------- Split distribution in ntile, deciles, quartile, etc.
* decile10 gives 1-10
* deciles gives 1-10 with 10 split in (10.0, 10.2, 10.4, 10.6, 10.8)
* percentile gives 1-99
* percentile_99_9 gives 1-99 with 99 split in 99.0, 99.1, 99.2, ..., 99.9

* use source_df, clear
use df_sample, clear

program decile_FUN

egen decile10 = xtile(total_income) , by(year) nq(10) 
egen percentile = xtile(total_income) , by(year) p(1(1)99) 
replace percentile = percentile-1
preserve
keep if decile10 >= 10 
egen top_ntile10 = xtile(total_income) , by(year) nq(10) 
egen top_ntile5 = xtile(total_income) , by(year) nq(5) 
keep tax_ID top_ntile10 top_ntile5 year
tempfile p99
save `p99'
restore

preserve
keep if percentile >= 99
egen perc_99 = xtile(total_income), by(year) nq(10) 
keep tax_ID perc_99 year
tempfile p99_9
save `p99_9'

restore
merge 1:1 tax_ID year using `p99' 
bys year: gen deciles = cond(!missing(top_ntile5), decile10+top_ntile5-1, decile10)
drop _merge
merge 1:1 tax_ID year using `p99_9' 
bys year: gen percentile_99_9 = cond(!missing(perc_99), percentile+(perc_99-1)/10, percentile)
drop _merge
end

*----------- Add the statutory rate STR, by deciles and by sectors
program STR_decilesFUN 
decile_FUN
replace STR= STR*100
collapse (mean) STR, by(country deciles)
end


* --Add STR, by percentile_99_9 
program STR_percFUN 
decile_FUN
replace STR= STR*100
collapse (mean) STR, by(country percentile_99_9)
end


* --Add STR, by sectors
* Need to define local industry 
*gl industry section

program STR_sectorsFUN 
replace STR= STR*100
collapse (mean) STR, by (country $industry) 
end


*----------- Construct the Effective Tax Rates for each firms, for different tax base (~denominator)
* Define gl denominator
program ETR_FUN
sum STR
 local max = r(max)*100 // We use max STR to cap the ETR
  
  * Keep only var we will use later
keep turnover total_income net_tax_base net_tax_liability country year   ///
log_GDP_pc tax_ID net_profit section loss_carryforward tot_deduc_taxbase  ///
tot_deduc_taxliab STR capital non_deduc_inp depreciation exempt_income /// 
log_turn_usd sector largesector tot_cost $denominator
decile_FUN // Add deciles, percentiles & etc.
gen NTL_pos = cond(net_tax_liability < 0, 0, net_tax_liability) // Tax liability is positive or zero (neg value set to 0)
gen ETR = 100*NTL_pos/$denominator
replace ETR = cond(missing($denominator), ., ETR) // ETR is missing if net profit is missing (and if tax liab is missing)
replace ETR = cond(missing(ETR), 0, ETR) // ETR is set to 0 if net profit == 0

gen ETR_winz = cond(ETR<0, 0, ETR)
replace ETR_winz = cond(ETR>`max', `max', ETR) // Winzorize ETR = [0, max(STR)]

   * Two samples: All firms vs Profitable firms
gen ETR_keep_neg = cond($denominator <=0, 0, ETR_winz) // All firms 
gen ETR_drop_neg = cond($denominator >0, ETR_winz, .) // Profitable firms (other ETRs are set to missing)
gen ETR_denominator = "$denominator"
end 

* Check that program works: 
use df_sample, clear
gl denominator NP // choose the denominator, here net profit
ETR_FUN

*----------- Re-construct the rate firms are facing at the tax base level (use GTL here instead of NTL)
program RATE_FUN 
 sum STR
 loc max = r(max)*100
 decile_FUN 
 gen GTL_pos = cond(gross_tax_liability < 0, 0, gross_tax_liability)
 gen ETR = 100*GTL_pos/net_tax_base
 gen ETR_winz = cond(ETR<0, 0, ETR)
 replace ETR_winz = cond(ETR>`max', `max', ETR) // Winzorize ETR = [0, max(STR)]
 gen ETR_drop_neg = cond(net_tax_base >0 & !missing(net_tax_base), ETR_winz, .)
 gen ETR_keep_neg = cond(net_tax_base<=0 & !missing(net_tax_base), 0, ETR_winz)
 gen ETR_denominator = "Check (GTL/NTB)" // Should fall back on the statutory rate
end


*----------- Use panel version to construct the ETR (i.e. ETR_i = sum(tax liab_i)/ sum(net_profit_i) across years)
* Define global denominator 
program ETR_panel_FUN 
  sum STR
  loc max = r(max)*100
  sum year
  loc max_year = r(max)
collapse (sum) $denominator net_tax_base net_tax_liability ///
         (mean) turnover total_income STR log_GDP_pc ///
         (count) year, by(tax_ID)
rename year n_year		 
gen year = `max_year'		 
decile_FUN

gen NTL_pos = cond(net_tax_liability < 0 & !missing(net_tax_liability), 0, net_tax_liability)
gen ETR = 100*NTL_pos/$denominator
replace ETR = cond(missing(ETR), 0, ETR)
 gen ETR_winz = cond(ETR<0, 0, ETR)
 replace ETR_winz = cond(ETR>`max', `max', ETR) // Winzorize ETR = [0, max(STR)]
 gen ETR_drop_neg = cond($denominator >0, ETR_winz, .)
 gen ETR_keep_neg = cond($denominator <=0, 0, ETR_winz)
gen ETR_denominator = "$denominator"
gen country = "$country_code"
end


************************ 3. FUNCTIONS SUBSETS ********************************* 

*----------- Subset dataframe for decile graph of ETR:
program ETR_decile_df_FUN 
  gen n_keep = 1
  gen n_drop = cond(ETR_drop_neg!=., 1, 0) 
  gen ETR_drop_med = ETR_drop_neg 
  gen ETR_keep_med = ETR_keep_neg
  collapse (sum) n_keep n_drop (mean) ETR_drop_neg ETR_keep_neg ///
		   (median) ETR_drop_med ETR_keep_med, ///
		   by(deciles country ETR_denominator log_GDP_pc)
end


*----------- Panel version
program ETR_decile_p_FUN 
  gen n_keep = 1
  gen n_drop = cond(ETR_drop_neg!=., 1, 0) 
  gen ETR_drop_med = ETR_drop_neg 
  gen ETR_keep_med = ETR_keep_neg
  collapse (sum) n_keep n_drop (mean) STR ETR_drop_neg ETR_keep_neg ///
		   (median) ETR_drop_med ETR_keep_med, ///
		   by(deciles country ETR_denominator)
end


*----------- Subset dataframe for percentiles graphs of ETR:
program ETR_perc_df_FUN
  gen n_keep = 1
  gen n_drop = cond(ETR_drop_neg!=., 1, 0) 
  gen ETR_drop_med = ETR_drop_neg 
  gen ETR_keep_med = ETR_keep_neg
  collapse (sum) n_keep n_drop (mean) ETR_drop_neg ETR_keep_neg ///
		   (median) ETR_drop_med ETR_keep_med, ///
		   by(percentile_99_9 country ETR_denominator log_GDP_pc)
end


*----------- Panel version
program ETR_perc_p_FUN
  gen n_keep = 1
  gen n_drop = cond(ETR_drop_neg!=., 1, 0) 
  gen ETR_drop_med = ETR_drop_neg 
  gen ETR_keep_med = ETR_keep_neg
  collapse (sum) n_keep n_drop (mean) ETR_drop_neg ETR_keep_neg STR ///
		   (median) ETR_drop_med ETR_keep_med, ///
		   by(percentile_99_9 country ETR_denominator)
end


*----------- Subset dataframe for logturnover graph of ETR
program ETR_log_df_FUN 
  gen n_keep = 1
  gen n_drop = cond(ETR_drop_neg!=., 1, 0)
  gen ETR_drop_med = ETR_drop_neg 
  gen ETR_keep_med = ETR_keep_neg
sum log_turn_usd
loc min : di %4.0f r(min) 
loc max = r(max)+2
egen bin = cut(log_turn_usd), at(`min'(2)`max')

  collapse (sum) n_keep n_drop (mean) ETR_drop_neg ETR_keep_neg  ///
		   (median) ETR_drop_med ETR_keep_med, ///
		   by(bin country ETR_denominator)
gen min_bin = `min'  
replace min_bin = cond(min_bin < 0, min_bin-1, min_bin) 
 
end


*----------- Subset dataframe for sector graph of ETR: 
* gl industry section
program ETR_sector_df_FUN
  gen n_keep = 1
  gen n_drop = cond(ETR_drop_neg!=., 1, 0) 
  gen ETR_drop_med = ETR_drop_neg 
  gen ETR_keep_med = ETR_keep_neg
  collapse (sum) n_keep n_drop (mean) ETR_drop_neg ETR_keep_neg ///
		   (median) ETR_drop_med ETR_keep_med, ///
		   by(log_GDP_pc country ETR_denominator $industry) 
end


*---------- Average Effective tax rate: Table
program ETR_avg_FUN 
  gen ETR_drop_wgt = ETR_drop_neg*turnover  
  gen ETR_keep_wgt = ETR_drop_neg*turnover
  collapse (sum) turnover ETR_keep_wgt ETR_drop_wgt ///
           (mean) ETR_drop_neg ETR_keep_neg, by (country ETR_denominator)
replace ETR_drop_wgt = ETR_drop_wgt/turnover 	
replace ETR_keep_wgt = ETR_keep_wgt/turnover 		   
drop turnover	   
rename ETR_drop_neg Drop
rename ETR_keep_neg Keep
rename ETR_drop_wgt Drop_weighted
rename ETR_keep_wgt Keep_weighted

end

************************ 4. DATAFRAMES ********************************* 

**** Extract percentile/decile
  use data, clear
  decile_FUN
  keep tax_ID year country top_ntile10 perc_99
  save quantiles_list, replace

****  Effective tax rate, by firm size (decile)
* ETR with different denominators
loc var turnover GP_m GP_m_l GP_m_l_o GP_m_l_o_c GP_m_l_o_c_f NP taxable_profit gross_tax_base net_tax_base
foreach l of local var {
	use df_sample, clear
	gl denominator `l'
	ETR_FUN
	ETR_decile_df_FUN
	tempfile df_`l'
	save `df_`l''
}
* Add Check function (GTL/NTB)
use df_sample, clear
RATE_FUN
ETR_decile_df_FUN

foreach l of local var {
append using `df_`l''
}

* Add STR
preserve 
use df_sample, clear
STR_decilesFUN
tempfile str
save `str'
restore	
merge m:1 country deciles using `str'
drop _merge

save "df.ETR.size.dta", replace
  
****  Effective tax rate, by firm size (percentile)
loc var turnover GP_m GP_m_l GP_m_l_o GP_m_l_o_c GP_m_l_o_c_f NP taxable_profit gross_tax_base net_tax_base
foreach l of local var {
	use df_sample, clear
	gl denominator `l'
	ETR_FUN
	ETR_perc_df_FUN
	tempfile df_`l'
	save `df_`l''
}
* Add Check function (GTL/NTB)
use df_sample, clear
RATE_FUN
ETR_perc_df_FUN

foreach l of local var {
append using `df_`l''
}

* Add STR
preserve 
use df_sample, clear
STR_percFUN
tempfile str
save `str'
restore	
merge m:1 country percentile_99_9 using `str'
drop _merge

save "df.ETR.size.p.dta", replace
  
**** PANEL

*-- ETR by firm size (decile): ETR = total(tax liab)/total(net profit) 
loc var  GP_m_l GP_m_l_o NP
foreach l of local var {
	use data, clear
	gl denominator `l'
	ETR_panel_FUN
	ETR_decile_p_FUN
	tempfile df_`l'
	save `df_`l''
}
clear
foreach l of local var {
append using `df_`l''
}
 save "df.ETR.panel.dta", replace

*-- ETR by firm size (percentile): ETR = total(tax liab)/total(net profit) 
loc var  GP_m_l GP_m_l_o NP
foreach l of local var {
	use data, clear
	gl denominator `l'
	ETR_panel_FUN
	ETR_perc_p_FUN
	tempfile df_`l'
	save `df_`l''
}
clear
foreach l of local var {
append using `df_`l''
}
 save "df.ETR.panel.perc.dta", replace

*-- ETR by firm size (decile): average of cross-sections
loc var  GP_m_l GP_m_l_o NP
foreach l of local var {
	use data, clear
	gl denominator `l'
	ETR_FUN
	ETR_decile_p_FUN
	tempfile df_`l'
	save `df_`l''
}

use data, clear
RATE_FUN
ETR_decile_p_FUN

foreach l of local var {
append using `df_`l''
}
 save "df.ETR.panel.cross.dta", replace


** Log scale, ETR
loc var turnover GP_m GP_m_l GP_m_l_o GP_m_l_o_c GP_m_l_o_c_f NP net_tax_base taxable_profit
foreach l of local var {
	use df_sample, clear
	gl denominator `l'
	ETR_FUN
	ETR_log_df_FUN
	tempfile df_`l'
	save `df_`l''
}
* Add Check function (GTL/NTB)
use df_sample, clear
RATE_FUN
ETR_log_df_FUN

foreach l of local var {
append using `df_`l''
}

save "df.ETR.logscale.dta", replace
  

****  Effective tax rate, by sector
program sector
loc var GP_m_l GP_m_l_o NP
foreach l of local var {
	use df_sample, clear
	gl denominator `l'
	ETR_FUN
	ETR_sector_df_FUN
	tempfile df_`l'
	save `df_`l''
}
* Add Check function (GTL/NTB)
use df_sample, clear
RATE_FUN
ETR_sector_df_FUN

foreach l of local var {
append using `df_`l''
}
* Add STR
preserve 
use df_sample, clear
STR_sectorsFUN
tempfile str
save `str'
restore	
merge m:1 country $industry using `str'
drop _merge

end

* Small sectors
gl industry sector
sector
save "df.ETR.sector.dta", replace
  
** Larger sectors
gl industry largesector
sector
save "df.ETR.largesector.dta", replace


*****Average Effective tax rate: Table
 use df_sample, clear
 gl denominator NP 
 ETR_FUN
 ETR_avg_FUN
 save "df.ETR.avg.table.dta", replace
 
 
**** Regressions samples
*-- Regression (dummies)
 use df_sample, clear
 gl denominator NP 
 ETR_FUN
 gen LCF = cond(!missing(loss_carryforward) & loss_carryforward!=0, 1, 0)
 gen Loss = cond(net_profit<0, 1, 0)
 gen exemptions = cond(!missing(tot_deduc_taxbase) & tot_deduc_taxbase!=0, 1, 0)
 gen tax_credits = cond(!missing(tot_deduc_taxliab) & tot_deduc_taxliab!=0, 1, 0)
 gen depr = cond(!missing(depreciation) & depreciation!=0, 1, 0)
 gen exempt_inc = cond(!missing(exempt_income) & exempt_income!=0, 1, 0)
 gen non_deductible = cond(!missing(non_deduc_inp) & non_deduc_inp!=0, 1, 0)
 gen reintegration = cond(exempt_inc == 1 | depr == 1, 1, 0)
 replace reintegration = cond(missing(reintegration), 0, reintegration)
 encode section, gen(int_section)
				   
 keep country log_GDP_pc net_profit section int_section ETR_denominator NTL_pos decile10 ETR_drop_neg tax_ID ///
           ETR_keep_neg ETR_drop_neg Loss LCF year exemptions tax_credits STR capital non_deductible depreciation ///
           exempt_income reintegration percentile_99_9 log_turn_usd  percentile
 save "df.ETR.reg.1.dta", replace

***** ETR and GDP relationship
 use df_sample, clear
 gl denominator NP 
 ETR_FUN
 keep if !missing(ETR_drop_neg)
 replace ETR = ETR_drop_neg/100 // unweighted
 replace ETR = cond(ETR<0, 0, ETR) // unweighted
 gen ETR_w = ETR*total_income
 gen STR_ratio = ETR/STR
 gen STR_caped = cond((ETR/STR)>1, 1, (ETR/STR))
 gen STR_w = cond(ETR!= 0 & STR!= 0, total_income*ETR/(STR), 0)
 gen STR_w_caped = cond(abs(ETR/STR)>1, total_income, STR_w)
            
 collapse (sum) total_income ETR_w STR_w STR_w_caped (mean) ETR STR_ratio STR_caped, by(country year log_GDP_pc)   
 
 rename total_income sum_total_income 
 replace ETR = ETR*100                
 replace ETR_w = 100*ETR_w/sum_total_income 
 gen STR_weighted = 100*STR_w/sum_total_income
 gen STR_weighted_caped = 100*STR_w_caped/sum_total_income
 gen STR_r = 100*STR_ratio
 replace STR_caped = 100*STR_caped

save "df.ETR.GDP.dta", replace 

*********************************** 5. REGRESSIONS  ********************************************

program regressions // Need explvar

 keep if !missing(ETR_keep_neg) & !missing($explvar) // data is at the country level
    
    local controls  "STR LCF exemptions reintegration tax_credits"
    local fe1 ib(freq).int_section  // industry fe
	local spec groupvar() bdec(3) sdec(3) // Options
    local base "ETR_keep_neg $explvar"
	
    * --base model 0
    eststo fit0: reghdfe `base', noabsorb
		* Store and predict
	outreg2 using "$country_code.reg.$reg_name.xlsx", replace label keep($explvar `controls') addtext(Sector FE, No) `spec' ///
	title("Dependent variable: ETR") 
		scalar b0_$explvar = _b["$explvar"]
		scalar se0_$explvar = _se["$explvar"]
		scalar b0_STR = .
		scalar se0_STR = .
 		scalar b0_LCF = .
		scalar se0_LCF = .
		scalar b0_exemptions = .
		scalar se0_exemptions = .
		scalar b0_reintegration = .
		scalar se0_reintegration = .
		scalar b0_tax_credits = .
		scalar se0_tax_credits = .
    * --base models      
     eststo fit1: reghdfe `base' if !missing(ETR_drop_neg), noabsorb
	 	* Store and predict
		outreg2 using "$country_code.reg.$reg_name.xlsx", label keep($explvar `controls') addtext(Sector FE, No) `spec' ///
	title("Dependent variable: ETR") 
		scalar b1_$explvar = _b["$explvar"]
		scalar se1_$explvar = _se["$explvar"]
		scalar b1_STR = .
		scalar se1_STR = .
 		scalar b1_LCF = .
		scalar se1_LCF = .
		scalar b1_exemptions = .
		scalar se1_exemptions = .
		scalar b1_reintegration = .
		scalar se1_reintegration = .
		scalar b1_tax_credits = .
		scalar se1_tax_credits = .
		
    * --Controls
     eststo fit2: reghdfe `base' if !missing(ETR_drop_neg), absorb(`fe1')
	 * Store and predict
	 	outreg2 using "$country_code.reg.$reg_name.xlsx", label keep($explvar `controls') addtext(Sector FE, Yes) `spec' ///
	title("Dependent variable: ETR") 
		scalar b2_$explvar = _b["$explvar"]
		scalar se2_$explvar = _se["$explvar"] 
		scalar b2_STR = .
		scalar se2_STR = .
 		scalar b2_LCF = .
		scalar se2_LCF = .
		scalar b2_exemptions = .
		scalar se2_exemptions = .
		scalar b2_reintegration = .
		scalar se2_reintegration = .
		scalar b2_tax_credits = .
		scalar se2_tax_credits = .
		
	 eststo fit3: reghdfe `base' STR if !missing(ETR_drop_neg), noabsorb
	 * Store and predict
	 	outreg2 using "$country_code.reg.$reg_name.xlsx", label keep($explvar `controls') addtext(Sector FE, No) `spec' ///
	title("Dependent variable: ETR") 
		scalar b3_$explvar = _b["$explvar"]
		scalar se3_$explvar = _se["$explvar"] 
		scalar b3_STR = _b[STR]
		scalar se3_STR = _se[STR] 
		scalar b3_LCF = .
		scalar se3_LCF = .
		scalar b3_exemptions = .
		scalar se3_exemptions = .
		scalar b3_reintegration = .
		scalar se3_reintegration = .
		scalar b3_tax_credits = .
		scalar se3_tax_credits = .
     eststo fit4: reghdfe `base' LCF if !missing(ETR_drop_neg), noabsorb
	 	 * Store and predict
		outreg2 using "$country_code.reg.$reg_name.xlsx", label keep($explvar `controls') addtext(Sector FE, No) `spec' ///
	title("Dependent variable: ETR") 
		scalar b4_$explvar = _b["$explvar"]
		scalar se4_$explvar = _se["$explvar"] 
		scalar b4_STR = .
		scalar se4_STR = .
		scalar b4_LCF = _b[LCF]
		scalar se4_LCF =  _se[LCF] 
		scalar b4_exemptions = .
		scalar se4_exemptions = .
		scalar b4_reintegration = .
		scalar se4_reintegration = .
		scalar b4_tax_credits = .
		scalar se4_tax_credits = .
		
     eststo fit5: reghdfe `base' exemptions if !missing(ETR_drop_neg), noabsorb
		* Store and predict
	 	outreg2 using "$country_code.reg.$reg_name.xlsx", label keep($explvar `controls') addtext(Sector FE, No) `spec' ///
	title("Dependent variable: ETR") 
		scalar b5_$explvar = _b["$explvar"]
		scalar se5_$explvar = _se["$explvar"] 
		scalar b5_LCF = .
		scalar se5_LCF = .
		scalar b5_STR = .
		scalar se5_STR = . 
		scalar b5_exemptions = _b[exemptions]
		scalar se5_exemptions = _se[exemptions]
		scalar b5_reintegration = .
		scalar se5_reintegration = .
		scalar b5_tax_credits = .
		scalar se5_tax_credits = .
     eststo fit6: reghdfe `base' reintegration if !missing(ETR_drop_neg), noabsorb
		* Store and predict
	 	outreg2 using "$country_code.reg.$reg_name.xlsx", label keep($explvar `controls') addtext(Sector FE, No) `spec' ///
	title("Dependent variable: ETR") 
		scalar b6_$explvar = _b["$explvar"]
		scalar se6_$explvar = _se["$explvar"] 
		scalar b6_LCF = .
		scalar se6_LCF = .
		scalar b6_STR = .
		scalar se6_STR = . 
		scalar b6_exemptions = .
		scalar se6_exemptions = .
		scalar b6_reintegration = _b[reintegration]
		scalar se6_reintegration = _se[reintegration]
		scalar b6_tax_credits = .
		scalar se6_tax_credits = .
     eststo fit7: reghdfe `base' tax_credits if !missing(ETR_drop_neg), noabsorb
		* Store and predict
	 	outreg2 using "$country_code.reg.$reg_name.xlsx", label keep($explvar `controls') addtext(Sector FE, No) `spec' ///
	title("Dependent variable: ETR") 
		scalar b7_$explvar = _b["$explvar"]
		scalar se7_$explvar = _se["$explvar"] 
		scalar b7_LCF = .
		scalar se7_LCF = .
		scalar b7_STR = .
		scalar se7_STR = . 
		scalar b7_exemptions = .
		scalar se7_exemptions = .
		scalar b7_reintegration = .
		scalar se7_reintegration = .
		scalar b7_tax_credits = _b[tax_credits]
		scalar se7_tax_credits = _se[tax_credits]
  
    * --All Controls
     eststo fit8: reghdfe `base' `controls' if !missing(ETR_drop_neg), absorb(`fe1')
		* Store and predict
	 	outreg2 using "$country_code.reg.$reg_name.xlsx", label keep($explvar `controls') addtext(Sector FE, Yes) `spec' ///
	title("Dependent variable: ETR") 
		scalar b8_$explvar = _b["$explvar"]
		scalar se8_$explvar = _se["$explvar"] 
        scalar b8_STR = _b[STR]
		scalar se8_STR = _se[STR]
 		scalar b8_LCF = _b[LCF]
		scalar se8_LCF = _se[LCF]
		scalar b8_exemptions = _b[exemptions]
		scalar se8_exemptions = _se[exemptions]
		scalar b8_reintegration = _b[reintegration]
		scalar se8_reintegration = _se[reintegration]
		scalar b8_tax_credits = _b[tax_credits]
		scalar se8_tax_credits = _se[tax_credits]
		

postfile test model str20 term estimate sderror using "df.ETR.fit.$reg_name.dta", replace
local estimates $explvar STR LCF exemptions reintegration tax_credits
forvalues i=0(1)8 {
	foreach e of local estimates{
	post test (`i') ("`e'") (b`i'_`e') (se`i'_`e')
		}
}
postclose test		
	
	use "df.ETR.fit.$reg_name.dta", clear
	drop if estimate == .
	gen statistic = estimate/sderror
	replace term = "percentile_99.9" if term == "percentile_99_9"
	gen country = "$country_code"
	export delimited using "df.ETR.fit.$reg_name.csv", replace
	erase "df.ETR.fit.$reg_name.dta"

end


* OLS, up to percentile 90:
use  "df.ETR.reg.1.dta", clear
keep if percentile_99_9 <=90
gl explvar "percentile_99_9"
gl reg_name "d9.perc"
regressions

* OLS, up to decile 9:
use  "df.ETR.reg.1.dta", clear
keep if decile10 <=9
gl explvar "decile10"
gl reg_name "d9.dec"
regressions

* OLS, top decile (dummy top99):
use  "df.ETR.reg.1.dta", clear
merge 1:1 tax_ID year country using quantiles_list.dta, keep(3)
keep if decile10 == 10 
gen top99 = cond(top_ntile10 == 10, 1, 0)
gl explvar "top99"
gl reg_name "d10.top99"
regressions


*********************************** 6. EXTRACT META-DATA  ********************************************
* * Section not useful for if working when having access to all data.  
* 
* *----------- EXTRACT METADATA if you want
* setwd(metadata)
* 
* * --List all graphs to append
* list.graph = list(df.ETR.descr, df.ETR.size, df.ETR.size.p, df.ETR.logscale, df.ETR.sector, 
*                    df.ETR.largesector, df.ETR.avg.table, df.ETR.panel, df.ETR.panel.cross, 
*                   df.perc.turnover)
* 
* list.name = c("ETR.descr", "ETR.size", "ETR.size.p", "ETR.logscale", "ETR.sector", 
*                "ETR.largesector", "ETR.2.avg.table", "ETR.panel", "ETR.panel.cross",
*                "perc.turnover")
* 
* 
* for (i in 1:length(list.graph)){
*   saveRDS(list.graph[[i]], file = paste0(pre, "df.", list.name[[i]], ".RDS", sep = "" ))
*   print(list.name[[i]])
* }

*********************************** 7. EXTRACT GRAPHS & OUTPUTS ********************************************

**** a. Last dataframe adjustments ****

*-----------  Rename function
program renameFUN 

replace ETR_denominator = cond(ETR_denominator == "turnover", "Turnover", ETR_denominator) 
replace ETR_denominator = cond(ETR_denominator == "GP_m", "T-M", ETR_denominator) 
replace ETR_denominator = cond(ETR_denominator == "GP_m_l", "Gross Profit", ETR_denominator) 
replace ETR_denominator = cond(ETR_denominator == "GP_m_l_o", "Operating Profit", ETR_denominator) 
replace ETR_denominator = cond(ETR_denominator == "GP_m_l_o_c", "T-M-L-O-C", ETR_denominator) 
replace ETR_denominator = cond(ETR_denominator == "GP_m_l_o_c_f", "T-M-L-O-C-F", ETR_denominator) 
replace ETR_denominator = cond(ETR_denominator == "NP", "Net Profit", ETR_denominator) 
replace ETR_denominator = cond(ETR_denominator == "taxable_profit", "Taxable profit", ETR_denominator) 
replace ETR_denominator = cond(ETR_denominator == "gross_tax_base", "GTB", ETR_denominator) 
replace ETR_denominator = cond(ETR_denominator == "net_tax_base", "NTB", ETR_denominator) 
replace ETR_denominator = cond(ETR_denominator == "Check (GTL/NTB)", "Check", ETR_denominator) 
 
                     
label var ETR_drop_neg "Profitable firms"
label var ETR_keep_neg "All firms"
end

*----------- Percentile function
program percentileFUN 
gen str4 p_chr = string(percentile_99_9, "%9.0gc") 
gen p = percentile_99_9
replace p = cond(p_chr=="99", 101, p)
replace p = cond(p_chr=="99.1", 103, p)
replace p = cond(p_chr=="99.2", 105, p)
replace p = cond(p_chr=="99.3", 107, p)
replace p = cond(p_chr=="99.4", 109, p)
replace p = cond(p_chr=="99.5", 111, p)
replace p = cond(p_chr=="99.6", 114, p)
replace p = cond(p_chr=="99.7", 117, p)
replace p = cond(p_chr=="99.8", 119, p) 
replace p = cond(p_chr=="99.9", 121, p) 
end



*----------- Define y_max by country: Need to do it before we merge all countries becuse relies on country_list
use "df.ETR.size.p.dta", clear
keep if ETR_denominator=="NP"
collapse (max) ETR_drop_neg, by(country) 
qui sum ETR_drop_neg
local y_limit_perc = r(mean)

use "df.ETR.panel.cross.dta", clear
keep if ETR_denominator=="NP"
collapse (max) ETR_drop_neg, by(country) 
qui sum ETR_drop_neg
local y_limit_panelc = r(mean)

gl y_limit = max(`y_limit_perc', `y_limit_panelc')


******************* b. ETR by firm-size: panel with multiple denominators ******************* 
grstyle init
grstyle set plain, horizontal 

* Keep only Turnover, Gross profit, Operating Profit, Net profit

*-----------  Decile
program ETR_dec_GRAPH
keep if country == "$country_code"
gen max = max(ETR_drop_neg, ETR_keep_neg)
gen min = min(ETR_drop_neg, ETR_keep_neg)
egen y_max = max(max)
replace y_max = round(y_max)+1
sum min
loc min : di %4.0f r(min) 
loc y_min = `min'-1
local main  ETR_keep_neg  ETR_drop_neg deciles, color("ebblue*.75" "red*.75")  xlabel(1 2 3 4 5 6 7 8 9 10 "10" 11 ".2" 12 ".4" 13 ".6" 14 ".8", nogrid) ylabel(, nogrid) lpattern(solid dash)  
local greyshading y_max deciles if inrange(deciles, 10, 14),  bartype(spanning) bcolor(gs14) base(`y_min') 
#delimit;
twoway  bar `greyshading' || connected `main' 
plotregion(margin(zero)) 
legend(order (2 "All firms" 3 "Profitable firms") region(lstyle(none)) rows(1) nobox pos(12)) 
xtitle("")
ytitle("$ETR_denominator") 
saving("$ETR_denominator", replace); 
#delimit cr
end


* Extract graph
use "df.ETR.size.dta", clear
renameFUN

levelsof ETR_denominator, local(levels)
di `levels'
foreach l of local levels {
use "df.ETR.size.dta", clear
renameFUN
gl ETR_denominator "`l'"
keep if ETR_denominator == "`l'"
ETR_dec_GRAPH
}
gr combine "Turnover.gph" "Gross Profit.gph" "Operating Profit.gph" "Net Profit.gph"
graph export "ETRavg_dec_$country_code.png", replace



*-----------  Percentile
program ETR_perc_GRAPH
keep if country == "$country_code"
gen max = max(ETR_drop_neg, ETR_keep_neg)
gen min = min(ETR_drop_neg, ETR_keep_neg)
egen y_max = max(max)
replace y_max = round(y_max)+1
sum min
loc min : di %4.0f r(min) 
loc y_min = `min'-1
local main1 ETR_keep_neg p, color("ebblue*.75")  deg(6) xlabel(10 20 30 40 50 60 70 80 90 101 "99.0" 111 "99.5" 121 "99.9", nogrid) ylabel(, nogrid) lpattern(solid)  
local main2 ETR_drop_neg p, color( "red*.75")  deg(6) lpattern(dash)
local greyshading y_max p if inrange(p, 101, 121), bcolor(gs14) bartype(spanning) base(`y_min') 
#delimit;
twoway  bar `greyshading' || lpoly `main1' || lpoly `main2' 
plotregion(margin(zero)) 
legend(order (2 "All firms" 3 "Profitable firms") region(lstyle(none)) rows(1) nobox pos(12)) 
xtitle("")
ytitle("$ETR_denominator") 
saving("$ETR_denominator", replace); 
#delimit cr
end


* Extract graph
use "df.ETR.size.p.dta", clear
renameFUN

levelsof ETR_denominator, local(levels)
di `levels'
foreach l of local levels {
use "df.ETR.size.p.dta", clear
renameFUN
percentileFUN
gl ETR_denominator "`l'"
keep if ETR_denominator == "`l'"
ETR_perc_GRAPH
}
gr combine "Turnover.gph" "Gross Profit.gph" "Operating Profit.gph" "Net Profit.gph"
graph export "ETRavg_perc_$country_code.png", replace


******************* c. ETR by firm-size, NET PROFIT (deciles) ******************* 
program ETR_firmsize_GRAPH
renameFUN
keep if country == "$country_code"
keep if ETR_denominator == "$ETR_denominator"
gen max = max(ETR_drop_neg, ETR_keep_neg)
gen min = min(ETR_drop_neg, ETR_keep_neg)
egen y_max = max(max)
replace y_max = round(y_max)+1
sum min
loc min : di %4.0f r(min) 
loc y_min = `min'-1
local main  ETR_keep_neg  ETR_drop_neg deciles, xlabel(1 2 3 4 5 6 7 8 9 10 "10" 11 ".2" 12 ".4" 13 ".6" 14 ".8", nogrid) ylabel(, nogrid) lpattern(solid dash) color("ebblue*.75" "red*.75") 
local greyshading y_max deciles if inrange(deciles, 10, 14),  bartype(spanning) bcolor(gs14) base(`y_min') 
#delimit;
twoway  bar `greyshading' || connected `main' 
plotregion(margin(zero)) 
legend(order (2 "All firms" 3 "Profitable firms") region(lstyle(none)) rows(1) nobox pos(12)) 
xtitle("Firm Size Distribution (Turnover)")
ytitle("Effective Tax Rate (%)") ;
#delimit cr
end


* Extract graph
use "df.ETR.size.dta", clear
gl ETR_denominator "Net Profit"
ETR_firmsize_GRAPH
graph export "ETRavg_dec_NP_$country_code.png", replace


********************* d. Panel (decile) ***********************
*----------- ETR created as average tax liab and tax base across all years
* Extract graph
use "df.ETR.panel.dta", clear
gl ETR_denominator "Net Profit"
ETR_firmsize_GRAPH
graph export "ETRavg_dec_panelNP_$country_code.png", replace

*-----------  yearly ETR, average across all years  
* Extract graph
use "df.ETR.panel.cross.dta", clear
gl ETR_denominator "Net Profit"
ETR_firmsize_GRAPH
graph export "ETRavg_dec_panelcrossNP_$country_code.png", replace

******************* e. ETR by firm-size, NET PROFIT (percentile) ******************* 
program ETR_size_p_GRAPH
percentileFUN
renameFUN
keep if country == "$country_code"
keep if ETR_denominator == "$ETR_denominator"
gen max = max(ETR_drop_neg, ETR_keep_neg)
gen min = min(ETR_drop_neg, ETR_keep_neg)
egen y_max = max(max)
replace y_max = round(y_max)+1
sum min
loc min : di %4.0f r(min) 
loc y_min = `min'-1
local main1 ETR_keep_neg p, color("ebblue*.75") deg(6) xlabel(10 20 30 40 50 60 70 80 90 101 "99.0" 111 "99.5" 121 "99.9", nogrid) ylabel(, nogrid) lpattern(solid)  
local main2 ETR_drop_neg p, color("red*.75") deg(6) lpattern(dash)
local greyshading y_max p if inrange(p, 101, 121), bcolor(gs14) bartype(spanning) base(`y_min') 
#delimit;
twoway  bar `greyshading' || lpoly `main1' || lpoly `main2' 
plotregion(margin(zero)) 
legend(order (2 "All firms" 3 "Profitable firms") region(lstyle(none)) rows(1) nobox pos(12)) 
xtitle("Firm Size Distribution (Turnover)")
ytitle("Effective Tax Rate (%)") ;
#delimit cr
end

* Extract graph
use "df.ETR.size.p.dta", clear
gl ETR_denominator "Net Profit"
ETR_size_p_GRAPH
graph export "ETRavg_perc_NP_$country_code.png", replace



******************* f.bis Number of observations by bins ******************* 
use "df.ETR.size.p.dta", clear
keep if ETR_denominator == "NP" 
keep country n_keep n_drop percentile_99_9
 
collapse (sum) n_keep n_drop, by(country percentile_99_9)   

export delimited "bins_observations.csv", replace


******************* e. ETR by firm-size, NET PROFIT (percentile) ******************* 
program ETR_logscale_GRAPH
renameFUN
keep if country == "$country_code"
keep if ETR_denominator == "$ETR_denominator"
local main1 ETR_keep_neg bin, color("ebblue*.75") deg(6)  xlabel(, nogrid) ylabel(, nogrid) lpattern(solid)  
local main2 ETR_drop_neg bin, color("red*.75")deg(6) lpattern(dash)
#delimit;
twoway  lpoly `main1' || lpoly `main2' 
plotregion(margin(zero)) 
legend(order (1 "All firms" 2 "Profitable firms") region(lstyle(none)) rows(1) nobox pos(12)) 
xtitle("log(turnover, USD)")
ytitle("Effective Tax Rate (%)") ;
#delimit cr
end

use  "df.ETR.logscale.dta", clear
drop if bin==.
gl ETR_denominator "Net Profit"
ETR_logscale_GRAPH
graph export "ETRavg_log_NP_$country_code.png", replace


******************* g. ETR by sectors ******************* 
program ETR_sector_GRAPH
renameFUN
encode $industry, gen(num_sector)
keep if country == "$country_code"
keep if ETR_denominator == "$ETR_denominator"
keep ETR_keep_neg ETR_drop_neg num_sector
rename ETR_keep_neg ETR_1
rename ETR_drop_neg ETR_2
reshape long ETR_ , i(num_sector) j(measure)
#delimit;
twoway (bar ETR_ num_sector if measure == 2 , barw(0.5) bcolor("red*.75"))
		(bar ETR_ num_sector if measure == 1, barw(0.5) bcolor("ebblue*.75")),	xlabel(1(1)$nb_sector, labsize(small) valuelabel noticks nogrid angle(45)) ylabel(0(5)25, nogrid) 
legend(order (2 "All firms" 1 "Profitable firms") region(lstyle(none)) rows(1) nobox pos(12)) 
ytitle("Effective Tax Rate (%)") xtitle("") ;
#delimit cr
end

use "df.ETR.sector.dta", clear
gl ETR_denominator "Net Profit"
gl industry sector
gl nb_sector 7
ETR_sector_GRAPH
graph export "ETRavg_sector_$country_code.png", replace


use "df.ETR.largesector.dta", clear
gl ETR_denominator "Net Profit"
gl industry largesector
gl nb_sector 5
ETR_sector_GRAPH
graph export "ETRavg_largesector_$country_code.png", replace

   
 
*** Delete stata files and convert output to .csv
erase quantiles_list.dta
erase df_sample.dta 
erase data.dta
erase df.ETR.reg.1.dta

	program erase_prog
	use "df.ETR.$files.dta", clear
	export delimited "df.ETR.$files.csv", replace
	erase "df.ETR.$files.dta"
end

global files "size"
erase_prog 
global files "size.p" 
erase_prog 
global files "avg.table"
erase_prog 
global files "panel"
erase_prog 
global files "panel.perc"
erase_prog 
global files "panel.cross"
erase_prog 
global files "largesector"
erase_prog 
global files "sector"
erase_prog 
global files "descr"
erase_prog 
global files "GDP"
erase_prog 
global files "logscale"
erase_prog 
