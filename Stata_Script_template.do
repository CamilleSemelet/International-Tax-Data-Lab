* 4/13/2020
* Authors:  Pablo Garriga
*			Camille Semelet, csemelet@worldbank.org 


* SIMULATION of the COVID 19 impact on formal firms

* This Script is written to read already cleaned tax data and analyse the impact of COVID 19 on formal firms
* It produces three figures (five graphs in total) and one table of statistics
* 
* The variables required are the following:
*   (make sure to rename your variable as follow or change them in the Script below):
*   "year": years
*	"tax_id": firm's unique id, identifiable across years
*   "turnover": annual firm's revenue
*   "gross_tax_base": annual firm's profits(losses), after deductions of costs from turnover
*   "labor_inp": annual costs in labor
*   "material_inp": annual costs in material
* 	"section": sectors string variables (fixed effects)


*************************** Set up directories ****************************************
* Paths

loc output " " // <- FILL IN where you want to store the figures produced
loc data ""  // <- FILL IN where your data is located

cd "`data'"
import delimited using " ", clear // <- FILL IN
tempfile sourcedata
save `sourcedata'

* Create two additional objects we'll need
g country_name = " "  // <- FILL IN
g country_code = " "  // <- FILL IN
loc country_name " "  // <- FILL IN
loc country_code " "  // <- FILL IN

*************************** Graph 0. Baseline ****************************************

  *** We are only considering the latest available year
  qui summ year
  keep if year==r(max)

  ** Turnover. Keep only firms where turnover is positive
  keep if turnover > 0
  
  ** Drop if gross tax base is missing
  drop if missing(gross_tax_base)
  
  ** Construct costs (sometimes the variable total_cost does not add up perfectly)
  gen costs = turnover - gross_tax_base 

  ** We take three steps to remove outliers: 1) Removes P5 in terms of turnover
  qui summ turnover, d
  keep if turnover>r(p5)

  * COMMENT STATA AND R COMPUTE PERCENTILES DIFFERENTLY. 
  * https://data.princeton.edu/stata/markdown/quantiles.htm
  
  ** Construct Profit margin 
  g profit_margin = gross_tax_base/turnover if turnover!=0
  drop if missing(profit_margin)

  ** We take three steps to remove outliers: 2) Removes P5 and P95 in terms of profit margin
  qui summ profit_margin, d
  keep if (profit_margin>r(p5) & profit_margin<r(p95))
  
  ** We take three steps to remove outliers: 3) Winzorise profit margin at [-1,1]
  replace profit_margin = -1 if profit_margin<-1
  replace profit_margin =  1 if profit_margin>1
  
  tempfile data_1
  save `data_1'

  ** Save the Baseline data in an object (profit margin and profit)
  keep profit_margin gross_tax_base country_name country_code
  g scenario = "baseline"
  tempfile baseline
  save `baseline'

  ** Store aggregates for revenue and wage bill for the total economy (used later)
  use `data_1', clear
  collapse (sum) wagebill=labor_inp revenue=turnover, by(country_name country_code)
  tempfile aggregate
  save `aggregate'

  di "Figure 0. Baseline, `country_name'"

*************************** Figure 1.a shock in revenue, no cost adjustement  ****************************************

  *** 1 month scenario
  use `data_1', clear 
  * COMMENT: not sure if this is equivalent to  data_1

  ** Simulate 1-month-output-loss
  replace turnover = turnover - (1/12)*turnover
  * Recalculate profits
  replace gross_tax_base = turnover - costs
  * Recalculate profit margin
  replace profit_margin = gross_tax_base/turnover if turnover!=0 
  drop if missing(profit_margin)

  qui sum profit_margin, d
  replace profit_margin = -1 if profit_margin<-1
  replace profit_margin =  1 if profit_margin>1
  
  ** Save 1-month-Scenario data in an object (profit margin and profit)
  keep profit_margin gross_tax_base country_name country_code
  g scenario = "1 month"
  tempfile 1month
  save `1month'
  
  *** 3 months scenario
  use `data_1', clear 
  
  ** Simulate 3-month-output-loss
  replace turnover = turnover - (3/12)*turnover
  * Recalculate profits
  replace gross_tax_base = turnover - costs
  * Recalculate profit margin
  replace profit_margin = gross_tax_base/turnover if turnover!=0 
  drop if missing(profit_margin)

  qui sum profit_margin, d
  replace profit_margin = -1 if profit_margin<-1
  replace profit_margin =  1 if profit_margin>1
  
  ** Save 3-month-Scenario data in an object (profit margin and profit)
  keep profit_margin gross_tax_base country_name country_code
  g scenario = "3 months"
  tempfile 3months
  save `3months'
  
  ******* Combine Baseline, Scenario 1 and 3 in one dataframe

  use `baseline', clear
  append using `1month'
  append using `3months'
  tempfile figure_1a
  save `figure_1a'

  di "Figure 1.a, `country_name'"

*************************** Figure 1.b shock in revenue, material cost adjustment  ****************************************
  
  *** 1 month scenario
  use `data_1', clear 

  ** Simulate 1-month-output-loss
  replace turnover = turnover - (1/12)*turnover
  * Adjust material costs proportionally to the revenue shock
  replace costs = costs - (1/12)*material_inp
  * Recalculate profits
  replace gross_tax_base = turnover - costs
  * Recalculate profit margin
  replace profit_margin = gross_tax_base/turnover if turnover!=0 
  drop if missing(profit_margin)

  qui sum profit_margin, d
  replace profit_margin = -1 if profit_margin<-1
  replace profit_margin =  1 if profit_margin>1
  
  ** Save 1-month-Scenario data in an object (profit margin and profit)
  keep profit_margin gross_tax_base country_name country_code
  g scenario = "1 month"
  tempfile 1month
  save `1month'
  
  *** 3 months scenario
  use `data_1', clear 
  
  ** Simulate 3-month-output-loss
  replace turnover = turnover - (3/12)*turnover
  * Adjust material costs proportionally to the revenue shock
  replace costs = costs - (3/12)*material_inp
  * Recalculate profits
  replace gross_tax_base = turnover - costs
  * Recalculate profit margin
  replace profit_margin = gross_tax_base/turnover if turnover!=0 
  drop if missing(profit_margin)

  qui sum profit_margin, d
  replace profit_margin = -1 if profit_margin<-1
  replace profit_margin =  1 if profit_margin>1
  
  ** Save 3-month-Scenario data in an object (profit margin and profit)
  keep profit_margin gross_tax_base country_name country_code
  g scenario = "3 months"
  tempfile 3months
  save `3months'
  
  ******* Combine Baseline, Scenario 1 and 3 in one dataframe

  use `baseline', clear
  append using `1month'
  append using `3months'
  tempfile figure_1b
  save `figure_1b'

  di "Figure 1.b, `country_name'"

  *************************** Figure 2. shock in revenue, material & labor costs adjustment  ****************************************
  ** We assume that material inputs adjust first, and that firms only cut their wage bill if they are still unprofitable after this adjustment
  ** Firms will adjust their labor cost differently
  
  *** 1 month scenario
  * Case 1. firms that were already making losses in baseline
  * Case 2. firms went from making profit in baseline to have losses in month 1
  * Case 3. firms still make profit even after a 1-month-output loss
  
  use `data_1', clear
  drop if missing(labor_inp)  

  * Save profit margin at baseline
  rename profit_margin baseline_rate
  
  ** Simulate 1-month-output-loss
  replace turnover = turnover - (1/12)*turnover

  * Adjustment for material costs first
  replace costs = costs - (1/12)*material_inp

  * Create hypothetical profits and profit rate if no adjustment in terms of labor costs is made
  g forecast_gross_tax_base = turnover - costs
  g forecast_rate = forecast_gross_tax_base/turnover 

  * Categorize firms with cases 1, 2 or 3 by comparing baseline rate and hypothetical rate
  g cases = .
  replace cases = 1 if baseline_rate <= 0
  replace cases = 2 if baseline_rate > 0 & forecast_rate <= 0
  replace cases = 3 if baseline_rate > 0 & forecast_rate > 0
  
  *** Adujst costs differently depending on cases
  ** 1 Full adjustment: adjust by the size of the shock
  g new_costs = costs - (1/12)*labor_inp
  replace costs = new_costs if cases == 1
  * Store drop in labor costs
  g reduction_labor = (1/12)*labor_inp

  ** 2 Partial adjustment
  * New optimal labor costs to have 0 losses
  g adjusted_labor = costs - turnover 
  * If reduction is more than 1/12 of original labor costs, then cap to 1/12
  replace adjusted_labor = reduction_labor if adjusted_labor > reduction_labor
  * Recalculate total costs
  replace costs = adjusted_labor if cases == 2
  * Store drop in labor costs
  replace reduction_labor = adjusted_labor if cases == 2
  
  ** 3 No adjusment
  * Store drop in labor costs
  replace reduction_labor = 0 if cases == 3

  * We will plot the drop in labor costs compared to baseline labor costs
  g newlabor_inp = labor_inp - reduction_labor
  g ratio_labor = 1 - (newlabor_inp/labor_inp) if labor_inp!=0

  ** Save 1-month-Scenario data in an object
  keep ratio_labor labor_inp reduction_labor
  g scenario = "1 month"
  tempfile 1month
  save `1month'
  
  *** 3 months scenario
  use `data_1', clear
  drop if missing(labor_inp)  

  * Save profit margin at baseline
  rename profit_margin baseline_rate
  
  ** Simulate 1-month-output-loss
  replace turnover = turnover - (3/12)*turnover

 * Adjustment for material costs first
  replace costs = costs - (3/12)*material_inp
  
  * Create hypothetical profits and profit rate if no adjustment in terms of labor costs is made
  g forecast_gross_tax_base = turnover - costs
  g forecast_rate = forecast_gross_tax_base/turnover 

  * Categorize firms with cases 1, 2 or 3 by comparing baseline rate and hypothetical rate
  g cases = .
  replace cases = 1 if baseline_rate <= 0
  replace cases = 2 if baseline_rate > 0 & forecast_rate <= 0
  replace cases = 3 if baseline_rate > 0 & forecast_rate > 0

  *** Adujst costs differently depending on cases
  ** 1 Full adjustment: adjust by the size of the shock
  g new_costs = costs - (3/12)*labor_inp
  replace costs = new_costs if cases == 1
  * Store drop in labor costs
  g reduction_labor = (3/12)*labor_inp

  ** 2 Partial adjustment
  * New optimal labor costs to have 0 losses
  g adjusted_labor = costs - turnover 
  * If reduction is more than 1/12 of original labor costs, then cap to 1/12
  replace adjusted_labor = reduction_labor if adjusted_labor > reduction_labor
  * Recalculate total costs
  replace costs = adjusted_labor if cases == 2
  * Store drop in labor costs
  replace reduction_labor = adjusted_labor if cases == 2
  
  ** 3 No adjusment
  * Store drop in labor costs
  replace reduction_labor = 0 if cases == 3
  
  * We will plot the drop in labor costs compared to baseline labor costs
  g newlabor_inp = labor_inp - reduction_labor
  g ratio_labor = 1 - (newlabor_inp/labor_inp) if labor_inp!=0

  ** Save 3-month-Scenario data in an object
  keep ratio_labor labor_inp reduction_labor
  g scenario = "3 months"
  tempfile 3months
  save `3months'
  
  ******* Combine Baseline, Scenario 1 and 3 in one dataframe 
  use `1month', clear
  append using `3months'
  drop if missing(ratio_labor)

  * Compute aggregate loss in wage bill, weighted by firm's total wage bill, and average firm loss
  bys scenario: egen tmp1 = total(ratio_labor*labor_inp)
  bys scenario: egen tmp2 = total(labor_inp)
  g aggregateloss = (tmp1/tmp2)*100
  drop tmp1 tmp2
  bys scenario: egen averageloss = mean(ratio_labor)
  
  ** Create bins for drop in labor costs
  * Categories
  g category = .
  replace category = 1 if ratio_labor >= 0    & ratio_labor < 0.04  
  replace category = 2 if ratio_labor >= 0.04 & ratio_labor < 0.08
  replace category = 3 if ratio_labor >= 0.08 & ratio_labor < 0.12
  replace category = 4 if ratio_labor >= 0.12 & ratio_labor < 0.16
  replace category = 5 if ratio_labor >= 0.16 & ratio_labor < 0.20
  replace category = 6 if ratio_labor >= 0.20 & ratio_labor < 0.24
  replace category = 7 if ratio_labor >= 0.24

  label define category  1 "[0-4[" 2 "[4-8[" 3 "[8-12[" 4 "[12-16[" 5 "[16-20[" 6 "[20-24[" 7 "[24-28["
  label values category category

  bys scenario category: g total = _N
  bys scenario category: keep if _n==1
  bys scenario: egen sum = total(total)
  g count= total/sum
  keep scenario category count
  preserve
  clear all
  set obs 16
  g scenario = ""
  replace scenario = "1 month" in 1/7
  replace scenario = "3 months" in 8/15
  g category = .
  bys scenario: replace category = _n
  tempfile tmp
  save `tmp'
  restore
  merge 1:1 scenario category using `tmp', nogen
  replace count = 0 if count ==.
  sort scenario category

  tempfile figure_2
  save `figure_2'

  di "Figure 2. `country_name'"

  *************************** Figure 3. Survival model (requires longitudinal data) ****************************************
  use `sourcedata', clear 
  
  keep if turnover > 0 | !missing(turnover)
	sum year
	local max = r(max)
	local beforelast = `max'-1
	* Last Year firm appears in the dataset
	bys tax_id: egen lastyear = max(year)
	gen dum_lastyear = 0
	replace dum_lastyear = 1 if lastyear == year
	replace dum_lastyear = . if year == `max' // Last year of the panel isn't an exit year 

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

	**** REGRESSION TABLE
	estimates clear
    cd "`output'"
	* CHOOSE 
	local fe1 ib(freq).ind  // industry fe
	local fe2 ib(freq).year // year fe
	local depvar "l1_shock1030 l1_shock3050 l1_shock50p"
	local spec groupvar() bdec(3) sdec(3) // Options
	cap drop y_hat_1 

	* One lag
	eststo s1: reghdfe exit `depvar' l1_arcprofit l1_logturnover, absorb(`fe1' `fe2')
	#delimit;
	outreg2 using "lm_exit_`country_code'.xls", replace label keep(`depvar' l1_arcprofit l1_logturnover) ctitle("One lag") addtext(Sector FE, Yes, Year FE, Yes) `spec'
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

	** Predict model 
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

	* Keep second to last year results */
	keep if year == `beforelast'
	collapse (mean) predicted_1 proba_* se_*
	gen country = "`country_code'"
  reshape long predicted_ proba_ se_ , i(country) j(tmp) string
  g measure = .
  replace measure=1 if tmp=="1"
  replace measure=2 if tmp=="shock1030"
  replace measure=3 if tmp=="shock3050"
  replace measure=4 if tmp=="shock50p"
  label define measure 1 "Baseline" 2 "3-1 months" 3 "3-6 months" 4 "More than 6 months"
  label values measure measure 
  drop tmp

  g value=.
  replace value = predicted_ if measure==1
  replace value = proba_ if measure!=1
  drop predicted_ proba_

  g cipos = .
  replace cipos = value + 1.96 * se_ if measure!=1
  g cineg = .
  replace cineg = value - 1.96 * se_ if measure!=1
  drop se_

  tempfile figure_3
  save `figure_3'

  di "Figure 3. `country_name'"

********************************** EXTRACT GRAPHS  *********************************

  cd "`output'"

  use `figure_1a', clear
  replace profit_margin = profit_margin*100
  twoway  (kdensity profit_margin if scenario == "baseline"& inrange(profit_margin,-50,50), bw(1.5) lcolor(gray)) ///
          (kdensity profit_margin if scenario == "1 month" & inrange(profit_margin,-50,50), bw(1.5) lcolor(orange)) ///
          (kdensity profit_margin if scenario == "3 months"& inrange(profit_margin,-50,50), bw(1.5) lcolor(red)), ///
          xline(0) xscale(range(-50 50)) xlabel(-50(25)50) ///
          title("`country_name'") ///
          xtitle("Profit margin (%)") ytitle("Density") ///
          legend(order(- "Revenue loss: " 1 "Baseline" 2 "1 month" 3 "3 months") r(1)) ///
          plotregion(icolor(white)) graphregion(fcolor(white) color(white)) bgcolor(white)
  graph export "figure_1a.png", replace

  use `figure_1b', clear
  replace profit_margin = profit_margin*100
  twoway  (kdensity profit_margin if scenario == "baseline" & inrange(profit_margin,-50,50), bw(1.5) lcolor(gray)) ///
          (kdensity profit_margin if scenario == "1 month" & inrange(profit_margin,-50,50), bw(1.5) lcolor(orange)) ///
          (kdensity profit_margin if scenario == "3 months"& inrange(profit_margin,-50,50), bw(1.5) lcolor(red)), ///
          xline(0) xscale(range(-50 50)) xlabel(-50(25)50) ///
          title("`country_name'") ///
          xtitle("Profit margin (%)") ytitle("Density") ///
          legend(order(- "Revenue loss: " 1 "Baseline" 2 "1 month" 3 "3 months") r(1)) ///
          plotregion(icolor(white)) graphregion(fcolor(white) color(white)) bgcolor(white)
  graph export "figure_1b.png", replace

  use `figure_2', clear
  replace count = count*100
  twoway  bar count category if scenario == "1 month", ///
          xscale(range(1(1)7)) xlabel(1(1)7, labsize(small) valuelabel noticks) color(orange) barw(0.65) yscale(range(0(20)100)) ylabel(0(20)100) ///
          xtitle("Drop in labor costs compared to baseline (%)") ytitle("Share of firms") ///
          plotregion(icolor(white)) graphregion(fcolor(white) color(white)) bgcolor(white)
  graph export "figure_2a.png", replace

  use `figure_2', clear
  replace count = count*100
  twoway  bar count category if scenario == "3 months", ///
          xscale(range(1(1)7)) xlabel(1(1)7, labsize(small) valuelabel noticks) color(red) barw(0.65) yscale(range(0(20)100)) ylabel(0(20)100) ///
          xtitle("Drop in labor costs compared to baseline (%)") ytitle("Share of firms") ///
          plotregion(icolor(white)) graphregion(fcolor(white) color(white)) bgcolor(white)          
  graph export "figure_2b.png", replace

  use `figure_3', clear
  replace value = value*100
  replace cipos = cipos*100
  replace cineg = cineg*100

  twoway  (bar value measure if measure == 1, barw(0.75) ///
			bcolor(gray)) ///
		 (bar value measure if measure == 2, barw(0.75) ///
			bcolor(orange)) ///
		 (bar value measure if measure == 3, barw(0.75) ///
			bcolor(red)) ///
		 (bar value measure if measure == 4, barw(0.75) bcolor(maroon)) ///
          (rcap cipos cineg measure), ///
          xlabel(, valuelabel noticks) ///
          yscale(range(0 18)) ylabel(0(2)18) ///
          xtitle("") ytitle("Share of firms exiting") ///
          legend(off) ///
          plotregion(icolor(white)) graphregion(fcolor(white) color(white)) bgcolor(white)
	
  graph export "figure_3.png", replace

exit