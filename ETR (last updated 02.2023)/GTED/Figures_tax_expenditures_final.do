
********************************************************************************
/* 			SCRIPT TO PRODUCE FIGURE OF TAX EXPENDITURES
																			*/
********************************************************************************

*directories and globals																				
	global root "C:\Users\s551964\Cross-Country\ETR\GTED"									
	cd "$root"

	set more off
	clear all 

	global raw "$root\01_Raw"
	global proc "$root\"
	global output "$root\"
	

********************************************************************************
** Calculate the Share of GDP Forgone due to CIT Expenditures ------------------
********************************************************************************
 
  * import from GTED
  import delimited "$raw/revenue-forgone-of-gdp.csv", clear

  * change revenue forgone from wide to long  
  foreach x of var * { 
	rename `x' x`x' 
	} 

  rename xcategory year

  reshape long x, i(year) j(country, string)
  
  rename x total_revenue_forgone
  
  save "$proc/total_revenue_forgone.dta", replace

  * import CIT from GTED
  import delimited "$raw/share-of-total-tax-expen.csv", clear

  * change CIT from wide to long 
   foreach x of var * { 
	rename `x' x`x' 
	} 

  rename xcategory year

  reshape long x, i(year) j(country, string)
  
  rename x cit_revenue_forgone
   
  * merge 2 datasets
  merge 1:1 country year using "$proc/total_revenue_forgone.dta"
  
  keep if _merge == 3
  
  drop _merge
  
  * calculate the share of GDP forgone due to CIT expanditure (%)
  gen cit_gdp_forgone = total_revenue_forgone * cit_revenue_forgone * 0.01
  
  * save the master dataset 
  save "$proc/master.dta", replace

********************************************************************************
** Import and clean CIT Revenue as a Share of GDP 
********************************************************************************

  * Import tax revenue data from UNU WIDER
  import excel "$raw/UNUWIDERGRD_2022_Full.xlsx", sheet("Merged") firstrow clear
 
  * clean UNWIDER 
  rename owCIT cit_revenue_gdp 
  
  rename Country country 
  
  rename Year year
  
  rename ISO country_code
  
  keep country year cit_revenue_gdp country_code
  
  replace cit_revenue_gdp = cit_revenue_gdp * 100 
  
  save "$proc/cit_revenue_gdp.dta", replace

  * Import tax revenue data from IMF World Revenue Longitudinal Data
  import delimited "$raw/WoRLD_timeSeries.csv", clear

  * clean tax revenue data 
  keep if countryname=="Honduras" & attribute=="Value" & indicatorname=="Corporate Income Tax Revenue in Percent of GDP"
	
  drop v38
  
  reshape long v, i(countryname) j(year)

  replace year = year + 1984
  
  rename countryname country
  
  rename v revenue_imf 
  
  keep country year revenue_imf
    
  keep if year >= 2016 & year <= 2020

  save "$proc/revenue_imf.dta", replace

  * fill in Honduras CIT/GDP in master

  use "$proc/cit_revenue_gdp.dta"
  
  merge 1:1 country year using "$proc/revenue_imf.dta"
  
  drop _merge
  
  destring revenue_imf, replace 
  
  replace revenue_imf = 0 if (revenue_imf >= .)  
  replace cit_revenue_gdp = 0 if (cit_revenue_gdp >= .)
  
  replace cit_revenue_gdp = cit_revenue_gdp + revenue_imf
  
  replace country = lower(country)
  
  save "$proc/cit_revenue_gdp.dta", replace
  
  * generate a master dataset 
  use "$proc/master.dta"
  
  replace country = "burkina faso" if country == "burkinafaso"
  replace country = "cape verde" if country == "caboverde"
  replace country = "central african republic" if country == "centralafricanrepublic"
  replace country = "costa rica" if country == "costarica"
  replace country = "cote d'ivoire" if country == "côtedivoire"
  replace country = "dominican republic" if country == "dominicanrepublic"
  replace country = "congo, democratic republic of the" if country == "drcongo"
  replace country = "el salvador" if country == "elsalvador"
  replace country = "equatorial guinea" if country == "equatorialguinea"
  replace country = "new zealand" if country == "newzealand"
  replace country = "north macedonia" if country == "northmacedonia"
  replace country = "papua new guinea" if country == "papuanewguinea"
  replace country = "russian federation" if country == "russianfederation"
  replace country = "sierra leone" if country == "sierraleone"
  replace country = "south africa" if country == "southafrica"
  replace country = "korea, republic of" if country == "southkorea"
  replace country = "sri lanka" if country == "srilanka"
  replace country = "united kingdom" if country == "unitedkingdom"
  replace country = "united states" if country == "unitedstates"
  replace country = "puerto rico" if country == "puertorico"

  merge 1:1 country year using "$proc/cit_revenue_gdp"

  drop if _merge == 2
  
  drop _merge
  
  save "$proc/master.dta", replace
  
	
********************************************************************************
** Load our own dataset of tax expenditures calculated
********************************************************************************
																	
	clear 
	import delimited using "$raw/df_taxgap_cap.csv"   		// Micro data estimates of tax gaps created by Camille
	drop v1
	
	drop if year<2014
	drop if year>2019	
	
	gen cit_exp_share_micro = (sum_np_str - sum_ntl_built)/ sum_ntl_built
	gen cit_gap_micro = (sum_np_str - sum_ntl_built)/ sum_np_str

	keep year country cit_exp_share_micro cit_gap_micro
	tempfile micro_data
	save `micro_data' 	
	
********************************************************************************	
** Load Global Tax Expenditure Dataset (GTED) 
********************************************************************************	

	clear
	use "$proc/master.dta"
	destring cit_gdp_forgone, force replace 
	
	keep country_code year cit_gdp_forgone
	rename country_code country
	
	merge 1:1 country year using "$raw/globalETR_bfjz.dta" 	// Harmonized tax tevenue collection data from Bachas, Fisher-Post, Jensen and Zucman (BFJZ)
	drop if year<2014
	drop if year>2019
	
	** See which countries merged
	count
	count if _m ==1
	drop if _m == 1 & year != 2019 // a few small countries, not included in BFJZ
	drop _m 
	
	keep country_name country year cit_gdp_forgone pct_1200
	rename pct_1200 CIT_GDPshare
	rename cit_gdp_forgone cit_gdp_forgone_GTED
	
	** For 2019 need to do a a quick imputation of the 2019 cit returns data 
	sort country year
	replace CIT_GDPshare = CIT_GDPshare[_n-1] if missing(CIT_GDPshare)	
	replace country_name = country_name[_n-1] if missing(country_name)	
	
	merge 1:1 country year using `micro_data' 	
	
	** MTN doesnt have CIT revenue cause not in BFJZ; instead use UNUWIDER-ICTD 
	
	replace CIT_GDPshare = 0.013  if country == "MNE" & year == 2014
	replace CIT_GDPshare = 0.0115  if country == "MNE" & year == 2015
	replace CIT_GDPshare = 0.0114  if country == "MNE" & year == 2016
	replace CIT_GDPshare = 0.0114  if country == "MNE" & year == 2017
	replace CIT_GDPshare = 0.0146  if country == "MNE" & year == 2018
	replace CIT_GDPshare = 0.0147  if country == "MNE" & year == 2019

	*** Create the variables for comparisons: 
	* on the micro side: 
	gen cit_gdp_forgone_micro = (cit_gap_micro * CIT_GDPshare)*100
	
	** on the macro side: 
	gen cit_exp_share_GTED = cit_gdp_forgone_GTED/ (CIT_GDPshare*100)

	gen cit_gap_GTED = cit_gdp_forgone_GTED / (cit_gdp_forgone_GTED+CIT_GDPshare*100)
	
	order country country_name year cit_gdp_forgone_GTED cit_exp_share_GTED cit_gap_GTED cit_gdp_forgone_micro cit_exp_share_micro cit_gap_micro
	
	** Create new data points	
	gen cit_gdp_forgone_macro = .
	gen cit_exp_share_macro =.
	gen cit_gap_macro = . 
	
	**********************************
	** Create Macro Numbers
	**********************************
	** Country by country, based on tax expenditure reviews. See notes below: only one data point 
	* Ecuador
	replace cit_gdp_forgone_macro = (1-0.632)*cit_gdp_forgone_GTED if country == "ECU" & year == 2018
	replace cit_exp_share_macro = cit_gdp_forgone_macro/ (CIT_GDPshare*100)  if country == "ECU" & year == 2018
	replace cit_gap_macro = cit_gdp_forgone_macro / (cit_gdp_forgone_macro+CIT_GDPshare*100)  if country == "ECU" & year == 2018
		
	list year cit_gdp_forgone_macro cit_gap_macro cit_gdp_forgone_micro cit_gap_micro cit_gdp_forgone_GTED cit_gap_GTED if country == "ECU"
	drop if country == "ECU" & year != 2018			
		
	* Mexico
	replace cit_gdp_forgone_macro = 0.67 if country == "MEX" & year == 2015
	replace cit_exp_share_macro = cit_gdp_forgone_macro/ (CIT_GDPshare*100)  if country == "MEX" & year == 2015
	replace cit_gap_macro = cit_gdp_forgone_macro / (cit_gdp_forgone_macro+CIT_GDPshare*100)  if country == "MEX" & year == 2015
	
	list year cit_gdp_forgone_macro cit_gap_macro cit_gdp_forgone_micro cit_gap_micro cit_gdp_forgone_GTED cit_gap_GTED if country == "MEX"
	drop if country == "MEX" & year != 2015
	
	* Honduras
	replace cit_gdp_forgone_macro = 1.85 if country == "HND" & year == 2018
	replace cit_exp_share_macro = cit_gdp_forgone_macro/ (CIT_GDPshare*100) if country == "HND" & year == 2018
	replace cit_gap_macro = cit_gdp_forgone_macro / (cit_gdp_forgone_macro+CIT_GDPshare*100)  if country == "HND" & year == 2018

	list year cit_gdp_forgone_macro cit_gap_macro cit_gdp_forgone_micro cit_gap_micro cit_gdp_forgone_GTED cit_gap_GTED if country == "HND"
	drop if country == "HND" & year != 2018
		
	* Costa Rica 
	replace cit_gdp_forgone_macro = 1.11 if country == "CRI" & year == 2018
	replace cit_exp_share_macro = cit_gdp_forgone_macro/ (CIT_GDPshare*100) if country == "CRI" & year == 2018
	replace cit_gap_macro = cit_gdp_forgone_macro / (cit_gdp_forgone_macro+CIT_GDPshare*100)  if country == "CRI" & year == 2018
		
	list year cit_gdp_forgone_macro cit_gap_macro cit_gdp_forgone_micro cit_gap_micro cit_gdp_forgone_GTED cit_gap_GTED if country == "CRI"
	drop if country == "CRI" & year != 2018		
		
	* Guatemala
	replace cit_gdp_forgone_macro = 0.8 if country == "GTM" & year == 2018
	replace cit_exp_share_macro = cit_gdp_forgone_macro/ (CIT_GDPshare*100) if country == "GTM" & year == 2018
	replace cit_gap_macro = cit_gdp_forgone_macro / (cit_gdp_forgone_macro+CIT_GDPshare*100) if country == "GTM" & year == 2018
	
	list year cit_gdp_forgone_macro cit_gap_macro cit_gdp_forgone_micro cit_gap_micro cit_gdp_forgone_GTED cit_gap_GTED if country == "GTM"
	drop if country == "GTM" & year != 2018		
	
	
	* For all the other countries, create an average of 2014-2019 years. This is our final dataset
	local varnames "cit_gdp_forgone_macro cit_gap_macro cit_gdp_forgone_micro cit_gap_micro cit_gdp_forgone_GTED cit_gap_GTED"
	
	sort country year 
	
	foreach var in `varnames' {
	by country: egen tmp_`var' = mean(`var')
	replace `var' = tmp_`var'
	drop tmp_`var'
	}
	
	duplicates drop country , force
		
	replace cit_gdp_forgone_macro = cit_gdp_forgone_GTED  if cit_gdp_forgone_macro == . 
	replace cit_gap_macro = cit_gap_GTED if cit_gap_macro == . 
	
	drop _merge
	
	** create the ones to plot when showing both micro and macro 
	
	gen cit_gdp_forgone_plot = cit_gdp_forgone_macro
	gen cit_gap_plot = cit_gap_macro	
	
	gen micro_sample = 0
	replace micro_sample = 1 if inlist(country, "MEX", "ALB", "DOM", "SEN", "RWA", "GTM", "CRI", "ECU", "HND")
	replace micro_sample = 1 if inlist(country, "MNE", "UGA", "ETH", "SWZ" )
	
	replace cit_gdp_forgone_plot = cit_gdp_forgone_micro if micro_sample == 1
	replace cit_gap_plot = cit_gap_micro if micro_sample == 1
		
	************************************
	*** Import GDP p.c. data 
	************************************
	preserve 
	
	clear
	import delimited using  "$raw/GDPPC.csv" , varnames(1)
	
	// We'll keep 2018 as a year as it maximizes coverage and often coincides with our year (this can be improved later) 
	
	keep countrycode indicatorname v63
	rename v63 gdp_pc_2018 
	rename countrycode country
	
	tempfile GDP_data
	save `GDP_data' 		
	
	restore
	
	merge 1:1 country using `GDP_data'
	keep if _m == 3
	drop _m 
		
	gen log_gdp = log(gdp_pc_2018)
		
	*****************************************************************
	* Summary Stats
	*****************************************************************		
	
	sum cit_gdp_forgone_plot , d 	// Cited in Paper, Section 3.1 
	sum cit_gdp_forgone_macro , d
	sum cit_gdp_forgone_micro , d	// Cited in Paper, Section 3.1 
	
	sum cit_gap_macro , d
	sum cit_gap_micro , d		
	
	corr cit_gdp_forgone_micro cit_gdp_forgone_macro  if micro_sample == 1	 // Cited in Paper, Section 3.1 
	corr cit_gap_micro cit_gap_macro  if micro_sample == 1  	
	
	*****************************************************************
	* Figures 
	*****************************************************************	

	** for the 45 degree line generate a grid 
	gen x1_45 = 0  if _n == 1
	gen	y1_45 = 0 if _n == 1
	replace x1_45 = 2  if _n == 2
	replace	y1_45 = 2 if _n == 2	
	
	gen x2_45 = 0  if _n == 1
	gen	y2_45 = 0 if _n == 1
	replace x2_45 = 0.5  if _n == 2
	replace	y2_45 = 0.5 if _n == 2
	
	
	local size medlarge
	local graph_region_options "graphregion(color(white)) bgcolor(white) plotregion(color(white))"
	local xtitle "xtitle("GDP per capita (Log)" , margin(medsmall) size(`size'))"
	local xaxis "xlabel(6(1)11, nogrid labsize(`size') )" 
	local ytitle1 "ytitle("CIT Expenditures (% of GDP)" , margin(medsmall) size(`size'))"
	local ytitle2 "ytitle("CIT Gap (% of potential revenue)" , margin(medsmall) size(`size'))"
	local legend legend(order(2 "Our Estimates" 1 "Government Estimates") ring(0) position(11) row(2))
		
	** Micro 
	#delim ; 	
		
	twoway (scatter cit_gdp_forgone_micro log_gdp, color(green) mlabel(country) mlabcolor(green)),
	`graph_region_options' 
	`ytitle1'	
	`xtitle' `xaxis'  
  	name(G1, replace); 	
	graph export "$output/Gap_G1.pdf" , replace	 ;
		
	#delim cr		
		

	** Micro + Macro 	
	#delim ; 
	
	twoway (scatter cit_gdp_forgone_plot log_gdp, msymbol(X)) (scatter cit_gdp_forgone_micro log_gdp, color(green)) ,	
	`graph_region_options' 
	`ytitle1'	
	`xtitle' `xaxis'  
	`legend'	
 	name(G3, replace)	; 	
	graph export "$output/Gap_G2.pdf" , replace	 ;		

		#delim cr		
		

	** Correlation micro and macro 	
	#delim cr	
	*/
	local size medlarge
	local graph_region_options "graphregion(color(white)) bgcolor(white) plotregion(color(white))"
	
	local ytitle1 "ytitle("CIT Expenditures: Our Estimates(% of GDP)" , margin(medsmall) size(`size'))"
	local xtitle1 "xtitle("CIT Expenditures: Gov. Estimates(% of GDP)" , margin(medsmall) size(`size'))"	
	local xaxis1 "xscale(range(0 2)) xlabel(0(0.5)2, nogrid labsize(`size') )" 
	local yaxis1 "yscale(range(0 2)) xlabel(0(0.5)2, nogrid labsize(`size') )" 	
	
	local ytitle2 "ytitle("CIT gap: Our Estimates(% of taxes)" , margin(medsmall) size(`size'))"
	local xtitle2 "xtitle("CIT gap: Gov. Estimates(% of taxes)" , margin(medsmall) size(`size'))"	
	local xaxis2 "xscale(range(0 0.5)) xlabel(0(0.1)0.5, nogrid labsize(`size') )" 
	local yaxis2 "yscale(range(0 0.5)) xlabel(0(0.1)0.5, nogrid labsize(`size') )" 	
	
	#delim ; 
	twoway (scatter cit_gdp_forgone_micro cit_gdp_forgone_macro  if micro_sample == 1 , color(green) mlabel(country) mlabcolor(green))
	/// (lfit cit_gdp_forgone_micro cit_gdp_forgone_macro  if micro_sample == 1, lcolor(green) lw(thin) )
	(line x1_45 y1_45 , lcolor(black) lw(thin) lp(dash)), 
	`graph_region_options' 
	`ytitle1' `xtitle1'
	`xaxis1' `yaxis1'
	legend(off)
 	name(G5, replace)	; 	
	graph export "$output/Gap_G3.pdf" , replace	 ;	
	
	#delim cr	

	* Erase intermediate files
	erase "$proc/cit_revenue_gdp.dta"
	erase "$proc/revenue_imf.dta"
	erase "$proc/total_revenue_forgone.dta"
	