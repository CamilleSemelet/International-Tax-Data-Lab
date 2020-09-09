# Simulation-Firms-COVID19

## Impact of COVID-19 on Formal Firms (NEW - September 2020)

- Replication file at the country level
- Replication file of the cross-country analysis

## World Bank blog post (April 2020)

### Link to blog post: https://blogs.worldbank.org/developmenttalk/using-administrative-tax-data-understand-implications-covid-19-coronavirus-formal

We created two versions of the code. 
The survival estimates are only estimated in Stata. If requested, we can write the R code to produce those regression estimates.

Stata users: 
	- Stata_Script_template.do : this do file runs the whole analysis including the survival model estimates and creates figures. 
R users: 
	- survival_estimates_template.do : produces a .csv document with the survival rate estimates.	
	- R_Script_template.R : this Script produces the whole analysis except for the survival model. To produce the full analysis it need to be combined with the stata do file 'survival_estimates_template.do'
