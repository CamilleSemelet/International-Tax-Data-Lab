# Simulation-Firms-COVID19
# The implication of COVID19 (Coronavirus) for formal firms (Replication codes)

# Link to blog post: https://blogs.worldbank.org/developmenttalk/using-administrative-tax-data-understand-implications-covid-19-coronavirus-formal

### We created two versions of the code. The figures presented in the blog post are created with the R script
### The survival estimates are only estimated in Stata. If requested, we can write the R code to produce those regression estimates.

# Stata users: 
	- Stata_Script_template.do : this do file runs the whole analysis including the survival model estimates and creates figures. 
# R users: 
	- survival_estimates_template.do : produces a .csv document with the survival rate estimates.	
	- R_Script_template.R : this Script produces the whole analysis except for the survival model. To produce the full analysis it need to be combined with the stata do file 'survival_estimates_template.do'
