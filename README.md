# Simulation-Firms-COVID19
The implication of COVID19 (Coronavirus) for formal firms (Replication codes)

### We created two versions of the code. The graphs presented in the blog post are created with the R script. 
### The survival estimates are only created via Stata. If requested, we may write the R code to produce those estimates.

# Stata users: 
	- Stata_Script_template.do : this do file run the whole analysis including the survival model estimates, and create graphs
# R users: 
	- survival_estimates_template.do : produces a .csv document with the survival rate estimates.	
	- R_Script_template.R : this Script produced the whole analysis excluding the survival model. It needs to be run after with the stata do file 'survival_estimates_template.do'
