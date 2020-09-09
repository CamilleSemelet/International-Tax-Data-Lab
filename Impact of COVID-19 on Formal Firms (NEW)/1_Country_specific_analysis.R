
########## SIMULATION of the COVID 19 impact on formal firms #######

# This Script is written to read already cleanned tax data and analyse the impact of COVID 19 on formal firms
# It produces three figures (five graphs in total) and one table of statistics
# 
# The variables required are the following:
#   (make sure to rename your variable as follow or change them in the Script below):
#   "year": years
#   "turnover": annual firm's revenue
#   "gross_tax_base": annual firm's profits(losses), after deductions of costs from turnover
#   "labor_inp": annual costs in labor
#   "material_inp": annual costs in material
#   "covid_categories": impact category sectors (high_risk, medium_risk, low_risk)
#   "tax_ID": unique firm tax id, similar across years
#   "official_exchange": official exchange rate for each year (LCU per USD)
#   "GDP_currentLCU": GDP (current LCU)

############################### Install packages  ##################################

  list.of.packages <- c("plyr", "ggplot2", "tidyr", "lattice", "expss", "DescTools", "dplyr", "tibble", "ggformula", "estprod", "plm")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  # Load
  library(plyr) # ddply command
  library(ggplot2) # Use for graph
  library(tidyr) #Use for drop_na
  library(lattice) # histogram with log
  library(expss)
  library(DescTools) # Windsorize
  library(dplyr) # Various data handling process
  library(tibble)
  library(ggformula) ## geom_spline
  library(estprod)
  library(plm)
  
############################### Set up directories  ####################################    
### CHANGE HERE
country_name <-c("")  
country_code <- c("")
## Paths
gitHub <- "C:\\Users\\s551964\\Cross-Country"   ### CHANGE HERE
data_dir <- paste0("D:/Crosscountrytax/proc_data/", country_name) ### CHANGE HERE  # Where the clean data is located on your marchine
output <- paste0(gitHub, "\\COVID19\\output\\", country_code)
proc_data <- paste0(gitHub, "\\COVID19\\proc data\\")
dofiles <-  paste0(gitHub, "\\COVID19\\dofiles\\")
## Load data
setwd(data_dir) 
Paste_data <- paste(country_code, "_withvars.csv", sep = "")
data <- read.csv(Paste_data, header = TRUE, sep = ",")
 

############################### Locals & Country specific modification ##################################

#1 Output loss (%) for each sector category 

  factor_loss <- c(1, 0.5, 0.2, 0)  ### CHANGE HERE


#2 CIT statutory tax rate 
  data$statutory_rate <- 0.25  ### CHANGE HERE



########################### Graph 0. Baseline ########################################
data_1 <- data

## Turnover. Keep only firms where turnover is positive
data_1 <- filter(data_1, turnover > 0)

## Drop if gross tax base is missing
data_1 <- data_1 %>% drop_na(gross_tax_base)

## Drop if sector is missing
data_1 <- data_1 %>% drop_na(covid_categories)
data_1 <- data_1[ which(data_1$covid_categories != ""),] 

## Replace if missing
data_1 <- data_1 %>% mutate(labor_inp=replace_na(labor_inp, 0))
data_1 <- data_1 %>% mutate(material_inp=replace_na(material_inp, 0))

## We take two steps to remove outliers: 1) Winzorize P5 in terms of turnover
data_1 <- data_1 %>% group_by(year) %>% mutate( turnover = Winsorize(turnover, probs = c(.05),  na.rm = TRUE)) %>% ungroup()

## Construct Profit margin 
data_1$profit_margin <- data_1$gross_tax_base/data_1$turnover 
data_1$profit_margin[is.nan(data_1$profit_margin)] <- NA

## We take two steps to remove outliers: 2) Winzorise profit margin at [-1,1]
data_1$profit_margin <- Winsorize(data_1$profit_margin, minval = -1, maxval = 1, na.rm = TRUE)

	## COST SHARES	
# 1. Construct share of labor inp and material inp as share of turnover	
data_1$labor_turn <- 100*data_1$labor_inp/data_1$turnover	
data_1$material_turn <- 100*data_1$material_inp/data_1$turnover	
data_1 <- data_1 %>% mutate(material_turn = if_else(material_turn < 0, 0, material_turn),	
                            material_turn = if_else(material_turn > 100, 100, material_turn),	
                            labor_turn = if_else(labor_turn < 0, 0, labor_turn),	
                            labor_turn = if_else(labor_turn > 100, 100, labor_turn))	
# Construct costs (sometimes the variable total_cost does not add up perfectly)	
data_1 <- data_1 %>% mutate(costs = turnover - gross_tax_base,	
                            gross_tax_base = if_else(costs <0, turnover, gross_tax_base),	
                            costs = if_else(costs <0, 0, costs))	
data_1$fixedcost <- data_1$costs - data_1$material_inp - data_1$labor_inp	
data_1 <- data_1 %>% mutate(fixedcost = if_else(data_1$fixedcost < 0, 0, data_1$fixedcost))	
#data_1$costs <-  data_1$material_inp + data_1$labor_inp + data_1$fixedcost	
data_1$fixed_turn <- 100*data_1$fixedcost/data_1$turnover	
data_1$fixed_turn <- if_else(data_1$fixed_turn > 100, 100, data_1$fixed_turn)	
# 2. Construct share of labor inp and material inp as share of total expenditures	
data_1 <- data_1 %>% mutate(total_exp = rowSums(.[c("labor_inp", "material_inp", "fixedcost")], na.rm = T),	
                            labor_exp = if_else(total_exp == 0, 0, 100*labor_inp/total_exp),	
                            material_exp = if_else(total_exp == 0, 0, 100*material_inp/total_exp),	
                            fixed_exp = if_else(total_exp == 0, 0, 100*fixedcost/total_exp)	
                            )	
### Re-calibrate inputs: sometimes, laborinp + material+ fixed cost != (turnover-grosstaxbase).	
# We use the shares calculated above to recalibrate the input costs	
data_1 <- data_1 %>% mutate(labor_inp = (labor_exp*costs)/100,	
                            material_inp = (material_exp*costs)/100,	
                            fixedcost = (fixed_exp*costs)/100)	
## Store aggregates for revenue and wage bill for the total economy (used later, last cross section only)	
aggregate  <- data_1[which(data_1$year == max(data_1$year)),] 	
GDP <- aggregate$GDP_currentLCU[1]	
aggregate <- aggregate %>% mutate(value_added = turnover - material_inp, 	
                                  value_added = if_else(value_added < 0, 0, value_added),	
                                  CITbase = gross_tax_base*statutory_rate,	
                                  CITbase = if_else(CITbase <0, 0, CITbase)) %>%	
  dplyr::summarise(wagebill = sum(labor_inp, na.rm = TRUE),	
                   material = sum(material_inp, na.rm = TRUE),	
                   revenue = sum(turnover, na.rm = TRUE),	
                   totalfirms = n(),	
                   value_added = sum(value_added, na.rm = TRUE),	
                   cit_revenue_nom = sum(CITbase, na.rm= T)) %>% 	
  mutate(cit_revenue_GDP = 100*cit_revenue_nom/GDP)	
aggregate$country <- country_code	
aggregate$GDP <- GDP	
aggregate <- select(aggregate, totalfirms, wagebill, revenue, material, country, value_added, cit_revenue_nom, 	
                    cit_revenue_GDP, GDP)	

print(paste("Figure 0. Baseline all firms,", country_name, sep = " "))	

	### Profit margins and firm size and cost shares:	
meta_0 <- data_1 %>% select(profit_margin, turnover, labor_exp, material_exp, fixed_exp) %>%	
  mutate(labor_over_mat = if_else(material_exp != 0, labor_exp/material_exp, NA_real_),	
         labor_over_fixed = if_else(fixed_exp != 0, labor_exp/fixed_exp, NA_real_ ),	
         mat_over_fixed = if_else(fixed_exp != 0, material_exp/fixed_exp, NA_real_ ),	
         country = country_code,	
         deciles = ntile(turnover, 10))  	

############################### Create three categories of sectors  ##################################
## Add factors of loss
data_1$factorloss <- 0
data_1$factorloss[data_1$covid_categories == "high_risk"] <- factor_loss[[1]]
data_1$factorloss[data_1$covid_categories == "medium_risk"] <- factor_loss[[2]]
data_1$factorloss[data_1$covid_categories == "low_risk"] <- factor_loss[[3]]

## Separate between last cross section and panel
cross_section  <- data_1[which(data_1$year == max(data_1$year)),] 

## Create cross section subsets 
high_risk <- cross_section[which(cross_section$covid_categories == "high_risk"),] 
medium_risk <- cross_section[which(cross_section$covid_categories == "medium_risk"),] 
low_risk <- cross_section[which(cross_section$covid_categories == "low_risk"),] 
## Create panel subsets 
panel_high_risk <- data_1[which(data_1$covid_categories == "high_risk"),] 
panel_medium_risk <- data_1[which(data_1$covid_categories == "medium_risk"),] 
panel_low_risk <- data_1[which(data_1$covid_categories == "low_risk"),] 

## LISTS
sectors_name <- c("High Impact Sectors", "Medium Impact Sectors", "Low Impact Sectors", "All Sectors")
sectors <- list(high_risk, medium_risk, low_risk, cross_section)
sectors_panel <- list(panel_high_risk, panel_medium_risk, panel_low_risk, data_1)
sector <- c("high_risk", "medium_risk", "low_risk", "all_sectors")
table <- list()
figure_1_a <- list()	
figure_1_b <- list()	
figure_1_c <- list()
figure_2 <- list()
figure_2_raw <-list()
figure2_CIT_subsidy <- list() ## MNE
figure_3 <- list()
figure_3_MNE <- list()

for (i in 1:length(sectors)) {
  
  ###########################  TABLE #########################
 table_data <- sectors[[i]]	
  table_data <- table_data %>% mutate(value_added = turnover - material_inp,	
                                      value_added = if_else(value_added < 0, 0, value_added))	
  table_data$profit_margin[ table_data$profit_margin < 0 ] <- 0 # Put negative profit margin at 0	
  table_data <- table_data %>%	
    dplyr::summarise(nb_firms = n(),	
                     share_turnover = sum(turnover, na.rm = T),	
                     share_payroll = sum(labor_inp, na.rm = T),	
                     averageLCU_size = mean(turnover, na.rm = T),	
                     averageUSD_size = mean(turnover, na.rm = T)/official_exchange[1],	
                     medianUSD_size = median(turnover, na.rm = T)/official_exchange[1],	
                     avg_profitmargin = 100*mean(profit_margin, na.rm = T),	
                     avg_labor_turn = mean(labor_turn, na.rm = T),	
                     avg_material_turn = mean(material_turn, na.rm = T),	
                     avg_fixed_turn = mean(fixed_turn, na.rm = T),	
                     avg_labor_exp = mean(labor_exp, na.rm = T),	
                     avg_material_exp = mean(material_exp, na.rm = T),	
                     avg_fixed_exp = mean(fixed_exp, na.rm = T),	
                     avg_shock = 100*mean(factorloss, na.rm = T),	
                     avg_shock_weight = 100*sum(factorloss*turnover)/sum(turnover, na.rm = T),	
                     ttl_payrollLCU = sum(labor_inp, na.rm = T),	
                     valueadded = sum(value_added, na.rm = T),	
                     valueadded_3 = sum(value_added - (factorloss*3/12*value_added)),	
                     valueadded_5 = sum(value_added - (factorloss*5/12*value_added))) %>%	
    mutate(share_firms = (nb_firms/aggregate$totalfirms[1])*100,	
           share_payroll = (share_payroll/aggregate$wagebill[[1]])*100,	
           share_turnover = (share_turnover/aggregate$revenue[[1]])*100) %>%	
    mutate_if(is.numeric, round, digits = 1) 	
  	
  table_data$Sectors <- sectors_name[[i]]	
  table_data <- table_data[, c(20, 1, 21, 2, 3, 4, 5 ,6 ,7, 8 ,9 ,10 ,11, 12, 13, 14, 15, 16, 17, 18, 19)]	
  table[[i]] <- table_data	

  ########################### Graph 0. Baseline ########################################	
  ### We are only considering the latest available year	
  data_init <- sectors[[i]] 	
  	
  ## Save the Baseline data in an object (profit margin and profit)	
  scenario0 <- select(data_init, profit_margin, gross_tax_base, statutory_rate)	
  scenario0$Scenario <- "Baseline"	
  	
  print(paste("Figure 0.bis Baseline,", sectors_name[[i]], sep = " "))	
  	
  ########################### Figure 1.a shock in revenue, no cost adjustement  ########################################	
  data_init <- sectors[[i]]	
  ### 3 month scenario	
  data_fig1 <- data_init 	
  	
  ## Simulate 3-month-output-loss	
  data_fig1$turnover <- data_fig1$turnover - data_fig1$factorloss*(3/12)*(data_fig1$turnover)	
  	
  # Recalculate profits	
  data_fig1$gross_tax_base <- data_fig1$turnover - data_fig1$costs # Recalculate profit margin	
  data_fig1$profit_margin <- data_fig1$gross_tax_base/data_fig1$turnover 	
  data_fig1$profit_margin[is.nan(data_fig1$profit_margin)] <- NA	
  data_fig1 <- data_fig1 %>% drop_na(profit_margin)	
  data_fig1$profit_margin <- Winsorize(data_fig1$profit_margin, minval = -1, maxval = 1, na.rm = TRUE)	
  	
  ## Save 3-month-Scenario data in an object (profit margin and profit)	
  scenario3 <- select(data_fig1, profit_margin, gross_tax_base, statutory_rate)	
  scenario3$Scenario <- "3 months"	
  	
  ### 5 months scenario	
  data_init <- sectors[[i]]	
  data_fig1 <- data_init  	
  	
  ## Simulate 5-month-output-loss	
  data_fig1$turnover <- data_fig1$turnover - data_fig1$factorloss*(5/12)*(data_fig1$turnover)	
  # Recalculate profits	
  data_fig1$gross_tax_base <- data_fig1$turnover - data_fig1$costs 	
  # Recalculate profit margin	
  data_fig1$profit_margin <- data_fig1$gross_tax_base/data_fig1$turnover 	
  data_fig1$profit_margin[is.nan(data_fig1$profit_margin)] <- NA	
  data_fig1 <- data_fig1 %>% drop_na(profit_margin)	
  data_fig1$profit_margin <- Winsorize(data_fig1$profit_margin, minval = -1, maxval = 1, na.rm = TRUE)	
  	
  ## Save 5-month-Scenario data in an object (profit margin and profit)	
  scenario5 <- select(data_fig1, profit_margin, gross_tax_base, statutory_rate)	
  scenario5$Scenario <- "5 months"	
  	
  ####### Combine Baseline, Scenario 1 and 3 in one dataframe 	
  figure_1a <- rbind(scenario0, scenario3, scenario5)	
  figure_1a$Scenario <- factor(figure_1a$Scenario, levels = c("Baseline", "3 months", "5 months"))	
  figure_1_a[[i]] <- figure_1a	
  	
  print(paste("Figure 1.a", sectors_name[[i]], sep = " "))	
  	
  ########################### Figure 1.b shock in revenue, material cost adjustment  ########################################	
  data_init <- sectors[[i]]	
  ### 1 month scenario	
  data_fig1 <- data_init 	
  	
  ## Simulate 3-month-output-loss	
  data_fig1$GTB_baseline <- data_fig1$gross_tax_base	
  data_fig1$turnover <- data_fig1$turnover - data_fig1$factorloss*(3/12)*(data_fig1$turnover)	
  # Adjust material costs proportionally to the revenue shock	
  data_fig1$costs <- data_fig1$costs - data_fig1$factorloss*(3/12)*(data_fig1$material_inp)	
  # Recalculate profits	
  data_fig1$gross_tax_base <- data_fig1$turnover - data_fig1$costs	
  data_fig1 <- data_fig1 %>% mutate(gross_tax_base = if_else(GTB_baseline < gross_tax_base, GTB_baseline, gross_tax_base)) 	
  # Recalculate profit margins	
  data_fig1 <- data_fig1 %>% mutate(gross_tax_base = if_else(GTB_baseline < gross_tax_base, GTB_baseline, gross_tax_base)) 	
  # Recalculate profit margins	
  data_fig1$profit_margin <- data_fig1$gross_tax_base/data_fig1$turnover 	
  data_fig1$profit_margin[is.nan(data_fig1$profit_margin)] <- NA	
  data_fig1 <- data_fig1 %>% drop_na(profit_margin)	
  data_fig1$profit_margin <- Winsorize(data_fig1$profit_margin, minval = -1, maxval = 1, na.rm = TRUE)	
  	
  ## Save 1-month-Scenario data in an object (profit margin and profit)	
  scenario1 <- select(data_fig1, profit_margin, gross_tax_base, statutory_rate)	
  scenario1$Scenario <- "3 months"	
  	
  ### 5 months scenario	
  data_init <- sectors[[i]]	
  data_fig1 <- data_init 	
  data_fig1$GTB_baseline <- data_fig1$gross_tax_base	
  	
  ## Simulate 5-month-output-loss	
  data_fig1$turnover <- data_fig1$turnover - data_fig1$factorloss*(5/12)*(data_fig1$turnover)	
  # Adjust material costs proportionally to the revenue shock	
  data_fig1$costs <- data_fig1$costs - data_fig1$factorloss*(5/12)*(data_fig1$material_inp)	
  # Recalculate profits	
  data_fig1$gross_tax_base <- data_fig1$turnover - data_fig1$costs	
  data_fig1 <- data_fig1 %>% mutate(gross_tax_base = if_else(GTB_baseline < gross_tax_base, GTB_baseline, gross_tax_base)) 	
  	
  # Recalculate profit margin	
  data_fig1$profit_margin <- data_fig1$gross_tax_base/data_fig1$turnover 	
  data_fig1$profit_margin[is.nan(data_fig1$profit_margin)] <- NA	
  data_fig1 <- data_fig1 %>% drop_na(profit_margin)	
  data_fig1$profit_margin <- Winsorize(data_fig1$profit_margin, minval = -1, maxval = 1, na.rm = TRUE)	
  	
  ## Save 3-month-Scenario data in an object (profit margin and profit)	
  scenario3 <- select(data_fig1, profit_margin, gross_tax_base, statutory_rate)	
  scenario3$Scenario <- "5 months"	
  	
  ####### Combine Baseline, Scenario 1 and 3 in one dataframe 	
  figure_1b <- rbind(scenario0, scenario1, scenario3)	
  figure_1b$Scenario <- factor(figure_1b$Scenario, levels = c("Baseline", "3 months", "5 months"))	
  figure_1_b[[i]] <- figure_1b 	
  	
  print(paste("Figure 1.b", sectors_name[[i]], sep = " "))	
  	
  ########################### Figure 2. shock in revenue, material & labor costs adjustment  ########################################	
  ## We assume that material inputs adjust first, and that firms only cut their wage bill if they are still unprofitable after this adjustment	
  ## Firms will adjust their labor cost differently	
  	
  ## 3 months scenario	
  data_fig2 <- sectors[[i]]	
  	
  ## Simulate 3-month-output-loss, material adjustment and labor adjustement	
  ## R - 3/12fR - c3 - c2 + 3/12fc2 - c1 + x3/12fc1, we want to find x:  	
  # A = R - 3/12R - c3 - c2 + 3/12fc2	
  data_fig2$A <- data_fig2$turnover - data_fig2$factorloss*(3/12)*(data_fig2$turnover) - data_fig2$fixedcost - 	
    (data_fig2$material_inp) + data_fig2$factorloss*(3/12)*(data_fig2$material_inp)	
  summary(data_fig2$A)	
  	
  ## x = factor of labor adjustment in order to break even	
  ## Set equation equal to Target = min {0, baseline profit}	
  data_fig2$Target <- pmin(0, data_fig2$gross_tax_base)	
  summary(data_fig2$Target)	
  # x = (c1 - A + Target) / ((3/12)*f*c1)	
  data_fig2$x <- (data_fig2$labor_inp + data_fig2$Target - data_fig2$A)/(data_fig2$factorloss*(3/12)*data_fig2$labor_inp)	
  	
  ## Some firms have labor input == 0, so create INF and NaN when we divide by 0. We replace x = NA for them.	
  data_fig2$x[is.na(data_fig2$x)] <- NA	
  data_fig2$x[is.infinite(data_fig2$x)] <- NA	
  summary(data_fig2$x)	
  	
  # We now have three cases:	
  # x <= 0: firms are still making profit after the shock and don't need to adjust their payroll	
  data_fig2$x[data_fig2$x < 0] <- 0	
  # 0 < x < 1: firms need to reduce their payroll by the factor x so that then can break even	
  # x >= 1: firms would need (more than) their total monthly payroll to break even	
  data_fig2$x[data_fig2$x > 1] <- 1	
  	
  data_fig2$ratio_labor <- (data_fig2$x*(3/12)*data_fig2$factorloss)	
  	
  summary(data_fig2$ratio_labor)	
  	
  # Recalculate profits: A - c1 + x3/12fc1	
  data_fig2$gross_tax_base <- data_fig2$A - data_fig2$labor_inp + 	
    ((3/12)*data_fig2$labor_inp*data_fig2$x*data_fig2$factorloss)	
  summary(data_fig2$gross_tax_base)	
  	
  data_fig2$gross_tax_base_2 <- data_fig2$A 	
  summary(data_fig2$gross_tax_base)	
  	
  data_fig2$gross_tax_base <- ifelse(is.na(data_fig2$x), data_fig2$gross_tax_base_2, data_fig2$gross_tax_base) 	
  data_fig2$turnover <- data_fig2$turnover - data_fig2$factorloss*(3/12)*(data_fig2$turnover)	
  summary(data_fig2$gross_tax_base)	
  	
  # Recalculate profit margin	
  data_fig2$profit_margin <- data_fig2$gross_tax_base/data_fig2$turnover 	
  data_fig2$profit_margin[is.nan(data_fig2$profit_margin)] <- NA	
  data_fig2 <- data_fig2 %>% drop_na(profit_margin)	
  data_fig2$profit_margin <- Winsorize(data_fig2$profit_margin, minval = -1, maxval = 1, na.rm = TRUE)	
  	
  summary(data_fig2$profit_margin)	
  summary(data_fig2$gross_tax_base)	
  ## Save 1-month-Scenario data in an object	
  Scenario3 <- select(data_fig2, ratio_labor, labor_inp, gross_tax_base, profit_margin, statutory_rate)	
  Scenario3$Scenario <- "3 months"	
  ### 5 months scenario	
  	
  data_fig2 <- sectors[[i]]  	
  	
  #data_fig2$fixedcost <- data_fig2$costs - data_fig2$material_inp - data_fig2$labor_inp	
  	
  ## Simulate 5-month-output-loss, material adjustment and labor adjustement	
  ## R - 5/12fR - c3 - c2 + 5/12fc2 - c1 + x5/12fc1, we want to find x:  	
  	
  # A = R - 5/12fR - c3 - c2 + 5/12fc2	
  data_fig2$A <- data_fig2$turnover - data_fig2$factorloss*(5/12)*(data_fig2$turnover) - data_fig2$fixedcost - 	
    (data_fig2$material_inp) + data_fig2$factorloss*(5/12)*(data_fig2$material_inp)	
  	
  ## x = factor of labor adjustment in order to break even	
  ## Set equation equal to Target = min {0, baseline profit}	
  data_fig2$Target <- pmin(0, data_fig2$gross_tax_base)	
  	
  # x = (c1 - A + Target) / ((5/12)*f*c1)	
  data_fig2$x <- (data_fig2$labor_inp + data_fig2$Target - data_fig2$A)/(data_fig2$factorloss*(5/12)*data_fig2$labor_inp)	
  	
  ## Some firms have labor input == 0, so create INF and NaN when we divide by 0. We replace x = NA for them.	
  data_fig2$x[is.na(data_fig2$x)] <- NA	
  data_fig2$x[is.infinite(data_fig2$x)] <- NA	
  summary(data_fig2$x)	
  	
  # We now have three cases:	
  # x <= 0: firms are still making profit after the shock and don't need to adjust their payroll	
  data_fig2$x[data_fig2$x < 0] <- 0	
  # 0 < x < 1: firms need to reduce their payroll by the factor x so that then can break even	
  # x >= 1: firms would need (more than) their total monthly payroll to break even	
  data_fig2$x[data_fig2$x > 1] <- 1	
  data_fig2$ratio_labor <- (data_fig2$x*(5/12)*data_fig2$factorloss) 	
  summary(data_fig2$ratio_labor)	
  	
  # Recalculate profits: A - c1 + x5/12fc1	
  data_fig2$gross_tax_base <- data_fig2$A - data_fig2$labor_inp + 	
    ((5/12)*data_fig2$labor_inp*data_fig2$x*data_fig2$factorloss)	
  summary(data_fig2$gross_tax_base)	
  	
  data_fig2$gross_tax_base_2 <- data_fig2$A 	
  	
  data_fig2$gross_tax_base <- ifelse(is.na(data_fig2$x), data_fig2$gross_tax_base_2, data_fig2$gross_tax_base) 	
  data_fig2$turnover <- data_fig2$turnover - data_fig2$factorloss*(5/12)*(data_fig2$turnover)	
  	
  # Recalculate profit margin	
  data_fig2$profit_margin <- data_fig2$gross_tax_base/data_fig2$turnover 	
  data_fig2$profit_margin[is.nan(data_fig2$profit_margin)] <- NA	
  data_fig2 <- data_fig2 %>% drop_na(profit_margin)	
  data_fig2$profit_margin <- Winsorize(data_fig2$profit_margin, minval = -1, maxval = 1, na.rm = TRUE)	
  	
  ## Save 5-month-Scenario data in an object	
  Scenario5 <- select(data_fig2, ratio_labor, labor_inp, gross_tax_base, profit_margin, statutory_rate)	
  Scenario5$Scenario <- "5 months"	
  	
  ####### Combine Baseline, Scenario 1 and 3 in one dataframe 	
  figure2 <- rbind(Scenario3, Scenario5)	
  figure2$Scenario <- factor(figure2$Scenario, levels = c("3 months", "5 months"))	
  temp <- select(figure2, gross_tax_base, Scenario)	
  temp$subsidy <- "Material & Labor adj."	
  figure_2_raw[[i]] <- temp 	
  	
  Scenario3 <- select(Scenario3, gross_tax_base, Scenario, profit_margin, statutory_rate)	
  Scenario5 <- select(Scenario5, gross_tax_base, Scenario, profit_margin, statutory_rate)	
  figure1c <- rbind(scenario0, Scenario3, Scenario5)	
  figure1c$Scenario <- factor(figure1c$Scenario, levels = c("Baseline", "3 months", "5 months"))	
  figure_1_c[[i]] <- figure1c 	
  	
  ## Create bins for drop in labor costs	
  # Categories	
  figure2$category <- ""	
  figure2$category[figure2$ratio_labor >= 0 & figure2$ratio_labor < 0.06]  <- "[0-5.9]"	
  figure2$category[figure2$ratio_labor >= 0.06 & figure2$ratio_labor <  0.12]  <- "[6-11.9]"	
  figure2$category[figure2$ratio_labor >= 0.12 & figure2$ratio_labor < 0.18]  <- "[12-17.9]"	
  figure2$category[figure2$ratio_labor >= 0.18 & figure2$ratio_labor < 0.24]  <- "[18-23.9]"	
  figure2$category[figure2$ratio_labor >= 0.24 & figure2$ratio_labor < 0.30]  <- "[24-29.9]"	
  figure2$category[figure2$ratio_labor >= 0.30 & figure2$ratio_labor < 0.36]  <- "[30-35.9]"	
  figure2$category[figure2$ratio_labor >= 0.36 ] <- "[36-41.9["	
  	
  figure2$category <- factor(figure2$category, levels = c("[0-5.9]", "[6-11.9]", "[12-17.9]", "[18-23.9]", "[24-29.9]", "[30-35.9]", "[36-41.9["))	
  	
  figure2 <- figure2 %>% drop_na(category)	
  	
  add_on_1 <- data.frame(category = c("[0-5.9]", "[6-11.9]", "[12-17.9]", "[18-23.9]", "[24-29.9]", "[30-35.9]", "[36-41.9["))	
  add_on_1$ratio_labor <- NA	
  add_on_1$Scenario <- "3 months"	
  add_on_2 <- add_on_1	
  add_on_2$Scenario <- "5 months"	
  figure2 <- select(figure2, category, ratio_labor, Scenario)	
  figure2 <- rbind(figure2, add_on_2, add_on_1)	
  figure_2[[i]] <- figure2	
  print(paste("Figure 2.", sectors_name[[i]], sep = " "))	
  	
  ######### Figure 3. Aggregate loss in payroll, weighed by firm's wagebill, as function of gvt wage subsidy  ########################################	
  ## Same as figure 2 but includes a wage subsidy [0,1]	
  	
  #### FIRST STEP: Evaluate agrgegagate loss at different subsidy rates: 	
  s <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)	
  y_axis <- list()	
  	
  #	
  for (j in 1:length(s)){	
    ### 3 month scenario	
    data_fig2 <- sectors[[i]]	
    	
    #Save baseline data in an object	
    data_fig2$ratio_labor <- NA	
    Scenario0 <- select(data_fig2, ratio_labor, labor_inp, gross_tax_base, statutory_rate)	
    Scenario0$Scenario <- "Baseline"	
    	
    ## Simulate 3-month-output-loss, material adjustment and labor adjustement + subsidy	
    ## R - 3/12fR - c3 - c2 + 3/12fc2 - c1 + (3/12)c1*(1-fx)*s + 3/12fc1x, we want to find x:  	
    	
    # A = R - 3/12fR - c3 - c2 + 3/12fc2 - c1	
    data_fig2$A <- data_fig2$turnover - data_fig2$factorloss*(3/12)*(data_fig2$turnover) - data_fig2$fixedcost - 	
      (data_fig2$material_inp) + data_fig2$factorloss*(3/12)*(data_fig2$material_inp) - data_fig2$labor_inp	
    	
    ## x = factor of labor adjustment in order to break even	
    ## Set equation equal to Target = min {0, baseline profit}	
    data_fig2$Target <- pmin(0, data_fig2$gross_tax_base)	
    	
    # x = (-A - 3/12c1s + T )/((1-s)(3/12)c1f	
    data_fig2$x <- (data_fig2$Target - data_fig2$A - (3/12)*s[[j]]*data_fig2$labor_inp) / ((1-s[[j]])*data_fig2$factorloss*(3/12)*data_fig2$labor_inp)	
    	
    ## Some firms have labor input == 0, so create INF and NaN when we divide by 0. We replace x = NA for them.	
    data_fig2$x[is.na(data_fig2$x)] <- NA	
    data_fig2$x[is.infinite(data_fig2$x)] <- NA	
    summary(data_fig2$x)	
    	
    # We now have three cases:	
    # x <= 0: firms are still making profit after the shock and don't need to adjust their payroll	
    data_fig2$x[data_fig2$x < 0] <- 0	
    # 0 < x < 1: firms need to reduce their payroll by the factor x so that then can break even	
    # x >= 1: firms would need (more than) their total monthly payroll to break even	
    data_fig2$x[data_fig2$x > 1] <- 1	
    	
    # Recalculate profits: R - 3/12fR - c3 - c2 + 3/12fc2 - c1 + (3/12)c1*(1-fx)*s + 3/12fc1x	
    data_fig2$gross_tax_base <- data_fig2$A + 	
      ((3/12)*data_fig2$labor_inp*(1-data_fig2$factorloss)*s[[j]]) +	
      ((3/12)*data_fig2$factorloss*data_fig2$labor_inp)	
    	
    data_fig2$gross_tax_base_2<- data_fig2$A + 	
      ((3/12)*data_fig2$labor_inp*(1-data_fig2$x*data_fig2$factorloss)*s[[j]]) +	
      (data_fig2$x*(3/12)*data_fig2$factorloss*data_fig2$labor_inp)	
    	
    data_fig2$gross_tax_base <- ifelse(is.na(data_fig2$x), data_fig2$gross_tax_base_2, data_fig2$gross_tax_base) 	
    sum(data_fig2$gross_tax_base, na.rm = T)	
    	
    data_fig2$ratio_labor <- (data_fig2$x*(3/12)*data_fig2$factorloss) 	
    	
    ## Save 3-month-Scenario data in an object	
    Scenario3 <- select(data_fig2, ratio_labor, labor_inp, gross_tax_base, statutory_rate)	
    Scenario3$Scenario <- "3 months"	
    	
    	
    ### 5 months scenario	
    data_fig2 <- sectors[[i]] 	
    	
    #data_fig2$fixedcost <- data_fig2$costs - data_fig2$material_inp - data_fig2$labor_inp	
    	
    ## Simulate 5-month-output-loss, material adjustment and labor adjustement + subsidy	
    ## R - 5/12fR - c3 - c2 + 5/12fc2 - c1 + (5/12)c1*(1-fx)*s + 5/12fc1x, we want to find x:  	
    	
    # A = R - 5/12fR - c3 - c2 + 5/12fc2 - c1	
    data_fig2$A <- data_fig2$turnover - data_fig2$factorloss*(5/12)*(data_fig2$turnover) - data_fig2$fixedcost - 	
      (data_fig2$material_inp) + data_fig2$factorloss*(5/12)*(data_fig2$material_inp) - data_fig2$labor_inp	
    	
    ## x = factor of labor adjustment in order to break even	
    ## Set equation equal to Target = min {0, baseline profit}	
    data_fig2$Target <- pmin(0, data_fig2$gross_tax_base)	
    	
    # x = (-A - 5/12c1s + T)/((1-s)(5/12)c1f	
    data_fig2$x <- (data_fig2$Target - data_fig2$A - (5/12)*s[[j]]*data_fig2$labor_inp) / ((1-s[[j]])*data_fig2$factorloss*(5/12)*data_fig2$labor_inp)	
    	
    ## Some firms have labor input == 0, so create INF and NaN when we divide by 0. We replace x = NA for them.	
    data_fig2$x[is.na(data_fig2$x)] <- NA	
    data_fig2$x[is.infinite(data_fig2$x)] <- NA	
    summary(data_fig2$x)	
    	
    # We now have three cases:	
    # x <= 0: firms are still making profit after the shock and don't need to adjust their payroll	
    data_fig2$x[data_fig2$x < 0] <- 0	
    # 0 < x < 1: firms need to reduce their payroll by the factor x so that then can break even	
    # x >= 1: firms would need (more than) their total monthly payroll to break even	
    data_fig2$x[data_fig2$x > 1] <- 1	
    	
    # Formula: ratio = x*5/12*f*c1 / c1	
    data_fig2$ratio_labor <- (data_fig2$x*(5/12)*data_fig2$factorloss) 	
    summary(data_fig2$ratio_labor)	
    	
    # Labor cut 	
    data_fig2$labor_cut <- data_fig2$ratio_labor*data_fig2$labor_inp	
    	
    # Recalculate profits: R - 5/12fR - c3 - c2 + 5/12fc2 - c1 + (5/12)c1*(1-fx)*s + 5/12fc1x	
    data_fig2$gross_tax_base <- data_fig2$A + 	
      ((5/12)*data_fig2$labor_inp*(1-data_fig2$x*data_fig2$factorloss)*s[[j]]) +	
      (data_fig2$x*(5/12)*data_fig2$factorloss*data_fig2$labor_inp)	
    	
    data_fig2$gross_tax_base_2 <- data_fig2$A + 	
      ((5/12)*data_fig2$labor_inp*(1-data_fig2$factorloss)*s[[j]]) +	
      ((5/12)*data_fig2$factorloss*data_fig2$labor_inp)	
    data_fig2$gross_tax_base <- ifelse(is.na(data_fig2$x), data_fig2$gross_tax_base_2, data_fig2$gross_tax_base) 	
    	
    ## Save 5-month-Scenario data in an object	
    Scenario5 <- select(data_fig2, ratio_labor, labor_inp, gross_tax_base, statutory_rate)	
    Scenario5$Scenario <- "5 months"	
    	
    ####### Combine Scenario 1 and 3 in one dataframe 	
    figure2 <- rbind(Scenario0, Scenario3, Scenario5)	
    figure2$Scenario <- factor(figure2$Scenario, levels = c("Baseline", "3 months", "5 months"))	
    	
    # Extract data with subsidy = 0.5 for CIT loss (MNE request)	
    if (s[[j]] == 0.5){	
      figure2_CIT_subsidy[[i]] <- figure2	
    }	
    	
    # Compute aggregate loss in wage bill, weighted by firm's total wage bill 	
    figure2_agg <- figure2[ which(figure2$Scenario != "Baseline"),]	
    figure2_agg <- figure2_agg %>% group_by(Scenario) %>%	
      dplyr::summarize(abs_loss = sum(ratio_labor*labor_inp, na.rm = T),	
                       aggregateloss = 100*sum(ratio_labor*labor_inp, na.rm = T)/sum(labor_inp, na.rm = T))	
    figure2_agg$subsidy <- s[[j]]	
    y_axis[[j]] <- figure2_agg	
  }	
  figure_3[[i]] <- do.call("rbind", y_axis)	
  print(paste("Figure 3.", sectors_name[[i]], sep = " "))
  
  
  } ## End of sectors loop



########################### Figure 4. Esimating exit rate using Panel data ########################################
figure4 <- list()

for (i in 1:length(sector)) {
## Panel data
pdata <- data_1
pdata <- select(pdata, tax_ID, year, turnover, gross_tax_base, covid_categories)
maxyear <- max(pdata$year)
pdata <- pdata.frame(pdata)

## Create exit var (includes temporary exits)
pdata$lead <- lead(pdata$gross_tax_base, 1)
pdata <- as.data.frame(pdata)
pdata$exit[ !is.na(pdata$lead) & pdata$year != maxyear] <- 0
pdata$exit[ is.na(pdata$lead) & pdata$year != maxyear] <- 1
pdata <- pdata[ which(pdata$year != maxyear),]

if (sector[[i]] == "all_sectors"){
  print("No change")
} else {
pdata <-  pdata[ which(pdata$covid_categories == sector[[i]]),]
}
## Create variable negative profit
pdata$negprofit[pdata$gross_tax_base <= 0] <- "Loss-making"
pdata$negprofit[pdata$gross_tax_base > 0] <- "Profit-making"
summary(pdata$exit)
summary(pdata$covid_categories)
## Create table for graph
sumstat_all <- pdata %>% 
  dplyr::summarise(mean_exit = mean(exit, na.rm = TRUE), N_exit = sum(!is.na(exit)), sd_exit = sd(exit, na.rm = TRUE)) 
sumstat_all$negprofit <- "All"
sumstat_negprofit <- pdata %>% group_by(negprofit) %>% dplyr::summarise(mean_exit = mean(exit, na.rm = TRUE), N_exit = sum(!is.na(exit)), sd_exit = sd(exit, na.rm = TRUE))
sumstat <- rbind(sumstat_all, sumstat_negprofit)
sumstat$lb <- sumstat$mean_exit - 1.96*sumstat$sd_exit*(sumstat$N_exit)^(-0.5)
sumstat$ub <- sumstat$mean_exit + 1.96*sumstat$sd_exit*(sumstat$N_exit)^(-0.5)
sumstat$negprofit <- factor(sumstat$negprofit, levels = c("All", "Loss-making", "Profit-making"))
sumstat$sector <- sectors_name[[i]]

#sumstat$survival_proba <- survival_proba
figure4[[i]] <- sumstat

}

figure_4 <- do.call("rbind", figure4)
figure_4$sector <- factor(figure_4$sector, levels = c("High Impact Sectors", "Medium Impact Sectors", "Low Impact Sectors", "All Sectors"))

########################################## Figure 5. Additional exit rate #################
figure5 <- list()

for (i in 1: length(sectors)){
  
  # Share of firms with negative profit at Baseline
  qst <- figure_1_a[[i]][ which(figure_1_a[[i]]$Scenario == "Baseline"),]
  un <-  nrow(qst)
  qst <- qst[ which(qst$gross_tax_base <= 0),]
  deux <-   nrow(qst)
  Baseline <- (deux/un)*100
  Baseline
 
  # Share of firms with negative profit after 3 months (material+labor adjustment)
  qst <- figure_2_raw[[i]][ which(figure_2_raw[[i]]$Scenario == "3 months"),]
  un <-  nrow(qst)
  qst <- qst[ which(qst$gross_tax_base <= 0),]
  deux <- nrow(qst)
  Three <- (deux/un)*100
  Three
  # Share of firms with negative profit after 5 months (material+labor adjustment)
  qst <- figure_2_raw[[i]][ which(figure_2_raw[[i]]$Scenario == "5 months"),]
  un <-  nrow(qst)
  qst <- qst[ which(qst$gross_tax_base <= 0),]
  deux <- nrow(qst)
  Five <- (deux/un)*100
  
  figure5[[i]] <- data.frame("share_negprofit" = c(Baseline, Three, Five), 
               "Scenario" = c("Baseline", "3 months", "5 months"),
               "sector" = sectors_name[[i]])
  
}

figure_5 <- do.call("rbind", figure5)
figure_5$sector <- factor(figure_5$sector, levels = c("High Impact Sectors", "Medium Impact Sectors", "Low Impact Sectors", "All Sectors"))

## Add probability of exit
temp <- select(figure_4, mean_exit, sector, negprofit) 
temp <- temp[ which(temp$negprofit =="Loss-making"),]

figure_5 <- merge(figure_5, temp, by.x = c("sector"), by.y = c("sector"), all = T)
figure_5 <- figure_5 %>% group_by(sector) %>%
  mutate(exit = mean_exit*share_negprofit,
         baseline = exit[Scenario == "Baseline"],
         pct_increase = 100*(exit- baseline)/baseline ,
         additional_exit = (share_negprofit-share_negprofit[Scenario== "Baseline"])*mean_exit) %>% 
  select(-c(negprofit, baseline, exit))

########################################### FUNCTIONS #####################################

bw <- function(b,x) { b/bw.nrd0(x)} ## function to smooth density

## Separate layers/ Baseline graphs
## Get max Y for haromonization of graphs
ymax <- c()

densMode <- function(x){
  td <- density(x)
  madDens <- which.max(td$y)
  return(td$y[madDens]) 
}

for (i in 1:3){
temp <- figure_1_a[[i]][which(figure_1_a[[i]]$Scenario == "Baseline"),]
temp$profit_margin <- temp$profit_margin*100
ymax[i] <- densMode(temp$profit_margin)+0.05
}
y_max <-max(ymax)
### FIGURE 1. a
graph_1_a_fun <- function(dataset, sectors_name){
  plot_title <- sectors_name
  dataset$profit_margin <- dataset$profit_margin*100 ## put profit rate in percentage
  p <- ggplot(dataset, aes(x = profit_margin, fill = Scenario)) + geom_density(aes(y=..density..), alpha= 0.6, color = "black", adjust=bw(2.5, dataset$profit_margin)) 
  p <- p + scale_fill_manual(values = c("Baseline" = "grey", "3 months" = "tomato", "5 months"  = "firebrick3")) 
  p <-  p + geom_vline(aes(xintercept = 0), color = "black", size = 1) 
  p <-  p + scale_x_continuous(limits = c(-50,50)) + scale_y_continuous(limits = c(0, y_max)) 
  print(p + labs(title = plot_title, y = "Density", x = "Profit margin (%)") + theme_minimal() + theme(axis.text = element_text(size = 16), axis.title.x = element_text(size = 16, vjust = -0.5), axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22),
                                                                                                       legend.position= "top", legend.text = element_text(size = 12), legend.title = element_text(size = 12)) + guides(fill = guide_legend("Revenue Loss:")))
}

### FIGURE 1. b
graph_1_b_fun <- function(dataset, sectors_name){
  plot_title <- sectors_name
  dataset$profit_margin <- dataset$profit_margin*100 ## put profit rate in percentage
  p <- ggplot(dataset, aes(x = profit_margin, fill = Scenario)) + geom_density(aes(y=..density..), alpha= 0.6, color = "black", adjust=bw(3.5, dataset$profit_margin)) 
  p <- p + scale_fill_manual(values = c("Baseline" = "grey", "3 months" = "tomato", "5 months" = "firebrick3")) 
  p <-  p + geom_vline(aes(xintercept = 0), color = "black", size = 1) + scale_x_continuous(limits = c(-50,50)) + scale_y_continuous(limits = c(0,y_max)) 
  print(p + labs(title = plot_title, y = "Density", x = "Profit margin (%)") + theme_minimal() + theme(axis.text = element_text(size = 16), axis.title.x = element_text(size = 16, vjust = -0.5), axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22),
                                                                                                       legend.position= "top", legend.text = element_text(size = 12), legend.title = element_text(size = 12)) + guides(fill = guide_legend("Revenue Loss:")))
}  

### FIGURE 1. c	
graph_1_c_fun <- function(dataset, sectors_name){	
  plot_title <- sectors_name	
  dataset$profit_margin <- dataset$profit_margin*100 ## put profit rate in percentage	
  p <- ggplot(dataset, aes(x = profit_margin, fill = Scenario)) + geom_density(aes(y=..density..), alpha= 0.6, color = "black", adjust=bw(3.5, dataset$profit_margin)) 	
  p <- p + scale_fill_manual(values = c("Baseline" = "grey", "3 months" = "tomato", "5 months" = "firebrick3")) 	
  p <-  p + geom_vline(aes(xintercept = 0), color = "black", size = 1) + scale_x_continuous(limits = c(-50,50)) + scale_y_continuous(limits = c(0,y_max)) 	
  print(p + labs(title = plot_title, y = "Density", x = "Profit margin (%)") + theme_minimal() + theme(axis.text = element_text(size = 16), axis.title.x = element_text(size = 16, vjust = -0.5), axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22),	
                                                                                                       legend.position= "top", legend.text = element_text(size = 12), legend.title = element_text(size = 12)) + guides(fill = guide_legend("Revenue Loss:")))	
} 	
graph_1_c_fun(figure_1_b[[2]], sectors_name[[1]])

### FIGURE 2. 
graph_2_fun <- function(dataset, sectors_name){
  #dataset <- dataset[ which(dataset$Scenario == "5 months"),]
  plot_title <- sectors_name
  dataset <- dataset %>% group_by(Scenario, category) %>%
    dplyr::summarise(total = n()) %>% 
    group_by(Scenario) %>%
    dplyr::mutate(count = (total/sum(total))*100)
  p <- ggplot(dataset, aes(x = category, y = count, fill = Scenario)) + 
    geom_bar(position = "dodge", width = 0.9, stat ="identity", color = "black") # Bar graph
  p <- p + scale_fill_manual(values = c("3 months" = "tomato", "5 months" = "firebrick3")) + scale_y_continuous(limits = c(0, 100))
  print(p + labs(title = plot_title, y = "Share of firms", x = "Drop in labor costs compared to baseline (%)") + theme_minimal() + 
          theme(legend.position= "top", axis.text = element_text(size = 14),  axis.title.x = element_text(size = 16, vjust = -0.5), 
                axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22)) + guides(fill = guide_legend("Lockdown time:")))
}
graph_2_fun(figure_2[[1]], sectors_name[[1]])

### FIGURE 3. 
y3_max <- max(figure_3[[1]]$aggregateloss)

graph_3_fun <- function(dataset, sectors_name){
  plot_title <- sectors_name
  dataset <- dataset %>% drop_na(aggregateloss)
  dataset$subsidy <- dataset$subsidy*100
  p <- ggplot(dataset, aes(x = subsidy, y = aggregateloss, color = factor(Scenario))) + 
    geom_line(size = 0.9) 
  p <- p + scale_color_manual(values = c("3 months" = "tomato", "5 months" = "firebrick3")) + scale_x_continuous(breaks = c(0, 30, 60, 90), limits = c(0, 90))
  print(p + labs(title = plot_title, y = "Aggregate loss in sector's payroll (%)", x = "Wage subsidy (%)") + theme_minimal() + scale_y_continuous(limits = c(0, y3_max))+ 
          theme(legend.position= "top", axis.text = element_text(size = 14),  axis.title.x = element_text(size = 16, vjust = -0.5), 
                axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22)) + guides(color = guide_legend("Lockdown time:")))
}
graph_3_fun(figure_3[[1]], sectors_name[[2]])


### FIGURE 4.
graph_4_fun <- function(dataset){
  levels(dataset$sector) <- gsub(" ", "\n", levels(dataset$sector))
  
  plot_title <- ""
  p <- ggplot(dataset, aes(x = sector, y = mean_exit, fill = negprofit, group = negprofit)) +
    geom_bar(position = position_dodge(width = 0.8), width = 0.8, stat ="identity", color = "black") + # Bar graph +
   geom_errorbar(data = dataset, mapping = aes(ymin = lb, ymax = ub), width = 0.3, size = 0.2, position = position_dodge(width = 0.8)) 
  p <- p + scale_fill_manual(values = c( "All" = "grey", "Loss-making" ="orangered2", "Profit-making" = "olivedrab3"))
  print(p + labs(title = plot_title, y = "Probability of exit at t+1", x = "") + theme_minimal() + theme(axis.text = element_text(size = 16), axis.text.x = element_text(size = 16), axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22),
                                                                                                             legend.position= "top", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
                                                                                                             strip.text.x = element_text(size = 10)) + guides(fill = guide_legend("Profit:")))
}
graph_4_fun(figure_4)

### FIGURE 5.
graph_5_fun <- function(dataset){
  levels(dataset$sector) <- gsub(" ", "\n", levels(dataset$sector))
  dataset <- dataset[ which(dataset$Scenario != "Baseline"),]
  plot_title <- ""
  p <- ggplot(dataset, aes(x = sector, y = pct_increase, fill = Scenario)) +
    geom_bar(position = position_dodge(width = 0.8), width = 0.8, stat ="identity", color = "black") # Bar graph
  p <- p + scale_fill_manual(values = c("5 months" ="firebrick3", "3 months" = "tomato"))
  print(p + labs(title = plot_title, y = "Increase in firm exits (%)", x = "") + theme_minimal() + theme(axis.text = element_text(size = 16), axis.text.x = element_text(size = 16), axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22),
                                                                                                         legend.position= "top", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
                                                                                                         strip.text.x = element_text(size = 10)) + guides(fill = guide_legend("Lockdown time:")))
}
graph_5_fun(figure_5)


################################## EXTRACT GRAPHS #################################
setwd(output)
## Table of aggregates
table <- do.call("rbind", table)
write.csv(table, file = "Table_2_overleaf.csv")

## FIGURE 1.a
png("1_a_Graph%02d.png")
all <- mapply(graph_1_a_fun, figure_1_a, sectors_name)
dev.off()

## FIGURE 1.b
png("1_b_Graph%02d.png")
all <- mapply(graph_1_b_fun, figure_1_b, sectors_name)
dev.off()

## FIGURE 1.c	
png("1_c_Graph%02d.png")	
all <- mapply(graph_1_c_fun, figure_1_c, sectors_name)	
dev.off()	

## FIGURE 2.
png("2_Graph%02d.png")
all <- mapply(graph_2_fun, figure_2, sectors_name) 
dev.off()

## FIGURE 3
png("3_Graph%02d.png")
all <- mapply(graph_3_fun, figure_3, sectors_name) 
dev.off()

## FIGURE 4
png("4_Graph%02d.png")
graph_4_fun(figure_4) 
dev.off()

## FIGURE 5
png("5_Graph%02d.png")
graph_5_fun(figure_5) 
dev.off()

################################### EXTRACT METADATA ###############################	
source(paste0(dofiles,"function_metadata.R"))	
## Figure 1 a	
df <- list()	
for (i in 1:length(sectors)){	
  df[[i]] = FUN_share_profitable(figure_1_a[[i]])	
  df[[i]]$Sector <- sectors_name[[i]]	
  df[[i]]$country <- country_code	
}	
metadata_1_a <- do.call("rbind", df)	
write.csv(metadata_1_a, file = paste0("meta_1_a_", country_code, ".csv"))	
## Figure 1 b	
df <- list()	
for (i in 1:length(sectors)){	
  df[[i]] = FUN_share_profitable(figure_1_b[[i]])	
  df[[i]]$Sector <- sectors_name[[i]]	
  df[[i]]$country <- country_code	
}	
metadata_1_b <- do.call("rbind", df)	
write.csv(metadata_1_b, file = paste0("meta_1_b_", country_code, ".csv"))	
## Figure 1 c	
df <- list()	
for (i in 1:length(sectors)){	
  df[[i]] = FUN_share_profitable(figure_1_c[[i]])	
  df[[i]]$Sector <- sectors_name[[i]]	
  df[[i]]$country <- country_code	
}	
metadata_1_c <- do.call("rbind", df)	
write.csv(metadata_1_c, file = paste0("meta_1_c_", country_code, ".csv"))	
## Average distribution of figure 1_c	
dataset <- as.data.frame(figure_1_c[1])	
nb <- seq(from = -1, to = 1 , by= 0.01)	
## Split distribution from -1 to 1 with ho	
dataset <- dataset %>% mutate(bins = NA_real_)	
for (i in 1: length(nb)) {	
  i_plus1 <- nb[[i]]+0.01	
  dataset <- dataset %>%	
    mutate(bins = if_else(profit_margin >= nb[[i]] & profit_margin < i_plus1, nb[[i]], bins))	
}	
dataset <- dataset %>% group_by(Scenario, bins) %>% summarise(N=n())	
dataset$country <- country_code	
write.csv(dataset, file = paste0("meta_1_c_avg", country_code, ".csv"))	
## 2. Loss in CIT revenue	
df <- list()	
for (i in 1:length(sectors)){	
  df[[i]] = FUN_loss_cit(figure_1_b[[i]])	
  df[[i]]$Sector <- sectors_name[[i]]	
  df[[i]]$country <- country_code	
}	
metadata_loss_cit <- do.call("rbind", df)	
write.csv(metadata_loss_cit, file = paste0("meta_2_", country_code, ".csv"))	
## Average drop in labor	
df <- list()	
for (i in 1:length(sectors)){	
  df[[i]] <- FUN_payroll_avgdrop(figure_2[[i]])	
  df[[i]]$Sector <- sectors_name[[i]]	
  df[[i]]$country <- country_code	
}	
meta_avgdrop <- do.call("rbind", df)	
write.csv(meta_avgdrop, file = paste0("meta_3_a_", country_code, ".csv"))	
## Marginal drop in labor (share of firms)	
df <- list()	
for (i in 1:length(sectors)){	
  df[[i]] <- FUN_payroll_marginal(figure_2[[i]])	
  df[[i]]$Sector <- sectors_name[[i]]	
  df[[i]]$country <- country_code	
}	
meta_ratio_labor <- do.call("rbind", df)	
 # All sectors must be a weighted average of the other sectors (max and min differs in each sector)	
temp <- meta_ratio_labor %>% filter(Sector!= "All Sectors") %>%	
                  group_by(Scenario) %>%	
                  summarize(share_margin = 100*sum(N_marginal)/sum(N),	
                            N= sum(N),	
                            N_marginal=sum(N_marginal)) %>%	
  mutate(country = country_code,	
         Sector = "All Sectors")	
meta_ratio_labor <- meta_ratio_labor %>% filter(Sector!= "All Sectors") 	
meta_ratio_labor <- rbind(meta_ratio_labor, temp)	
write.csv(meta_ratio_labor, file = paste0("meta_3_b_", country_code, ".csv"))	
## Wage susbsidy (ratio)	
df <- list()	
for (i in 1:length(sectors)){	
  df[[i]] <- FUN_subsidy(figure_3[[i]])	
  df[[i]]$Sector <- sectors_name[[i]]	
  df[[i]]$country <- country_code	
}	
meta_wagesubsidy <- do.call("rbind", df)	
write.csv(meta_wagesubsidy, file = paste0("meta_4_", country_code, ".csv"))	
## Wage susbsidy 	
df <- list()	
for (i in 1:length(sectors)){	
  df[[i]] <- figure_3[[i]]	
  df[[i]]$Sector <- sectors_name[[i]]	
  df[[i]]$country <- country_code	
}	
meta_wagesubsidy <- do.call("rbind", df)	
write.csv(meta_wagesubsidy, file = paste0("meta_4_b_", country_code, ".csv"))	
## Exit panel A	
figure_4$country <- country_code	
write.csv(figure_4, file = paste0("meta_5_a_", country_code, ".csv"))	
## Exit panel B	
figure_5$country <- country_code	
write.csv(figure_5, file = paste0("meta_5_b_", country_code, ".csv"))	
## Profit margins and cost shares	
write.csv(figure_6_a, file = paste0("meta_6_a_", country_code, ".csv"))	
write.csv(figure_6_b, file = paste0("meta_6_b_", country_code, ".csv"))	
## Figure 7	
figure_7_a$country <- country_code	
figure_7_b$country <- country_code	
write.csv(figure_7_a, file = paste0("meta_7_a_", country_code, ".csv"))	
write.csv(figure_7_b, file = paste0("meta_7_b_", country_code, ".csv"))	
write.csv(aggregate, file = paste0("meta_aggregate_", country_code, ".csv"))

############################################ COMPUTATIONS TABLE 3 #################################
question_3months <- list()
question_5months <- list()

for (i in 1: length(sectors)){
  # Figure 1. b bis
  # Number of firms in average with < profit margin after 1 month
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "Baseline"),]
  un <-  nrow(qst)
  qst <- qst[ which(qst$profit_margin >= 0),]
  deux <- nrow(qst)
  share_profitable0_3 <- (deux/un)*100
  share_profitable0_5 <- (deux/un)*100
  # Number of firms in average with < profit margin after 1 month
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "3 months"),]
  un <-  nrow(qst)
  qst <- qst[ which(qst$profit_margin >= 0),]
  deux <- nrow(qst)
  share_profitable_3 <- (deux/un)*100
  
  # Number of firms in average with 0 < profit margin after 3 months
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "5 months"),]
  un <-  nrow(qst)
  qst <- qst[ which(qst$profit_margin >= 0),]
  deux <- nrow(qst)
  share_profitable_5 <- (deux/un)*100
  
  
  ## Figure 1.bis
   # %loss in tax compared to baseline
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "Baseline"),]
  qst <- qst[ which(qst$gross_tax_base > 0),]
  un <-  sum(qst$gross_tax_base*qst$statutory_rate, na.rm = TRUE)
  summary(qst$gross_tax_base)
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "3 months"),]
  qst <- qst[ which(qst$gross_tax_base > 0),]
  summary(qst$gross_tax_base)
  deux <-  sum(qst$gross_tax_base*qst$statutory_rate, na.rm = TRUE)
  loss1 <- un - deux 
  loss_citrevenue_3 <- (loss1/un)*100
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "5 months"),]
  qst <- qst[ which(qst$gross_tax_base > 0),]
  trois <-  sum(qst$gross_tax_base*qst$statutory_rate, na.rm = TRUE)
  loss2 <- un - trois 
  loss_citrevenue_5 <- (loss2/un)*100
  
  
  # %loss in tax compared to baseline after a 50% wage subsidy	
  qst <- figure2_CIT_subsidy[[i]][ which(figure2_CIT_subsidy[[i]]$Scenario == "Baseline"),]	
  qst <- qst[ which(qst$gross_tax_base > 0),]	
  un <-  sum(qst$gross_tax_base*qst$statutory_rate, na.rm = TRUE)	
  qst <- figure2_CIT_subsidy[[i]][ which(figure2_CIT_subsidy[[i]]$Scenario == "3 months"),]	
    qst <- qst[ which(qst$gross_tax_base > 0),]	
  deux <-  sum(qst$gross_tax_base*qst$statutory_rate, na.rm = TRUE)	
  loss <- un - deux 	
  loss_cit50_3 <- (loss/un)*100	
  qst <- figure2_CIT_subsidy[[i]][ which(figure2_CIT_subsidy[[i]]$Scenario == "5 months"),]	
    qst <- qst[ which(qst$gross_tax_base > 0),]	
  trois <-  sum(qst$gross_tax_base*qst$statutory_rate, na.rm = TRUE)	
  loss <- un - trois 	
  loss_cit50_5 <- (loss/un)*100	
  	
  # increase in absolute vaue of losses (as percentage of GDP)	
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "Baseline"),]	
    qst <- qst[ which(qst$gross_tax_base < 0),]	
  un <-  sum(qst$gross_tax_base, na.rm = TRUE)	
  #	
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "3 months"),]	
    qst <- qst[ which(qst$gross_tax_base < 0),]	
  deux <-  sum(qst$gross_tax_base, na.rm = TRUE)	
  absoluteloss_3 <- 100*(abs(deux) - abs(un))/data_1$GDP_currentLCU[1]	
  absloss_base_3 <- 100*(abs(deux) - abs(un))/abs(un)	
  	
  #	
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "5 months"),]	
    qst <- qst[ which(qst$gross_tax_base < 0),]	
  trois <-  sum(qst$gross_tax_base, na.rm = TRUE)	
  absoluteloss_5 <- 100*(abs(trois) - abs(un))/ data_1$GDP_currentLCU[1]	
  absloss_base_5 <- 100*(abs(trois) - abs(un))/abs(un)	
  
  
  ## Figure 3. Loss in payroll
  qst <- figure_3[[i]][ which(figure_3[[i]]$Scenario == "3 months"),]
  payroll_loss_0_3 <-  qst$aggregateloss[[1]]
  payroll_loss_50_3 <-  qst$aggregateloss[[6]]
  payroll_loss_90_3 <-  qst$aggregateloss[[10]]
  
  qst <- figure_3[[i]][ which(figure_3[[i]]$Scenario == "5 months"),]
  payroll_loss_0_5 <-  qst$aggregateloss[[1]]
  payroll_loss_50_5 <-  qst$aggregateloss[[6]]
  payroll_loss_90_5 <-  qst$aggregateloss[[10]]
  
  ## Figure 5 
  qst <- figure_5[ which(figure_5$Scenario == "3 months"),]
  qst <- qst[ which(qst$sector == sectors_name[[i]]),]
  additional_exit_3 <- qst$pct_increase
  qst <- figure_5[ which(figure_5$Scenario == "5 months"),]
  qst <- qst[ which(qst$sector == sectors_name[[i]]),]
  additional_exit_5 <- qst$pct_increase
  
	  	
  ## Figure 5. permanent loss in turnover and payroll	
  qst <- figure_5[ which(figure_5$Scenario == "3 months"),]	
  qst <- qst[ which(qst$sector == sectors_name[[i]]),]	
  additionalexit_3 <- qst$additional_exit	
  qst <- figure_5[ which(figure_5$Scenario == "5 months"),]	
  qst <- qst[ which(qst$sector == sectors_name[[i]]),]	
  additionalexit_5 <- qst$additional_exit	
  qst$sum_turnover <- sum(sectors[[i]]$turnover, na.rm = T)	
  qst$sum_labor_inp <- sum(sectors[[i]]$labor_inp, na.rm = T)	
  total_loss_payroll_3 <- additionalexit_3*qst$sum_labor_inp[1]/data_1$GDP_currentLCU[1]	
  total_loss_payroll_5 <-additionalexit_5*qst$sum_labor_inp[1]/data_1$GDP_currentLCU[1]	
  total_loss_turnover_3 <-additionalexit_3*qst$sum_turnover[1]/data_1$GDP_currentLCU[1]	
  total_loss_turnover_5 <-additionalexit_5*qst$sum_turnover[1]/data_1$GDP_currentLCU[1]	

  sectorsname <- sectors_name[[i]]	
  question_3months[[i]] <- data.frame(sectorsname, share_profitable0_3, share_profitable_3, loss_citrevenue_3, loss_cit50_3, absoluteloss_3, absloss_base_3,	
                                      payroll_loss_0_3, payroll_loss_50_3, payroll_loss_90_3, additional_exit_3,	
                                      total_loss_payroll_3, total_loss_turnover_3) 	
  question_5months[[i]] <- data.frame(sectorsname, share_profitable0_5, share_profitable_5, loss_citrevenue_5, loss_cit50_5, absoluteloss_5, absloss_base_5,	
                                      payroll_loss_0_5, payroll_loss_50_5, payroll_loss_90_5, additional_exit_5,	
                                      total_loss_payroll_5, total_loss_turnover_5) 
}

qst_3 <- do.call("rbind", question_3months)
qst_5 <- do.call("rbind", question_5months)

qst_3$total_loss_turnover_3[4] <- qst_3$total_loss_turnover_3[1]+qst_3$total_loss_turnover_3[2]+qst_3$total_loss_turnover_3[3]
qst_5$total_loss_turnover_5[4] <- qst_5$total_loss_turnover_5[1]+qst_5$total_loss_turnover_5[2]+qst_5$total_loss_turnover_5[3]
qst_3$total_loss_payroll_3[4] <- qst_3$total_loss_payroll_3[1]+qst_3$total_loss_payroll_3[2]+qst_3$total_loss_payroll_3[3]
qst_5$total_loss_payroll_5[4] <- qst_5$total_loss_payroll_5[1]+qst_5$total_loss_payroll_5[2]+qst_5$total_loss_payroll_5[3]

	## Reshape format 	
rowname <- c("sectors", "share_profitable0", "share_profitable", "loss_citrevenue", "loss_cit50revenue", "absoluteloss", "absloss_base",	
             "payroll_loss_0", "payroll_loss_50", "payroll_loss_90",	
             "additional_exit", "total_loss_payroll", "total_loss_turnover")	
qst_3 <- t(qst_3)	
colnames(qst_3) <- c("High 3months", "Medium 3months", "Low 3 months", "All 3 months")	
rownames(qst_3) <- rowname	
qst_5 <- t(qst_5)	
colnames(qst_5) <- c("High 5months", "Medium 5months", "Low 5 months", "All 5 months")	
rownames(qst_5) <- rowname	
Answers <- merge(qst_3, qst_5, by=0, all=TRUE)	
Answers <- Answers[, c(1,2,6,3,7,4,8,5,9)]	
Answers <- Answers %>% mutate (Row.names = factor(Row.names, levels = rowname)) %>% 	
  arrange(Row.names) 	
Answers <- Answers[-c(1),]	
if (country_code != "MNE"){	
  Answers <- Answers %>% filter(Row.names != "loss_cit50revenue")	
}	
write.csv(Answers, file = "Table_3_overleaf.csv")
