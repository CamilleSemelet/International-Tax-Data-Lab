# 4/22/2020
# Camille Semelet, csemelet@worldbank.org

########## SIMULATION of the COVID 19 impact on formal firms #######


# This Script is written to read already cleanned tax data and analyse the impact of COVID 19 on formal firms
# It produces three figures (five graphs in total) and one table of statistics
# 
# The variables required are the following:
#   (make sure to rename your variable as follow or change them in the Script below):
#   "year": years
#   "section": sector codes (we used ISIC 4)
#   "turnover": annual firm's revenue
#   "gross_tax_base": annual firm's profits(losses), after deductions of costs from turnover
#   "labor_inp": annual costs in labor
#   "material_inp": annual costs in material

##  MAKE SURE TO FILL IN INFORMATION ABOUT YOUR DATA UNTIL LINE 64 
##  THEN RUN THE SCRIPT


############################### Install packages  ##################################
library(plyr) # ddply command
library(ggplot2) # Use for graph
library(tidyr) #Use for drop_na 
library(lattice) # histogram with log
library(expss)
library(DescTools) # Windsorize
library(dplyr) # Various data handling process
library(tibble)
library(ggformula) ## geom_spline

############################### Set up directories  ##################################    
## Paths
output <- " " ## <- FILL IN where you want to store the figures produced
data <- " "  ## <- FILL IN where your data is located

setwd(data) 
data <- read.csv(" ", header = TRUE, sep = ",")  ## <- FILL IN

### Create two additional objects we'll need
country_name <- c(" ") ## <- FILL IN
country_code <- c(" ")  ## <- FILL IN
statutory_rate <- c()  ## <- FILL IN CIT rate of latest year, e.g. 0.30


############################### Create three categories of sectors  ##################################

## CHOOSE WHAT SECTORs GO INTO WHICH IMPACT CATEGORY ##
# Low risk sectors (e.g. Essential retail)
data$sectors <- "low_risk"

# High risk sectors (e.g. Tourism, Transportation, Hotel and food, other services)
data$sectors[data$section == "H" | data$section == "I" | data$section == "S"] <- "high_risk"

# Medium risk sectors (e.g. Education )
data$sectors[ data$section == "P"| data$section == "G"] <- "medium_risk"

## CHOOSE WHAT % SHOCK ON TURNOVER FACED BY EACH SECTOR
factor_loss <- c(1, 2/3, 1/3, 0)

## 1: High Impact sectors losse 100% of output (monthly)
## 2/3: Medium Impact sectors losse 66% of output (monthly)
## 1/3: Low Impact sectors losse 33% of output (monthly)


########################### Graph 0. Baseline ########################################
### We are only considering the latest available year
data_1 <- data[which(data$year == max(data$year)),] 

## Turnover. Keep only firms where turnover is positive
data_1 <- filter(data_1, turnover > 0)

## Drop if gross tax base is missing
data_1 <- data_1 %>% drop_na(gross_tax_base)

## Drop if sector is missing
data_1 <- data_1 %>% drop_na(section)
data_1 <- data_1[ which(data_1$section != ""),] 

## Replace if missing
data_1[ is.na(data_1$labor_inp)] <- 0
data_1[ is.na(data_1$material_inp)] <- 0

## We take three steps to remove outliers: 1) Removes P5 in terms of turnover
q <- quantile(data_1$turnover, probs = c(0.05, 1), na.rm = TRUE)
data_1 <- data_1[ which(data_1$turnover > q[1]),]

## Construct Profit margin 
data_1$profit_margin <- data_1$gross_tax_base/data_1$turnover 
data_1$profit_margin[is.nan(data_1$profit_margin)] <- NA
data_1 <- data_1 %>% drop_na(profit_margin)

## We take three steps to remove outliers: 2) Removes P5 and P95 in terms of profit margin
q <- quantile(data_1$profit_margin, probs = c(0.05, 0.95), na.rm = TRUE)
data_1 <- data_1[ which(data_1$profit_margin > q[1] & data_1$profit_margin < q[2]),]

## We take three steps to remove outliers: 3) Winzorise profit margin at [-1,1]
data_1$profit_margin <- Winsorize(data_1$profit_margin, minval = -1, maxval = 1, na.rm = TRUE)

## Construct share of labor inp and material inp as share of turnover
data_1$share_labor_inp <- 100*data_1$labor_inp/data_1$turnover
data_1$share_material_inp <- 100*data_1$material_inp/data_1$turnover

## Construct costs (sometimes the variable total_cost does not add up perfectly)
data_1$costs <- data_1$turnover - data_1$gross_tax_base 
data_1$fixedcost <- data_1$costs - data_1$material_inp - data_1$labor_inp
data_1$share_fixedcost <- 100*data_1$fixedcost/data_1$turnover

## Store aggregates for revenue and wage bill for the total economy (used later)
aggregate  <- data_1
aggregate <- aggregate %>%
  summarise(wagebill = sum(labor_inp, na.rm = TRUE), 
            revenue = sum(turnover, na.rm = TRUE),
            totalfirms = n() )
aggregate$country <- country_code
aggregate <- select(aggregate, totalfirms, wagebill, revenue, country )

print(paste("Figure 0. Baseline all firms,", country_name, sep = " "))

############################### Initiate Loop over sector categories  ##################################

## Add factors of loss
data_1$factorloss <- 0
data_1$factorloss[data_1$sectors == "high_risk"] <- factor_loss[[1]]
data_1$factorloss[data_1$sectors == "medium_risk"] <- factor_loss[[2]]
data_1$factorloss[data_1$sectors == "low_risk"] <- factor_loss[[3]]

## Create subsets 
high_risk <- data_1[which(data_1$sectors == "high_risk"),] 
medium_risk <- data_1[which(data_1$sectors == "medium_risk"),] 
low_risk <- data_1[which(data_1$sectors == "low_risk"),] 

## LISTS
sectors_name <- c("High Impact Sectors", "Medium Impact Sectors", "Low Impact Sectors", "All Sectors")
sectors <- list(high_risk, medium_risk, low_risk, data_1)
sector <- c("high_risk", "medium_risk", "low_risk", "all_sectors")
table <- list()
figure_1_b <- list()
figure_1_a <- list()
figure_2 <- list()
figure_3 <- list()
figure_4 <- list()
model <- list()

for (i in 1:length(sectors)) {
  
  ###########################  TABLE #########################
  table_data <- sectors[[i]]
  table_data$profit_margin[ table_data$profit_margin < 0 ] <- 0 # Put negative profit margin at 0
  table_data <- table_data %>%
    summarise(nb_firms = n(),
              average_size = mean(turnover, na.rm = T),
              share_payroll = sum(labor_inp, na.rm = T),
              avg_laborinp = mean(share_labor_inp, na.rm = T),
              avg_materialinp = mean(share_material_inp, na.rm = T),
              avg_fixedcost = mean(share_fixedcost, na.rm = T),
              avg_profitmargin = 100*mean(profit_margin, na.rm = T),
              share_turnover = sum(turnover, na.rm = T))
  table_data$share_firms <- (table_data$nb_firms/aggregate$totalfirms[1])*100
  table_data$share_payroll <- (table_data$share_payroll/aggregate$wagebill[[1]])*100
  table_data$share_turnover <- (table_data$share_turnover/aggregate$revenue[[1]])*100
  table_data$Sectors <- sectors_name[[i]]
  table[[i]] <- table_data
  
  ########################### Graph 0. Baseline ########################################
  ### We are only considering the latest available year
  data_1 <- sectors[[i]] 
  
  ## Save the Baseline data in an object (profit margin and profit)
  scenario0 <- select(data_1, profit_margin, gross_tax_base)
  scenario0$Scenario <- "Baseline"
  
  print(paste("Figure 0.bis Baseline,", sectors_name[[i]], sep = " "))
  
  ########################### Figure 1.a shock in revenue, no cost adjustement  ########################################
  data_1 <- sectors[[i]]
  ### 3 month scenario
  data_fig1 <- data_1 
  
  ## Simulate 3-month-output-loss
  data_fig1$turnover <- data_fig1$turnover - data_fig1$factorloss*(3/12)*(data_fig1$turnover)
  # Recalculate profits
  data_fig1$gross_tax_base <- data_fig1$turnover - data_fig1$costs
  # Recalculate profit margin
  data_fig1$profit_margin <- data_fig1$gross_tax_base/data_fig1$turnover 
  data_fig1$profit_margin[is.nan(data_fig1$profit_margin)] <- NA
  data_fig1 <- data_fig1 %>% drop_na(profit_margin)
  data_fig1$profit_margin <- Winsorize(data_fig1$profit_margin, minval = -1, maxval = 1, na.rm = TRUE)
  
  ## Save 3-month-Scenario data in an object (profit margin and profit)
  scenario3 <- select(data_fig1, profit_margin, gross_tax_base)
  scenario3$Scenario <- "3 months"
  
  ### 5 months scenario
  data_1 <- sectors[[i]]
  data_fig1 <- data_1  
  
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
  scenario5 <- select(data_fig1, profit_margin, gross_tax_base)
  scenario5$Scenario <- "5 months"
  
  ####### Combine Baseline, Scenario 1 and 3 in one dataframe 
  figure_1a <- rbind(scenario0, scenario3, scenario5)
  figure_1a$Scenario <- factor(figure_1a$Scenario, levels = c("Baseline", "3 months", "5 months"))
  figure_1_a[[i]] <- figure_1a
  
  print(paste("Figure 1.a", sectors_name[[i]], sep = " "))
  
  ########################### Figure 1.b shock in revenue, material cost adjustment  ########################################
  data_1 <- sectors[[i]]
  ### 1 month scenario
  data_fig1 <- data_1 
  
  ## Simulate 1-month-output-loss
  data_fig1$turnover <- data_fig1$turnover - data_fig1$factorloss*(3/12)*(data_fig1$turnover)
  # Adjust material costs proportionally to the revenue shock
  data_fig1$costs <- data_fig1$costs - data_fig1$factorloss*(3/12)*(data_fig1$material_inp)
  # Recalculate profits
  data_fig1$gross_tax_base <- data_fig1$turnover - data_fig1$costs
  # Recalculate profit margins
  data_fig1$profit_margin <- data_fig1$gross_tax_base/data_fig1$turnover 
  data_fig1$profit_margin[is.nan(data_fig1$profit_margin)] <- NA
  data_fig1 <- data_fig1 %>% drop_na(profit_margin)
  data_fig1$profit_margin <- Winsorize(data_fig1$profit_margin, minval = -1, maxval = 1, na.rm = TRUE)
  
  ## Save 1-month-Scenario data in an object (profit margin and profit)
  scenario1 <- select(data_fig1, profit_margin, gross_tax_base)
  scenario1$Scenario <- "3 months"
  
  ### 3 months scenario
  data_1 <- sectors[[i]]
  data_fig1 <- data_1 
  ## Simulate 3-month-output-loss
  data_fig1$turnover <- data_fig1$turnover - data_fig1$factorloss*(5/12)*(data_fig1$turnover)
  # Adjust material costs proportionally to the revenue shock
  data_fig1$costs <- data_fig1$costs - data_fig1$factorloss*(5/12)*(data_fig1$material_inp)
  # Recalculate profits
  data_fig1$gross_tax_base <- data_fig1$turnover - data_fig1$costs
  # Recalculate profit margin
  data_fig1$profit_margin <- data_fig1$gross_tax_base/data_fig1$turnover 
  data_fig1$profit_margin[is.nan(data_fig1$profit_margin)] <- NA
  data_fig1 <- data_fig1 %>% drop_na(profit_margin)
  data_fig1$profit_margin <- Winsorize(data_fig1$profit_margin, minval = -1, maxval = 1, na.rm = TRUE)
  
  ## Save 3-month-Scenario data in an object (profit margin and profit)
  scenario3 <- select(data_fig1, profit_margin, gross_tax_base)
  scenario3$Scenario <- "5 months"
  
  ####### Combine Baseline, Scenario 1 and 3 in one dataframe 
  figure_1b <- rbind(scenario0, scenario1, scenario3)
  figure_1b$Scenario <- factor(figure_1b$Scenario, levels = c("Baseline", "3 months", "5 months"))
  figure_1_b[[i]] <- figure_1b %>% drop_na(profit_margin)
  
  print(paste("Figure 1.b", sectors_name[[i]], sep = " "))
  
  ########################### Figure 2. shock in revenue, material & labor costs adjustment  ########################################
  ## We assume that material inputs adjust first, and that firms only cut their wage bill if they are still unprofitable after this adjustment
  ## Firms will adjust their labor cost differently
  
  ## 3 months scenario
  data_1 <- sectors[[i]]
  
  data_fig2 <- data_1 %>% drop_na(labor_inp)
  
  ## Remove firms with 0 labor input ?? 
  data_fig2 <- data_fig2[ which(data_fig2$labor_inp != 0),]
  
  data_fig2$fixedcost <- data_fig2$costs - data_fig2$material_inp - data_fig2$labor_inp
  
  ## Simulate 3-month-output-loss, material adjustment and labor adjustement
  ## R - 3/12fR - c3 - c2 + 3/12fc2 - c1 + x3/12fc1, we want to find x:  
  
  # A = R - 3/12R - c3 - c2 + 3/12c2
  data_fig2$A <- data_fig2$turnover - data_fig2$factorloss*(3/12)*(data_fig2$turnover) - data_fig2$fixedcost - 
    (data_fig2$material_inp) + data_fig2$factorloss*(3/12)*(data_fig2$material_inp)
  
  ## x = factor of labor adjustment in order to break even
  ## Set equation equal to Target = min {0, baseline profit}
  data_fig2$Target <- pmin(0, data_fig2$gross_tax_base)
  
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
  ## Save 1-month-Scenario data in an object
  Scenario3 <- select(data_fig2, ratio_labor, labor_inp)
  Scenario3$Scenario <- "3 months"
  
  ### 5 months scenario
  
  data_fig2 <- data_1 %>% drop_na(labor_inp)  
  ## Remove firms with 0 labor input ?? 
  data_fig2 <- data_fig2[ which(data_fig2$labor_inp != 0),]
  
  data_fig2$fixedcost <- data_fig2$costs - data_fig2$material_inp - data_fig2$labor_inp
  
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
  ## Save 5-month-Scenario data in an object
  Scenario5 <- select(data_fig2, ratio_labor, labor_inp)
  Scenario5$Scenario <- "5 months"
  
  ####### Combine Baseline, Scenario 1 and 3 in one dataframe 
  figure2 <- rbind(Scenario3, Scenario5)
  figure2$Scenario <- factor(figure2$Scenario, levels = c("3 months", "5 months"))
  figure2 <- figure2 %>% drop_na(ratio_labor)
  
  # Compute aggregate loss in wage bill, weighted by firm's total wage bill, and average firm loss 
  figure2_aggregate <- figure2 %>% group_by(Scenario) %>%
    summarize(aggregateloss = 100*sum(ratio_labor*labor_inp)/sum(labor_inp), 
              averageloss = mean(ratio_labor, na.rm = TRUE))  
  
  ## Create bins for drop in labor costs
  # Categories
  figure2$category <- ""
  figure2$category[figure2$ratio_labor >= 0 & figure2$ratio_labor < 0.04]  <- "[0-6["
  figure2$category[figure2$ratio_labor >=  0.06 & figure2$ratio_labor <  0.12]  <- "[6-12["
  figure2$category[figure2$ratio_labor >=  0.12 & figure2$ratio_labor < 0.18]  <- "[12-18["
  figure2$category[figure2$ratio_labor >= 0.18 & figure2$ratio_labor < 0.24]  <- "[18-24["
  figure2$category[figure2$ratio_labor >= 0.24 & figure2$ratio_labor < 0.30]  <- "[24-30["
  figure2$category[figure2$ratio_labor >= 0.30 & figure2$ratio_labor < 0.36]  <- "[30-36["
  figure2$category[figure2$ratio_labor >= 0.36 ] <- "[36-42["
  
  figure2$category <- factor(figure2$category, levels = c("[0-6[", "[6-12[", "[12-18[", "[18-24[", "[24-30[", "[30-36[", "[36-42["  ))
  
  figure2 <- figure2 %>% drop_na(category)
  
  add_on_1 <- data.frame(category =  c("[0-6[", "[6-12[", "[12-18[", "[18-24[", "[24-30[", "[30-36[", "[36-42["))
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
    data_1 <- sectors[[i]]
    data_fig2 <- data_1 %>% drop_na(labor_inp)  
    ## Remove firms with 0 labor input ?? 
    data_fig2 <- data_fig2[ which(data_fig2$labor_inp != 0),]
    
    data_fig2$fixedcost <- data_fig2$costs - data_fig2$material_inp - data_fig2$labor_inp
    
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
    
    # Save x density 
    x_distribution <- select(data_fig2, x)
    
    data_fig2$ratio_labor <- (data_fig2$x*(3/12)*data_fig2$factorloss) 
    summary(data_fig2$ratio_labor)
    ## Save 3-month-Scenario data in an object
    Scenario3 <- select(data_fig2, ratio_labor, labor_inp)
    Scenario3$Scenario <- "3 months"
    
    
    ### 5 months scenario
    data_fig2 <- data_1 %>% drop_na(labor_inp)  
    
    data_fig2$fixedcost <- data_fig2$costs - data_fig2$material_inp - data_fig2$labor_inp
    
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
    
    ## Save 5-month-Scenario data in an object
    Scenario5 <- select(data_fig2, ratio_labor, labor_inp)
    Scenario5$Scenario <- "5 months"
    
    ####### Combine Scenario 1 and 3 in one dataframe 
    figure2 <- rbind(Scenario3, Scenario5)
    figure2$Scenario <- factor(figure2$Scenario, levels = c("3 months", "5 months"))
    figure2 <- figure2 %>% drop_na(ratio_labor)
    
    # Compute aggregate loss in wage bill, weighted by firm's total wage bill, and average firm loss 
    figure2_agg <- figure2 %>% group_by(Scenario) %>%
      summarize(aggregateloss = 100*sum(ratio_labor*labor_inp)/sum(labor_inp))
    figure2_agg$subsidy <- s[[j]]
    y_axis[[j]] <- figure2_agg
  }
  figure_3[[i]] <- do.call("rbind", y_axis)
  
  
  ########################### Figure 4. Import data from survival model in stata ########################################
  
  ### Import data from Stata (see survival model stata)
  data_4 <- paste0(output, "\\survival_1_sectors_", country_code, ".csv", sep = "")
  data_fig4  <- read.csv(data_4, header = TRUE, sep = ",")
  data_fig4 <- data_fig4[ which(data_fig4$sectors == sector[[i]]),]
  data_fig4$baseline <- data_fig4$y_hat_1
  data_fig4$moderate <- data_fig4$proba1_shock1030
  data_fig4$large <- data_fig4$proba1_shock3050
  data_fig4$verylarge <- data_fig4$proba1_shock50p
  
  # Confidence Interval
  data_fig4$cipos_moderate <- data_fig4$moderate + 1.96 * (data_fig4$se1_shock1030)
  data_fig4$cineg_moderate <- data_fig4$moderate - 1.96 * (data_fig4$se1_shock1030)
  data_fig4$cipos_large <- data_fig4$large + 1.96 * (data_fig4$se1_shock3050)
  data_fig4$cineg_large <- data_fig4$large - 1.96 * (data_fig4$se1_shock3050)
  data_fig4$cipos_verylarge <- data_fig4$verylarge + 1.96 * (data_fig4$se1_shock50p)
  data_fig4$cineg_verylarge <- data_fig4$verylarge - 1.96 * (data_fig4$se1_shock50p)
  
  ## Select and rename
  data_fig4_0 <- select(data_fig4, baseline, moderate, large, verylarge, sectors)
  data_fig4_0 <- data_fig4_0 %>%
    rename(
      "Baseline" = baseline,
      "Moderate shock" = moderate,
      "Large shock" = large,
      "Very large shock" = verylarge
    )
  
  ## Put in long format to plot by(factor)
  data_fig4_0 <- data_fig4_0  %>% gather(Measure, y_axis, "Baseline":"Very large shock")
  data_fig4_0$Measure <- factor(data_fig4_0$Measure, levels = c("Baseline", "Moderate shock", "Large shock", "Very large shock"))
  
  ## Add Confidence interval
  # Positive CI
  newnames <- c("cipos_moderate", "cipos_large", "cipos_verylarge")
  data_fig4_cipos <- select(data_fig4, cipos_moderate, cipos_large, cipos_verylarge, sectors)
  data_fig4_cipos <- reshape(data_fig4_cipos, direction = "long", varying = newnames,
                             v.names = c("cipos"), idvar="sectors")
  colnames(data_fig4_cipos)[colnames(data_fig4_cipos) == 'time'] <- "Type"
  data_fig4_cipos$Type[data_fig4_cipos$Type == 1] <- "Moderate shock"
  data_fig4_cipos$Type[data_fig4_cipos$Type == 2] <- "Large shock"
  data_fig4_cipos$Type[data_fig4_cipos$Type == 3] <- "Very large shock"
  
  # Negative CI
  newnames <- c("cineg_moderate", "cineg_large", "cineg_verylarge")
  data_fig4_cineg <- select(data_fig4, cineg_moderate, cineg_large, cineg_verylarge, sectors)
  
  data_fig4_cineg <- reshape(data_fig4_cineg, direction = "long", varying = newnames,
                             v.names = c("cineg"), idvar="sectors")
  colnames(data_fig4_cineg)[colnames(data_fig4_cineg) == 'time'] <- "Type"
  data_fig4_cineg$Type[data_fig4_cineg$Type == 1] <- "Moderate shock"
  data_fig4_cineg$Type[data_fig4_cineg$Type == 2] <- "Large shock"
  data_fig4_cineg$Type[data_fig4_cineg$Type == 3] <- "Very large shock"
  
  # Merge back with original data
  data_fig4_0 <- merge(data_fig4_0, data_fig4_cipos, by.x = c("Measure", "sectors"), by.y = c("Type", "sectors"), all = T)
  data_fig4_0 <- merge(data_fig4_0, data_fig4_cineg, by.x = c("Measure", "sectors"), by.y = c("Type", "sectors"), all = T)
  
  # X axis 
  data_fig4_0$x_axis <- c(0, 20, 40, 60) 
  data_fig4_0$y_axis <- data_fig4_0$y_axis*100
  data_fig4_0 <- select(data_fig4_0, y_axis, x_axis, cineg, cipos)
  data_fig4_0$x_axis2 <- data_fig4_0$x_axis^2
  
  ## Get quadratic equation
  model[[i]] <- lm(data_fig4_0$y_axis ~ data_fig4_0$x_axis +  data_fig4_0$x_axis2)
  
  ## Add yearly loss in revenue 
  # 3 months
  data_fig4_0$loss_3months <- factor_loss[[i]]*(3/12)*100
  # 5 months
  data_fig4_0$loss_5months <- factor_loss[[i]]*(5/12)*100
  data_fig4_0$sum_turnover <- sum(sectors[[i]]$turnover, na.rm = T)
  data_fig4_0$sum_labor_inp <- sum(sectors[[i]]$labor_inp, na.rm = T)
  
  figure_4[[i]] <- data_fig4_0
  print(paste("Figure 4.", sectors_name[[i]], sep = " "))
  
}



########################################### FUNCTIONS #####################################

fitted_equation = function(x, model){
  coefs <- coef(model)
  res <- coefs[1] + (coefs[2]*x) + (coefs[3] * x^2)
  return(res)
}

bw <- function(b,x) { b/bw.nrd0(x)} ## function to smooth density

## Separate layers/ Baseline graphs

### FIGURE 1. a
graph_1_a_fun <- function(dataset, sectors_name){
  plot_title <- sectors_name
  dataset$profit_margin <- dataset$profit_margin*100 ## put profit rate in percentage
  p <- ggplot(dataset, aes(x = profit_margin, fill = Scenario)) + geom_density(aes(y=..density..), alpha= 0.6, color = "black", adjust=bw(2.5, dataset$profit_margin)) 
  p <- p + scale_fill_manual(values = c("Baseline" = "grey", "3 months" = "tomato", "5 months"  = "firebrick3")) 
  p <-  p + geom_vline(aes(xintercept = 0), color = "black", size = 1) + scale_x_continuous(limits = c(-50,50)) + scale_y_continuous(limits = c(0,NA)) 
  print(p + labs(title = plot_title, y = "Density", x = "Profit margin (%)") + theme_minimal() + theme(axis.text = element_text(size = 16), axis.title.x = element_text(size = 16, vjust = -0.5), axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22),
                                                                                                       legend.position= "top", legend.text = element_text(size = 12), legend.title = element_text(size = 12)) + guides(fill = guide_legend("Revenue Loss:")))
}
graph_1_a_fun(figure_1_a[[2]], sectors_name[[2]])
### FIGURE 1. b
graph_1_b_fun <- function(dataset, sectors_name){
  plot_title <- sectors_name
  dataset$profit_margin <- dataset$profit_margin*100 ## put profit rate in percentage
  p <- ggplot(dataset, aes(x = profit_margin, fill = Scenario)) + geom_density(aes(y=..density..), alpha= 0.6, color = "black", adjust=bw(2.5, dataset$profit_margin)) 
  p <- p + scale_fill_manual(values = c("Baseline" = "grey", "3 months" = "tomato", "5 months" = "firebrick3")) 
  p <-  p + geom_vline(aes(xintercept = 0), color = "black", size = 1) + scale_x_continuous(limits = c(-50,50)) + scale_y_continuous(limits = c(0,NA)) 
  print(p + labs(title = plot_title, y = "Density", x = "Profit margin (%)") + theme_minimal() + theme(axis.text = element_text(size = 16), axis.title.x = element_text(size = 16, vjust = -0.5), axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22),
                                                                                                       legend.position= "top", legend.text = element_text(size = 12), legend.title = element_text(size = 12)) + guides(fill = guide_legend("Revenue Loss:")))
}  

graph_1_b_fun(figure_1_b[[1]], sectors_name[[1]])
### FIGURE 2. 
graph_2_fun <- function(dataset, sectors_name){
  #dataset <- dataset[ which(dataset$Scenario == "5 months"),]
  plot_title <- sectors_name
  dataset <- dataset %>% group_by(Scenario, category) %>%
    summarise(total = n()) %>% 
    group_by(Scenario) %>%
    mutate(count = (total/sum(total))*100)
  p <- ggplot(dataset, aes(x = category, y = count, fill = Scenario)) + 
    geom_bar(position = "dodge", width = 0.9, stat ="identity", color = "black") # Bar graph
  p <- p + scale_fill_manual(values = c("3 months" = "tomato", "5 months" = "firebrick3")) + scale_y_continuous(limits = c(0, 100))
  print(p + labs(title = plot_title, y = "Share of firms", x = "Drop in labor costs compared to baseline (%)") + theme_minimal() + 
          theme(legend.position= "top", axis.text = element_text(size = 14),  axis.title.x = element_text(size = 16, vjust = -0.5), 
                axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22)) + guides(fill = guide_legend("Lockdown time:")))
}
graph_2_fun(figure_2[[1]], sectors_name[[1]])

### FIGURE 3. 
graph_3_fun <- function(dataset, sectors_name){
  plot_title <- sectors_name
  dataset <- dataset %>% drop_na(aggregateloss)
  dataset$subsidy <- dataset$subsidy*100
  p <- ggplot(dataset, aes(x = subsidy, y = aggregateloss, color = factor(Scenario))) + 
    geom_line(size = 0.9) 
  p <- p + scale_color_manual(values = c("3 months" = "tomato", "5 months" = "firebrick3")) + scale_x_continuous(breaks = c(0, 30, 60, 90), limits = c(0, 90))
  print(p + labs(title = plot_title, y = "Aggregate loss in sector's payroll (%)", x = "Wage subsidy (%)") + theme_minimal() + scale_y_continuous(limits = c(0, 43))+ 
          theme(legend.position= "top", axis.text = element_text(size = 14),  axis.title.x = element_text(size = 16, vjust = -0.5), 
                axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22)) + guides(color = guide_legend("Lockdown time:")))
}
graph_3_fun(figure_3[[1]], sectors_name[[1]])

### FIGURE 4. 
graph_4_1fun <- function(dataset, sectors_name){
  plot_title <- sectors_name
  three <- dataset$loss_3months[1]
  five <- dataset$loss_5months[1]
  min_y <- min(dataset$y_axis)
  p <- ggplot(dataset, aes(x = x_axis, y = y_axis)) + 
    geom_smooth(method = "lm", formula = y~poly(x,2), se = FALSE, color = "black") +
    geom_vline(aes(xintercept = three), color = "tomato", size = 1) +
    geom_vline(aes(xintercept = five), color = "firebrick3", size = 1) +
    geom_vline(aes(xintercept = 0), color = "grey", size = 1) +
    
    ggplot2::annotate("text", three-6, min_y-4 , label= "3 months", color = "tomato") +
    ggplot2::annotate("text", five+6, min_y-4 , label= "5 months", color = "firebrick3")
  print(p + labs(title = plot_title, y = "Exit rate (%)", x = "Yearly output loss (%)") + theme_minimal() + scale_y_continuous( limits = c(0, NA))+ 
          theme(legend.position= "top", axis.text = element_text(size = 14),  axis.title.x = element_text(size = 16, vjust = -0.5), 
                axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22)) + guides(color = guide_legend("Lockdown time:")))
}
graph_4_1fun(figure_4[[1]], sectors_name[[1]])

graph_4_2fun <- function(dataset, sectors_name){
  plot_title <- sectors_name
  three <- dataset$loss_3months[1]
  five <- dataset$loss_5months[1]
  min_y <- min(dataset$y_axis)
  p <- ggplot(dataset, aes(x = x_axis, y = y_axis)) + 
    geom_smooth(method = "lm", formula = y~poly(x,2), se = FALSE, color = "black") +
    geom_vline(aes(xintercept = three), color = "tomato", size = 1) +
    geom_vline(aes(xintercept = five), color = "firebrick3", size = 1) +
    geom_vline(aes(xintercept = 0), color = "grey", size = 1) +
    ggplot2::annotate("text", three-6, min_y-4 , label= "3 months", color = "tomato") +
    ggplot2::annotate("text", five+6, min_y-4 , label= "5 months", color = "firebrick3")
  print(p + labs(title = plot_title, y = "Exit rate (%)", x = "Yearly output loss (%)") + theme_minimal() + scale_y_continuous( limits = c(0, NA))+ 
          theme(legend.position= "top", axis.text = element_text(size = 14),  axis.title.x = element_text(size = 16, vjust = -0.5), 
                axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22)) + guides(color = guide_legend("Lockdown time:")))
}


graph_4_3fun <- function(dataset, sectors_name){
  plot_title <- sectors_name
  three <- dataset$loss_3months[1]
  five <- dataset$loss_5months[1]
  min_y <- min(dataset$y_axis)
  p <- ggplot(dataset, aes(x = x_axis, y = y_axis)) + 
    #geom_line() +
    geom_smooth(method = "lm", formula = y~poly(x,2), se = FALSE, color = "black") +
    geom_vline(aes(xintercept = three), color = "tomato", size = 1) +
    geom_vline(aes(xintercept = five), color = "firebrick3", size = 1) +
    geom_vline(aes(xintercept = 0), color = "grey", size = 1) +
    ggplot2::annotate("text", three-6, min_y-4 , label= "3 months", color = "tomato") +
    ggplot2::annotate("text", five+6, min_y-4 , label= "5 months", color = "firebrick3")
  print(p + labs(title = plot_title, y = "Exit rate (%)", x = "Yearly output loss (%)") + theme_minimal() + scale_y_continuous( limits = c(0, NA))+ 
          theme(legend.position= "top", axis.text = element_text(size = 14),  axis.title.x = element_text(size = 16, vjust = -0.5), 
                axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22)) + guides(color = guide_legend("Lockdown time:")))
}
graph_4_3fun(figure_4[[1]], sectors_name[[1]])
################################## EXTRACT GRAPHS  #################################
setwd(output)
## Table of aggregates
table <- do.call("rbind", table)
write.csv(table, file = "Table_aggregates_2.csv")

## FIGURE 1.a
png("1_a_Graph%02d.png")
all <- mapply(graph_1_a_fun, figure_1_a, sectors_name)
dev.off()

## FIGURE 1.b
png("1_b_Graph%02d.png")
all <- mapply(graph_1_b_fun, figure_1_b, sectors_name)
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
graph_4_1fun(figure_4[[1]], sectors_name[[1]]) 
graph_4_2fun(figure_4[[2]], sectors_name[[2]]) 
graph_4_3fun(figure_4[[3]], sectors_name[[3]]) 

dev.off()


############################################ COMPUTATIONS #################################
questions <- list()
col13 <- c()
col14 <- c()

for (i in 1: length(sectors)){
  # Figure 1. b bis
  # Number of firms in average with < profit margin after 1 month
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "3 months"),]
  un <-  nrow(qst)
  qst <- qst[ which(qst$profit_margin > 0),]
  deux <- nrow(qst)
  threemonths_1_bis <- (deux/un)*100
  
  # Number of firms in average with 0 < profit margin after 3 months
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "5 months"),]
  un <-  nrow(qst)
  qst <- qst[ which(qst$profit_margin > 0),]
  deux <- nrow(qst)
  fivemonths_1_bis <- (deux/un)*100
  
  
  ## Figure 1.bis
  # %loss in tax compared to baseline
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "Baseline"),]
  qst <- qst[ which(qst$gross_tax_base > 0),]
  un <-  sum(qst$gross_tax_base*statutory_rate)
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "3 months"),]
  qst <- qst[ which(qst$gross_tax_base > 0),]
  deux <-  sum(qst$gross_tax_base*statutory_rate)
  loss <- un - deux 
  threemonths_loss_cit <- (loss/un)*100
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "5 months"),]
  qst <- qst[ which(qst$gross_tax_base > 0),]
  trois <-  sum(qst$gross_tax_base*statutory_rate)
  loss <- un - trois 
  fivemonths_loss_cit <- (loss/un)*100
  
  # increase in absolute vaue of losses (as percentage of GDP)
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "Baseline"),]
  qst <- qst[ which(qst$gross_tax_base < 0),]
  un <-  sum(qst$gross_tax_base)
  #
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "3 months"),]
  qst <- qst[ which(qst$gross_tax_base < 0),]
  deux <-  sum(qst$gross_tax_base)
  threemonths_absloss <- 100*(abs(deux) - abs(un))/ data_1$GDP_currentLCU[1]
  #
  qst <- figure_1_b[[i]][ which(figure_1_b[[i]]$Scenario == "5 months"),]
  qst <- qst[ which(qst$gross_tax_base < 0),]
  trois <-  sum(qst$gross_tax_base)
  fivemonths_absloss <- 100*(abs(trois) - abs(un))/ data_1$GDP_currentLCU[1]
  
  ## Figure 3. Loss in payroll
  qst <- figure_3[[i]][ which(figure_3[[i]]$Scenario == "3 months"),]
  threemonths_aggloss_0 <-  qst$aggregateloss[[1]]
  threemonths_aggloss_50 <-  qst$aggregateloss[[6]]
  threemonths_aggloss_90 <-  qst$aggregateloss[[10]]
  
  qst <- figure_3[[i]][ which(figure_3[[i]]$Scenario == "5 months"),]
  fivemonths_aggloss_0 <-  qst$aggregateloss[[1]]
  fivemonths_aggloss_50 <-  qst$aggregateloss[[6]]
  fivemonths_aggloss_90 <-  qst$aggregateloss[[10]]
  
  ## Figure 4 
  qst <- figure_4[[i]]
  zero <- fitted_equation(0, model[[i]])
  trois <- fitted_equation(qst$loss_3months[[1]], model[[i]])
  cinq <- fitted_equation(qst$loss_5months[[1]], model[[i]])
  additional_exit_3 <- trois - zero
  additional_exit_5 <- cinq - zero
  additional_exit_3[additional_exit_3 < 0] <- 0
  additional_exit_5[additional_exit_5 < 0] <- 0
  
  ## Aggregate last figure (weigthed average for all sectors)
  col13[[i]] <- (additional_exit_3*table$share_firms[i])/100
  col14[[i]] <- (additional_exit_5*table$share_firms[i])/100
  
  ## Figure 4. permanent loss in turnover and payroll
  three_loss_payroll <- additional_exit_3*qst$sum_labor_inp[1]/data_1$GDP_currentLCU[1]
  five_loss_payroll <-additional_exit_5*qst$sum_labor_inp[1]/data_1$GDP_currentLCU[1]
  three_loss_turnover <-additional_exit_3*qst$sum_turnover[1]/data_1$GDP_currentLCU[1]
  five_loss_turnover <-additional_exit_5*qst$sum_turnover[1]/data_1$GDP_currentLCU[1]
  
  
  sectorsname <- sectors_name[[i]]
  questions[[i]] <- data.frame(sectorsname, threemonths_1_bis, fivemonths_1_bis, threemonths_loss_cit, 
                               fivemonths_loss_cit, threemonths_absloss, fivemonths_absloss, threemonths_aggloss_0,
                               threemonths_aggloss_50, threemonths_aggloss_90, fivemonths_aggloss_0, 
                               fivemonths_aggloss_50, fivemonths_aggloss_90, additional_exit_3, additional_exit_5,
                               three_loss_turnover, five_loss_turnover, three_loss_payroll, five_loss_payroll) 
}

Answers <- do.call("rbind", questions)

## Aggregate last figure (need to do weigthed average)
rows <- data.frame(sectors_name, col13, col14)
rows <- rows[ which(rows$sectors_name != "All Sectors"), ]
rows <- rows %>% summarise(col13 = mean(col13, na.rm = T), col14 = mean(col14, na.rm = T)) 
Answers$additional_exit_3[4] <- rows$col13[1]
Answers$additional_exit_5[4] <- rows$col14[1]

# Permanent loss in turnover and payroll (sum for all sectors)
Answers$three_loss_turnover[4] <- Answers$three_loss_turnover[1]+Answers$three_loss_turnover[2]+Answers$three_loss_turnover[3]
Answers$five_loss_turnover[4] <- Answers$five_loss_turnover[1]+Answers$five_loss_turnover[2]+Answers$five_loss_turnover[3]
Answers$three_loss_payroll[4] <- Answers$three_loss_payroll[1]+Answers$three_loss_payroll[2]+Answers$three_loss_payroll[3]
Answers$five_loss_payroll[4] <- Answers$five_loss_payroll[1]+Answers$five_loss_payroll[2]+Answers$five_loss_payroll[3]


write.csv(Answers, file = "Table_aggregates_3.csv")