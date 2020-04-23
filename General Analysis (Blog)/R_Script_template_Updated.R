# 4/13/2020
# Author: Camille Semelet, csemelet@worldbank.org 


########## SIMULATION OF COVID 19 IMPACT ON FORMAL FIRMS #######

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

# IMPORTANT: Figure 3 is created based on survival estimates obtained by first running
# the STATA do file "Survival_estimates.do". If you'd like us to provide you the R Script to get these estimates
# let us know by email.

############################### Set up directories  ##################################    
## Paths
output <- " " ## <- FILL IN where you want to store the figures produced
data <- " "  ## <- FILL IN where your data is located

setwd(data) 
data <- read.csv(" ", header = TRUE, sep = ",")  ## <- FILL IN

### Create two additional objects we'll need
country_name <- c(" ") ## <- FILL IN
country_code <- c(" ")  ## <- FILL IN

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

########################### Graph 0. Baseline ########################################
  ### We are only considering the latest available year
  data_1 <- data[which(data$year == max(data$year)),] 

  ## Turnover. Keep only firms where turnover is positive
  data_1 <- filter(data_1, turnover > 0)
  
  ## Drop if gross tax base is missing
  data_1 <- data_1 %>% drop_na(gross_tax_base)
  
  ## Construct costs (sometimes the variable total_cost does not add up perfectly)
  data_1$costs <- data_1$turnover - data_1$gross_tax_base 

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
  
  ## Save the Baseline data in an object (profit margin and profit)
  scenario0 <- select(data_1, profit_margin, gross_tax_base)
  scenario0$Scenario <- "Baseline"
  
  ## Store aggregates for revenue and wage bill for the total economy (used later)
  aggregate  <- data_1
  aggregate <- aggregate %>%
    summarise(wagebill = sum(labor_inp, na.rm = TRUE), 
              revenue = sum(turnover, na.rm = TRUE))
  aggregate$country <- country_code
  aggregate <- select(aggregate, wagebill, revenue, country )
  
  print(paste("Figure 0. Baseline,", country_name, sep = " "))

  ########################### Figure 1.a shock in revenue, no cost adjustement  ########################################
  
  ### 1 month scenario
  data_fig1 <- data_1 
  
  ## Simulate 1-month-output-loss
  data_fig1$turnover <- data_fig1$turnover - (1/12)*(data_fig1$turnover)
  # Recalculate profits
  data_fig1$gross_tax_base <- data_fig1$turnover - data_fig1$costs
  # Recalculate profit margin
  data_fig1$profit_margin <- data_fig1$gross_tax_base/data_fig1$turnover 
  data_fig1$profit_margin[is.nan(data_fig1$profit_margin)] <- NA
  data_fig1 <- data_fig1 %>% drop_na(profit_margin)
  data_fig1$profit_margin <- Winsorize(data_fig1$profit_margin, minval = -1, maxval = 1, na.rm = TRUE)
  
  ## Save 1-month-Scenario data in an object (profit margin and profit)
  scenario1 <- select(data_fig1, profit_margin, gross_tax_base)
  scenario1$Scenario <- "1 month"
  
  ### 3 months scenario
  data_fig1 <- data_1  
  
  ## Simulate 3-month-output-loss
  data_fig1$turnover <- data_fig1$turnover - (3/12)*(data_fig1$turnover)
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
  
  ####### Combine Baseline, Scenario 1 and 3 in one dataframe 
  figure_1a <- rbind(scenario0, scenario1, scenario3)
  figure_1a$Scenario <- factor(figure_1a$Scenario, levels = c("Baseline", "1 month", "3 months"))

  print(paste("Figure 1.a", country_name, sep = " "))
  
  ########################### Figure 1.b shock in revenue, material cost adjustment  ########################################
  
  ### 1 month scenario
  data_fig1 <- data_1 
  
  ## Simulate 1-month-output-loss
  data_fig1$turnover <- data_fig1$turnover - (1/12)*(data_fig1$turnover)
  # Adjust material costs proportionally to the revenue shock
  data_fig1$costs <- data_fig1$costs - (1/12)*(data_fig1$material_inp)
  # Recalculate profits
  data_fig1$gross_tax_base <- data_fig1$turnover - data_fig1$costs
  # Recalculate profit margins
  data_fig1$profit_margin <- data_fig1$gross_tax_base/data_fig1$turnover 
  data_fig1$profit_margin[is.nan(data_fig1$profit_margin)] <- NA
  data_fig1 <- data_fig1 %>% drop_na(profit_margin)
  data_fig1$profit_margin <- Winsorize(data_fig1$profit_margin, minval = -1, maxval = 1, na.rm = TRUE)
  
  ## Save 1-month-Scenario data in an object (profit margin and profit)
  scenario1 <- select(data_fig1, profit_margin, gross_tax_base)
  scenario1$Scenario <- "1 month"
  
  ### 3 months scenario
  data_fig1 <- data_1 
  ## Simulate 3-month-output-loss
  data_fig1$turnover <- data_fig1$turnover - (3/12)*(data_fig1$turnover)
  # Adjust material costs proportionally to the revenue shock
  data_fig1$costs <- data_fig1$costs - (3/12)*(data_fig1$material_inp)
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
  
  ####### Combine Baseline, Scenario 1 and 3 in one dataframe 
  figure_1b <- rbind(scenario0, scenario1, scenario3)
  figure_1b$Scenario <- factor(figure_1b$Scenario, levels = c("Baseline", "1 month", "3 months"))
  figure_1b <- figure_1b %>% drop_na(profit_margin)
 
  print(paste("Figure 1.b", country_name, sep = " "))
  
  ########################### Figure 2. shock in revenue, material & labor costs adjustment  ########################################
  ## We assume that material inputs adjust first, and that firms only cut their wage bill if they are still unprofitable after this adjustment
  ## Firms will adjust their labor cost differently
  
  ## 1 months scenario
  
  data_fig2 <- data_1 %>% drop_na(labor_inp)
  data_fig2$factorloss <- 1 # Percentage of loss
  ## Remove firms with 0 labor input ?? 
  data_fig2 <- data_fig2[ which(data_fig2$labor_inp != 0),]
  
  data_fig2$fixedcost <- data_fig2$costs - data_fig2$material_inp - data_fig2$labor_inp
  
  ## Simulate 1-month-output-loss, material adjustment and labor adjustement
  ## R - 1/12fR - c3 - c2 + 1/12fc2 - c1 + x1/12fc1, we want to find x:  
  
  # A = R - 1/12R - c3 - c2 + 1/12c2
  data_fig2$A <- data_fig2$turnover - data_fig2$factorloss*(1/12)*(data_fig2$turnover) - data_fig2$fixedcost - 
    (data_fig2$material_inp) + data_fig2$factorloss*(1/12)*(data_fig2$material_inp)
  
  ## x = factor of labor adjustment in order to break even
  ## Set equation equal to Target = min {0, baseline profit}
  data_fig2$Target <- pmin(0, data_fig2$gross_tax_base)
  
  # x = (c1 - A + Target) / ((1/12)*f*c1)
  data_fig2$x <- (data_fig2$labor_inp + data_fig2$Target - data_fig2$A)/(data_fig2$factorloss*(1/12)*data_fig2$labor_inp)
  
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
  
  data_fig2$ratio_labor <- (data_fig2$x*(1/12)*data_fig2$factorloss)
  
  summary(data_fig2$ratio_labor)
  ## Save 1-month-Scenario data in an object
  Scenario1 <- select(data_fig2, ratio_labor, labor_inp)
  Scenario1$Scenario <- "1 month"
  
  ### 3 months scenario
  
  data_fig2 <- data_1 %>% drop_na(labor_inp)  
  data_fig2$factorloss <- 1 # Percentage of loss
  
  ## Remove firms with 0 labor input ?? 
  data_fig2 <- data_fig2[ which(data_fig2$labor_inp != 0),]
  
  data_fig2$fixedcost <- data_fig2$costs - data_fig2$material_inp - data_fig2$labor_inp
  
  ## Simulate 3-month-output-loss, material adjustment and labor adjustement
  ## R - 3/12fR - c3 - c2 + 3/12fc2 - c1 + x3/12fc1, we want to find x:  
  
  # A = R - 3/12fR - c3 - c2 + 3/12fc2
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
  ## Save 3-month-Scenario data in an object
  Scenario3 <- select(data_fig2, ratio_labor, labor_inp)
  Scenario3$Scenario <- "3 months"
  
  ####### Combine Baseline, Scenario 1 and 3 in one dataframe 
  figure2 <- rbind(Scenario1, Scenario3)
  figure2$Scenario <- factor(figure2$Scenario, levels = c("1 month", "3 months"))
  figure2 <- figure2 %>% drop_na(ratio_labor)
  
  # Compute aggregate loss in wage bill, weighted by firm's total wage bill, and average firm loss 
  figure2_aggregate <- figure2 %>% group_by(Scenario) %>%
    summarize(aggregateloss = 100*sum(ratio_labor*labor_inp)/sum(labor_inp), 
              averageloss = mean(ratio_labor, na.rm = TRUE))  
  
  ## Create bins for drop in labor costs
  # Categories
  figure2$category <- ""
  figure2$category[figure2$ratio_labor >= 0 & figure2$ratio_labor < 0.04]  <- "[0-4["
  figure2$category[figure2$ratio_labor >=  0.04 & figure2$ratio_labor <  0.08]  <- "[4-8["
  figure2$category[figure2$ratio_labor >=  0.08 & figure2$ratio_labor < 0.12]  <- "[8-12["
  figure2$category[figure2$ratio_labor >= 0.12 & figure2$ratio_labor < 0.16]  <- "[12-16["
  figure2$category[figure2$ratio_labor >= 0.16 & figure2$ratio_labor < 0.20]  <- "[16-20["
  figure2$category[figure2$ratio_labor >= 0.20 & figure2$ratio_labor < 0.24]  <- "[20-24["
  figure2$category[figure2$ratio_labor >= 0.24 ] <- "[24-28["
  
  figure2 <- figure2 %>% group_by(Scenario, category) %>%
    summarise(total = n()) %>%
    group_by(Scenario) %>%
    mutate(count = total/sum(total)) 
  
  figure2$category <- factor(figure2$category, levels = c("[0-4[", "[4-8[", "[8-12[", "[12-16[", "[16-20[", "[20-24[", "[24-28["  ))
  figure_2 <- figure2
  
  print(paste("Figure 2.", country_name, sep = " "))
  
  ########################### Figure 3. Import data from survival model in stata ########################################
  
  # Import data from Stata (see survival model stata)
  data_fig3  <- read.csv(paste0(output, "\\survival_estimates.csv"), header = TRUE, sep = ",")
  data_fig3$baseline <- data_fig3$y_hat_1
  data_fig3$moderate <- data_fig3$proba1_shock1030
  data_fig3$large <- data_fig3$proba1_shock3050
  data_fig3$verylarge <- data_fig3$proba1_shock50p
  
  # Confidence Interval
  data_fig3$cipos_moderate <- data_fig3$moderate + 1.96 * (data_fig3$se1_shock1030)
  data_fig3$cineg_moderate <- data_fig3$moderate - 1.96 * (data_fig3$se1_shock1030)
  data_fig3$cipos_large <- data_fig3$large + 1.96 * (data_fig3$se1_shock3050)
  data_fig3$cineg_large <- data_fig3$large - 1.96 * (data_fig3$se1_shock3050)
  data_fig3$cipos_verylarge <- data_fig3$verylarge + 1.96 * (data_fig3$se1_shock50p)
  data_fig3$cineg_verylarge <- data_fig3$verylarge - 1.96 * (data_fig3$se1_shock50p)
  
  ## Select and rename
  data_fig3_0 <- select(data_fig3, baseline, moderate, large, verylarge, country)
  data_fig3_0 <- data_fig3_0 %>% 
    rename(
      "Baseline" = baseline, 
      "1-3 months" = moderate,
      "3-6 months" = large,
      "More than 6months" = verylarge
    )
  
  ## Put in long format to plot by(factor)
  data_fig3_0 <- data_fig3_0  %>% gather(Measure, value, "Baseline":"More than 6months") 
  data_fig3_0$Measure <- factor(data_fig3_0$Measure, levels = c("Baseline", "1-3 months", "3-6 months", "More than 6months"))
  
  ## Add Confidence interval
  # Positive CI
  newnames <- c("cipos_moderate", "cipos_large", "cipos_verylarge")
  data_fig3_cipos <- select(data_fig3, cipos_moderate, cipos_large, cipos_verylarge, country)
  data_fig3_cipos <- reshape(data_fig3_cipos, direction = "long", varying = newnames,
                            v.names = c("cipos"), idvar="country")         
  colnames(data_fig3_cipos)[colnames(data_fig3_cipos) == 'time'] <- "Type"
  data_fig3_cipos$Type[data_fig3_cipos$Type == 1] <- "1-3 months"
  data_fig3_cipos$Type[data_fig3_cipos$Type == 2] <- "3-6 months"
  data_fig3_cipos$Type[data_fig3_cipos$Type == 3] <- "More than 6months"
  
  # Negative CI
  newnames <- c("cineg_moderate", "cineg_large", "cineg_verylarge")
  data_fig3_cineg <- select(data_fig3, cineg_moderate, cineg_large, cineg_verylarge, country)
  
  data_fig3_cineg <- reshape(data_fig3_cineg, direction = "long", varying = newnames, 
                            v.names = c("cineg"), idvar="country")          
  colnames(data_fig3_cineg)[colnames(data_fig3_cineg) == 'time'] <- "Type"
  data_fig3_cineg$Type[data_fig3_cineg$Type == 1] <- "1-3 months"
  data_fig3_cineg$Type[data_fig3_cineg$Type == 2] <- "3-6 months"
  data_fig3_cineg$Type[data_fig3_cineg$Type == 3] <- "More than 6months"
  
  # Merge back with original data
  data_fig3_0 <- merge(data_fig3_0, data_fig3_cipos, by.x = c("Measure", "country"), by.y = c("Type", "country"), all = T)
  data_fig3_0 <- merge(data_fig3_0, data_fig3_cineg, by.x = c("Measure", "country"), by.y = c("Type", "country"), all = T)

  figure_3 <- data_fig3_0
  print(paste("Figure 3.", country_name, sep = " "))
  

########################################### FUNCTIONS #####################################

bw <- function(b,x) { b/bw.nrd0(x)} ## function to smooth density

## Separate layers/ Baseline graphs

### FIGURE 1. a
graph_1_a_fun <- function(dataset, name){
  plot_title <- name
  dataset$profit_margin <- dataset$profit_margin*100 ## put profit rate in percentage
  p <- ggplot(dataset, aes(x = profit_margin, fill = Scenario)) + geom_density(aes(y=..density..), alpha= 0.6, color = "black", adjust=bw(1.5, dataset$profit_margin)) 
  p <- p + scale_fill_manual(values = c("Baseline" = "grey","1 month" = "tomato", "3 months" = "firebrick3")) 
  p <-  p + geom_vline(aes(xintercept = 0), color = "black", size = 1) + scale_x_continuous(limits = c(-50,50))  
  print(p + labs( y = "Density", x = "Profit margin (%)") + theme_minimal() + theme(axis.text = element_text(size = 16), axis.title.x = element_text(size = 16, vjust = -0.5), axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22),
                                                                                    legend.position= "top", legend.text = element_text(size = 12), legend.title = element_text(size = 12)) + guides(fill = guide_legend("Revenue Loss:")))
}

### FIGURE 1. b
graph_1_b_fun <- function(dataset, name){
  plot_title <- name
  dataset$profit_margin <- dataset$profit_margin*100 ## put profit rate in percentage
  p <- ggplot(dataset, aes(x = profit_margin, fill = Scenario)) + geom_density(aes(y=..density..), alpha= 0.6, color = "black", adjust=bw(1.5, dataset$profit_margin)) 
  p <- p + scale_fill_manual(values = c("Baseline" = "grey", "1 month" = "tomato", "3 months" = "firebrick3")) 
  p <-  p + geom_vline(aes(xintercept = 0), color = "black", size = 1) + scale_x_continuous(limits = c(-50,50)) 
  print(p + labs( y = "Density", x = "Profit margin (%)") + theme_minimal() + theme(axis.text = element_text(size = 16), axis.title.x = element_text(size = 16, vjust = -0.5), axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22),
                                                                                    legend.position= "top", legend.text = element_text(size = 12), legend.title = element_text(size = 12)) + guides(fill = guide_legend("Revenue Loss:")))
}  

### FIGURE 2. 
# Panel a. 1 month
graph_2_a_fun <- function(dataset, country_name){
  dataset <- dataset[ which(dataset$Scenario == "1 month"),]
  dataset$count <-   dataset$count*100
  plot_title <- country_name
  p <- ggplot(dataset, aes(x = category, y = count, fill = factor(Scenario))) +  geom_bar(stat = "identity", color = "black") # Bar graph
  p <- p + scale_fill_manual(values = c("1 month" = "tomato", "3 months" = "firebrick3")) + scale_y_continuous(limits = c(0, 95))
  print(p + labs( y = "Share of firms", x = "Drop in labor costs compared to baseline (%)") + theme_minimal() + 
          theme(legend.position= "none", axis.text = element_text(size = 14),  axis.title.x = element_text(size = 16, vjust = -0.5), axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22)))
}
# Panel b. 3 months
graph_2_b_fun <- function(dataset, country_name){
  dataset <- dataset[ which(dataset$Scenario == "3 months"),]
  dataset$count <-   dataset$count*100
  plot_title <- country_name
  p <- ggplot(dataset, aes(x = category, y = count, fill = factor(Scenario))) +  geom_bar(stat = "identity", color = "black") # Bar graph
  p <- p + scale_fill_manual(values = c( "1 month" = "tomato", "3 months" = "firebrick3")) + scale_y_continuous(limits = c(0,95))
  print(p + labs( y = "Share of firms", x = "Drop in labor costs compared to baseline (%)") + theme_minimal() + 
          theme(legend.position= "none", axis.text = element_text(size = 14),  axis.title.x = element_text(size = 16, vjust = -0.5), axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22)))
}

### FIGURE 3. 
graph_3_fun <- function(dataset, country_name){
  levels(dataset$Measure) <- gsub(" ", "\n", levels(dataset$Measure))
  plot_title <- country_name
  dataset$value <- dataset$value*100 
  dataset$cipos <- dataset$cipos*100 
  dataset$cineg <- dataset$cineg*100 
  p <- ggplot(dataset, aes(x = Measure, y = value, fill = Measure)) +  geom_bar(stat = "identity", width = 0.8) +
    geom_errorbar(data = dataset, mapping = aes(x = Measure, ymin = cineg, ymax = cipos), width = 0.3, size = 0.2) 
  p <- p + scale_fill_manual(values = c( "Baseline" = "grey", "1-3\nmonths" ="tomato", "3-6\nmonths" = "firebrick3", "More\nthan\n6months" = "brown4")) + scale_y_continuous(limits = c(0, NA)) 
  print(p + labs(y = "Share of firms exiting", x = "") + theme_minimal() + theme(legend.position = "none", axis.text = element_text(size = 14),  axis.title.x = element_text(size = 16, vjust = -0.5), axis.title.y = element_text(size = 16, margin = margin(r = 10)), plot.title = element_text(size = 22)))                                                                                 
}

################################## EXTRACT GRAPHS  #################################
setwd(output)

## FIGURE 1.a
png("1_a_Graph%02d.png")
graph_1_a_fun(figure_1a, country_name)
dev.off()

## FIGURE 1.b
png("1_b_Graph%02d.png")
graph_1_b_fun(figure_1b, country_name)
dev.off()

## FIGURE 2.a
png("2_a_Graph%02d.png")
graph_2_a_fun(figure_2, country_name) 
dev.off()

## FIGURE 2.b
png("2_b_Graph%02d.png")
graph_2_b_fun(figure_2, country_name) 
dev.off()

## FIGURE 3
png("3_Graph%02d.png")
graph_3_fun(figure_3, country_name) 
dev.off()

