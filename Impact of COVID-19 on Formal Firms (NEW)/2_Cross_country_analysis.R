## COVID 19 - CROSS COUNTRY PAPER

# Uses another Script to rank countries by GDP pc order
# All the metadata used is produced by the script Country_specific_analysis.R

## Packages
rm(list=ls()) 
gc()
list.of.packages <- c("tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(tidyverse) 
library(reshape)
library(ggformula) ## spline
library(haven) ## load .dta

##
gitHub <- "C:\\Users\\s551964\\Cross-Country"   
output <- paste0(gitHub, "\\COVID19\\output\\Paper\\FINAL")
dofiles <-  paste0(gitHub, "\\COVID19\\dofiles\\")
## 
source(paste0(dofiles, "Ranking_GDP_pc.R"))
##
country_name <-c("Albania", "Costa Rica", "Ecuador", "Ethiopia", "Guatemala", "Montenegro", "Uganda", "Senegal", "Eswatini")  #  We use this in many parts of the code
country_code <- c("ALB", "CRI", "ECU",  "ETH", "GTM", "MNE", "UGA", "SEN", "SWZ", "RWA") 
##
data <- list()

########################### CONSTRUCTION GRAPHS ################################### 
#### GRAPH 1 ####
## Graph 1 a _ firms profitability
files <- paste0("meta_1_a_", country_code, ".csv", sep = "")  
for (i in 1: length(files)){
  directory <- paste0(gitHub, "\\COVID19\\output\\", country_code[i], sep = "") 
  setwd(directory)
  data[[i]]  <- read.csv(files[i], header = TRUE, sep = ",")
}
meta_1_a <- do.call("rbind", data)
meta_1_a <- merge(meta_1_a, GDP_ranks, by.x = c("country"), by.y = c("country_code"))
meta_1_a <- meta_1_a %>% select(country, rank_incr, Scenario, Sector, profitable_firms) 
levels(meta_1_a$country)[levels(meta_1_a$country) =="SWZ"] <- "ESW"
levels(meta_1_a$Scenario)[levels(meta_1_a$Scenario) =="3 months"] <- "3 month-lockdown"
levels(meta_1_a$Scenario)[levels(meta_1_a$Scenario) =="5 months"] <- "5 month-lockdown"
meta_1_a <- meta_1_a %>% filter(Sector == "All Sectors")
meta_1_a$Scenario <- factor(meta_1_a$Scenario, levels = c("Baseline", "3 month-lockdown", "5 month-lockdown"))


## Graph 1 b _ firms profitability (material adjustment)
data <- list()
files <- paste0("meta_1_b_", country_code, ".csv", sep = "")  
for (i in 1: length(files)){
  directory <- paste0(gitHub, "\\COVID19\\output\\", country_code[i], sep = "") 
  setwd(directory)
  data[[i]]  <- read.csv(files[i], header = TRUE, sep = ",")
}
meta_1_b <- do.call("rbind", data)
meta_1_b <- merge(meta_1_b, GDP_ranks, by.x = c("country"), by.y = c("country_code"))
meta_1_b <- meta_1_b %>% select(country, rank_incr, Scenario, Sector, profitable_firms) 
levels(meta_1_b$country)[levels(meta_1_b$country) =="SWZ"] <- "ESW"
meta_1_b <- meta_1_b %>% filter(Sector == "All Sectors")
levels(meta_1_b$Scenario)[levels(meta_1_b$Scenario) =="3 months"] <- "3 month-lockdown"
levels(meta_1_b$Scenario)[levels(meta_1_b$Scenario) =="5 months"] <- "5 month-lockdown"
meta_1_b$Scenario <- factor(meta_1_b$Scenario, levels = c("Baseline", "3 month-lockdown", "5 month-lockdown"))

## Graph 1 c _ firms profitability (material adjustment & labor) distribution
data <- list()
files <- paste0("meta_1_c_avg", country_code, ".csv", sep = "")  
for (i in 1: length(files)){
  directory <- paste0(gitHub, "\\COVID19\\output\\", country_code[i], sep = "") 
  setwd(directory)
  data[[i]]  <- read.csv(files[i], header = TRUE, sep = ",")
}
meta_1_c <- do.call("rbind", data)
meta_1_c <- meta_1_c %>% group_by(Scenario, bins) %>%
  summarize(N = mean(N, n.arm = T) )
levels(meta_1_c$Scenario)[levels(meta_1_c$Scenario) =="3 months"] <- "3 month-lockdown"
levels(meta_1_c$Scenario)[levels(meta_1_c$Scenario) =="5 months"] <- "5 month-lockdown"
meta_1_c$Scenario <- factor(meta_1_c$Scenario, levels = c("Baseline", "3 month-lockdown", "5 month-lockdown"))


#### GRAPH 2 ####
## Graph 2 a _ Average drop in payroll 
data <- list()
files <- paste0("meta_3_a_", country_code, ".csv", sep = "")  
for (i in 1: length(files)){
  directory <- paste0(gitHub, "\\COVID19\\output\\", country_code[i], sep = "") 
  setwd(directory)
  data[[i]]  <- read.csv(files[i], header = TRUE, sep = ",")
}
meta_2_a <- do.call("rbind", data)
meta_2_a <- merge(meta_2_a, GDP_ranks, by.x = c("country"), by.y = c("country_code"))
meta_2_a <- meta_2_a %>% select(country, log_GDP_pc, Sector, Scenario, avg_drop) %>%
  mutate(avg_drop = avg_drop*100)
levels(meta_2_a$Scenario)[levels(meta_2_a$Scenario) =="3 months"] <- "3 month-lockdown"
levels(meta_2_a$Scenario)[levels(meta_2_a$Scenario) =="5 months"] <- "5 month-lockdown"
meta_2_a$Scenario <- factor(meta_2_a$Scenario, levels = c("3 month-lockdown", "5 month-lockdown"))
meta_2_a <- meta_2_a %>% filter(Sector == "All Sectors")

payroll_avgdrop <- meta_2_a %>% select(Scenario, country, avg_drop) %>% t() %>% as.data.frame() 
setwd(output)
write.csv(payroll_avgdrop, file = "Appendix_payrollavg.csv")

## Graph 2 b _ Exit rate
data <- list()
files <- paste0("meta_5_a_", country_code, ".csv", sep = "")  
for (i in 1: length(files)){
  directory <- paste0(gitHub, "\\COVID19\\output\\", country_code[i], sep = "") 
  setwd(directory)
  data[[i]]  <- read.csv(files[i], header = TRUE, sep = ",")
}
meta_2_b <- do.call("rbind", data) 
meta_2_b <- merge(meta_2_b, GDP_ranks, by.x = c("country"), by.y = c("country_code"))
meta_2_b <- meta_2_b %>% select(country, log_GDP_pc, sector, negprofit, mean_exit) %>%
  filter(country != "GHA" ) ## Ghana does not have panel data
levels(meta_2_b$negprofit)[levels(meta_2_b$negprofit) =="Loss-making"] <- "Loss-making firms"
levels(meta_2_b$negprofit)[levels(meta_2_b$negprofit) =="Profit-making"] <- "Profit-making firms"
meta_2_b$negprofit <- factor(meta_2_b$negprofit, levels = c("Loss-making firms", "All", "Profit-making firms"))
meta_2_b <- meta_2_b %>% filter(sector == "All Sectors")

## Graph 2 c _ Exit rate
data <- list()
files <- paste0("meta_5_b_", country_code, ".csv", sep = "")  
for (i in 1: length(files)){
  directory <- paste0(gitHub, "\\COVID19\\output\\", country_code[i], sep = "") 
  setwd(directory)
  data[[i]]  <- read.csv(files[i], header = TRUE, sep = ",")
}
meta_2_c <- do.call("rbind", data) 
meta_2_c <- merge(meta_2_c, GDP_ranks, by.x = c("country"), by.y = c("country_code"))
meta_2_c <- meta_2_c %>% select(country, log_GDP_pc, Scenario, sector, pct_increase, additional_exit) %>%
  filter(country != "GHA" & Scenario!="Baseline")  ## Ghana does not have panel data
levels(meta_2_c$Scenario)[levels(meta_2_c$Scenario) =="3 months"] <- "3 month-lockdown"
levels(meta_2_c$Scenario)[levels(meta_2_c$Scenario) =="5 months"] <- "5 month-lockdown"
meta_2_c$Scenario <- factor(meta_2_c$Scenario, levels = c("3 month-lockdown", "5 month-lockdown"))
meta_2_c <- meta_2_c %>% filter(sector == "All Sectors")


#### GRAPH 3 ####
## Graph 3 A. 
data <- list()
files <- paste0("meta_4_b_", country_code, ".csv", sep = "")  
for (i in 1: length(files)){
  directory <- paste0(gitHub, "\\COVID19\\output\\", country_code[i], sep = "") 
  setwd(directory)
  data[[i]]  <- read.csv(files[i], header = TRUE, sep = ",")
}
meta_3_a <- do.call("rbind", data)
meta_3_a <- meta_3_a %>% group_by(Sector, Scenario, subsidy) %>% 
  summarise(avg_aggloss = mean(aggregateloss, na.rm = T)) 
meta_3_a <- meta_3_a %>% select(Sector, Scenario, avg_aggloss, subsidy)
levels(meta_3_a$Scenario)[levels(meta_3_a$Scenario) =="3 months"] <- "3 month-lockdown"
levels(meta_3_a$Scenario)[levels(meta_3_a$Scenario) =="5 months"] <- "5 month-lockdown"
meta_3_a$Scenario <- factor(meta_3_a$Scenario, levels = c("3 month-lockdown", "5 month-lockdown"))

## Graph 3 b _ Aggregate loss in payroll with/without subsidy
data <- list()
files <- paste0("meta_4_", country_code, ".csv", sep = "")  
for (i in 1: length(files)){
  directory <- paste0(gitHub, "\\COVID19\\output\\", country_code[i], sep = "") 
  setwd(directory)
  data[[i]]  <- read.csv(files[i], header = TRUE, sep = ",")
}
meta_3_b <- do.call("rbind", data)
meta_3_b <- merge(meta_3_b, GDP_ranks, by.x = c("country"), by.y = c("country_code"))
meta_3_b <- meta_3_b %>% select(country, log_GDP_pc, Sector, Scenario, diff_aggloss_50, diff_aggloss_90)
levels(meta_3_b$Scenario)[levels(meta_3_b$Scenario) =="3 months"] <- "3 month-lockdown"
levels(meta_3_b$Scenario)[levels(meta_3_b$Scenario) =="5 months"] <- "5 month-lockdown"
meta_3_b$Scenario <- factor(meta_3_b$Scenario, levels = c("3 month-lockdown", "5 month-lockdown"))


#### TABLE 1 ####
data <- list()
for (i in 1: length(country_code)){
  directory <- paste0(gitHub, "\\COVID19\\output\\", country_code[i], sep = "") 
  setwd(directory)
  data[[i]]  <- read.csv("Table_2_overleaf.csv", header = TRUE, sep = ",")
  data[[i]]$country_code <- country_code[[i]]
  data[[i]]  <- merge( data[[i]], GDP_ranks, by.x = c("country_code"), by.y = c("country_code"))
  data[[i]] <- data[[i]] %>% mutate(share_high = share_firms[[1]],
                                    share_medium = share_firms[[2]],
                                    share_low= share_firms[[3]]) %>% 
    filter(Sectors == "All Sectors") %>%
    select(GDP_pc_cst, nb_firms, share_high, share_medium, share_low, averageUSD_size, medianUSD_size, avg_profitmargin, 
           avg_labor_exp, avg_material_exp, avg_fixed_exp, avg_shock, avg_shock_weight, ttl_payrollLCU,
           valueadded, valueadded_3, valueadded_5) %>% 
    mutate(averageUSD_size = averageUSD_size/1000, medianUSD_size = medianUSD_size/1000) %>% t()
  colnames(data[[i]])[1] <- country_code[[i]]
  
}
table_1  <- as.data.frame(data)
table_1 <- table_1 %>% mutate(Average = rowMeans(.[], na.rm = T))   %>%
  mutate_if(is.numeric, round, digits = 0) 
table_1$vars <- c("GDP_pc_cst", "nb_firms", "share_high", "share_medium", "share_low", "averageUSD_size", "medianUSD_size", "avg_profitmargin", 
                  "avg_labor_exp", "avg_material_exp", "avg_fixed_exp", "avg_shock", "avg_shock_weight", "ttl_payrollLCU", 
                  "valueadded", "valueadded_3", "valueadded_5")
setwd(output)
export <- table_1 %>% filter(vars != "valueadded" & vars !=  "valueadded_3" &vars != "valueadded_5")
  write.csv(export, file = "Table_1.csv")

value_added <- table_1 %>% filter(vars == "valueadded" | vars ==  "valueadded_3" | vars == "valueadded_5") %>% 
  t() 
value_added <- as.data.frame(value_added) 
colnames(value_added) <- c("valueadded", "valueadded_3", "valueadded_5")
value_added <- value_added[-nrow(value_added),]
value_added <- value_added[-nrow(value_added),]
value_added <- value_added %>% mutate(valueadded = as.numeric(as.character(valueadded)),
                                      valueadded_3 = as.numeric(as.character(valueadded_3)),
                                      valueadded_5 = as.numeric(as.character(valueadded_5)))
value_added$country <- country_code

## TABLE 2
data <- list()
for (i in 1: length(country_code)){
  directory <- paste0(gitHub, "\\COVID19\\output\\", country_code[i], sep = "") 
  setwd(directory)
  data[[i]]  <- read.csv("Table_3_overleaf.csv", header = TRUE, sep = ",")
  data[[i]]$country <- country_code[[i]]
}
table_2<- do.call("rbind", data)
three <- table_2 %>% select(All.3.months, Row.names, country) %>%
  filter(Row.names == "share_profitable0" | Row.names == "loss_citrevenue" | Row.names == "absoluteloss" |
           Row.names == "absloss_base" |
           Row.names == "payroll_loss_0")
three <- three %>% mutate(Row.names = paste0(Row.names, "_3")) 
three <- three %>%  cast(country ~ Row.names, mean, value=c("All.3.months"))
five <- table_2 %>% select(All.5.months, Row.names, country) %>%
  filter(Row.names == "share_profitable0" | Row.names == "loss_citrevenue" | Row.names == "absoluteloss" |
           Row.names == "absloss_base" |
           Row.names == "payroll_loss_0")
five <- five %>% mutate(Row.names = paste0(Row.names, "_5")) 
five <- five %>%  cast(country ~ Row.names, mean, value=c("All.5.months"))
table_2_a <- merge(three, five, by.x = c("country"), by.y = c("country"))

#### TABLE 2 B ####
data <- list()
for (i in 1: length(country_code)){
directory <- paste0(gitHub, "\\COVID19\\output\\", country_code[i], sep = "") 
setwd(directory)
data[[i]]  <- read.csv(paste0("meta_aggregate_", country_code[i], ".csv"))
data[[i]]$country <- country_code[[i]]
}
table_2_b <- do.call("rbind", data)
table_2_b <- table_2_b %>% select(country, wagebill, value_added, cit_revenue_GDP, GDP)

table_2 <- merge(table_2_a, table_2_b, by.x=c("country"), by.y = c("country"))
table_2 <- merge(table_2, GDP_ranks, by.x = c("country"), by.y = c("country_code")) 
table_2 <- merge(table_2, VAT_data, by.x = c("country"), by.y = c("country_code"), all = T)
table_2 <- merge(table_2, value_added, by.x = c("country"), by.y = c("country"))

## Create variables 
# VAT effective rate
table_2 <- table_2 %>% mutate(effective_rate = Macro.data/value_added
                              )
table_2 <- table_2 %>% mutate(CIT_loss_B_3 = loss_citrevenue_3,
                              CIT_loss_B_5 = loss_citrevenue_5,
                              CIT_loss_GDP_3 = loss_citrevenue_3*cit_revenue_GDP/100,
                              CIT_loss_GDP_5 = loss_citrevenue_5*cit_revenue_GDP/100,
                              VAT_loss_B_3 = 100*(valueadded-valueadded_3)/
                                (valueadded),
                              VAT_loss_B_5 = 100*(valueadded-valueadded_5)/
                                (valueadded),
                              VAT_loss_GDP_3 = VAT_loss_B_3*valueadded*effective_rate/(GDP),
                              VAT_loss_GDP_5 = VAT_loss_B_5*valueadded*effective_rate/(GDP),
                              absloss_B_3 = absloss_base_3,
                              absloss_B_5 = absloss_base_5,
                              absloss_GDP_3 = absoluteloss_3,
                              absloss_GDP_5 = absoluteloss_5,
                              payroll_loss_B_3 = payroll_loss_0_3,
                              payroll_loss_B_5 = payroll_loss_0_5,
                              payroll_loss_GDP_3 = payroll_loss_0_3*payrolltax*wagebill/GDP,
                              payroll_loss_GDP_5 = payroll_loss_0_5*payrolltax*wagebill/GDP) %>% 
  select(country, CIT_loss_B_3,CIT_loss_B_5, CIT_loss_GDP_3, CIT_loss_GDP_5, VAT_loss_B_3, 
         VAT_loss_B_5,VAT_loss_GDP_3, VAT_loss_GDP_5,absloss_B_3, absloss_B_5, absloss_GDP_3, 
         absloss_GDP_5,payroll_loss_B_3, payroll_loss_B_5, payroll_loss_GDP_3, payroll_loss_GDP_5,
          share_profitable0_3, share_profitable0_5 ) %>% t() %>% as.data.frame()

table_2$vars <- c('country', 'CIT_loss_B_3','CIT_loss_B_5', 'CIT_loss_GDP_3', 'CIT_loss_GDP_5', 'VAT_loss_B_3', 
'VAT_loss_B_5','VAT_loss_GDP_3', 'VAT_loss_GDP_5','absloss_B_3', 'absloss_B_5', 'absloss_GDP_3', 
'absloss_GDP_5','payroll_loss_B_3', 'payroll_loss_B_5', 'payroll_loss_GDP_3', 'payroll_loss_GDP_5',
'share_profitable0_3', 'share_profitable0_5')
setwd(output)
write.csv(table_2, file = "Table_2.csv")

########################### FUNCTION GRAPHS #######################################

### Figure 1 a & b
graph_1_fun <- function(dataset){
  dataset <- dataset %>% group_by(country) %>%
    mutate(min = profitable_firms[Scenario=="Baseline"],
           max = profitable_firms[Scenario=="5 month-lockdown"])
  p <- ggplot(dataset, aes(x=reorder(country, -rank_incr), y = profitable_firms, color = Scenario, shape = Scenario)) +
    geom_point(size = 4) + scale_shape_manual(values = c(18, 19, 17)) +  scale_color_manual(values = c("Baseline" = "grey", "3 month-lockdown" = "#D95F02", "5 month-lockdown" = "#7570B3")) +
    geom_pointrange(dataset[dataset$Scenario == "3 month-lockdown",], 
                    mapping = aes(x = country, y = profitable_firms, ymin =min, ymax =max), color = "black", alpha = 0.2, size = 0) +
    coord_flip() + scale_y_continuous(limits = c(0,100))
  print(p + labs(y = "Share of profitable firms (%)", x = "") + theme_minimal() + 
          theme(axis.text = element_text(size = 16), axis.title.y = element_text(size = 16, margin = margin(r = 15)), axis.title.x = element_text(size = 16, vjust = -0.8), 
                plot.title = element_text(size = 22),  legend.position= "top", legend.justification = "left", legend.text = element_text(size = 13.5), legend.title = element_text(size = 14)) + 
          guides(shape = guide_legend(""), color = guide_legend("") ))
}
graph_1_fun(meta_1_a)

### Figure 1 c
graph_1_c_fun <- function(dataset){
  dataset$bins <- dataset$bins*100 ## put profit rate in percentage
  p <-ggplot(dataset, aes(x = bins, y = N, color = Scenario)) + 
    geom_spline(aes(x = bins, y = N, color = Scenario, linetype=Scenario), size= 1, spar = 0.6, df =5) #spar
  p <- p + scale_color_manual(values = c("Baseline" = "#666666", "3 month-lockdown" = "#D95F02", "5 month-lockdown" = "#7570B3")) +
  scale_linetype_manual(values = c("dotted", "solid", "longdash")) 
  
   p <-  p + geom_vline(aes(xintercept = 0), color = "black", size = 1) + scale_x_continuous(limits = c(-50,50)) # + scale_y_continuous(limits = c(0,0.075)) 
  print(p + labs( y = "", x = "Profit margin (%)") + theme_minimal() + theme(axis.text = element_text(size = 16), axis.title.x = element_text(size = 16, vjust = -0.5), axis.text.y = element_blank(), plot.title = element_text(size = 22),
                                                                             legend.position= "top", legend.text = element_text(size = 13.5), legend.title = element_text(size = 14)) + 
          guides(color = guide_legend(""), linetype = guide_legend("")))
}
graph_1_c_fun(meta_1_c)

### Figure 2 a
graph_2_a_fun <- function(dataset){
  dataset <- dataset %>% group_by(country) %>%
    mutate(min = avg_drop[Scenario=="3 month-lockdown"],
           max = avg_drop[Scenario=="5 month-lockdown"])
  p <- ggplot(dataset, aes(x=log_GDP_pc, y = avg_drop, color = Scenario, shape = Scenario)) +
    geom_point(size = 4) +  scale_shape_manual(values = c(19, 17)) + scale_color_manual(values = c( "3 month-lockdown" = "#D95F02", "5 month-lockdown" = "#7570B3")) +
    geom_pointrange(dataset[dataset$Scenario == "3 month-lockdown",], 
                    mapping = aes(x = log_GDP_pc, y = avg_drop, ymin =min, ymax =max), 
                    color = "black", alpha = 0.2, size = 0) +  scale_y_continuous(limits = c(0,20))
  
  print(p + labs( y = "Average drop in payroll (%)", x = "log(GDP per capita)") + theme_minimal() + 
          theme(axis.text = element_text(size = 16), axis.title.y = element_text(size = 16, margin = margin(r = 15)), axis.title.x = element_text(size = 16, vjust = -0.8), 
                plot.title = element_text(size = 22),  legend.position= "top", legend.text = element_text(size = 13.5), legend.title = element_text(size = 14)) + 
          guides(shape = guide_legend(""), color = guide_legend("") ))
}
graph_2_a_fun(meta_2_a)
### Figure 2 b
graph_2_b_fun <- function(dataset){
  dataset <- dataset %>% group_by(country) %>%
    mutate(min = mean_exit[negprofit=="Profit-making firms"],
           max = mean_exit[negprofit=="Loss-making firms"]) %>% filter(negprofit != "All")
  dataset<- dataset %>% mutate(log_GDP_pc = if_else(country=="RWA", log_GDP_pc+ 0.1, log_GDP_pc))
  p <- ggplot(dataset, aes(x=log_GDP_pc, y = mean_exit, color = negprofit, shape = negprofit, fill=factor(negprofit))) +
    geom_point(size = 4) + scale_shape_manual(values = c(25, 24)) +
    scale_color_manual(values = c("Loss-making firms" ="#56B4E9", "Profit-making firms" = "olivedrab3")) +
    scale_fill_manual(values = c("Loss-making firms" ="#56B4E9", "Profit-making firms" = "olivedrab3")) +
  geom_pointrange(dataset[dataset$negprofit == "Profit-making firms",], 
                    mapping = aes(x = log_GDP_pc, y = mean_exit, ymin =min, ymax =max), 
                    color = "black", alpha = 0.2, size = 0) +  scale_y_continuous(limits = c(0,0.34))
  
  print(p + labs( y = "Probability of firms' exit", x = "log(GDP per capita)") + theme_minimal() + 
          theme(axis.text = element_text(size = 16), axis.title.y = element_text(size = 16, margin = margin(r = 15)), axis.title.x = element_text(size = 16, vjust = -0.8), 
                plot.title = element_text(size = 22),  legend.position= "top", legend.text = element_text(size = 13.5), legend.title = element_text(size = 14)) + 
          guides(shape = guide_legend(""), color = guide_legend(""),  fill = guide_legend("") ))
}
graph_2_b_fun (meta_2_b)

### Figure 2 c
graph_2_c_fun <- function(dataset){
  dataset <- dataset %>% group_by(country) %>%
    mutate(min = pct_increase[Scenario=="3 month-lockdown"],
           max = pct_increase[Scenario=="5 month-lockdown"])
  p <- ggplot(dataset, aes(x=log_GDP_pc, y = pct_increase, color = Scenario, shape = Scenario)) +
    geom_point(size = 4) +  scale_shape_manual(values = c(19, 17)) + scale_color_manual(values = c( "3 month-lockdown" = "#D95F02", "5 month-lockdown" = "#7570B3")) +
    geom_pointrange(dataset[dataset$Scenario == "3 month-lockdown",], 
                    mapping = aes(x = log_GDP_pc, y = pct_increase, ymin =min, ymax =max), 
                    color = "black", alpha = 0.2, size = 0) +  scale_y_continuous(limits = c(0,NA))
  
  print(p + labs( y = "Increase in firms' exit (%)", x = "log(GDP per capita)") + theme_minimal() + 
          theme(axis.text = element_text(size = 16), axis.title.y = element_text(size = 16, margin = margin(r = 15)), axis.title.x = element_text(size = 16, vjust = -0.8), 
                plot.title = element_text(size = 22),  legend.position= "top", legend.text = element_text(size = 13.5), legend.title = element_text(size = 14)) + 
          guides(shape = guide_legend(""), color = guide_legend("") ))
}

### Figure 3 a
graph_3_a_fun <- function(dataset){
  dataset <- dataset %>% filter( Sector == "All Sectors")
  dataset$subsidy <- dataset$subsidy*100
  p <- ggplot(dataset, aes(x = subsidy, y = avg_aggloss, color = factor(Scenario), linetype = Scenario)) + 
    geom_line(size = 0.9) 
  p <- p + scale_linetype_manual(values = c("solid", "dashed")) + scale_color_manual(values = c("3 month-lockdown" = "#D95F02", "5 month-lockdown" = "#7570B3")) + scale_x_continuous(breaks = c(0, 30, 60, 90), limits = c(0, 90))
  print(p + labs(y = "Loss in aggregate yearly payroll (% of baseline)", x = "Wage subsidy (% of payroll covered)") + theme_minimal() + 
          theme(axis.text = element_text(size = 16), axis.title.y = element_text(size = 16, margin = margin(r = 15)), axis.title.x = element_text(size = 16, vjust = -0.8), 
                plot.title = element_text(size = 22),  legend.position= "top", legend.text = element_text(size = 13.5), legend.title = element_text(size = 14)) +
          guides(linetype = guide_legend(""), color = guide_legend("") ))
}

### Figure 3 b
graph_3_b_fun <- function(dataset){
  dataset <- dataset %>% filter(Scenario == "3 month-lockdown" & Sector == "All Sectors")
  fit <- lm(diff_aggloss_50~log_GDP_pc, data = dataset)
  a <- signif(fit$coef[[1]], digits = 2)
  b <- signif(fit$coef[[2]], digits = 2)
  se <- signif(summary(fit)$coef[2,2], digits = 2)
  
  p <- ggplot(dataset, aes(x=log_GDP_pc, y = diff_aggloss_50, color = Scenario)) +
    geom_point(size = 4) + scale_color_manual(values = c( "3 month-lockdown" = "#D95F02")) + geom_smooth(method = 'lm', fill = NA) #+
   # geom_text(x =7.05 , y=2.5 , label=("beta"), parse = T, col = "black", size = 4 , fontface = "italic") +
   # geom_text(x =7.2 , y=2.5 , label=paste("= ", b, sep=""), col = "black", size = 4 , fontface = "italic") +

   #   geom_text(x =7.2 , y=2.3 , label=paste("se = ", se, sep=""), col = "black", size = 4 , fontface = "italic")
  
  print(p + labs(y = "Aggregate yearly payroll saved (% of baseline)", x = "log(GDP per capita)") + theme_minimal() + 
          theme(axis.text = element_text(size = 16), axis.title.y = element_text(size = 16, margin = margin(r = 15)), axis.title.x = element_text(size = 16, vjust = -0.8), 
                plot.title = element_text(size = 22),  legend.position= "top", legend.text = element_text(size = 13.5), legend.title = element_text(size = 14)) + guides(color = guide_legend("Revenue Loss:")))
  
}
graph_3_b_fun(meta_3_b)
########################### EXTRACT GRAPHS #######################################
setwd(output)

## FIGURE 1.a
png("1_a_Graph%02d.png")
graph_1_fun(meta_1_a)
dev.off()
write.csv(meta_1_a, file = paste0("meta_1_a", ".csv"))

## FIGURE 1.b
png("1_b_Graph%02d.png")
graph_1_fun(meta_1_b)
dev.off()
write.csv(meta_1_b, file = paste0("meta_1_b", ".csv"))

## FIGURE 1.c
png("1_c_Graph%02d.png")
graph_1_c_fun(meta_1_c)
dev.off()
write.csv(meta_1_c, file = paste0("meta_1_c", ".csv"))




## FIGURE 2 a
png("2_a_Graph%02d.png")
graph_2_a_fun(meta_2_a)
dev.off()
write.csv(meta_2_a, file = paste0("meta_2_a", ".csv"))

## FIGURE 2 b
png("2_b_Graph%02d.png")
graph_2_b_fun(meta_2_b)
dev.off()
write.csv(meta_2_b, file = paste0("meta_2_b", ".csv"))

## FIGURE 2 c
png("2_c_Graph%02d.png")
graph_2_c_fun(meta_2_c)
dev.off()
write.csv(meta_2_c, file = paste0("meta_2_c", ".csv"))



## FIGURE 3.A
png("3_a_fig%02d.png")
graph_3_a_fun(meta_3_a)
dev.off()
write.csv(meta_3_a, file = paste0("meta_3_a", ".csv"))

## FIGURE 3.B
png("3_b_fig%02d.png")
graph_3_b_fun(meta_3_b)
dev.off()
write.csv(meta_3_b, file = paste0("meta_3_b", ".csv"))



