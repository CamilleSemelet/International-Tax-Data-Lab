rm(list=ls(all=TRUE))

# Master Script
# Title : Effective Tax Rate and Firm Size
# Authors: Pierre Bachas, Anne Brockmeyer, Roel Dom and Camille Semelet
# Data Sources: CIT admin data, WDI

#########################################################################.
# 0. READ ME 

# To be able to run the code on another country than Mexico, action is needed in the following parts:
  # - 1. USERS: 
  #   set the paths according to your environment 
  #   Set the country name and country code
  #     in the case of Mexico, this is "Mexico", and "MEX"
  #   
  # - 3. LOAD DATA:
  #   change the name of the clean CIT admin data (prepared based on the CIT_country.do cleaning do file)
  #     in the case of Mexico, the file is called MEX_withvars.csv
  #   code the statutory tax rate (STR) of the country. This has to be at the firm level and year specific.
  #     in the case of Mexico, the STR is similar for all firms and all years of the panel
  #   


#########################################################################.
# 1. USERS

  ## Paths & objects 
  country_name <-c("Mexico")  # FILL
  country_code <- c("MEX")    # FILL
  proc_data <- "C:\\Users\\s551964\\Cross-Country\\ETR\\public_repository\\OpenSourceData" # Where the processed/clean data is located        
  ## On gitHub
  gitHub <- "C:\\Users\\s551964\\Cross-Country\\"    
  code <- paste0(gitHub, "/ETR/public_repository")  # Where the other scripts/codes are located   
  metadata <- paste0(gitHub, "/ETR/public_repository") # Where we want to export the metadata
  stat <- paste0(gitHub, "/ETR/public_repository") # Where we want to export the descriptive tables
  graph <- paste0(gitHub, "/ETR/public_repository") # Where we want to export the figures 
  reg <- paste0(gitHub, "/ETR/public_repository")# Where we want to export the regression results
  
  # Here we localize everything at the same place, but this can be changed.

#########################################################################.
# 2. PACKAGES

install.packages('tidyverse') # base
install.packages('WDI')
install.packages('lazyeval') # use df$var in functions
install.packages("DescTools") # winzorise
install.packages('viridis') # color package
install.packages('gghighlight') # highlight part of a graph
install.packages('ggformula') # spline
install.packages('plm') # FE regression
install.packages('broom') # tidy df
install.packages('stargazer') # export regression
install.packages('jtools') # export regression
install.packages('hexbin') # hexagonal graphs
install.packages('fixest') # fixed effects reg
install.packages('ggrepel') # fixed effects reg
install.packages('directlabels') # Labels on graph lines
install.packages('data.table') #fast read
install.packages('npreg') # Non parametric regression
install.packages('collapse') #fsum and more
install.packages('grid') # grid on ggplot
install.packages('pBrackets') # bracket on ggplot
install.packages('huxtable')


library(tidyverse) # base
library(WDI) # World Development Indicator database
library(lazyeval) # use df$var in functions
library(DescTools) # winzorise
library(viridis) # color package
library(gghighlight) # highlight part of a graph
library(ggformula) # spline
library(plm) # FE regression 
library(broom) # tidy df
library(stargazer) # export regression
library(jtools) # export reg
library(hexbin) # hexagonal graphs
library(fixest) # fixed effects reg 
library(ggrepel) # fixed effects reg 
library(directlabels) # Labels on graph lines 
library(data.table) #fast read 
library(npreg) # Non parametric regression
library(collapse) #fsum and more
library(grid) # grid on ggplot
library(pBrackets) # bracket on ggplot
library(huxtable) # table latex

#########################################################################.
# 3. LOAD DATA
  
setwd(proc_data)
data_raw  <- read.csv("MEX_clean.csv", sep = ",") 

# --Add Statutory Rate schedule (Here it is adapted to MEXICO)

if (country_code[[1]] == "MEX"){
  data <- data_raw %>% mutate(STR = NA_real_,
                          STR = if_else(year >= 2010, 0.3, STR))
} else {
  data <- data_raw %>% mutate(STR = NA_real_) ## FILL HERE 
}

print("Data loaded")
  

#########################################################################.
# 4. PREPARATION DATA

# Take clean data, add WDI variables, STR and create the main dataset:
#   cross-sectional (last year) (list df.sample)
#   panel data (list data)
#   Restrictions are as follows: 
#       - turnover > 1

setwd(code)
source("1_prepare_datasets_one_country.R")

#########################################################################.
# 5. ANALYSIS 

# LONG TO RUN
# Compute the ETR
# Create all the result dataframes (descriptive stats, reg, metadata)
# Extract all data to be able to extract the output
setwd(code)
source("2_analysis_one_country.R")

#########################################################################.
# 6. OUTPUT 

# Extract the output 
setwd(code)
source("3_output.R")

### Clean environment
rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) != "character"])
