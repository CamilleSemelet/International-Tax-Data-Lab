
############################|  PREPARE DATA |#################################


print("Begin cleaning")

### + Add WDI: ####
#....................

  WDI_data<- WDI(
    indicator = c(
      "GDP_pc_const2015" = "NY.GDP.PCAP.KD", 
      "official_exchange" = "PA.NUS.FCRF",
      "cpi" = "FP.CPI.TOTL"),
    country = c(country_code[[1]]),
    start = 2000,
    end = max(data_raw$year))
    # automatically adjusts to end year in question
  WDI_data <- WDI_data %>% select(-iso2c, -iso3c)
  WDI_data$country <- country_code[[1]]
  data <- merge(data, WDI_data, by =c("country","year"), all.x = T)


print("WDI data merged")

### + Adapt sectors: We pool sector based on Sections from NACE Rev 2 into larger sectors: ####
#....................
  
  data <- data %>% mutate(largesector = "Other",
                          largesector = if_else(section=="G", "Retail", largesector),
                          largesector = if_else(section=="A" | section == "B" , "Primary", largesector),
                          largesector = if_else(section=="C" | section == "F" | section=="D" | section=="E" , "Secondary", largesector),
                          largesector = if_else(section=="Q" |  section=="I" | section=="H" | section=="D" | 
                                                section=="R" | section=="S" | section=="O" | section=="N" | section == "L" | 
                                                section=="K" | section == "J" | section == "M" | section == "P" , "Services", largesector),
                          largesector = factor(largesector, levels= c("Primary", "Secondary", "Retail", "Services")))
  
### + Convert Turnover to USD, and adjust for inflation: ####
#....................  
  cpi_usd <- WDI(
      indicator = c("cpi_usd" = "FP.CPI.TOTL"),
      country = c("USA"),
      start = 2000,
      end = 2020)
  cpi_usd <- cpi_usd %>% select(-iso2c, -country) 
  base_cpi <- cpi_usd$cpi_usd[cpi_usd$year==2019]
  data <- merge(data, cpi_usd, by=c("year"))
  data <- data %>% mutate(turnover_usd = total_income/official_exchange,
                          index = cpi_usd/base_cpi,
                          turnover_usdadj = if_else(total_income>0, turnover_usd/index, NA_real_),
                          log_turn_usd = if_else(total_income>0, log(turnover_usdadj), NA_real_),
                          log_GDP_pc = log(GDP_pc_const2015))

### + Additional variables: ####
#....................  
  
data <- data %>% mutate(#net_profit_pos = if_else(net_profit <0, 0 , net_profit))
                        foreign_credit = as.numeric(cred_foreign_tax),
                        foreign_credit = if_else(is.na(foreign_credit) | foreign_credit<0, 0, foreign_credit),
                        ntl_noftc = net_tax_liability + foreign_credit)
  
  
  
### + Sample restrictions for all: ####
#....................  
# - keep last year available for each year. SEN has no exemption variables from 2017 onwards
# - keep only firms with turnover > 1. Some firms report 0.1 turnover. Limiting to 0 would still produce outlier ratios.
 
  df.sample <- data  %>% filter(year == max(year) & turnover > 1)
  data  <- data %>% filter(turnover > 1)


# Remove list/object for memory purposes
rm(cpi_usd, WDI_data, data_raw)
rm(base_cpi)

print("End cleaning")