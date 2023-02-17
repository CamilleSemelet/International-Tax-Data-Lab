

### 2. Analysis 

# This Script is to be called by the 0_master_one_country.
# We create function so that they can easily be applied to several dataframes at once
# when we run the analysis on the several countries. For single countries, we can easily 
# run the analysis on one dataframe. 

print("Begin Analysis")


########################| 1. FUNCTIONS MUTATE |#################################
# these functions create new variables within dataframes, but they do not restrict or collapse the df
 

#----------- Split distribution in ntile, deciles, quartile, etc.
# We'll use percentile in most cases
decile_FUN = function(source_df){
  df <- source_df %>% group_by(year) %>% mutate(decile10 = ntile(total_income, 10), decile10 = as.numeric(decile10),
                                                percentile = ntile(total_income, 100), percentile = as.numeric(percentile),
                                                # We want percentile to go from 0 to 99
                                                percentile = percentile-1) %>% ungroup()
  # Decompose Decile 10 in 5 and 10 bins
  top10 <- df %>% filter(decile10 == 10) %>% group_by(year) %>% 
    mutate(top_ntile10 = ntile(total_income, 10),
           top_ntile5 = ntile(total_income, 5)) %>%
    select(tax_ID, top_ntile10, top_ntile5, year) %>% ungroup()
  
  # Decompose percentile 99 in 5 bins
  top98 <- df %>% filter(percentile == 99) %>% group_by(year) %>% mutate(perc_98 = ntile(total_income, 5)) %>% 
    select(tax_ID, perc_98, year)  %>% ungroup()
  
  # Decompose percentile 99 in 10 bins
  top99 <- df %>% filter(percentile == 99) %>% group_by(year) %>% mutate(perc_99 = ntile(total_income, 10)) %>% 
    select(tax_ID, perc_99, year)  %>% ungroup()
  
  # Create new bins variables
  df <- merge(df, top10, by =c("tax_ID", "year"), all = TRUE) %>% group_by(year) %>% 
    mutate(top_ntile5 = as.numeric(top_ntile5),
           deciles = if_else(!is.na(top_ntile5), decile10+top_ntile5-1, decile10)) %>% ungroup()
  
  df <- merge(df, top98, by =c("tax_ID", "year"), all = TRUE) %>% group_by(year) %>%
    mutate(perc_98 = as.numeric(perc_98),
           percentile_99.9 = if_else(!is.na(perc_98), percentile+(perc_98-1)/10, percentile)) %>% ungroup 
  
  df <- merge(df, top99, by =c("tax_ID", "year"), all = TRUE) %>% group_by(year) %>%
    mutate(perc_99 = as.numeric(perc_99),
           percentile_99_rob = if_else(!is.na(perc_99), percentile+(perc_99-1)/10, percentile)) %>% ungroup 
  
  #only if split top 1 percent in 5
  df <- df %>% mutate( percentile_99.9 = if_else(percentile_99.9 == 99.4, 99.9, percentile_99.9),
                       percentile_99.9 = if_else(percentile_99.9 == 99.3, 99.7, percentile_99.9),
                       percentile_99.9 = if_else(percentile_99.9 == 99.2, 99.5, percentile_99.9),
                       percentile_99.9 = if_else(percentile_99.9 == 99.1, 99.3, percentile_99.9),
                       percentile_99.9 = if_else(percentile_99.9 == 99.0, 99.1, percentile_99.9))
  
  
  
}

## Uncomment to test the function:

#df <- decile_FUN(df.sample)  


#----------- Add the statutory rate STR, by decile and by sectors


# --Add STR, by percentile and by sectors
STR_percFUN = function(source_df){
  df <- source_df %>% decile_FUN() %>% group_by(country, percentile_99.9) %>%
    summarise(STR = mean(STR*100, na.rm = TRUE))
}
#df <- STR_percFUN(df.sample)

STR_sectorsFUN = function(source_df, industry){
  source_df$x <- f_eval(~uq(industry), data = source_df)
  df <- source_df %>% group_by(country, x) %>%
    summarise(STR = mean(STR*100, na.rm = TRUE))
}


#----------- Construct the Effective Tax Rates for each firms, for different tax base (~denominator)
ETR_FUN= function(source_df, denominator){
  source_df$x <- f_eval(~uq(denominator), data = source_df)
  max <- max(source_df$STR)*100
  df  <- source_df
  
  df <- df  %>%  decile_FUN() %>% mutate(x = as.numeric(x),
                                         x = if_else(x < 1, 0, x),
                                         NTL_pos = if_else(net_tax_liability < 1, 0, net_tax_liability), ## tax is positive or zero (neg value set to 0)
                                         ETR = 100*NTL_pos/x,
                                         ETR = if_else(x == 0 & NTL_pos >0 , max, ETR),
                                         ETR = if_else(x == 0 & NTL_pos ==0, 0, ETR),
                                         ETR = if_else(is.nan(ETR), 0, ETR))
  if (df$country[1] == "MEX") {
    # Mexico ETR above 30 are non-randomly distributed, there is a peak at percentile 94. Around 600 firms report ETR>100, against less than 200 for the rest.
    # We want to pick that up in our data and thus allow ETR to go up to 100 to have a sense of the magnitude.
    df <- df  %>% mutate(ETR_winz = Winsorize(ETR, minval = 0, maxval = 100 , na.rm = TRUE))
  } else{
    df <- df  %>% mutate(ETR_winz = Winsorize(ETR, minval = 0, maxval = max , na.rm = TRUE))
  }
  df <- df  %>% mutate(
    ETR_drop_neg = if_else(x>0, ETR_winz, NA_real_),
    ETR_keep_neg = if_else(x<=0, 0, ETR_winz))
  
  df <- df  %>% mutate(ETR_minus_STR =  ETR_drop_neg - (STR*100))
  denom <- paste(denominator)
  df <- df %>% mutate(ETR_denominator = denom[2])
}

# df <- ETR_FUN(df.sample, ~net_profit)  

#----------- Construct the Effective Tax Rates for each firms, for different tax base (~denominator)
ETR_num_FUN= function(source_df, numerator){
  source_df$x <- f_eval(~uq(numerator), data = source_df)
  max <- max(source_df$STR)*100
  df  <- source_df 
  df <- df  %>%  decile_FUN() %>% mutate(net_profit = if_else(net_profit < 1 & net_profit>0, 0, net_profit),
                                         x = if_else(x < 1, 0, x), ## tax is positive or zero (neg value set to 0)
                                         ETR = 100*x/net_profit,
                                         ETR = if_else(is.na(net_profit), NA_real_, ETR),
                                         ETR = if_else(is.nan(ETR), 0, ETR))
  if (df$country[1] == "MEX") {
    # Mexico ETR above 30 are non-randomly distributed, there is a peak at percentile 94. Around 600 firms report ETR>100, against less than 200 for the rest.
    # We want to pick that up in our data and thus allow ETR to go up to 100 to have a sense of the magnitude.
    df <- df  %>% mutate(ETR_winz = Winsorize(ETR, minval = 0, maxval = 100 , na.rm = TRUE))
  } else{
    df <- df  %>% mutate(ETR_winz = Winsorize(ETR, minval = 0, maxval = max , na.rm = TRUE))
  }
  df <- df  %>% mutate(
    ETR_drop_neg = if_else(net_profit>0, ETR_winz, NA_real_),
    ETR_keep_neg = if_else(net_profit<=0, 0, ETR_winz))
  
  df <- df  %>% mutate(ETR_minus_STR =  ETR_drop_neg - (STR*100))
  denom <- paste(numerator)
  df <- df %>% mutate(ETR_denominator = denom[2])
}


#df <- ETR_num_FUN(df.sample, ~net_tax_liability)  


#----------- Construct the life-time Effective Tax Rates for a balanced panel of firms

ETR_balpanel_FUN = function(source_df, nb_year){
  country <- source_df$country[1]
  max <- max(source_df$STR)*100
  first_year <- max(source_df$year) - nb_year + 1 
  # Keep only panel appearing in last cross-section obs
  df.id <- source_df %>% filter(year == max(year)) %>% pull(tax_ID) 
  
  df <- source_df %>% mutate(sample = if_else(tax_ID %in% df.id, 1, 0)) %>%
    filter(sample == 1)
  
  # Keep if firms appear (nb_year), need to be balanced panel, and no change in STR
    df <- df %>% filter(year >= first_year) %>% # Depending on nb_year, first_year changes
                 arrange(tax_ID, year) %>% group_by(tax_ID) %>% 
                 mutate(year_change = year - dplyr::lag(year),
                        str_change = STR - dplyr::lag(STR)) %>% ungroup() %>%
      filter(year_change == 1 | is.na(year_change)) %>%  
      filter(str_change == 0 | is.na(str_change))
    
  # Construct ETRs lifetime and for the last cross section
  # Cross-section:
  df.cross <- ETR_FUN(df %>% filter(year == max(year)) , ~net_profit) %>% 
    mutate(profitable = if_else(net_profit>0, 1, 0)) %>%
    select(tax_ID, percentile_99.9, profitable) 
  
  # Panel (assumption is that LCF will impact ETR, so we don't need to account for previous losses):
  df.life <- df %>% 
    mutate(NTL_pos = if_else(net_tax_liability < 0, 0, net_tax_liability)) %>% # tax to pay is >0
           #net_profit_pos = if_else(net_profit < 0, 0, net_profit)) %>% # losses from one year to the other or not substracted
    group_by(tax_ID) %>% 
    summarize(ETR_all_panel = 100*sum(NTL_pos)/sum(net_profit),
              ETR_all_panel = if_else(is.na(net_profit), NA_real_, ETR_all_panel),
              ETR_all_panel = if_else(is.nan(ETR_all_panel), 0, ETR_all_panel),
              count_year = n())  %>% ungroup()  %>% mutate(country = country) 
  # No separation between profitable and unet_profitrofitable firms here
  if ( country_code[[1]] == "MEX") {
    df.life <- df.life  %>% mutate(ETR_all_panel = Winsorize(ETR_all_panel, minval = 0, maxval = 100 , na.rm = TRUE))
  } else{
    df.life <- df.life  %>% mutate(ETR_all_panel = Winsorize(ETR_all_panel, minval = 0, maxval = max , na.rm = TRUE))
  }

  # Merge: 
  df <- merge(df.cross, df.life, by=c("tax_ID")) %>% mutate(country = country) 
  ## count year? 1 by 1?
  # Percentile level
  df <- df %>% group_by(country, percentile_99.9) %>% 
    mutate(ETR_prof_cross = if_else(profitable == 1, ETR_all_panel, NA_real_)) %>%
    summarize(n_all = length(ETR_all_panel),
              n_prof = length(ETR_all_panel[!is.na(ETR_all_panel)]),
              ETR_all_panel = mean(ETR_all_panel, na.rm = T),  
              ETR_prof_cross = mean(ETR_prof_cross, na.rm = T)) %>%
    mutate(year = nb_year)
}

#df <- ETR_balpanel_FUN(data[[2]], 1)


########################| 2. FUNCTIONS SUBSETS |################################# 
# Those functions collapse the dataframes at the percentile level--or any other level chosen


#----------- Subset dataframe for percentile graph of ETR:
ETR_perc_df_FUN = function(source_df){
  df <- source_df %>% group_by(country, log_GDP_pc, percentile_99.9, ETR_denominator) %>%
    summarize(n_keep = n(),
              n_drop = length(ETR_drop_neg[!is.na(ETR_drop_neg)]),
              ETR_drop_avg= mean(ETR_drop_neg, na.rm =T),
              ETR_keep_avg= mean(ETR_keep_neg, na.rm =T),
              ETR_drop_med= median(ETR_drop_neg, na.rm =T),
              ETR_keep_med= median(ETR_keep_neg, na.rm =T),
              ETR_minus_STR = mean(ETR_minus_STR, na.rm = T))
  }
#df <- ETR_FUN(df.sample, ~net_profit) 
#df <-  df %>% ETR_perc_df_FUN() 


#----------- Subset dataframe for sector graph of ETR: 
ETR_sector_perc_FUN = function(source_df, industry){
  source_df$x <- f_eval(~uq(industry), data = source_df)
  df <- source_df %>% group_by(country, log_GDP_pc, x, ETR_denominator, percentile_99.9) %>%
    summarize(n_keep = n(),
              n_drop = length(ETR_drop_neg[!is.na(ETR_drop_neg)]),
              ETR_drop_avg= mean(ETR_drop_neg, na.rm =T),
              ETR_keep_avg= mean(ETR_keep_neg, na.rm =T),
              ETR_drop_med= median(ETR_drop_neg, na.rm =T),
              ETR_keep_med= median(ETR_keep_neg, na.rm =T))
}
# df <- ETR_FUN(df.sample, ~net_profit) 
# df <-  df %>% ETR_sector_perc_FUN(., ~sector) 

#----------- Subset dataframe for reg of ETR: 
dummy_reg_FUN = function(source_df){  # Exemptions are dummies
  
  list_regressors <- c('exempt_income', 'non_deduc_inp', 
                       'tot_deduc_taxbase', 'depreciation', 'investment_taxbase', 'capital_allowance', #'other_ded_taxbase',
                       'loss_carryforward',
                       'tot_noelse_taxliab', 'cred_foreign_tax', 'special_taxliab', 'trade_taxliab',
                       'tot_deduc_taxliab',  'investment_taxliab',  'other_cred_taxliab') 
  list_control <- c('section', 'FEZ', 'capital_city', 'first_year', 'firm_age', 'foreign_ownership', 
                    "asset_tax", "min_tax", "special_regime",  'region', 'tax_center')
  # Dummy if tax provision is claimed
  df <- source_df %>% mutate(across(list_regressors, ~if_else(!is.na(.) & .!=0, 1, 0)))
  
  # Other dummy:
  df <- df %>% mutate(loss = if_else(net_profit<0, 1, 0),
                      ftc_else_cred = if_else(cred_foreign_tax ==1 | other_cred_taxliab == 1, 1, 0))
  
  df <- df %>% group_by(country) %>% mutate(max_STR = max(STR),
                                            reduced_rate = if_else(STR < max(STR), 1, 0),
                                            STR = STR*100)
    
  df <- df %>% select(all_of(list_regressors), one_of(list_control), 
                      ETR_keep_neg, ETR_drop_neg, country, STR, percentile_99.9, percentile_99_rob, decile10, 
                      loss, ftc_else_cred, reduced_rate,  year, tax_ID)   
}

 # df <- ETR_FUN(df.sample, ~net_profit) 
 # df <-  df %>% dummy_reg_FUN() 


#----------- Subset dataframe for into country tax gap and Development level (profitable firms only)

ETR_taxgap_cappedFUN = function(source_df){
  # Profitable firms only (sample of all firms give the same results by construction)
  df.prof <- source_df %>% filter(!is.na(ETR_drop_neg) & !is.na(net_profit)) %>%
    mutate(ETR = ETR_drop_neg/100, #Here we take into account the capped ETR
           STR_ETR = STR-ETR,
           #STR_ETR = if_else(STR_ETR<0, 0, STR_ETR),
           net_profit_pos = if_else(net_profit<0, 0, net_profit),
           gap = net_profit_pos*STR_ETR,
           ntl = if_else(net_tax_liability>0, net_tax_liability, 0),
           ntl_built = net_profit_pos*ETR,
           net_profit_str = net_profit_pos*STR) %>%
    group_by(country, year, log_GDP_pc) %>%
    summarise(sum_ntl_built = sum(ntl_built, na.rm = T),
              sum_ntl = sum(ntl, na.rm = T),
              sum_gap = sum(gap, na.rm = T),
              sum_net_profit_str = sum(net_profit_str, na.rm = T)) %>%
    mutate(rev_forgone = 100*(sum_gap/(sum_ntl+sum_gap)),
           rev_forgone_built = 100*(sum_gap/(sum_ntl_built+sum_gap)),
           rev_forgone_str = 100*(sum_gap/(sum_net_profit_str)))
  
}

#----------- A Global Minimum Tax (Top 1% with an ETR lower than 15%)

ETR_lower15_FUN = function(source_df) {
  df <- source_df %>% group_by(year) %>% 
    # Sample 
    filter(!is.na(ETR_drop_neg)) %>%
    mutate(
      #flag firms with etr<15
      less_15 = if_else(ETR_drop_neg < 15, 1, 0),
      # Compute old tax liability:
      ntl_pos = if_else(net_tax_liability >0, net_tax_liability, 0),
      # Assumption is that negative net profit ensure no taxes paid
      ntl_pos_built = if_else(net_profit>0, round(net_profit * (ETR_drop_neg / 100), digits = 0), 0), ##USE ETR OR ETR_winz ?? Problem for large differences?
      # Compute new tax liability:
      new_ntl = if_else(less_15 == 1, round(net_profit * 0.15, digits = 0), ntl_pos),
      new_ntl_built = if_else(less_15 == 1, round(net_profit * 0.15, digits = 0), ntl_pos_built)
    ) %>% # Assumption is that negative net profit ensure no taxes paid
    group_by(country, log_GDP_pc, percentile_99.9, year) %>%
    summarize(
      #    avg_ETR_all = mean(ETR_keep_neg[less_15 == 1], na.rm = T),
      avg_ETR_prof = mean(ETR_drop_neg[less_15 == 1], na.rm = T),
      n = n(),
      #      n_prof = length(ETR_drop_neg[!is.na(ETR_drop_neg)]),
      n_less15 = sum(less_15, na.rm = T),
      #     n_less15_prof = sum(less_15[!is.na(ETR_drop_neg)]),
      new_taxrev = sum(new_ntl, na.rm = T),
      new_taxrev_built = sum(new_ntl_built, na.rm = T),
      old_taxrev = sum(ntl_pos, na.rm = T),
      old_taxrev_built = sum(ntl_pos_built, na.rm = T),
      STR = mean(STR * 100)
    ) %>%
    mutate(
      less15_shr = 100 * n_less15 / n,
      #less15_prof_shr = 100 * n_less15_prof / n_prof
    ) %>% ungroup()
}

print("All functions loaded")


######################## | 3. DATAFRAMES | ################################# 
# This section applies the functions created to above to build the desire datasets

# Here we will store the names of all the df we create to keep track of them and be able to call them all at once
df_name <- c()


#### + Descriptive Tables  ####
#..............................
  min_year <- min(data$year)
  df <- data %>% filter(year == max(year))
  df <- df %>% mutate(pos_turn = if_else(turnover > 0 & !is.na(turnover), 1, 0),
                      pos_netprofit = if_else(net_profit > 0 & !is.na(net_profit), 1, 0),
                      turnover_usdadj = if_else(pos_turn ==0, NA_real_, turnover_usdadj)) %>%
    summarize(year = mean(year),
              count = n(),
              pos_netprofit = sum(pos_netprofit),
              turnover_avg =  mean(turnover_usdadj, na.rm = T), 
              GDP_pc_USD = mean(GDP_pc_const2015),
              STR = max(STR)) %>% 
    mutate("Net profit>0" = 100*pos_netprofit/count)
  
  df$country <- country_code[[1]]
  df$country_name <- country_name[[1]]
  df$min_year <- min_year
  df.ETR.descr <- df
 # Add average ETR for all firms and profitable firms only:
  df <- ETR_FUN(df.sample, ~net_profit) %>% group_by(country) %>% 
        summarise(ETR_avg_profitable= mean(ETR_drop_neg, na.rm =T), 
                  ETR_avg_all= mean(ETR_keep_neg, na.rm =T)) 
 # Merge
df.ETR.descr <- merge(df.ETR.descr, df, by = "country")
df_name <- append(df_name, "df.ETR.descr")



#### Percentile Distribution Stats:
  df <- data %>% filter(year == max(year)) %>% decile_FUN()
  
  df <- df %>% 
    mutate(pos_profit = if_else(net_profit < 0, 0, net_profit),
           pos_liability = if_else(net_tax_liability < 0, 0, net_tax_liability),
           labor_inp = abs(labor_inp)) %>% 
    select(year, percentile, pos_profit, total_income, pos_liability, labor_inp) %>%
    group_by(year, percentile) %>% 
    fsum() 
  
  df <- df %>% mutate(pos_profit =  100*pos_profit/sum(pos_profit),
                      total_income =  100*total_income/sum(total_income),
                      pos_liability =  100*pos_liability/sum(pos_liability),
                      labor_inp = 100*labor_inp/sum(labor_inp))
  
  df$country <- country_name[[1]]
 
df.perc.distribution <- df
df_name <- append(df_name, "df.perc.distribution")


#### Percentile 90: stats for employees and turnover:
  #Employee
  if ("nb_employee" %in% colnames(df.sample) == "TRUE") {
  df.e <- df.sample %>% decile_FUN() %>% ungroup() %>% filter(percentile == 90) %>% arrange(total_income) 
  df.e <- df.e[1,] 
  df.e <- df.e %>% select(year, country, nb_employee, percentile) 
  } else {
    df.e <- data.frame(country = c(df.sample$country[1]),
                       nb_employee = c(NA_real_), 
                       year = c(df.sample$year[1]),
                       percentile = c(90))
  }
     
df.t <- df.sample %>% decile_FUN() %>% ungroup() %>% filter(percentile == 90) %>% arrange(turnover_usdadj) 
df.t <- df.t[1,] 
df.t <- df.t %>% select(year, country, turnover_usdadj, percentile) 

df <- merge(df.t, df.e, by = c("country", "year", "percentile"), all = T)

df.p90.stat <- df
df_name <- append(df_name, "df.p90.stat")


#### + Dataframes for figures####
#..............................

####  ETR, by firm size (percentile) 
  df <- ETR_FUN(df.sample, ~net_profit) %>% ETR_perc_df_FUN() 
  str <- df.sample %>% STR_percFUN()
  df <- merge(df, str, by=c("country", "percentile_99.9"), all = T)
df.ETR.size.p <- df
df_name <- append(df_name, "df.ETR.size.p")

                      
####  ETR, Robustness: numerator 
  df.0 <- ETR_num_FUN(df.sample, ~net_tax_liability) %>% ETR_perc_df_FUN() 
  df.1 <- ETR_num_FUN(df.sample, ~ntl_noftc) %>% ETR_perc_df_FUN() 
  df.2 <- ETR_num_FUN(df.sample, ~ntl_noelse) %>% ETR_perc_df_FUN() 
  df <- rbind(df.0, df.1, df.2)
  str <- df.sample %>% STR_percFUN()
  df <- merge(df, str, by=c("country", "percentile_99.9"), all = T)
df.ETR.size.num.p <- df
df_name <- append(df_name, "df.ETR.size.num.p")

####  ETR, Robustness: 10 bins at the top instead of 5 ####
  df.ETR.size.p.rob <- ETR_FUN(df.sample, ~net_profit) %>% select(-percentile_99.9) %>% 
    rename(percentile_99.9 = percentile_99_rob) %>% ETR_perc_df_FUN() 
df_name <- append(df_name, "df.ETR.size.p.rob")


####  ETR (one measure of ETR for all years), by firm size (percentile) 
  df.1 <- ETR_balpanel_FUN(data, 1) ## Decide number of years a firm stays in the panel
  df.2 <- ETR_balpanel_FUN(data, 2) ## Decide number of years a firm stays in the panel
  df.3 <- ETR_balpanel_FUN(data, 3) ## Decide number of years a firm stays in the panel
  df.4 <- ETR_balpanel_FUN(data, 4)
  df.5 <- ETR_balpanel_FUN(data, 5) 
  df <- rbind(df.1, df.2, df.3, df.4, df.5)
df.ETR.panel.bal <- df
df_name <- append(df_name, "df.ETR.panel.bal")


####  ETR by percentile of payroll
## MEXICO does not have data on payroll or total assets 
  df1 <- ETR_FUN(df.sample, ~net_profit) %>% ungroup()

  if (all(is.na(c(df.sample$labor_inp))) == "FALSE") { 
    
    df1 <- df1 %>% mutate(perc_labor =  ntile(labor_inp, 100), perc_labor = as.numeric(perc_labor),
                                                  # We want percentile to go from 0 to 99
                                                  perc_labor = perc_labor-1)
    # Decompose percentile 99 in 10 bins
    top99 <- df1 %>% filter(perc_labor == 99) %>%  mutate(p_99 = ntile(labor_inp, 10)) %>%
      select(tax_ID, p_99)  %>% ungroup()
    
    df1 <- merge(df1, top99, by =c("tax_ID"), all = TRUE) %>%
      mutate(perc_99 = as.numeric(perc_99),
             perc_labor_99.9 = if_else(!is.na(p_99), perc_labor+(p_99-1)/10, perc_labor)) %>% ungroup()
    
    # Get correlation coefficient between two measures of percentiles
    corr.test <- cor.test(df1$percentile_99.9, df1$perc_labor_99, method = c("pearson"))
    
    df <- df1 %>%  group_by(perc_labor_99.9, country) %>%
      summarize(n_keep = n(),
                n_drop = length(ETR_drop_neg[!is.na(ETR_drop_neg)]),
                ETR_drop_avg= mean(ETR_drop_neg, na.rm =T),
                ETR_keep_avg= mean(ETR_keep_neg, na.rm =T),
                labor_inp = mean(labor_inp, na.rm = T),
                perc_99.9_avg = mean(percentile_99.9, na.rm = T),
                perc_99.9_med = median(percentile_99.9, na.rm = T))
    df$correlation <- corr.test$estimate

df.ETR.payroll <- df
df_name <- append(df_name, "df.ETR.payroll")
}

  
####  ETR by percentile of total_assets
  df1 <- ETR_FUN(df.sample, ~net_profit) %>% ungroup()
  
  if ("total_assets" %in% colnames(df.sample) == "TRUE") {
    df1 <- df1 %>% mutate(perc_asset =  ntile(total_assets, 100), perc_asset = as.numeric(perc_asset),
                                                  # We want percentile to go from 0 to 99
                                                  perc_asset = perc_asset-1) 
    # Decompose percentile 99 in 10 bins
    top99 <- df1 %>% filter(perc_asset == 99) %>%  mutate(p_99 = ntile(total_assets, 10)) %>%
      select(tax_ID, p_99)  %>% ungroup()
    
    df1 <- merge(df1, top99, by =c("tax_ID"), all = TRUE) %>%
      mutate(perc_99 = as.numeric(perc_99),
             perc_asset_99.9 = if_else(!is.na(p_99), perc_asset+(p_99-1)/10, perc_asset)) %>% ungroup()
    # Get correlation coefficient between two measures of percentiles
    corr.test <- cor.test(df1$percentile_99.9, df1$perc_asset_99, method = c("pearson"))
    
    df <- df1 %>%  group_by(perc_asset_99.9, country) %>%
      summarize(n_keep = n(),
                n_drop = length(ETR_drop_neg[!is.na(ETR_drop_neg)]),
                ETR_drop_avg= mean(ETR_drop_neg, na.rm =T),
                ETR_keep_avg= mean(ETR_keep_neg, na.rm =T),
                total_assets = mean(total_assets, na.rm = T),
                perc_99.9_avg = mean(percentile_99.9, na.rm = T),
                perc_99.9_med = median(percentile_99.9, na.rm = T)) 
    df$correlation <- corr.test$estimate

df.ETR.assets <- df
df_name <- append(df_name, "df.ETR.assets")
}


## Larger sectors Percentiles
  df <- ETR_FUN(df.sample, ~net_profit) %>% ETR_sector_perc_FUN(., ~largesector) 
  str <- df.sample %>% STR_sectorsFUN(., ~largesector)
  df <- merge(df, str, by=c("country", "x"))
df.ETR.largesector.p <- df
df_name <- append(df_name, "df.ETR.largesector.p")


### Tag gap capped ETR: revenue forgone (all years) 
  df <- ETR_FUN(data, ~net_profit) %>% ETR_taxgap_cappedFUN()
df.taxgap.cap <- df
df_name <- append(df_name, "df.taxgap.cap")


### ETR less than 15 (Global Minimum Income Tax)
  df <- ETR_FUN(data, ~net_profit) %>% ETR_lower15_FUN()
df.ETR.lower15 <- df
df_name <- append(df_name, "df.ETR.lower15")


print("All dataframes stored")


########################| 4. REGRESSIONS |################################# 
# Now we move on to regression analyses. We first define the df we'll use, 
# then construct a general function with arguments, and then run our
# different specifications


#### + Dataframes:####
#..............................

### Regression (dummies)
df.ETR.reg.1 <- ETR_FUN(df.sample, ~net_profit) %>% dummy_reg_FUN() 


#### + Reg Function:####
#..............................

# Requires tidyverse, stargazer, broom and fixest packages
# Choose the following when calling the function:
# Dataset, explanatory variable, regression type, regression name

regFUN = function(data, explvar, reg_type, reg_name){  
  
  #### SET UP #####.
  #..................
  
  df_profitable <- data.frame(df) %>% filter(!is.na(ETR_drop_neg) & !is.na(explvar)) # data is at the country level
  
  var_agg  <- "STR + exempt_income + tot_deduc_taxbase + loss_carryforward + tot_deduc_taxliab "
  var_agg_c  <- c( "STR", "exempt_income", "tot_deduc_taxbase", "loss_carryforward","tot_deduc_taxliab") # reduced_rate",
  
  var_disagg  <- "reduced_rate + exempt_income + depreciation + investment_taxbase + capital_allowance + 
                  loss_carryforward + investment_taxliab + special_taxliab + trade_taxliab +  
                  cred_foreign_tax + other_cred_taxliab"
  
  char <- c("asset_tax", "min_tax", "foreign_ownership", "capital_city", "firm_age", "first_year", "FEZ",
            'region', 'tax_center')
  
  # BASE 
  base <- paste0("ETR_keep_neg ~ ", explvar)
  
  ###
  country <- df_profitable$country[1]
  
  # --base model 0 ####.
  #..................
  
  # Profitable firms only
  fit0 <- feols(as.formula(paste(base)), data =df_profitable) 
  
  # --base with characteristics (model 1) ####.
  #..................
  
  if (reg_type == "OLS" ){ #| reg_type == "POLS" ){
    # All Countries have sectors
    base <- paste0("ETR_keep_neg ~ ", explvar, " + factor(section)")
    
    # Add characteristics individually for each country
    characteristic_FUN = function(var){
      t <- c()
      if (all(!is.na(df_profitable[[var]])) == TRUE & any(names(df_profitable) == var) ){
        t <- append(t, var)  
      }}
    
    #Cannot use across() so we will use a loop:
    control <- lapply(char, characteristic_FUN) %>% unlist()
    ctr_list <- paste(control, collapse = " + ")
    if (is.null(control) == FALSE){
      base_char <- paste0(base, " + ", ctr_list)
    } else {
      base_char <- paste0(base)
    }
    base <- paste0("ETR_keep_neg ~ ", explvar)
  }
  
  
  # --Regressions ####.
  #..................
  if (reg_type == "OLS"){
    
    f_char <- feols(as.formula(paste(base_char)), data =df_profitable) 
    # --Controls
    f_rate <- feols(as.formula(paste(base, "STR", sep = "+")), data =df_profitable)
    f_exem <- feols(as.formula(paste(base, "exempt_income", sep = "+")), data =df_profitable)
    f_ince <- feols(as.formula(paste(base, "tot_deduc_taxbase", sep = "+")), data =df_profitable)
    f_loss <- feols(as.formula(paste(base, "loss_carryforward", sep = "+")), data =df_profitable)
    f_cred <- feols(as.formula(paste(base, "tot_deduc_taxliab", sep = "+")), data =df_profitable)
    # --All variables
    f_all <- feols(as.formula(paste(base_char, var_agg, sep = "+")), data =df_profitable)
    
    # Print regression table
    sink("NUL") # don't show R console output
    # .tex output
    etable(fit0, f_char, f_rate,  f_exem, f_ince, f_loss, f_cred, f_all,
           tex = TRUE, file = paste0(country, "_reg_", reg_name, ".tex"), replace = TRUE,
           drop = "section", title = paste0(country, "_reg_", reg_name), digits = 2, digits.stats = 2)
    sink()
    print("Regression table saved.")
    
  }
  
  
  sum <- summary(fit0)
  # Save coeff for country average
  tidy0 <- fit0 %>% tidy %>% mutate(model = 0)
  tidy1 <- f_char %>% tidy %>% mutate(model = 1)
  tidy2 <- f_rate %>% tidy %>% mutate(model = 2)
  tidy3 <- f_exem %>% tidy %>% mutate(model = 3)
  tidy4 <- f_ince %>% tidy %>% mutate(model = 4)
  tidy5 <- f_loss %>% tidy %>% mutate(model = 5)
  tidy6<- f_cred %>% tidy %>% mutate(model = 6)
  tidy7 <- f_all %>% tidy %>% mutate(model = 7)
  
  tidyfit <- rbind(tidy0,
                   tidy1,  tidy2, tidy3, tidy4, tidy5, tidy6, tidy7) %>% 
    mutate(country = df$country[[1]], to_drop = NA_real_) %>% 
    filter( term %in% var_agg_c | term %in% char |
              term == "top99" |  term == paste0(explvar) ) 
  
  
} #END 


#### + Apply specifications:####
#..............................
setwd(reg)
reg_name <- c()

# a. Main regressions ####
#...............

# OLS, P90-P99 (dummy top 1 %):
  print(paste0(country_code[[1]], ", OLS model, top 1%"))
  df <- df.ETR.reg.1 %>% filter(country == country_code[[1]]) 
  df <- df %>% 
    filter(decile10 == 10) %>% mutate(top99 = if_else(percentile_99.9 >= 99 , 1, 0))
  df.ETR.fit.T10 <- regFUN(df, "top99", "OLS", "T10")
reg_name <- append(reg_name, "df.ETR.fit.T10")


# OLS, P1-P89 (percentiles):
  print(paste0(country_code[[1]], ", OLS model, up to P90"))
  df <- df.ETR.reg.1 %>% filter(country == country_code[[1]] & percentile_99.9 <=90)
  df.ETR.fit.B90  <- regFUN(df, "percentile_99.9", "OLS", "B90")
reg_name <- append(reg_name, "df.ETR.fit.B90")


# b. Robustness regressions ####
#...............
# + sample is top 10, we change the dummy ####
# + Dummy is top 5% ####.
# OLS, P90-P99 (dummy top 5 %):
  print(paste0(country_code[[1]], ", OLS model, top 5%"))
  df <- df.ETR.reg.1 %>% filter(country == country_code[[1]]) 
  df <- df %>% #merge(df, quantiles_list[[i]], by = c("country", "tax_ID", "year")) %>% 
    filter(decile10 == 10) %>% mutate(top5 = if_else(percentile_99.9 >= 95, 1, 0))
  df.ETR.fit.T10.D5 <- regFUN(df, "top5", "OLS", "T10.D5")
reg_name <- append(reg_name, "df.ETR.fit.T10.D5")

# + Dummy is top 3% ####.
# OLS, P90-P99 (dummy top 3 %):
  print(paste0(country_code[[1]], ", OLS model, top 3%"))
  df <- df.ETR.reg.1 %>% filter(country == country_code[[1]]) 
  df <- df %>% 
    filter(decile10 == 10) %>% mutate(top3 = if_else(percentile_99.9 >= 97, 1, 0))
  df.ETR.fit.T10.D3 <- regFUN(df, "top3", "OLS", "T10.D3")
reg_name <- append(reg_name, "df.ETR.fit.T10.D3")

# + Dummy is top 2% ####.
# OLS, P90-P99 (dummy top 2 %):
  print(paste0(country_code[[1]], ", OLS model, top 2%"))
  df <- df.ETR.reg.1 %>% filter(country == country_code[[1]]) 
  df <- df %>% 
    filter(decile10 == 10) %>% mutate(top2 = if_else(percentile_99.9 >= 98, 1, 0))
  df.ETR.fit.T10.D2 <- regFUN(df, "top2", "OLS", "T10.D2")
reg_name <- append(reg_name, "df.ETR.fit.T10.D2")

# + Dummy is top 0.1% ####.
# OLS, P90-P99 (dummy top 0.1 %):
  print(paste0(country_code[[1]], ", OLS model, top 0.1%"))
  df <- df.ETR.reg.1 %>% filter(country == country_code[[1]]) 
  df <- df %>% 
    filter(decile10 == 10) %>% mutate(top999 = if_else(percentile_99_rob == 99.9 , 1, 0))
  df.ETR.fit.T10.D01  <- regFUN(df, "top999", "OLS", "T10.D01")
reg_name <- append(reg_name, "df.ETR.fit.T10.D01")


# + Percentile continuous at the top 10####.
# OLS, P90-P99 (percentiles):
  print(paste0(country_code[[1]], ", OLS model, continuous, top10"))
  df <- df.ETR.reg.1 %>% filter(country == country_code[[1]]) 
  df <- df %>% 
    filter(decile10 == 10) %>% mutate(percentiles = if_else(percentile_99.9 >= 99 , 99, percentile_99.9))
  df.ETR.fit.T10.C  <- regFUN(df, "percentiles", "OLS", "T10.C")
reg_name <- append(reg_name, "df.ETR.fit.T10.C")


# + sample is top 20, we change the dummy ####
# + dummy top 1 % ####.
# OLS, P80-P99 (dummy top 1 %):
  print(paste0(country_code[[1]], ", OLS model, top 1%"))
  df <- df.ETR.reg.1 %>% filter(country == country_code[[1]]) 
  df <- df %>% 
    filter(decile10 >= 9) %>% mutate(top99 = if_else(percentile_99.9 >= 99, 1, 0))
  df.ETR.fit.T20 <- regFUN(df, "top99", "OLS", "T20")
reg_name <- append(reg_name, "df.ETR.fit.T20")


# + Dummy is top 2% ####.
# OLS, P90-P99 (dummy top 5 %):
  print(paste0(country_code[[1]], ", OLS model, top 2%"))
  df <- df.ETR.reg.1 %>% filter(country == country_code[[1]]) 
  df <- df %>% 
    filter(decile10 >= 9) %>% mutate(top2 = if_else(percentile_99.9 >= 98, 1, 0))
  df.ETR.fit.T20.D2 <- regFUN(df, "top2", "OLS", "T20.D2")
reg_name <- append(reg_name, "df.ETR.fit.T20.D2")

# + Dummy is top 3% ####.
# OLS, P90-P99 (dummy top 3 %):
  print(paste0(country_code[[1]], ", OLS model, top 3%"))
  df <- df.ETR.reg.1 %>% filter(country == country_code[[1]]) 
  df <- df %>% 
    filter(decile10 >= 9) %>% mutate(top3 = if_else(percentile_99.9 >= 97, 1, 0))
  df.ETR.fit.T20.D3  <- regFUN(df, "top3", "OLS", "T20.D3")
reg_name <- append(reg_name, "df.ETR.fit.T20.D3")

# + Dummy is top 5% ####.
# OLS, P90-P99 (dummy top 5 %):
  print(paste0(country_code[[1]], ", OLS model, top 5%"))
  df <- df.ETR.reg.1 %>% filter(country == country_code[[1]]) 
  df <- df %>% 
    filter(decile10 >= 9) %>% mutate(top5 = if_else(percentile_99.9 >= 95, 1, 0))
  df.ETR.fit.T20.D5 <- regFUN(df, "top5", "OLS", "T20.D5")
reg_name <- append(reg_name, "df.ETR.fit.T20.D5")


# + Dummy is top 0.1% ####.
# OLS, P90-P99 (dummy top 0.1 %):
  print(paste0(country_code[[1]], ", OLS model, top 0.1%"))
  df <- df.ETR.reg.1 %>% filter(country == country_code[[1]]) 
  df <- df %>% 
    filter(decile10 >= 9) %>% mutate(top999 = if_else(percentile_99_rob == 99.9 , 1, 0))
  df.ETR.fit.T20.D01 <- regFUN(df, "top999", "OLS", "T20.D01")
reg_name <- append(reg_name, "df.ETR.fit.T20.D01")

# + Percentile continuous at the top 20 ####.
# OLS, P80-P99 (percentiles):
  print(paste0(country_code[[1]], ", OLS model, continuous, top 20"))
  df <- df.ETR.reg.1 %>% filter(country == country_code[[1]]) 
  df <- df %>% 
    filter(decile10 >= 9) %>% mutate(percentiles = if_else(percentile_99.9 >= 99 , 99, percentile_99.9))
  df.ETR.fit.T20.C <- regFUN(df, "percentiles", "OLS", "T20.C")
reg_name <- append(reg_name, "df.ETR.fit.T20.C")


print("Regressions done")


########################| 5. EXTRACT META-DATA  |############################################

#Only need to run this part of the script if you want to extract the metadata.

# #----------- EXTRACT METADATA
# setwd(metadata)
# 
# # --List all graphs to append
 list.name <- append(df_name, reg_name)
# # and store them as objects in one list
# list.df <- list() 
# for (i in 1:length(list.name)){
# list.df[[i]] <- get(list.name[[i]])
# }
# 
# # Save in RDS or csv format
# for (i in 1:length(list.df)){
#   #saveRDS(list.df[[i]], file = paste0(list.name[[i]], ".RDS", sep = "" ))
#   #write.csv(list.df[[i]], file = paste0(list.name[[i]], ".csv", sep = "" ))
#   print(list.name[[i]])
# }


print("End Analysis")
