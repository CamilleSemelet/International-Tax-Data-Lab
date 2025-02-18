
### 3. Output 


print("Begin Output")


########################| 1. EXPORT STATS |#################################


setwd(stat)
write.csv(df.taxgap.cap, file = "df.taxgap.cap.csv") ## Used to create Figure A.2 (see do.file XXXX. with GTED)


########################| 2. FORMAT AND EXPORT TABLES |#################################
setwd(stat)

#### + Table 1: Summary statistics  ####
#..............................
table_sumstat <- df.ETR.descr %>%  select(-pos_netprofit) %>%
                 arrange(country_name) %>% 
                 mutate(STR = STR*100, turnover_avg = turnover_avg/1000) %>%
                 mutate_if(is.numeric, round, digits = 2) %>% 
                 mutate(country_name = if_else(country_name == "Dominican Republic", "Dominican Rep.", country_name),
                        country_name = paste0(country_name, " (", country, ")"),
                        year = paste0(min_year, "-", year)) %>%
  select(-country, -min_year)

table_sumstat <- table_sumstat[, c(7, 1, 4, 2, 3, 6, 5, 9, 8)]
# export
stargazer(table_sumstat, rownames = FALSE, summary = FALSE,  out = "table_sumstat.tex", digits = 1)


#### + Table A2: Top 1 percent: income, CIT, profit and payroll   ####
#..............................
df <- df.perc.distribution %>% filter(percentile == 99) %>% select(-percentile)

df.avg <- df %>%  summarize(pos_profit =  mean(pos_profit),
                            total_income =  mean(total_income),
                            pos_liability =  mean(pos_liability),
                            labor_inp = mean(labor_inp, na.rm = T)) %>%
  mutate(country = "Average",
         year = "")

df <- rbind(df.avg, df) 
df <- df[, c(5, 6, 2, 1, 3, 4)]

df <- df %>% rename( "Revenue" = total_income ,
                     "CIT" = pos_liability , 
                     "Profit"= pos_profit,
                     "Payroll" = labor_inp ) %>%
  mutate_if(is.numeric, round, digits = 1)
# export
stargazer(df, rownames = FALSE, summary = FALSE,  out = "top1_stats.tex", digits = 1)


#### + Table A3: Number of firms in each bin   ####
#..............................
df_avg <- df.ETR.size.p %>% gather(Measure, value, "ETR_drop_avg":"ETR_keep_avg")  
df99 <- df_avg %>% filter(Measure == "ETR_keep_avg") %>%  
  select(country, n_keep, n_drop, percentile_99.9) %>% 
  mutate(percentile_99.9 = if_else(percentile_99.9 >= 99, 99, percentile_99.9)) %>% 
  group_by(country, percentile_99.9) %>% 
  summarize(n_keep = sum(n_keep, na.rm = T), 
            n_drop = sum(n_drop, na.rm = T)) %>% ungroup() 

# Number for sample of all firms
N.all <- df99 %>% select(country, n_keep, percentile_99.9) %>%
  filter(percentile_99.9 == "90" | percentile_99.9 == "98" | percentile_99.9 == "99") %>%
  pivot_wider(names_from = "country", values_from = n_keep) 

# Number for sample of profitable firms
N.prof <- df99 %>% select(country, n_drop, percentile_99.9) %>%  
  filter(percentile_99.9 == "90" | percentile_99.9 == "98" | percentile_99.9 == "99") %>%
  pivot_wider(names_from = "country", values_from = n_drop) 
# export
stargazer(N.all, summary = FALSE,  out = "N_firms_perc90_all.tex")
stargazer(N.prof, summary = FALSE,  out = "N_firms_perc90_profitable.tex")


########################| 3. REGRESSION TABLES |#################################
setwd(reg)

# List of dataframes used
df.fit <- list(df.ETR.fit.B90, df.ETR.fit.T10)
regression.nb <- c("B90", "T10")

#### + Table 2 and A4: Explaining the ETR relationship ####
#..............................
# - sign of the coefficient
# - one-sided t-test
# - number of countries 

for (i in 1:length(df.fit)){
  # Store explanatory var
  explvar <- df.fit[[i]]$term[1]
  df.fit[[i]]$reg <- regression.nb[[i]]
  df.ETR.fit <- df.fit[[i]]  %>% mutate(
    # - sign of the coefficient
    pos_coeff = if_else(estimate > 0, 1, 0),
    neg_coeff = if_else(estimate < 0, 1, 0),
    # - one-sided t-test
    one_sided_pos = if_else(pos_coeff == 1 & statistic >= 1.645, 1, 0),
    one_sided_neg = if_else(neg_coeff == 1 & statistic <= -1.645, 1, 0)) %>% # direction of the estimate Panel A
    group_by(model, term) %>%
    # - number of countries 
    summarise(nbr_country= length(estimate), estimate=mean(estimate), std.error = mean(std.error),
              pos_coeff = sum(pos_coeff, na.rm = T), 
              neg_coeff = sum(neg_coeff, na.rm = T),
              ttest_pos = sum(one_sided_pos),
              ttest_neg = sum(one_sided_neg),
              tstat= mean(statistic)) %>% ungroup() 
  
  # Save results with all coefficients 
  write.csv(df.ETR.fit, file = paste0("all_coeff_", regression.nb[[i]], ".csv")) # Raw
  
  # Select only the coefficient of interest Beta 
  table.fit <- df.ETR.fit %>% filter(term == explvar) %>%
    select(estimate,  pos_coeff, neg_coeff, ttest_pos, ttest_neg, tstat,nbr_country) %>%
    t() %>% as.data.frame() ## transpose 
  # Rename 
  colnames(table.fit) <-c("Baseline", "Characteristics", "Reduced rate", "Exempt income", "Special deductions", "Re-timing", "Credits", "All")
  # colnames(table.fit) <-c("Base", "Characteristics", "Reduced rate", "Exempt income", "Special deductions", "Re-timing", "Credits",
  #                         "All", "Base (all)")
  # Subset columns
  if (regression.nb[[i]] =="B90") {
    rownames(table.fit) <- c("Percentile (1-89)",  'N positive coeff.', 'N negative coeff.', 'Upper one-sided t-test', 'Lower one-sided t-test', "tstat","N country") 
    table.fit <- subset(table.fit, rownames(table.fit) %in% c("Percentile (1-89)",  'N positive coeff.', 'Upper one-sided t-test',"N country")) 
  } else {
    rownames(table.fit) <- c("Dummy Top 1%",  'N positive coeff.', 'N negative coeff.', 'Upper one-sided t-test', 'Lower one-sided t-test', "tstat","N country") 
    table.fit <- subset(table.fit, rownames(table.fit) %in% c("Dummy Top 1%",  'N negative coeff.', 'Lower one-sided t-test',"N country")) 
  }
  
  # -- Extract 
  sink("NUL") 
  stargazer(table.fit, summary = FALSE, out = paste0("table_fit_", regression.nb[[i]], ".tex"), digits = 2 )
  sink()
  print(regression.nb[[i]])
}


#### + Table A6: Explaining the ETR relationship (Detailed coefficients for each country) ####
#..............................

# For each regression:
# Add stars
starsFUN <- function(x, out=NULL, ...){
  undo <- gsub("\\\\textasteriskcentered", "*", stargazer(x, ...))
  restar <- gsub("* * *", "$^{***}$", undo, fixed = TRUE)
  restar <- gsub("* *", "$^{**}$", restar, fixed = TRUE)
  restar <- gsub("* ", "$^{*}$", restar, fixed = TRUE)
  if(!is.null(out)) cat(restar, file = out, sep="\n")
  restar
}

for (i in 1:length(df.fit)){
  # Store explanatory var
  explvar <- df.fit[[i]]$term[1]
  
  df.ETR.fit <- df.fit[[i]]  %>%
    group_by(model, term) %>%
    summarise(nbr_country= length(estimate), estimate=mean(estimate, na.rm = T), std.error = mean(std.error),
              p.value = length(p.value[p.value<0.05])) %>% ungroup()
  
  # --merge average and countries separately
  table.fit <- df.ETR.fit %>% filter(term==explvar) %>% select(estimate) %>% mutate(estimate = as.character(round(estimate, 2))) %>%
    t() %>% as.data.frame() %>% mutate(country = "Average") 
  colnames(table.fit) <- c("0", "1", "2", "3", "4", "5", "6", "7", "country")

  temp <- df.fit[[i]] %>% filter(term==explvar) %>% mutate(stars = if_else(p.value<0.1, "*", ""),
                                                           stars = if_else(p.value<0.05, "**", stars),
                                                           stars = if_else(p.value<0.01, "***", stars),
                                                           estimate.star = paste(round(estimate, 2), stars)) %>% 
    group_by(country, model) %>% select(estimate.star) %>% spread(model, estimate.star)
  rownames(temp) <- temp$country
  
  
  df <- bind_rows(table.fit, temp)
  colnames(df) <- c( "Baseline", "Characteristics", "Reduced rate", "Exempt income", "Special deductions", "Re-timing", "Credits",
                     "All", "country")
  df <- df[, c(9, 1,2,3,4,5,6,7, 8)]
  
  # -- Extract
  sink("NUL") 
  starsFUN(df, 
          summary = FALSE, 
          rownames = FALSE, 
          out = paste0("all_countries_fit_", regression.nb[[i]], ".tex"),
          type = "latex",
          colnames = TRUE, 
          notes = "Levels: ${}^{***} p < .01$, ${}^{**} p < .05$, ${}^{*} p < .1$")
  sink()
  print(regression.nb[[i]])
}


#### + Table A5: Robustness  ####
#..............................
# List of regressions used
df.fit <- list(df.ETR.fit.T10, df.ETR.fit.T10.D2, df.ETR.fit.T10.D3, df.ETR.fit.T10.D01, df.ETR.fit.T10.C,
               df.ETR.fit.T20, df.ETR.fit.T20.D2, df.ETR.fit.T20.D3, df.ETR.fit.T20.D01, df.ETR.fit.T20.C)
regression.nb <- c("T10", "T10.D2", "T10.D3", "T10.D01", "T10.C", 
                   "T20", "T20.D2", "T20.D3", "T20.D01", "T20.C")

temp <- list()

for (i in 1:length(df.fit)){
  # Store explanatory var
  explvar <- df.fit[[i]]$term[1]
  df.fit[[i]]$reg <- regression.nb[[i]]
  df.ETR.fit <- df.fit[[i]]  %>% mutate(# - sign of the coefficient
    pos_coeff = if_else(estimate > 0, 1, 0),
    neg_coeff = if_else(estimate < 0, 1, 0),
    # - one-sided t-test
    one_sided_pos = if_else(pos_coeff == 1 & statistic >= 1.645, 1, 0),
    one_sided_neg = if_else(neg_coeff == 1 & statistic <= -1.645, 1, 0)) %>% # direction of the estimate Panel A
    group_by(model, term) %>%
    summarise(nbr_country= length(estimate), estimate=mean(estimate), std.error = mean(std.error),
              pos_coeff = sum(pos_coeff, na.rm = T), 
              neg_coeff = sum(neg_coeff, na.rm = T),
              ttest_pos = sum(one_sided_pos),
              ttest_neg = sum(one_sided_neg),
              tstat= mean(statistic)) %>% ungroup() %>% filter(model == 0)
  
  # Select only the coefficient of interest Beta 
  table.fit <- df.ETR.fit %>% filter(term == explvar) %>%
    select(estimate,  pos_coeff, neg_coeff, ttest_pos, ttest_neg, tstat,nbr_country) %>%
    t() %>% as.data.frame() ## transpose 
  # Rename 
  colnames(table.fit) <-c(regression.nb[[i]])
  # Subset columns
  rownames(table.fit) <- c(regression.nb[[i]],  'N positive coeff.', 'N negative coeff.', 'Upper one-sided t-test', 'Lower one-sided t-test', "tstat","N country") 
  table.fit <- subset(table.fit, rownames(table.fit) %in% c(regression.nb[[i]],  'N negative coeff.', 'Lower one-sided t-test',"N country")) 
  
  temp[[i]] <- table.fit
}

table.fit <- do.call("cbind", temp)

# -- Extract 
sink("NUL") 
stargazer(table.fit, summary = FALSE, out = paste0("table_fit_robustness.tex"), digits = 2 )
sink()
print("Robustness table")




########################| 4. CONSTRUCT AND EXPORT FIGURES |#################################

#### Preamble  ####
#..............................

#-----------  Rename function
renameFUN <- function(df){
  df <- df %>% mutate(ETR_denominator = recode(ETR_denominator, net_profit="Net Profit"),
                              Measure = recode(Measure, ETR_drop_avg = "Profitable firms", 
                                                        ETR_keep_avg = "All firms"),
                              Measure = factor(Measure, levels = c("Profitable firms", "All firms")))
}
#----------- Choose themes and options for graph 

line_size <- 0.9
dot_size <- 0.9

theme_niwot <- function(){
  theme_bw() +
    theme(#text = element_text(family = "Helvetica Light"),
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 18, face = "plain"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      
      #panel.grid.major.y = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
      plot.title = element_text(size = 18, vjust = 1, hjust = 0),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      legend.position = "top",
      legend.key = element_blank())
}
theme_set(theme_niwot())


#----------- Percentile function: allows us to control the visual gap between quantiles at the top
percentileFUN = function(df){
  df1 <- df  %>% 
    mutate(percentile_99.9 = round(percentile_99.9, digits = 1),
           p_chr = as.character(percentile_99.9),
           p = percentile_99.9,
           p = if_else(p_chr=="99", 101, p),
           p = if_else(p_chr=="99.1", 103, p),
           p = if_else(p_chr=="99.2", 105, p),
           p = if_else(p_chr=="99.3", 107, p),
           p = if_else(p_chr=="99.4", 109, p),
           p = if_else(p_chr=="99.5", 111, p),
           p = if_else(p_chr=="99.6", 114, p),
           p = if_else(p_chr=="99.7", 117, p),
           p = if_else(p_chr=="99.8", 119, p), 
           p = if_else(p_chr=="99.9", 121, p)) ##1
  
}



#### ETR by Percentile of Turnover #### 

#------ More on geom_spline ------#

# SPLINE: spar (low number=better fit to data), df (does not seem to be moving)
# fit a cubic smoothing spline: 
#  m=2 , i.e. the cubic smoothing line penalizes the squared second derivative of the function
#  Cubic smoothing splines estimate f(.) using piecewise cubic functions, 
#  which are connected at points known as 'knots', which are all the x's.
# We should in theory use cross validation to avoid overfitting. 

#  
# df: desired equivalent degrees of freedom 
# spar: smoothing parameter 
#---------------------------------#
## ss comes from npreg package


# Data:
df_avg <- df.ETR.size.p %>% gather(Measure, value, "ETR_drop_avg":"ETR_keep_avg") 
# Add full country names 
df <- df.ETR.descr %>% select(country_name, country) 
df_avg <- merge(df_avg, df, by = c("country"))
# Add max STR by country, and groups
df_avg <- df_avg %>% group_by(country) %>% 
  mutate(max_STR = max(STR)) %>%
  renameFUN() %>% percentileFUN()


# Graph (control version spline):
ETR_group_GRAPH <- function(dataset, colour, knot){
    dataset$country_name <- reorder(dataset$country_name, dataset$max_STR) 
    dSTR <- dataset %>% group_by(country_name) %>% summarise(value = max(STR), Measure = "Top STR", p = p) 
    p <- ggplot(data = dataset, aes(x = p, 
                                    y = value, colour=Measure, linetype=Measure))
    p <- p  +  scale_x_continuous(#breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
                                  breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
                                  #labels =  c("","20","","40","","60","","80","","99", "", ".9")) +
                                  labels =  c("","20","","40","","60","","80","","99","",  "99.9")) + 
                                  
     #annotate("rect", xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf, alpha = 0.1) + 
      geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf, fill = "Top 1%"), colour = NA, alpha = 0.05) +
      geom_point( shape = c(3), size = 0.5, color = 'grey60') +
     #geom_spline(aes(x = p, y = value), size= 0.9, spar = 0.6, df =9) +
      geom_spline(aes( x = p, y = value), size= 0.9, nknots = knot) +
      geom_line(data = dSTR, aes(x = p, y = value), size = 0.8) +
      facet_wrap(~country_name, drop = T, ncol = 3) 
    
     #Aesthetic
      p <- p + scale_colour_manual(values = c(colour, "grey40")) +
      scale_linetype_manual(values = c("solid", "dashed")) +
      scale_fill_manual("Top 1%", values = "lightgrey", 
                        guide = guide_legend(override.aes = list(alpha = 1))) +
      theme(axis.text = element_text(size = 9),   panel.grid.major.y = element_blank(),
            axis.title =  element_text(size = 11, margin = margin(r = 10))) #  +
      #scale_y_continuous(limits = c(0,NA)) 
      p <- p +  labs(#title = paste0("STR", group), 
      y = "", x = "\nFirm Size Quantiles") +
      guides(shape = guide_legend(""), color = guide_legend(""), #fill = guide_legend(""), 
             linetype = guide_legend(""), label = guide_legend("")) + theme(strip.background = element_blank()) 
}
 
#### + Figure 2: ETR, all firms:  ####
#................................

df.all <- df_avg  %>% 
  filter(Measure == 'All firms' & ETR_denominator == "Net Profit") %>% 
  select(country_name, country, value, Measure, p, ETR_denominator, STR, max_STR) %>%
  mutate(Measure = "ETR (All firms, incl. loss-making)") 


# Export
setwd(graph)
pdf("ETR_all_firms.pdf")
print(ETR_group_GRAPH(df.all, "#00BFC4", 6))  # #F28500 ##92a1cf
dev.off()


#### + Figure A3: ETR, profitable firms:  ####
#................................
df.prof <- df_avg %>% 
  filter(Measure == 'Profitable firms' & ETR_denominator == "Net Profit") %>% 
  select(country_name, country, value, Measure, p, ETR_denominator, STR, max_STR) %>%
  mutate(Measure = "ETR (Profitable firms only)") 


# Export
setwd(graph)
pdf("ETR_profiable_firms.pdf")
print(ETR_group_GRAPH(df.prof, "#8073ac", 6))  # #F28500 ##92a1cf
dev.off()



#### + Figure 3: ETR-STR, profitable firms:  ####
#................................

# Data:
df_str_etr <- df.ETR.size.p %>% group_by(country) %>% 
  mutate(max_STR = max(STR)) %>% 
  ungroup() %>%
  select(STR, ETR_minus_STR, ETR_drop_avg, percentile_99.9, country, max_STR) %>%
  percentileFUN() 
df_c <- df.ETR.descr %>% select(country_name, country)
df_str_etr <- merge(df_str_etr, df_c, by = c("country"))


# Graph:
ETR_STR_p_GRAPH <- function(dataset, yvar){
  dataset$yvar<- f_eval(~uq(yvar), data = dataset)
  dataset <- dataset %>% mutate(Measure = "ETR-STR (Profitable firms only)")
  dataset$country_name <- reorder(dataset$country_name, dataset$max_STR) 
  
  p <- ggplot(data = dataset, aes(x = p, 
                                  y = yvar, colour=Measure)) 
  p <- p  + 
    scale_x_continuous( breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
                        labels =  c("","20","","40","","60","","80","","99", "", "99.9")) +
    geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf, fill = "Top 1%"), colour = NA, alpha = 0.05) +
    geom_point( shape = c(3), size = 0.5, color = 'grey60') +
    geom_hline( yintercept = 0, size  = 0.2, color = "grey40", linetype ="longdash") + 
    # geom_spline(aes(x = p, y = yvar), size= 0.9, spar = 0.6, df =9) +
    geom_spline(aes(x = p, y = yvar), size= 0.9, nknots = 6) +
    scale_fill_manual("Top 1%", values = "lightgrey", 
                      guide = guide_legend(override.aes = list(alpha = 1))) +
    scale_colour_manual(values = c("#F8766D")) +
    facet_wrap(~country_name, drop = T, ncol = 3)  
  p  <- p +  labs(#title = title, 
    y = "", x = "\nFirm Size Quantiles") +
    theme(axis.text = element_text(size = 9),   panel.grid.major.y = element_blank(),
          axis.title =  element_text(size = 11, margin = margin(r = 10)))  
  
  p <- p +  guides(shape = guide_legend(""), #fill = guide_legend(""), 
                   linetype = guide_legend(""), label = guide_legend("")) + theme(strip.background = element_blank())
}
#dev.off()
print(ETR_STR_p_GRAPH(df_str_etr, ~ETR_minus_STR))
dev.off()


# Export:
setwd(graph)
pdf("ETR_minus_STR_profitable.pdf")
print(ETR_STR_p_GRAPH(df_str_etr, ~ETR_minus_STR))
dev.off()



#### + Figure 4: Robustness based on Average distribution  ####
#................................


#### +-- 4.a.: Average Distribution  ####
#................................
# [HERE IT IS REDUNDANT with Figure A4 if ran on only one country

# Graph: 
ETR_size_avg_GRAPH <- function(dataset, ytitle){
  dataset <- dataset %>% percentileFUN() %>%
    filter(Measure ==  "ETR_drop_avg") %>% mutate(Measure = "Average ETR (Profitable firms only)")
  sub <- dataset
  p <- ggplot(data = dataset, aes(x = p, 
                                  y = value, group = Measure, color=factor(Measure)))  
  p <- p  +  
    geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf), colour = NA, alpha = 0.1, fill = "lightgrey") +
    geom_spline(aes(x = p, y = value), size= 0.9, nknots = 6) +
    geom_point(shape = c(3), size = 0.9, color = "grey60") +
    scale_x_continuous( breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
                        labels =  c("10","20","30","40","50","60","70","80","90","99.0", "", "99.9")) +
    scale_color_manual(values = c("#F8766D")) 
  p  <- p + labs(  x = "\nFirm Size Quantiles", y = "")  +
    theme(axis.text = element_text(size = 11),   panel.grid.major.y = element_blank(),
          axis.title =  element_text(size = 13, margin = margin(r = 10))) 
  p <- p + guides(shape = guide_legend(""), fill = guide_legend(""), 
                  linetype = guide_legend(""), label = guide_legend("")) + 
    theme(strip.background = element_blank()) 
  print(p)
  
}

# Data:
df_avg_NP <- df.ETR.size.p  %>% 
  group_by(percentile_99.9) %>%
  summarize(ETR_drop_avg=mean(ETR_drop_avg, na.rm = T), 
            ETR_keep_avg=mean(ETR_keep_avg, na.rm = T),
            n_keep =mean(n_keep), n_drop = mean(n_drop)) %>% 
  gather(Measure, value, "ETR_drop_avg":"ETR_keep_avg")  %>% ungroup()  %>%
  mutate(country_name = 1) 

# Export:
setwd(graph)
pdf("robust_ETR_avg_profit.pdf")
ETR_size_avg_GRAPH(df_avg_NP, "Effective Tax Rate (%)\n")
dev.off()


#### +-- 4.b.: Sectors  ####
#................................

df <- df.ETR.largesector.p %>% percentileFUN() %>% group_by(p, x) %>% 
  summarize(value = mean(ETR_drop_avg, na.rm = T),
            nb_country = n())

# Graph: 
ETR_largesectordec_GRAPH <- function(dataset){
  dataset <- dataset %>% filter(!is.na(x)) %>%
    mutate(x = factor(x, levels = c("Primary", "Secondary", "Retail", "Services")))
  p <- ggplot(data = dataset, aes(x = p, 
                                  y = value, group = x, color=factor(x), linetype = factor(x)))  
  p <- p  +   geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf), colour = NA, alpha = 0.1, fill = "lightgrey") +
    geom_spline(aes(x = p, y = value), size= 0.9, nknots = 6) +
    scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
                       labels =  c("10","20","30","40","50","60","70","80","90","99.0", "", "99.9")) +
    scale_color_manual(values = c("#008ECE" ,"#990000" ,"#588300","#FAAB18"  )) + # +"#DF536B",  , "#F5C710"
    scale_linetype_manual(values = c("dashed", "dotted", "longdash", "dotdash"))
  p  <- p +  labs(
    x = "\nFirm Size Quantiles", y = "") +
    theme(axis.text = element_text(size = 11),  panel.grid.major.y = element_blank(),
          axis.title =  element_text(size = 13, margin = margin(r = 10))) 
  p <- p + guides(shape = guide_legend(""), color = guide_legend(""), fill = guide_legend(""),
                  linetype = guide_legend(""), label = guide_legend("")) 
  
  print(p)
}

# Export:
setwd(graph)
pdf("ETRavg_sector_perc.pdf")
ETR_largesectordec_GRAPH(df)
dev.off()



#### +-- 4.c.: Panel Dimension  ####
#................................

# Data:
df <- df.ETR.panel.bal %>% gather(Measure, value, "ETR_all_panel":"ETR_prof_cross") %>% 
  group_by(percentile_99.9, Measure, year) %>%
  summarize(value = mean(value, na.rm = T),
            n_country = length(country)) %>% mutate(country = "") %>% ungroup() %>%
  select(Measure, percentile_99.9, value, year, n_country)

df_avg_panel_prof <- df %>% filter(Measure == "ETR_prof_cross") %>% filter(year>1)

# Graph:
ETR_balpanel_GRAPH = function(dataset){
  dataset <- dataset %>% percentileFUN() %>%
    mutate(year = paste0(year, "-year-ETR"))
  p <- ggplot(data = dataset, aes(x = p, 
                                  y = value, group = year, color=factor(year), linetype = factor(year)))  
  p <- p  +   geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf), colour = NA, alpha = 0.1, fill = "lightgrey") +
    geom_spline(aes(x = p, y = value), size= 0.9, nknots = 6) +
    scale_x_continuous( breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
                        labels =  c("10","20","30","40","50","60","70","80","90","99.0", "", "99.9")) #+
  p <- p + labs(
    x = "\nFirm Size Quantiles", y = "")  
  p <- p + guides(shape = guide_legend(""), color = guide_legend("", nrow = 2), fill = guide_legend(""),
                  linetype = guide_legend("", nrow = 2), label = guide_legend("")) +
    theme(axis.text = element_text(size = 11),   panel.grid.major.y = element_blank(),
          axis.title =  element_text(size = 13, margin = margin(r = 10))) 
  
}

# Export: 
setwd(graph)
pdf("ETR_avg_x_year_prof.pdf")
print(ETR_balpanel_GRAPH(df_avg_panel_prof)) 
dev.off()


#### + Figure 5: A Minimum Tax  ####
#...............................


# Data: 
#Share of firms
df_share <-
  df.ETR.lower15 %>% filter(percentile_99.9 > 98) %>% group_by(country) %>% 
  filter(year == max(year)) %>%
  fsum %>%
  mutate(
    percentile_99.9 = "top 1%",
    less15_shr = round(100 * n_less15 / n, digits = 0)) %>%
  select(-old_taxrev,-new_taxrev, -STR, -year, -old_taxrev_built, -new_taxrev_built)

#Tax revenue increase (top 1% moved to 15%)
#Sum tax revenue for all firms with normal ETRs
df_old <-
  df.ETR.lower15 %>% group_by(country, year) %>% 
  summarise(old_taxrev = sum(old_taxrev),
            old_taxrev_built = sum(old_taxrev_built))
# For those in the top 1%, apply the new tax revenue if ETR is minimum 15%
df_new <-
  df.ETR.lower15 %>% mutate(new_taxrev = if_else(percentile_99.9 > 98, new_taxrev, old_taxrev)) %>%
  group_by(country, year) %>% 
  summarise(new_taxrev  = sum(new_taxrev),
            new_taxrev_built  = sum(new_taxrev_built))
df <- merge(df_old, df_new, by = c("country", "year"))
df <- df %>% mutate(pct_incr = round(100*(new_taxrev - old_taxrev)/old_taxrev, digits = 0),
                    pct_incr_built = round(
                      100 * (new_taxrev_built - old_taxrev_built) / old_taxrev_built, digits = 0))

df <- df %>% group_by(country) %>% filter(year == max(year)) 

#Merge both+country name
df <- merge(df_share, df, by = c("country"))
name <- df.ETR.descr %>% select(country_name, country, STR)
df <- merge(df, name, by = c("country"))
df <- df %>% mutate(max_STR = STR*100)


# Graph: 
ETR_lower15_GRAPH <- function(dataset, x, x_title, color, upperbound){
  dataset$x <- f_eval(~uq(x), data = dataset)
  dataset$country_name <- reorder(dataset$country_name, dataset$max_STR) 
  dataset <- dataset %>% mutate(label = as.character(x))
  p <- ggplot(data = dataset, aes(x = x, 
                                  y = country_name)) +
    geom_bar(stat = "identity", position = "dodge", fill = color) +
    geom_text(aes(label = label), hjust = -.2, size = 4.5)
  p <- p + labs(#title = metric,  
    x = x_title, y = "")  
  p <- p +scale_x_continuous(limits = c(0,upperbound)) +
    theme(axis.text = element_text(size = 14),   panel.grid.major.y = element_blank(),
          axis.title =  element_text(size = 14, margin = margin(r = 10))) 
  p <- p + guides(shape = guide_legend(""), color = guide_legend(""), fill = guide_legend(""),
                  linetype = guide_legend(""), label = guide_legend("")) 
  print(p)
  
}
grid.newpage()

#### +-- 5.a.: ETR less than 15%  ####
#...............................
setwd(graph)
pdf("less15_prof_shr.pdf")
ETR_lower15_GRAPH(df, ~less15_shr, "Share of firms (%)", "steelblue4", 50)
dev.off()

#### +-- 5.b.: Tax Revenue Forgone ####
#...............................
setwd(graph)
pdf("less15_taxrev.pdf")
ETR_lower15_GRAPH(df, ~pct_incr_built, "\nPercentage increase (%)", "steelblue1", 115)
dev.off()



#### + Figure A4: Alternative Measure of size  ####
#...............................

print("line 662: Uncomment Figure A4 if data on total payroll or asset is available")
# # Data Payroll: 
# df <- df.ETR.payroll %>% gather(Measure, value, "ETR_drop_avg":"ETR_keep_avg") %>%
#   filter(Measure=="ETR_drop_avg") %>% rename(percentile_99.9 = perc_labor_99.9)
# name <- df.ETR.descr %>% select(country_name, country)
# df.payroll <- merge(df, name, by = c("country"))
# 
# # Data Assets: 
# df <- df.ETR.assets %>% gather(Measure, value, "ETR_drop_avg":"ETR_keep_avg") %>%
#   filter(Measure=="ETR_drop_avg") %>% rename(percentile_99.9 = perc_asset_99.9)
# name <- df.ETR.descr %>% select(country_name, country)
# df.assets <- merge(df, name, by = c("country"))
# 
# 
# # Data:merge asset and payroll
# df.payroll <- df.payroll %>% mutate(Measure = "Payroll") %>% select(-labor_inp)
# df.assets <- df.assets %>% mutate(Measure = "Total assets") %>% select(-total_assets)
# df.merge.a.p <- rbind(df.payroll, df.assets)
# 
# 
# # Graph: 
# ETR_sizerank_group_GRAPH <- function(dataset, var, color1, color2){
#   
#   dataset <- dataset %>% filter(!is.na(percentile_99.9))  %>% percentileFUN() 
#   p <- ggplot(data = dataset, aes(x = p, 
#                                   y = value, group = Measure, color=factor(Measure), linetype = factor(Measure)))  
#   p <- p  +   geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf), colour = NA, alpha = 0.1, fill = "lightgrey") +
#     geom_spline(aes(x = p, y = value), size= 0.9, nknots = 7) +
#     scale_x_continuous( breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
#                         labels =  c("","20","","40","","60","","80","","99", "", "99.9")) +
#     facet_wrap(~country_name, drop = T, ncol = 3) 
#   p <- p + labs(#title = metric,  
#     x = paste0("\n Percentile Distribution (Payroll or Total assets)"), y = "Effective Tax Rate (%)\n")  
#   p <- ggstyleFUN(p) + scale_color_manual(values = c(color1, color2)) + scale_linetype_manual(values = c("solid", "longdash")) +
#     theme(axis.text = element_text(size = 11),   panel.grid.major.y = element_blank(),
#           axis.title =  element_text(size = 13, margin = margin(r = 10))) 
#   p <- p + guides(shape = guide_legend(""), color = guide_legend(""), fill = guide_legend(""),
#                   linetype = guide_legend(""), label = guide_legend("")) 
#   print(p)
# }
# 
# 
# # Export
# setwd(graph)
# pdf("ETR_size_robust.pdf")
# ETR_sizerank_group_GRAPH(df.merge.a.p, "see legend", "#61D04F", "#1B9E77") 
# dev.off()
# 
# # extract correlation coefficients between percentile of Measures and Revenue
# corr <- df.merge.a.p %>% group_by(Measure, country) %>% select(correlation) %>% fmean()
# setwd(stat)
write.csv(corr, "correlation_robust_size.csv")



#### + Figure A5: Alternative Numerator  ####
#................................
# Data:
df_avg <- df.ETR.size.num.p %>% gather(Measure, value, "ETR_drop_avg":"ETR_keep_avg") 
# Add full country names 
df <- df.ETR.descr %>% select(country_name, country) 
df_avg <- merge(df_avg, df, by = c("country")) %>% group_by(country) %>% 
  mutate(max_STR = max(STR))

df.prof <- df_avg  %>% percentileFUN() %>% filter(Measure == "ETR_drop_avg") %>% 
  select(country_name, country, value, p, Measure, ETR_denominator, STR, max_STR) %>%
  filter(ETR_denominator == "ntl_noelse" | ETR_denominator == "net_tax_liability") %>%
  mutate(Measure = if_else(ETR_denominator == "ntl_noelse", "Alternative ETR Measure", Measure),
         Measure = if_else(ETR_denominator == "net_tax_liability", "Main ETR Measure", Measure),
         Measure = factor(Measure, levels=c("Main ETR Measure", "Alternative ETR Measure")))



# Graph (control version spline):
ETR_numerator_GRAPH <- function(dataset, colour, knot, numerator, name){
  dataset$numerator <- f_eval(~uq(numerator), data = dataset)
  dataset$country_name <- reorder(dataset$country_name, dataset$max_STR) 
  dSTR <- dataset %>% group_by(country_name) %>% summarise(value = max(STR), Measure = "Top STR", p = p) 
  p <- ggplot(data = dataset, aes(x = p, 
                                  y = value, colour = Measure, linetype = Measure))
  p <- p  +  scale_x_continuous( breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
                                 labels =  c("","20","","40","","60","","80","","99", "", "99.9")) +
    geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf, fill = "Top 1%"), colour = NA, alpha = 0.05) +
    geom_spline(aes( x = p, y = value), size= 0.9, nknots = knot) +
    facet_wrap(~country_name, drop = T, ncol = 3) 
  
  #Aesthetic
  p <- p + scale_colour_manual(values = c(colour,  "grey40")) +
    scale_linetype_manual(values = c( "solid", "dashed")) +
    scale_fill_manual("Top 1%", values = "lightgrey", 
                      guide = guide_legend(override.aes = list(alpha = 1))) 
  p <- p +  labs(
    y = "", x = "\nFirm Size Quantiles") +
    guides(shape = guide_legend(""), color = guide_legend(""),
           linetype = guide_legend(""), label = guide_legend("")) + 
    theme(strip.background = element_blank(), axis.text = element_text(size = 11),   
          panel.grid.major.y = element_blank(), 
          axis.title =  element_text(size = 13, margin = margin(r = 10)))
}


# Export
setwd(graph)
pdf("ETR_prof_ntl_noelse.pdf")
print(ETR_numerator_GRAPH(df.prof, "#92a1cf", 7, "ntl_noelse", "Foreign & Other Tax Credits are excluded"))  # #F28500 ##92a1cf
dev.off()


#### +  Figure A6: Robustness fit  ####
#................................

# Data:
df_avg_NP_98 <- df.ETR.size.p  %>% 
  group_by(percentile_99.9) %>%
  summarize(ETR_drop_avg=mean(ETR_drop_avg, na.rm = T), ETR_keep_avg=mean(ETR_keep_avg, na.rm = T),
            n_keep =mean(n_keep), n_drop = mean(n_drop)) %>% 
  gather(Measure, value, "ETR_drop_avg":"ETR_keep_avg")  %>% ungroup()  %>%
  mutate(country_name = 1) %>% filter(Measure == "ETR_drop_avg") %>%
  mutate(Measure = "Top 1% split in 5 bins")

df_avg_NP_99 <- df.ETR.size.p.rob  %>% 
  group_by(percentile_99.9) %>%
  summarize(ETR_drop_avg=mean(ETR_drop_avg, na.rm = T), ETR_keep_avg=mean(ETR_keep_avg, na.rm = T),
            n_keep =mean(n_keep), n_drop = mean(n_drop)) %>% 
  gather(Measure, value, "ETR_drop_avg":"ETR_keep_avg")  %>% ungroup()  %>%
  mutate(country_name = 1) %>% filter(Measure == "ETR_drop_avg") %>%
  mutate(Measure = "Top 1% split in 10 bins")

df <- rbind(df_avg_NP_98, df_avg_NP_99) %>% percentileFUN()

 
# Graph: 
ETR_size_avg_r_GRAPH <- function(dataset, ytitle){
  sub <- df
  p <- ggplot(data = df, aes(x = p, 
                                  y = value, group = Measure, color=factor(Measure)))  
  p <- p  +  
    geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf), colour = NA, alpha = 0.1, fill = "lightgrey") +
    geom_point(shape = c(3), size = 0.9, color = "grey60") +
    lapply(c(4,6,8), function(i){
      geom_spline(data = ~cbind(., facet = i),
                  size= 0.9, nknots = i) 
    }) +
    facet_wrap(vars(facet), ncol = 3) +
    scale_x_continuous( breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
                        labels =  c("","20","","40","","60","","80","","99", "", "99.9")) 
  p  <- p + labs(
    x = "\nFirm Size Quantiles", y = "")  +
    theme(axis.text = element_text(size = 11),   panel.grid.major.y = element_blank(),
          axis.title =  element_text(size = 13, margin = margin(r = 10))) #+
  p <- p + guides(shape = guide_legend(""), fill = guide_legend(""), 
                  linetype = guide_legend(""), label = guide_legend("")) + theme(strip.background = element_blank()) #, axis.title = element_blank())
  print(p)
  
}

# Export:
setwd(graph)
pdf("robust_ETR_avg_profit_fit.pdf")
ETR_size_avg_r_GRAPH(df, "Effective Tax Rate (%)\n")
dev.off()

