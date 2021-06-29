################################################################################
#
# Investigation of SAAQ Excessive Speeding Laws
#
# Fixed effects regressions where only the policy and points group interactions
# remain in the model.
# These are the only variables that vary across the individual series. 
# 
# This requires a dataset with the counts for point balances for each driver
# across the individual driving histories. 
#
#
#
# Lee Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# June 25, 2021
#
################################################################################
#
################################################################################

################################################################################
# Clearing Workspace and Declaring Packages
################################################################################

# Clear workspace, if running interactively.
rm(list=ls(all=TRUE))

# Load data table package for quick selection on seq.
library(data.table)

# Load PRROC package for calculating area under the ROC curve.
library(PRROC)

# Load xtable library to create tex scripts for tables.
library(xtable)

# Load scales library because it has a function
# to display large numbers in comma format.
library(scales)


################################################################################
# Set parameters for file IO
################################################################################

# Set working directory, if running interactively.
drive_path <- 'C:/Users/le279259/OneDrive - University of Central Florida/Documents'
git_path <- 'Research/SAAQ/SAAQspeeding/SAAQ_XS_de_Vitesse_2008'
wd_path <- sprintf('%s/%s',drive_path, git_path)
setwd(wd_path)

# The original data are stored in 'Data/'.
data_in_path <- 'Data'

# The data of counts of licensed drivers are also stored in 'Data/'.
data_out_path <- 'Data'

# Set name of output file for training, testing and estimation samples.
train_file_name <- 'saaq_train_by_seq.csv'
test_file_name <- 'saaq_test_by_seq.csv'


# Set name of output file for full dataset.
# out_file_name <- 'saaq_out.csv'


set.seed(42)



# Set directory for storing figures.
fig_dir <- 'Figures'

# Set directory for storing tables.
tab_dir <- 'Tables'

# Set directory for storing text output.
text_dir <- 'Text'

# Specify samples in terms of driver activity. 
# file_tag <- 'high_pts'
# file_tag <- 'all_pts'
file_tag_list <- c('all_pts', 'high_pts')

# Specify subsamples for either male, female, or all drivers
sex_sel_list <- c('A', 'M', 'F')


################################################################################
# Set Parameters for variables
################################################################################



# Age group categories for defining factors.
# age_group_list <- c('0-15', '16-19', '20-24', '25-34', '35-44', '45-54',
#                     '55-64', '65-74', '75-84', '85-89', '90-199')
# Coarser grouping to merge less-populated age groups:
age_group_list <- c('0-19', 
                    '20-24', '25-34', '35-44', '45-54',
                    '55-64', '65-199')

# Current points group categories for defining factors.
curr_pts_grp_list <- c(seq(0,10), '11-20', '21-30', '31-150')

# Weekday indicators can't be used in this version. 
# Time dimension is not included in the datasets with entire driver histories. 
# # Weekday indicators.
# weekday_list <- c('Sunday',
#                   'Monday',
#                   'Tuesday',
#                   'Wednesday',
#                   'Thursday',
#                   'Friday',
#                   'Saturday')

# Set date of policy change.
april_fools_date <- '2008-04-01'
# No joke: policy change on April Fool's Day!


################################################################################
# Load Datasets
################################################################################

#-------------------------------------------------------------------------------
# Load Training Dataset
#-------------------------------------------------------------------------------

# Dataset for in-sample model fitting. 
in_path_file_name <- sprintf('%s/%s', data_in_path, train_file_name)
saaq_train <- fread(in_path_file_name)

summary(saaq_train)

# summary(saaq_train[, .N, by = date])
# head(saaq_train, 394)

# table(saaq_train[, sex], useNA = 'ifany')
# 
# table(saaq_train[, age_grp], useNA = 'ifany')
# 
# table(saaq_train[, past_active], useNA = 'ifany')
# 
# table(saaq_train[, past_active], saaq_train[, sex], useNA = 'ifany')
# 
# table(saaq_train[, curr_pts_grp], saaq_train[, past_active], useNA = 'ifany')


# length(unique(saaq_train[, date]))
# # [1] 1461 days of driving.
# 
# 2*length(age_group_list)*2*length(curr_pts_grp_list)
# # [1] 392 combinations of categories per day.
# 
# # Observations added with observed tickets.
# nrow(saaq_train) - 2*length(age_group_list)*2*length(curr_pts_grp_list)*1826


# Tabulate the points, which are the events to be predicted.
# saaq_train[date >= sample_beg & date <= sample_end,
#                 sum(as.numeric(num)), by = points][order(points)]
saaq_train[, sum(as.numeric(num)), by = points][order(points)]


#-------------------------------------------------------------------------------
# Load Testing Dataset
#-------------------------------------------------------------------------------

# Dataset for out-of-sample model testing. 
in_path_file_name <- sprintf('%s/%s', data_in_path, test_file_name)
saaq_test <- fread(in_path_file_name)

summary(saaq_test)

# summary(saaq_test[, .N, by = date])
# head(saaq_test, 394)
# 
# table(saaq_test[, sex], useNA = 'ifany')
# 
# table(saaq_test[, age_grp], useNA = 'ifany')
# 
# table(saaq_test[, past_active], useNA = 'ifany')
# 
# table(saaq_test[, past_active], saaq_test[, sex], useNA = 'ifany')
# 
# table(saaq_test[, curr_pts_grp], saaq_test[, past_active], useNA = 'ifany')



# length(unique(saaq_test[, date]))
# # [1] 1461 days of driving.
# 
# 2*length(age_group_list)*2*length(curr_pts_grp_list)
# # [1] 392 combinations of categories per day.
# 
# # Observations added with observed tickets.
# nrow(saaq_test) - 2*length(age_group_list)*2*length(curr_pts_grp_list)*1826


# Tabulate the points, which are the events to be predicted.
# saaq_test[date >= sample_beg & date <= sample_end,
#           sum(as.numeric(num)), by = points][order(points)]
saaq_test[, sum(as.numeric(num)), by = points][order(points)]


################################################################################
# Stack the datasets and label by sample
################################################################################

saaq_train[, sample := 'train']
saaq_test[, sample := 'test']
saaq_data <- rbind(saaq_train, saaq_test)

rm(saaq_train, saaq_test)


# saaq_data[date >= sample_beg & date <= sample_end,
#           sum(as.numeric(num)), by = points][order(points)]
saaq_data[, sum(as.numeric(num)), by = points][order(points)]


################################################################################
# Define additional variables
################################################################################

# Define categorical variables as factors.
saaq_data[, sex := factor(sex, levels = c('M', 'F'))]
saaq_data[, age_grp := factor(age_grp, levels = age_group_list)]
saaq_data[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]

# With individual fixed effects, the data are aggregated by individual
# but not by date. 
# # Define new variables for seasonality.
# # Numeric indicator for month. 
# # saaq_data[, 'month'] <- substr(saaq_data[, 'date'], 6, 7)
# saaq_data[, month := substr(date, 6, 7)]
# month_list <- unique(saaq_data[, month])
# month_list <- month_list[order(month_list)]
# saaq_data[, month := factor(month, levels = month_list)]
# table(saaq_data[, 'month'], useNA = "ifany")
# 
# # Weekday indicator.
# saaq_data[, weekday := weekdays(date)]
# saaq_data[, weekday := factor(weekday, levels = weekday_list)]
# table(saaq_data[, 'weekday'], useNA = "ifany")

# Define the indicator for the policy change.
saaq_data[, policy := date >= april_fools_date]

# Individual-specific variables will be adjusted out by the
# individual intercept. 

# # Create some additional indicators for categories.
# 
# # There is more traffic on weekdays.
# # People get more sensible, rational tickets on weekdays.
# # People get more crazy, irrational tickets on weekends. 
# saaq_data[, weekend := weekday %in% c('Sunday', 'Saturday')]
# # table(saaq_data[, 'weekday'], saaq_data[, 'weekend'], useNA = "ifany")
# saaq_data[, .N, by = c('weekday', 'weekend')]
# 
# # Drivers get fewer tickets in December to January. 
# saaq_data[, winter := month %in% c('01', '12')]
# saaq_data[, .N, by = c('month', 'winter')]
# 
# 
# # Indicators for drivers with no points and many points.
# saaq_data[, zero_curr_pts := curr_pts_grp %in% c('0')]
# saaq_data[, .N, by = c('curr_pts_grp', 'zero_curr_pts')]
# saaq_data[, high_curr_pts := curr_pts_grp %in% c('11-20', '21-30', '31-150')]
# saaq_data[, .N, by = c('curr_pts_grp', 'high_curr_pts')]
# 
# # Indicators for the younger or middle age groups.
# # age_group_list
# saaq_data[, young_age := age_grp %in% c('0-19', '20-24')]
# saaq_data[, .N, by = c('age_grp', 'young_age')]
# saaq_data[, mid_age := age_grp %in% c('25-34', '35-44')]
# saaq_data[, .N, by = c('age_grp', 'mid_age')]

# 
# 
# # Select observations based on past activity.
# if (file_tag == 'all_pts') {
#   saaq_data[, sel_obsn := sample == 'train']
# } else if (file_tag == 'high_pts') {
#   saaq_data[, sel_obsn := past_active == TRUE & sample == 'train']
# } else {
#   stop(sprintf("file_tag '%s' not recognized.", file_tag))
# }
# 

# saaq_data[date >= sample_beg & date <= sample_end,
#           sum(as.numeric(num)), by = points][order(points)]



################################################################################
# First stage regressions for fixed effects.
################################################################################


# Each driver has 1461 days of driving.
# num_days <- length(unique(saaq_data[, date]))
# But the number of events differs slightly if they get multiple tickets. 
# Use individual-level counts of days in the projections of the driver dummies. 

# Calculate denominators by driver for regressions on driver dummies. 
saaq_data[, num_by_seq := sum(num), by = 'seq']
saaq_data[, num_policy_by_seq := sum(num*policy), by = 'seq']
head(saaq_data[, c('seq', 'num_by_seq', 'num_policy_by_seq')])
head(saaq_data[seq > 0, c('seq', 'num_by_seq', 'num_policy_by_seq')])
summary(saaq_data[, c('seq', 'num_by_seq', 'num_policy_by_seq')])
summary(saaq_data[seq > 0, c('seq', 'num_by_seq', 'num_policy_by_seq')])



# Define dependent variable:
# All violations combined.
saaq_data[, events := points > 0]

# Notice that dataset of tickets is aggregated. 
summary(saaq_data[events == 1, num])
saaq_data[events == 1 & num > 1, .N]
saaq_data[events == 1 & num > 1, ]
# Make sure to weight by number of drivers.

# Generate a new dependent variable for fixed-effect regressions. 
# For Frisch-Waugh-Lovell, this is the deviations from individual means.
saaq_data[, avg_events := sum(events*num)/sum(num), by = 'seq']
saaq_data[, dev_events := events - avg_events, by = 'seq']
# summary(saaq_data[, c('events', 'avg_events')])

# Create an FWL projection of the policy indicator.
saaq_data[, avg_policy := sum(policy*num)/sum(num), by = 'seq']
saaq_data[, dev_policy := policy - avg_policy, by = 'seq']

summary(saaq_data[, c('dev_events', 'dev_policy')])



# Generate new variables for current points categories. 
for (curr_pts_level in curr_pts_grp_list) {
  
  
  print(sprintf('FWL projections for curr_pts_grp %s', curr_pts_level))
  
  # Generate a new column to indicate the average time at this point level. 
  saaq_data[, is_curr_pts_grp := (curr_pts_grp == curr_pts_level)]
  saaq_data[, avg_FWL_count := 
              sum(is_curr_pts_grp*num)/sum(num), by = 'seq']

  # Allocate this variable to a new column.
  col_var_name <- sprintf('curr_pts_%s', gsub('-', '_', curr_pts_level))
  saaq_data[, col_var_name] <- saaq_data[, is_curr_pts_grp - avg_FWL_count]
  
  
  print(sprintf('FWL projections for policy*curr_pts_grp %s', curr_pts_level))
  
  # Now calculate a new column to indicate the average time at this point level, 
  # during the post-policy period: a policy-points-level interaction. 
  saaq_data[, avg_FWL_count := 
              sum(is_curr_pts_grp*policy*num)/sum(num), by = 'seq']
  
  # Allocate this variable to a new column.
  col_var_name <- sprintf('curr_pts_%s_policy', gsub('-', '_', curr_pts_level))
  saaq_data[, col_var_name] <- saaq_data[, is_curr_pts_grp*policy - avg_FWL_count]
  
}


colnames(saaq_data)
# summary(saaq_data)



################################################################################
# 
# Fixed Effects Regressions: 
# Current points group and policy interaction
# Estimate for subsamples by ticket activity
# and on subsamples for male, female and all drivers
# 
################################################################################


# file_tag <- 'high_pts'
# file_tag <- 'all_pts'
for (file_tag in file_tag_list) {
  
  
  #-------------------------------------------------------------------------------
  # Select data for sample of driver type.
  #-------------------------------------------------------------------------------
  
  
  # Select observations based on past activity.
  if (file_tag == 'all_pts') {
    saaq_data[, sel_obsn := sample == 'train']
  } else if (file_tag == 'high_pts') {
    saaq_data[, sel_obsn := past_active == TRUE & sample == 'train']
  } else {
    stop(sprintf("file_tag '%s' not recognized.", file_tag))
  }
  
  #-------------------------------------------------------------------------------
  # Select subsample for either male, female, or all drivers
  #-------------------------------------------------------------------------------
  
  # Initialize a matrix of coefficients. 
  FE_estimates <- NULL
  # 
  # sex_sel_list <- c('A', 'M', 'F')
  # sex_sel <- 'M'
  for (sex_sel in sex_sel_list) {
    
    print(sprintf('Estimating model for sex %s drivers in points group %s.', 
                  sex_sel, file_tag))
    
    
    ################################################################################
    # Fixed Effects Regressions: 
    # Current points group and policy interaction
    ################################################################################
    
    
    if (sex_sel == 'A') {
      saaq_data[, sub_sel_obsn := sel_obsn == TRUE]
    } else if (sex_sel == 'M') {
      saaq_data[, sub_sel_obsn := sex == 'M' & sel_obsn == TRUE]
    } else if (sex_sel == 'F') {
      saaq_data[, sub_sel_obsn := sex == 'F' & sel_obsn == TRUE]
    } else {
      stop(sprintf('Sample selection sex_sel = %s not recognized.', sex_sel))
    }
    
    
    
    
    # Set the list of variables by points category.
    # Assumes constant omitted:
    var_list_1 <- sprintf('curr_pts_%s', gsub('-', '_', curr_pts_grp_list))
    var_list_2 <- sprintf('curr_pts_%s_policy', gsub('-', '_', curr_pts_grp_list))
    # Assumes constant included:
    # var_list_1 <- var_list_1[2:length(var_list_1)]
    # var_list_2 <- var_list_2[2:length(var_list_2)]
    # var_list <- c(var_list_1, 'avg_policy', var_list_2)
    var_list <- c(var_list_1, var_list_2)
    
    # Eliminate the constant term.
    fmla_str <- sprintf('dev_events ~ 0 + %s',
                        paste(var_list, collapse = " + "))
    fmla <- as.formula(fmla_str)
    
    # Fit regression model on training sample for male drivers. 
    lm_spec <- lm(formula = fmla, 
                  data = saaq_data[sub_sel_obsn == TRUE, ], 
                  weights = num, 
                  model = FALSE) #, x = FALSE, y = FALSE, qr = FALSE)
    
    # Print a summary to screen.
    print(summary(lm_spec))
    
    # Store the item for output. 
    summ_sub <- summary(lm_spec)
    
    # attributes(summ_sub)
    
    # summ_sub$coefficients
    
    # # Some objects are very large.
    # object.size(summ_sub$weights)
    # object.size(summ_sub$residuals)
    
    # Calculate (observation-weighted) sum of squared residuals.
    SSR_sub <- sum(summ_sub$weights*summ_sub$residuals^2)
    num_sub <- sum(summ_sub$weights)
    
    # Drop large elements to focus on estimates.
    summ_sub$weights <- NULL
    summ_sub$residuals <- NULL
    
    # Store SSR and number of observations. 
    summ_sub$SSR <- SSR_sub
    summ_sub$num <- num_sub
    
    # Count number of drivers in the sample.
    summ_sub$num_seq <- length(unique(saaq_data[sub_sel_obsn == TRUE, seq]))
    
    
    
    
    # Append estimates to the matrix of coefficients. 
    FE_estimates <- cbind(FE_estimates, 
                          summ_A$coefficients[, c('Estimate', 'Std. Error')])
    new_cols <- (ncol(FE_estimates) - 1):ncol(FE_estimates)
    colnames(FE_estimates)[new_cols] <- c(sprintf('Est_%s', sex_sel), 
                                          sprintf('SE_%s', sex_sel))
    
    
    # Calculate confidence bounds on estimates. 
    # FE_estimates <- cbind(FE_estimates, FE_estimates*0)
    # colnames(FE_estimates) <- c('Est_M', 'SE_M', 'Est_F', 'SE_F', 'Est_A', 'SE_A', 
    #                             'CI_L_M', 'CI_U_M', 'CI_L_F', 'CI_U_F', 'CI_L_A', 'CI_U_A')
    
    # FE_estimates[, 'CI_U_M'] <- FE_estimates[, 'Est_M'] + 
    #   qnorm(0.975)*FE_estimates[, 'SE_M']
    
    FE_estimates <- cbind(FE_estimates, 
                          FE_estimates[, sprintf('Est_%s', sex_sel)] + 
                            qnorm(0.975)*FE_estimates[, sprintf('SE_%s', sex_sel)])
    FE_estimates <- cbind(FE_estimates, 
                          FE_estimates[, sprintf('Est_%s', sex_sel)] - 
                            qnorm(0.975)*FE_estimates[, sprintf('SE_%s', sex_sel)])
    
    new_cols <- (ncol(FE_estimates) - 1):ncol(FE_estimates)
    colnames(FE_estimates)[new_cols] <- c(sprintf('CI_U_%s', sex_sel), 
                                          sprintf('CI_L_%s', sex_sel))
    
    
    
    # if (sex_sel == 'A') {
    #   
    #   # Append to the matrix of coefficients. 
    #   FE_estimates <- cbind(FE_estimates, 
    #                         summ_A$coefficients[, c('Estimate', 'Std. Error')])
    #   colnames(FE_estimates) <- c('Est_M', 'SE_M', 'Est_F', 'SE_F',
    #                               'Est_A', 'SE_A')
    #   
    #   
    # } else if (sex_sel == 'M') {
    #   
    #   # Initialize a matrix of coefficients. 
    #   # Append to the matrix of coefficients. 
    #   FE_estimates <- summ_sub$coefficients[, c('Estimate', 'Std. Error')]
    #   colnames(FE_estimates) <- c('Est_M', 'SE_M')
    #   
    # } else if (sex_sel == 'F') {
    #   
    #   # Append to the matrix of coefficients. 
    #   FE_estimates <- cbind(FE_estimates, 
    #                         summ_F$coefficients[, c('Estimate', 'Std. Error')])
    #   colnames(FE_estimates) <- c('Est_M', 'SE_M', 'Est_F', 'SE_F')
    #   
    # } 
    
    
    # Collect estimation output.
    if (sex_sel == 'A') {
      summ_A <- summ_sub
    } else if (sex_sel == 'M') {
      summ_M <- summ_sub
    } else if (sex_sel == 'F') {
      summ_F <- summ_sub
    }
    
    
    
    
  }
  
  
  ################################################################################
  # Compile estimates for output
  ################################################################################
  
  
  #-------------------------------------------------------------------------------
  # Calculate an F-statistic to test the hypothesis 
  # of equal coefficients for males and females
  #-------------------------------------------------------------------------------
  
  # Restricted model has equal coefficients across the sexes.
  RSSR <- summ_A$SSR
  
  # Unrestricted model has two of every parameter. 
  USSR <- summ_M$SSR + summ_F$SSR
  
  # Both are equal. Check.
  # num_obs <- summ_A$num
  num_obs <- summ_M$num + summ_F$num
  
  # Number of parameters is the same across models.
  nrow(summ_A$coefficients)
  nrow(summ_M$coefficients)
  nrow(summ_F$coefficients)
  # Full model has twice as many parameters (males and females).
  num_vars <- nrow(summ_M$coefficients) + nrow(summ_F$coefficients)
  
  # A test of restrictions on each male/female parameter.
  num_restr <- nrow(summ_A$coefficients)
  
  # Calculate the F-statistic. 
  F_stat <- (RSSR - USSR)/num_restr /
    USSR*(num_obs - num_vars - 1)
  
  print('F_stat = ')
  print(F_stat)
  # All drivers:
  # [1] "F_stat = "
  # [1] 2384077
  # High-point drivers:
  
  
  # Calculate the p-value.
  p_value <- pf(q = F_stat, df1 = num_restr, df2 = (num_obs - num_vars - 1), lower.tail = FALSE)
  
  print('p_value = ')
  print(p_value)
  # All drivers:
  # [1] "p_value = "
  # [1] 0
  # High-point drivers:
  
  
  # In that case, calculate a critical value at the 1% level.
  c_value <- qf(p = 0.05, df1 = num_restr, df2 = (num_obs - num_vars - 1), lower.tail = FALSE)
  
  print('c_value = ')
  print(c_value)
  # [1] "c_value = "
  # [1] 1.485677
  
  # Equality of parameters decisively rejected. 
  
  # Collect into a statement to add to the tex file.
  f_test_desc <- c('We calculated the $F$-statistic to test the restriction', 
                   'that the parameters are the same for both male and female drivers.', 
                   sprintf('The unrestricted sum of squared residuals was %s.', 
                           comma_format()(USSR)), 
                   sprintf('The restricted sum of squared residuals was %s.', 
                           comma_format()(RSSR)), 
                   sprintf('The value of the $F$-statistic was %s,', 
                           comma_format()(F_stat)), 
                   'which corresponds to a $p$-value of nearly zero, since the $F$-statistic is much higher than', 
                   sprintf(' the one percent critical value of %6.4f.', 
                           c_value), 
                   'This strongly suggests that male and female driving behaviour should be modeled separately.')
  
  # Output this description to a file.
  text_file_path <- sprintf('%s/FE_regs_F_stat_%s.tex', text_dir, file_tag)
  cat('\n\n%% Results of F-test for equality of parameters by gender \n\n\n', 
      file = text_file_path, append = FALSE)
  for (desc_row in 1:length(f_test_desc)) {
    cat(sprintf('%s \n', f_test_desc[desc_row]), file = text_file_path, append = TRUE)
  }
  
  
  # Collect other parameters to count observations. 
  # summ_A$num_seq <- length(unique(saaq_data[sample == 'train', seq]))
  # summ_M$num_seq <- length(unique(saaq_data[sex == 'M' & sample == 'train', seq]))
  # summ_F$num_seq <- length(unique(saaq_data[sex == 'F' & sample == 'train', seq]))
  
  
  # Collect all other parameters into a table of statistics. 
  FFX_stats <- data.frame(num_drivers = c(summ_A$num_seq, summ_sub$num_seq, summ_F$num_seq), 
                          num_obs = c(summ_A$num, summ_sub$num, summ_F$num), 
                          SSR = c(summ_A$SSR, summ_sub$SSR, summ_F$SSR))
  
  
  
  
  
  #-------------------------------------------------------------------------------
  # Table of estimates
  #-------------------------------------------------------------------------------
  
  
  # # Calculate confidence bounds on estimates. 
  # FE_estimates <- cbind(FE_estimates, FE_estimates*0)
  # # colnames(FE_estimates) <- c('Est_M', 'SE_M', 'Est_F', 'SE_F', 'Est_A', 'SE_A', 
  # #                             'CI_L_M', 'CI_U_M', 'CI_L_F', 'CI_U_F', 'CI_L_A', 'CI_U_A')
  # 
  # FE_estimates[, 'CI_U_M'] <- FE_estimates[, 'Est_M'] + 
  #   qnorm(0.975)*FE_estimates[, 'SE_M']
  # 
  # FE_estimates[, 'CI_L_M'] <- FE_estimates[, 'Est_M'] - 
  #   qnorm(0.975)*FE_estimates[, 'SE_M']
  # 
  # FE_estimates[, 'CI_U_F'] <- FE_estimates[, 'Est_F'] + 
  #   qnorm(0.975)*FE_estimates[, 'SE_F']
  # 
  # FE_estimates[, 'CI_L_F'] <- FE_estimates[, 'Est_F'] - 
  #   qnorm(0.975)*FE_estimates[, 'SE_F']
  # 
  # 
  # FE_estimates[, 'CI_U_A'] <- FE_estimates[, 'Est_A'] + 
  #   qnorm(0.975)*FE_estimates[, 'SE_A']
  # 
  # FE_estimates[, 'CI_L_A'] <- FE_estimates[, 'Est_A'] - 
  #   qnorm(0.975)*FE_estimates[, 'SE_A']
  # 
  # 
  # # FE_estimates[, c('CI_L_M', 'CI_U_M', 'CI_L_F', 'CI_U_F', 'CI_L_A', 'CI_U_A')]
  
  
  #-------------------------------------------------------------------------------
  # Plot estimates and standard error bands
  #-------------------------------------------------------------------------------
  
  
  # Plot levels of tickets before policy change.
  var_nums <- 1:13
  n_vars <- length(var_nums)
  fig_filename <- sprintf('%s/FFX_reg_points_grp_%s.pdf', fig_dir, file_tag)
  pdf(fig_filename)
  plot(FE_estimates[var_nums, 'Est_M'], type = 'l', col = 'black', lwd = 3, 
       ylim = c(-0.02, 0.02))
  lines(1:n_vars, rep(0, n_vars), col = 'black', lwd = 1)
  lines(1:n_vars, FE_estimates[var_nums, 'CI_U_M'], col = 'black', lwd = 3, lty = 'dashed')
  lines(1:n_vars, FE_estimates[var_nums, 'CI_L_M'], col = 'black', lwd = 3, lty = 'dashed')
  
  # Female drivers in grey.
  grey_F <- gray.colors(n = 1, start = 0.6, end = 0.6)
  lines(1:n_vars, FE_estimates[var_nums, 'Est_F'], col = grey_F, lwd = 3)
  lines(1:n_vars, FE_estimates[var_nums, 'CI_U_F'], col = grey_F, lwd = 3, lty = 'dashed')
  lines(1:n_vars, FE_estimates[var_nums, 'CI_L_F'], col = grey_F, lwd = 3, lty = 'dashed')
  
  # All drivers in red (not for publication). 
  # lines(1:n_vars, FE_estimates[var_nums, 'Est_A'], col = 'red', lwd = 3)
  # lines(1:n_vars, FE_estimates[var_nums, 'CI_U_A'], col = 'red', lwd = 3, lty = 'dashed')
  # lines(1:n_vars, FE_estimates[var_nums, 'CI_L_A'], col = 'red', lwd = 3, lty = 'dashed')
  # Similar to the numbers for males.
  
  legend('topleft', legend = c('Male Drivers', 'Female Drivers'), 
         col = c('black', grey_F), lwd = 3, lty = 'solid', cex = 1.5)
  dev.off()
  
  # Adjust figure parameters for sample.
  if (file_tag == 'all_pts') {
    ylim_fig <- c(-0.002, 0.002)
  } else if (file_tag == 'high_pts') {
    ylim_fig <- c(-0.005, 0.002)
  }
  
  
  
  # Plot levels of tickets after policy change.
  n_vars_L <- 14
  n_vars_U <- 27
  var_nums <- n_vars_L:n_vars_U
  n_vars <- length(var_nums)
  
  curr_pts_labels <- rownames(FE_estimates)[var_nums]
  curr_pts_labels <- gsub('curr_pts_', '', curr_pts_labels)
  curr_pts_labels <- gsub('_policy', '', curr_pts_labels)
  curr_pts_labels <- gsub('_', '-', curr_pts_labels)
  
  # FE_estimates[var_nums, c('CI_L_M', 'CI_U_M', 'CI_L_F', 'CI_U_F')]
  
  # fig_filename <- sprintf('%s/FFX_reg_policy_points_grp.eps', fig_dir)
  # postscript(fig_filename)
  fig_filename <- sprintf('%s/FFX_reg_policy_points_grp_%s.pdf', fig_dir, file_tag)
  pdf(fig_filename)
  plot(1:n_vars, FE_estimates[n_vars_L:n_vars_U, 'Est_M'], 
       xlab = 'Demerit Point Category', 
       ylab = 'Policy Effect', 
       type = 'l', col = 'black', lwd = 3, 
       ylim = ylim_fig, xaxt = 'n', 
       cex.lab = 1.5,    # X-axis and Y-axis labels size
       cex.axis = 1.5)
  axis(1, at = 1:n_vars, labels = curr_pts_labels, cex = 0.5)
  lines(1:n_vars, rep(0, n_vars), col = 'black', lwd = 1)
  lines(1:n_vars, FE_estimates[var_nums, 'CI_U_M'], col = 'black', lwd = 3, lty = 'dashed')
  lines(1:n_vars, FE_estimates[var_nums, 'CI_L_M'], col = 'black', lwd = 3, lty = 'dashed')
  
  lines(1:n_vars, FE_estimates[var_nums, 'Est_F'], col = grey_F, lwd = 3)
  lines(1:n_vars, FE_estimates[var_nums, 'CI_U_F'], col = grey_F, lwd = 3, lty = 'dashed')
  lines(1:n_vars, FE_estimates[var_nums, 'CI_L_F'], col = grey_F, lwd = 3, lty = 'dashed')
  
  # # All drivers in red (not for publication). 
  # lines(1:n_vars, FE_estimates[var_nums, 'Est_A'], col = 'red', lwd = 3)
  # lines(1:n_vars, FE_estimates[var_nums, 'CI_U_A'], col = 'red', lwd = 3, lty = 'dashed')
  # lines(1:n_vars, FE_estimates[var_nums, 'CI_L_A'], col = 'red', lwd = 3, lty = 'dashed')
  # Similar to the numbers for males.
  
  legend('topleft', legend = c('Male Drivers', 'Female Drivers'), 
         col = c('black', grey_F), lwd = 3, lty = 'solid', cex = 1.5)
  dev.off()
  
  
  #-------------------------------------------------------------------------------
  # Generate LaTeX tables of estimates
  #-------------------------------------------------------------------------------
  
  # Create a table for display. 
  colnames(FE_estimates)
  FE_est_out <- FE_estimates[, c('Est_A', 'SE_A', 
                                 'Est_M', 'SE_M', 
                                 'Est_F', 'SE_F')]
  
  
  # Output to TeX file.
  tab_file_path <- sprintf('%s/FE_regs_%s.tex', tab_dir, file_tag)
  
  
  # Ouput TeX code for tables.
  if (file_tag == 'all_pts') {
    header <- "Estimates from Fixed Effects Regression Models (all demerit-point levels)"
    caption <- 'Estimates from fixed effects regression models (all demerit-point levels)'
  } else if (file_tag == 'high_pts') {
    header <- "Estimates from Fixed Effects Regression Models (drivers with high demerit-point balances)"
    caption <- 'Estimates from fixed effects regression models (drivers with high demerit-point balances)'
  }
  description <- c('Fixed effects regression coefficients after estimating driver-specific intercept coefficients.', 
                   'Samples are drawn by randomly selecting seventy per cent of the drivers.')
  label <- sprintf('tab:FE_regs_%s', file_tag)
  
  
  # Output header.
  cat(sprintf('%% %s \n\n', header),
      file = tab_file_path, append = FALSE)
  cat('\\begin{table}% [ht] \n', file = tab_file_path, append = TRUE)
  cat('\\centering \n', file = tab_file_path, append = TRUE)
  cat('\\begin{tabular}{l r r r r r r} \n', file = tab_file_path, append = TRUE)
  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)
  
  # Output column headers.
  cat('\nSample \n', file = tab_file_path, append = TRUE)
  for (sample_name in c('All', 'Male', 'Female')) {
    cat(sprintf(" & \\multicolumn{2}{c}{%s  Drivers} ", sample_name),
        file = tab_file_path, append = TRUE)
  }
  cat('  \\\\ \n \n', file = tab_file_path, append = TRUE)
  
  cat('\n \\cmidrule(lr){1-1}\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7} \n', 
      file = tab_file_path, append = TRUE)
  
  cat('\nEstimate ', file = tab_file_path, append = TRUE)
  for (i in 1:3) {
    cat(' & Coefficient & Std. Error ', file = tab_file_path, append = TRUE)
  }
  cat('  \\\\ \n \n', file = tab_file_path, append = TRUE)
  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)
  
  
  # Output table of estimates.
  cat('\\multicolumn{4}{l}{\\textbf{Demerit points group indicators:}}  \\\\ \n \n', file = tab_file_path, append = TRUE)
  for (row in 1:nrow(FE_est_out)) {
    
    
    var_name <- rownames(FE_est_out)[row]
    var_name <- gsub('curr_pts_', '', var_name)
    var_name <- gsub('_policy', '', var_name)
    var_name <- gsub('_', '-', var_name)
    var_name <- sprintf('%s points', var_name)
    
    cat(sprintf('%s ', var_name), file = tab_file_path, append = TRUE)
    for (col in 1:ncol(FE_est_out)) {
      
      cat(sprintf(' & %6.3f ', FE_est_out[row, col]*1000), 
          file = tab_file_path, append = TRUE)
    }
    cat('  \\\\ \n \n', file = tab_file_path, append = TRUE)
    if (row == 13) {
      cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)
      cat('\\multicolumn{4}{l}{\\textbf{Policy and points group interactions:}}  \\\\ \n \n', file = tab_file_path, append = TRUE)
    }
    
  }
  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)
  
  # Output summary statistics.
  # FFX_stats
  
  cat('\nDrivers \n', file = tab_file_path, append = TRUE)
  for (i in 1:3) {
    num_drivers <- comma_format()(FFX_stats[i, 'num_drivers'])
    cat(sprintf(' & \\multicolumn{2}{r}{%s} ', num_drivers), 
        file = tab_file_path, append = TRUE)
  }
  cat('  \\\\ \n \n', file = tab_file_path, append = TRUE)
  
  cat('\nDriver days \n', file = tab_file_path, append = TRUE)
  for (i in 1:3) {
    num_obs <- comma_format()(FFX_stats[i, 'num_obs'])
    cat(sprintf(' & \\multicolumn{2}{r}{%s} ', num_obs), 
        file = tab_file_path, append = TRUE)
  }
  cat('  \\\\ \n \n', file = tab_file_path, append = TRUE)
  
  cat('\nSSR \n', file = tab_file_path, append = TRUE)
  for (i in 1:3) {
    SSR <- comma_format()(FFX_stats[i, 'SSR'])
    cat(sprintf(' & \\multicolumn{2}{r}{%s} ', SSR), 
        file = tab_file_path, append = TRUE)
  }
  cat('  \\\\ \n \n', file = tab_file_path, append = TRUE)
  
  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)
  
  
  # Output closing arguments.
  cat('\\end{tabular} \n', file = tab_file_path, append = TRUE)
  cat(sprintf('\\caption{%s} \n', caption), file = tab_file_path, append = TRUE)
  for (desc_row in 1:length(description)) {
    cat(sprintf('%s \n', description[desc_row]), file = tab_file_path, append = TRUE)
  }
  cat(sprintf('\\label{%s} \n', label), file = tab_file_path, append = TRUE)
  cat('\\end{table} \n \n', file = tab_file_path, append = TRUE)
  
  
}

################################################################################
# End
################################################################################

