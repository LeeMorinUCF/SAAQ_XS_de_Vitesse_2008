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
# Fit fixed-effects models
################################################################################

#-------------------------------------------------------------------------------
# Current points group only
#-------------------------------------------------------------------------------

# Set the zero-points category as the benchmark.
var_list <- sprintf('curr_pts_%s', gsub('-', '_', curr_pts_grp_list))
var_list <- var_list[2:length(var_list)]


fmla_str <- sprintf('dev_events ~ %s',
                    paste(var_list, collapse = " + "))
fmla <- as.formula(fmla_str)

# Fit regression model on training sample. 
lm_spec <- lm(formula = fmla, 
              # data = saaq_data[sample == 'train', ],
              # data = saaq_data, # Full sample.
              # data = saaq_data[sex == 'M'], # Full sample of male drivers.
              data = saaq_data[sex == 'M' & sample == 'train'], # Training sample of male drivers.
              # data = saaq_data[sex == 'F'], # Full sample of female drivers.
              # data = saaq_data[sex == 'F' & sample == 'train'], # Training sample of female drivers.
              weights = num, 
              model = FALSE) #, x = FALSE, y = FALSE, qr = FALSE)

# Print a summary to screen.
summary(lm_spec)



# # On sample of male drivers:
# Call:
#   lm(formula = fmla, data = saaq_data[sex == "M" & sample == "train"], 
#      weights = num, model = FALSE)
# 
# Weighted Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.17610 -0.00175 -0.00001  0.00312  0.40955 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     3.268e-05  2.059e-07   158.7   <2e-16 ***
#   curr_pts_1      1.519e-03  2.210e-06   687.2   <2e-16 ***
#   curr_pts_2      1.450e-03  1.018e-06  1424.7   <2e-16 ***
#   curr_pts_3      1.400e-03  9.331e-07  1500.4   <2e-16 ***
#   curr_pts_4      2.933e-03  2.520e-06  1163.9   <2e-16 ***
#   curr_pts_5      2.691e-03  1.973e-06  1364.3   <2e-16 ***
#   curr_pts_6      3.086e-03  2.324e-06  1327.6   <2e-16 ***
#   curr_pts_7      4.120e-03  3.919e-06  1051.4   <2e-16 ***
#   curr_pts_8      4.151e-03  3.789e-06  1095.6   <2e-16 ***
#   curr_pts_9      3.602e-03  4.120e-06   874.2   <2e-16 ***
#   curr_pts_10     4.616e-03  5.890e-06   783.8   <2e-16 ***
#   curr_pts_11_20  6.169e-03  2.694e-06  2290.0   <2e-16 ***
#   curr_pts_21_30  9.782e-03  1.039e-05   941.0   <2e-16 ***
#   curr_pts_31_150 1.488e-02  2.007e-05   741.3   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.008613 on 7765330 degrees of freedom
# Multiple R-squared:  0.8242,	Adjusted R-squared:  0.8242 
# F-statistic: 2.801e+06 on 13 and 7765330 DF,  p-value: < 2.2e-16
# 
# 
# 
# 
# # On sample of female drivers:
# Call:
#   lm(formula = fmla, data = saaq_data[sex == "F" & sample == "train"], 
#      weights = num)
# 
# Weighted Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.144821 -0.001410 -0.000049  0.002947  0.152024 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     2.438e-05  1.768e-07   137.8   <2e-16 ***
#   curr_pts_1      1.503e-03  2.346e-06   640.9   <2e-16 ***
#   curr_pts_2      1.443e-03  1.071e-06  1347.9   <2e-16 ***
#   curr_pts_3      1.406e-03  1.098e-06  1281.2   <2e-16 ***
#   curr_pts_4      2.956e-03  3.320e-06   890.5   <2e-16 ***
#   curr_pts_5      2.748e-03  2.779e-06   988.9   <2e-16 ***
#   curr_pts_6      3.151e-03  3.558e-06   885.7   <2e-16 ***
#   curr_pts_7      4.431e-03  6.612e-06   670.2   <2e-16 ***
#   curr_pts_8      4.321e-03  6.736e-06   641.4   <2e-16 ***
#   curr_pts_9      2.944e-03  6.540e-06   450.2   <2e-16 ***
#   curr_pts_10     4.794e-03  1.176e-05   407.4   <2e-16 ***
#   curr_pts_11_20  6.137e-03  6.370e-06   963.3   <2e-16 ***
#   curr_pts_21_30  1.015e-02  3.726e-05   272.5   <2e-16 ***
#   curr_pts_31_150 1.485e-02  9.508e-05   156.2   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.007617 on 3472776 degrees of freedom
# Multiple R-squared:  0.8069,	Adjusted R-squared:  0.8068 
# F-statistic: 1.116e+06 on 13 and 3472776 DF,  p-value: < 2.2e-16





#-------------------------------------------------------------------------------
# Current points group and policy interaction
#-------------------------------------------------------------------------------

# Set the zero-points category as the benchmark.
var_list_1 <- sprintf('curr_pts_%s', gsub('-', '_', curr_pts_grp_list))
# var_list_1 <- var_list_1[2:length(var_list_1)]
var_list_2 <- sprintf('curr_pts_%s_policy', gsub('-', '_', curr_pts_grp_list))
# var_list_2 <- var_list_2[2:length(var_list_2)]
# var_list <- c(var_list_1, 'avg_policy', var_list_2)
var_list <- c(var_list_1, var_list_2)

# Eliminate the constant term.
fmla_str <- sprintf('dev_events ~ 0 + %s',
                    paste(var_list, collapse = " + "))
fmla <- as.formula(fmla_str)

# Fit regression model on training sample for male drivers. 
lm_spec <- lm(formula = fmla, 
              # data = saaq_data[sample == 'train', ],
              # data = saaq_data, # Full sample.
              # data = saaq_data[sex == 'M'], # Full sample of male drivers.
              data = saaq_data[sex == 'M' & sample == 'train'], # Training sample of male drivers.
              # data = saaq_data[sex == 'F'], # Full sample of female drivers.
              # data = saaq_data[sex == 'F' & sample == 'train'], # Training sample of female drivers.
              weights = num, 
              model = FALSE) #, x = FALSE, y = FALSE, qr = FALSE)

# Print a summary to screen.
summary(lm_spec)

# Store the item for output. 
summ_M <- summary(lm_spec)

attributes(summ_M)

summ_M$coefficients

# Some objects are very large.
object.size(summ_M$weights)
object.size(summ_M$residuals)

# Drop them to focus on estimates.
summ_M$weights <- NULL
summ_M$residuals <- NULL


# Initialize a matrix of coefficients. 
FE_estimates <- summ_M$coefficients[, c('Estimate', 'Std. Error')]
colnames(FE_estimates) <- c('Est_M', 'SE_M')



# Fit regression model on training sample for female drivers. 
lm_spec <- lm(formula = fmla, 
              # data = saaq_data[sample == 'train', ],
              # data = saaq_data, # Full sample.
              # data = saaq_data[sex == 'M'], # Full sample of male drivers.
              # data = saaq_data[sex == 'M' & sample == 'train'], # Training sample of male drivers.
              # data = saaq_data[sex == 'F'], # Full sample of female drivers.
              data = saaq_data[sex == 'F' & sample == 'train'], # Training sample of female drivers.
              weights = num, 
              model = FALSE) #, x = FALSE, y = FALSE, qr = FALSE)

# Print a summary to screen.
summary(lm_spec)

# Store the item for output. 
summ_F <- summary(lm_spec)

# Drop large elements to focus on estimates.
summ_F$weights <- NULL
summ_F$residuals <- NULL


# Initialize a matrix of coefficients. 
FE_estimates <- cbind(FE_estimates, 
                      summ_F$coefficients[, c('Estimate', 'Std. Error')])
colnames(FE_estimates) <- c('Est_M', 'SE_M', 'Est_F', 'SE_F')




#-------------------------------------------------------------------------------
# Calculate output
#-------------------------------------------------------------------------------

# Calculate confidence bounds on estimates. 
FE_estimates <- cbind(FE_estimates, FE_estimates*0)
# FE_estimates[, 'CI_U_M'] <- NA
# FE_estimates[, 'CI_L_M'] <- NA
# FE_estimates[, 'CI_U_F'] <- NA
# FE_estimates[, 'CI_L_F'] <- NA
colnames(FE_estimates) <- c('Est_M', 'SE_M', 'Est_F', 'SE_F', 
                            'CI_L_M', 'CI_U_M', 'CI_L_F', 'CI_U_F')

FE_estimates[, 'CI_U_M'] <- FE_estimates[, 'Est_M'] + 
  qnorm(0.975)*FE_estimates[, 'SE_M']

FE_estimates[, 'CI_L_M'] <- FE_estimates[, 'Est_M'] - 
  qnorm(0.975)*FE_estimates[, 'SE_M']

FE_estimates[, 'CI_U_F'] <- FE_estimates[, 'Est_F'] + 
  qnorm(0.975)*FE_estimates[, 'SE_F']

FE_estimates[, 'CI_L_F'] <- FE_estimates[, 'Est_F'] - 
  qnorm(0.975)*FE_estimates[, 'SE_F']


FE_estimates[, c('CI_L_M', 'CI_U_M', 'CI_L_F', 'CI_U_F')]

# Plot levels of tickets before policy change.
plot(FE_estimates[1:14, 'Est_M'], type = 'l', col = 'blue', lwd = 2, 
     ylim = c(0, 0.025))
lines(1:14, FE_estimates[1:14, 'CI_U_M'], col = 'blue', lwd = 2)
lines(1:14, FE_estimates[1:14, 'CI_L_M'], col = 'blue', lwd = 2)

lines(1:14, FE_estimates[1:14, 'Est_F'], col = 'red', lwd = 2)
lines(1:14, FE_estimates[1:14, 'CI_L_F'], col = 'red', lwd = 2)
lines(1:14, FE_estimates[1:14, 'CI_L_F'], col = 'red', lwd = 2)


# Plot levels of tickets after policy change.
FE_estimates[15:28, c('Est_M', 'Est_F')]

plot(FE_estimates[15:28, 'Est_M'], type = 'l', col = 'blue', lwd = 2, 
     ylim = c(-0.020, 0.0))
lines(1:14, FE_estimates[15:28, 'CI_U_M'], col = 'blue', lwd = 2)
lines(1:14, FE_estimates[15:28, 'CI_L_M'], col = 'blue', lwd = 2)


# plot(FE_estimates[15:28, 'Est_F'], type = 'l', col = 'red', lwd = 2, 
#      ylim = c(-0.020, 0.0))

lines(1:14, FE_estimates[15:28, 'Est_F'], col = 'red', lwd = 2)
lines(1:14, FE_estimates[15:28, 'CI_L_F'], col = 'red', lwd = 2)
lines(1:14, FE_estimates[15:28, 'CI_L_F'], col = 'red', lwd = 2)






#-------------------------------------------------------------------------------
# Printed output:
#-------------------------------------------------------------------------------

# On sample of male drivers:

# Call:
#   lm(formula = fmla, data = saaq_data[sex == "M" & sample == "train"], 
#      weights = num, model = FALSE)
# 
# Weighted Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.19729 -0.00252  0.00009  0.00257  0.39258 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             5.970e-03  2.091e-05   285.5   <2e-16 ***
#   curr_pts_1              6.115e-04  3.394e-06   180.2   <2e-16 ***
#   curr_pts_2              5.942e-04  1.739e-06   341.7   <2e-16 ***
#   curr_pts_3              5.101e-04  1.544e-06   330.4   <2e-16 ***
#   curr_pts_4              1.425e-03  3.907e-06   364.6   <2e-16 ***
#   curr_pts_5              1.227e-03  2.986e-06   410.8   <2e-16 ***
#   curr_pts_6              1.576e-03  3.425e-06   460.1   <2e-16 ***
#   curr_pts_7              1.894e-03  5.824e-06   325.2   <2e-16 ***
#   curr_pts_8              1.992e-03  5.568e-06   357.8   <2e-16 ***
#   curr_pts_9              1.596e-03  5.902e-06   270.3   <2e-16 ***
#   curr_pts_10             3.077e-03  9.425e-06   326.4   <2e-16 ***
#   curr_pts_11_20          3.578e-03  4.495e-06   796.1   <2e-16 ***
#   curr_pts_21_30          6.418e-03  1.845e-05   347.8   <2e-16 ***
#   curr_pts_31_150         1.031e-02  4.057e-05   254.2   <2e-16 ***
#   avg_policy             -1.168e-02  4.107e-05  -284.4   <2e-16 ***
#   curr_pts_1_policy       1.372e-03  5.100e-06   269.0   <2e-16 ***
#   curr_pts_2_policy       1.211e-03  2.455e-06   493.4   <2e-16 ***
#   curr_pts_3_policy       1.335e-03  2.239e-06   596.3   <2e-16 ***
#   curr_pts_4_policy       2.467e-03  5.599e-06   440.6   <2e-16 ***
#   curr_pts_5_policy       2.522e-03  4.451e-06   566.5   <2e-16 ***
#   curr_pts_6_policy       2.627e-03  5.107e-06   514.3   <2e-16 ***
#   curr_pts_7_policy       3.972e-03  8.515e-06   466.4   <2e-16 ***
#   curr_pts_8_policy       3.935e-03  8.164e-06   482.0   <2e-16 ***
#   curr_pts_9_policy       3.702e-03  8.774e-06   421.9   <2e-16 ***
#   curr_pts_10_policy      2.466e-03  1.254e-05   196.7   <2e-16 ***
#   curr_pts_11_20_policy   4.542e-03  6.571e-06   691.1   <2e-16 ***
#   curr_pts_21_30_policy   5.562e-03  2.426e-05   229.2   <2e-16 ***
#   curr_pts_31_150_policy  7.160e-03  5.077e-05   141.0   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.007575 on 7765316 degrees of freedom
# Multiple R-squared:  0.8641,	Adjusted R-squared:  0.8641 
# F-statistic: 1.828e+06 on 27 and 7765316 DF,  p-value: < 2.2e-16


# On sample of female drivers:

# Call:
#   lm(formula = fmla, data = saaq_data[sex == "F" & sample == "train"], 
#      weights = num, model = FALSE)
# 
# Weighted Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.149663 -0.002485  0.000071  0.002321  0.134567 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)             8.743e-03  2.191e-05  398.989  < 2e-16 ***
#   curr_pts_1              4.512e-04  3.567e-06  126.484  < 2e-16 ***
#   curr_pts_2              4.521e-04  1.837e-06  246.113  < 2e-16 ***
#   curr_pts_3              3.987e-04  1.783e-06  223.693  < 2e-16 ***
#   curr_pts_4              1.375e-03  5.037e-06  272.894  < 2e-16 ***
#   curr_pts_5              1.219e-03  4.052e-06  300.729  < 2e-16 ***
#   curr_pts_6              1.571e-03  5.108e-06  307.633  < 2e-16 ***
#   curr_pts_7              2.048e-03  9.892e-06  207.047  < 2e-16 ***
#   curr_pts_8              2.073e-03  9.694e-06  213.814  < 2e-16 ***
#   curr_pts_9              9.681e-04  9.236e-06  104.814  < 2e-16 ***
#   curr_pts_10             3.413e-03  1.959e-05  174.173  < 2e-16 ***
#   curr_pts_11_20          3.332e-03  1.037e-05  321.142  < 2e-16 ***
#   curr_pts_21_30          6.033e-03  6.757e-05   89.282  < 2e-16 ***
#   curr_pts_31_150         1.546e-02  1.822e-04   84.889  < 2e-16 ***
#   avg_policy             -1.711e-02  4.298e-05 -398.095  < 2e-16 ***
#   curr_pts_1_policy       1.319e-03  5.267e-06  250.373  < 2e-16 ***
#   curr_pts_2_policy       1.173e-03  2.519e-06  465.804  < 2e-16 ***
#   curr_pts_3_policy       1.294e-03  2.543e-06  508.944  < 2e-16 ***
#   curr_pts_4_policy       2.429e-03  7.117e-06  341.235  < 2e-16 ***
#   curr_pts_5_policy       2.461e-03  5.988e-06  410.972  < 2e-16 ***
#   curr_pts_6_policy       2.565e-03  7.480e-06  342.942  < 2e-16 ***
#   curr_pts_7_policy       3.924e-03  1.391e-05  282.188  < 2e-16 ***
#   curr_pts_8_policy       3.863e-03  1.386e-05  278.627  < 2e-16 ***
#   curr_pts_9_policy       3.262e-03  1.361e-05  239.617  < 2e-16 ***
#   curr_pts_10_policy      1.884e-03  2.469e-05   76.309  < 2e-16 ***
#   curr_pts_11_20_policy   4.579e-03  1.446e-05  316.732  < 2e-16 ***
#   curr_pts_21_30_policy   6.022e-03  8.442e-05   71.336  < 2e-16 ***
#   curr_pts_31_150_policy -5.992e-04  2.266e-04   -2.645  0.00817 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.006453 on 3472762 degrees of freedom
# Multiple R-squared:  0.8614,	Adjusted R-squared:  0.8614 
# F-statistic: 7.993e+05 on 27 and 3472762 DF,  p-value: < 2.2e-16


