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
# summary(saaq_data[, c('events', 'avg_events')])
# Create an FWL projection of the policy indicator.
saaq_data[, avg_policy := sum(policy*num)/sum(num), by = 'seq']
summary(saaq_data[, avg_policy])



# Generate new variables for current points categories. 
for (curr_pts_level in curr_pts_grp_list) {
  
  
  print(sprintf('FWL projections for curr_pts_grp %s', curr_pts_level))
  
  # Generate a new column to indicate the average time at this point level. 
  saaq_data[, avg_FWL_count := 
              sum((curr_pts_grp == curr_pts_level)*num)/sum(num), by = 'seq']

  # Allocate this variable to a new column.
  col_var_name <- sprintf('curr_pts_%s', gsub('-', '_', curr_pts_level))
  saaq_data[, col_var_name] <- saaq_data[, avg_FWL_count]
  
  
  # Now calculate a new column to indicate the average time at this point level, 
  # during the post-policy period: a policy-points-level interaction. 
  saaq_data[, avg_FWL_count := 
              sum((curr_pts_grp == curr_pts_level)*policy*num)/sum(num), by = 'seq']
  
  # Allocate this variable to a new column.
  col_var_name <- sprintf('curr_pts_%s_policy', gsub('-', '_', curr_pts_level))
  saaq_data[, col_var_name] <- saaq_data[, avg_FWL_count]
  
}


summary(saaq_data)


################################################################################
# Fit a series of models
################################################################################

#-------------------------------------------------------------------------------
# Define list of models
#-------------------------------------------------------------------------------


