
################################################################################
#
# Investigation of SAAQ Excessive Speeding Laws
#
# Produces a dataset for comparison of results with the fixed effects model
# with cluster-robust standard errors in Stata.
#
# The model estimates fixed effects regressions where only the policy
# and points group interactions remain in the model.
# These are the only variables that vary across the individual series.
#
# This requires a dataset with the counts for point balances for each driver
# across the individual driving histories.
# However, Stata is not equipped with the functionality for frequency weights
# that vary by individual.
# Therefore, this script makes two datasets to test the R code:
#   1. With one observation per line (and many zeros), for use in Stata.
#   2. With the above data aggregated so that identical observations are grouped
#     and weighted with a frequency weight, for use in R only.
#
# The second aggregated dataset compresses the dataset by a factor of 1000
# with the actual dataset.
#
# The goal of this validation exercise is to verify that the coded FE and CRVE
# estimates in R with weighted observations match those in Stata with
# weight one on each observation.
#
#
#
# Lee Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# August 3, 2021
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

# Load library for estimating FE and CRVE estimators with frequency-weighted data.
source('Lib/FE_CRVE_lib.R')


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
sim_file_name <- 'Stata_FE_CRVE_sim_data.csv'


# Set name of output file for test dataset.
out_file_name <- sprintf('%s/%s', data_out_path, sim_file_name)




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
# This variable can vary over time for each driver.
# It needs to vary across observations.
pts_group_probs <- c(0.80 - 0.0786, seq(0.10, 0.01, by = - 0.01)/2, 0.0025, 0.0010, 0.0001)
pts_group_probs
length(pts_group_probs)
sum(pts_group_probs)


# Set date of policy change.
april_fools_date <- '2008-04-01'
# No joke: policy change on April Fool's Day!


# Set list of dates.
sample_beg <- '2006-04-01'
sample_end <- '2010-03-31'
day_1 <- as.numeric(as.Date(sample_beg))
day_T <- as.numeric(as.Date(sample_end))
date_list <- as.Date(seq(day_1, day_T), origin = as.Date('1970-01-01'))

num_dates <- length(date_list)
min(date_list)
max(date_list)



# Set sequence of index numbers for drivers.
num_drivers_each_age <- 500
num_drivers <- num_drivers_each_age*length(age_group_list)
seq_list <- seq(num_drivers)



################################################################################
# Generate Dataset
################################################################################

# Start with combinations of categories.
saaq_data <- data.table(expand.grid(date = date_list,
                                    seq = seq_list))

# Add age groups in a way that leaves them fixed by driver.
# Age is defined as the age at the policy change.
# Drivers are ordered by age group.
saaq_data[, age_grp := rep(age_group_list, each = num_dates*num_drivers_each_age)]
# Check number of observations:
num_dates*num_drivers_each_age*length(age_group_list) == nrow(saaq_data)


# Assign drvers' observations to current points group.
set.seed(42)
saaq_data[, pts_unif := runif(nrow(saaq_data))]
saaq_data[, curr_pts_grp := cut(x = pts_unif,
                                breaks = c(0, cumsum(pts_group_probs)),
                                labels = curr_pts_grp_list)]


# Define categorical variables as factors.
saaq_data[, age_grp := factor(age_grp, levels = age_group_list)]
saaq_data[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]


# Define the indicator for the policy change.
saaq_data[, policy := date >= april_fools_date]


# Define a driver-specific fixed effect.
set.seed(42)
# num_drivers <- num_drivers_each_age*length(age_group_list)
# num_dates <- length(date_list)
saaq_data[, FE_unif := rep(runif(num_drivers), each = num_dates)]
# Verify transitions at each new driver.
saaq_data[seq(1, num_dates + 2), ]
saaq_data[seq(nrow(saaq_data) - num_dates - 2, nrow(saaq_data)), ]

# Uniformly distributed with no variance by driver.
summary(saaq_data[, FE_unif])
summary(saaq_data[, var(FE_unif), by = 'seq'])


# Inspection of covariates.
head(saaq_data, 50)
tail(saaq_data, 50)
summary(saaq_data)

saaq_data[, .N, by = 'age_grp']
saaq_data[, .N, by = 'curr_pts_grp']


# Generate probability of event.
# Notice that the coefficients are an order of magnitude larger
# to account for the fact that the simulated sample is much smaller.
beta_0 <- 0.05
beta_policy <- - 0.02
beta_FE <- 0.01
beta_age <- 0.01*c(1.0, 7.5, 10, 7.5, 5.0, 2.0, 1.0)
# Model 1: Past points indicate tendency to get a ticket.
# beta_pts <- 0.01*c(seq(0, 10), 12.0, 15.0, 20.0)
# Model 2: Past points are an ineffective deterrent, just balancing
# with the benefit of speeding.
beta_pts <- 0.00*c(seq(0, 10), 12.0, 15.0, 20.0)
beta_pts_policy <- - 0.001*c(seq(0, 10), 12.0, 15.0, 20.0)/2


# Generate probability of event.
saaq_data[, prob := beta_0]
saaq_data[, prob := prob + beta_FE*FE_unif]
saaq_data[policy == TRUE,
          prob := prob + beta_policy]
for (i in length(age_group_list)) {
  saaq_data[age_grp == age_group_list[i],
            prob := prob + beta_age[i]]
}
for (i in length(curr_pts_grp_list)) {
  saaq_data[curr_pts_grp == curr_pts_grp_list[i],
            prob := prob + beta_pts[i]]
}
for (i in length(curr_pts_grp_list)) {
  saaq_data[curr_pts_grp == curr_pts_grp_list[i] &
              policy == TRUE,
            prob := prob + beta_pts_policy[i]]
}


summary(saaq_data[, prob])


# Generate dependent variable:
set.seed(42)
saaq_data[, event_unif := runif(nrow(saaq_data))]
saaq_data[, events := event_unif < prob]

summary(saaq_data[, events])



summary(saaq_data)
head(saaq_data, 50)
tail(saaq_data, 50)



################################################################################
# Modify Variables for Compatibility with Stata
################################################################################


saaq_data[, date_stata := sprintf('%s%s%s',
                                  format(date, '%d'),
                                  tolower(month.abb[as.integer(format(date, '%m'))]),
                                  format(date, '20%y'))]

head(saaq_data[, date_stata])
tail(saaq_data[, date_stata])


# Convert policy and event variables to binary integers instead of logical.
saaq_data[, policy_int := as.integer(policy)]
saaq_data[, events_int := as.integer(events)]

# Verify conversion.
table(saaq_data[, policy_int],
      saaq_data[, policy], useNA = 'ifany')
table(saaq_data[, events_int],
      saaq_data[, events], useNA = 'ifany')


################################################################################
# Select Data for Output to Stata
################################################################################


colnames(saaq_data)
check_var_names <- c('date_stata', 'seq', 'age_grp', 'curr_pts_grp',
                     'policy_int', 'events_int')

write.csv(saaq_data[, check_var_names, with = FALSE],
          file = out_file_name, row.names = FALSE)


################################################################################
################################################################################
# Analysis using algorithms in R
################################################################################
################################################################################


# Without weights, all observations have weight 1.
saaq_data[, num := 1L]

# With the artificial data, all observations are included.
saaq_data[, sub_sel_obsn := TRUE]


################################################################################
# First stage FWL Regressions
################################################################################


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
# Aggregate the data to keep only the unique observations with a
# weight for the frequency of each observation.
################################################################################


# Store the original dataset.
saaq_data_orig <- copy(saaq_data)
# Replace with the original in case of problems.
# saaq_data <- copy(saaq_data_orig)
# The copy function makes a deep copy, instead of a pointer.

# Aggregate by identical observations across each driver.
colnames(saaq_data)

saaq_data <- saaq_data[, num := sum(num),
                            by = c('seq', 'policy', 'events',
                                   'age_grp', 'curr_pts_grp',
                                   'sub_sel_obsn')]

summary(saaq_data)

saaq_data <- unique(saaq_data[, c('seq', 'policy', 'events',
                                  'age_grp', 'curr_pts_grp',
                                  'sub_sel_obsn', 'num'),
                              with = FALSE])
# A nice small dataset, aggregated over number of identical observations.

# Verify that the dataset matches.
saaq_data[seq == 1, ]

summary(saaq_data_orig[, sum(num), by = 'seq'])
summary(saaq_data[, sum(num), by = 'seq'])

summary(saaq_data_orig[, sum(num), by = 'age_grp'])
summary(saaq_data[, sum(num), by = 'age_grp'])

saaq_data_orig[, sum(num), by = 'age_grp']
saaq_data[, sum(num), by = 'age_grp']


summary(saaq_data_orig[, sum(num), by = 'curr_pts_grp'])
summary(saaq_data[, sum(num), by = 'curr_pts_grp'])

saaq_data_orig[, sum(num), by = 'curr_pts_grp']
saaq_data[, sum(num), by = 'curr_pts_grp']

# Looks the same, except that the aggregated data set
# is much smaller: 93790 vs. 5113500 unique observations.




################################################################################
# First stage FWL Regressions
################################################################################




################################################################################
#
# Fixed Effects Regressions:
# Full model: current points group and policy interaction.
#
################################################################################

# First variable is the policy indicator,
# projected off the fixed effects indicators.
var_list_0 <- c('dev_policy')

# Set the list of variables by points category.
# Assumes first points category omitted:
var_list_1 <- sprintf('curr_pts_%s',
                      gsub('-', '_', curr_pts_grp_list[2:length(curr_pts_grp_list)]))
var_list_2 <- sprintf('curr_pts_%s_policy',
                      gsub('-', '_', curr_pts_grp_list[2:length(curr_pts_grp_list)]))
var_list <- c(var_list_0, var_list_1, var_list_2)

# Keep the constant to match Stata.
fmla_str <- sprintf('dev_events ~ %s',
                    paste(var_list, collapse = " + "))
fmla <- as.formula(fmla_str)

# Fit regression model on training sample for male drivers.
lm_spec <- lm(formula = fmla,
              data = saaq_data[sub_sel_obsn == TRUE, ],
              weights = num,
              model = FALSE)

# Print a summary to screen.
print(summary(lm_spec))
# Slope coefficients are the same: the estimates, that is.

# Standard error needs some adjustment for degrees of freedom.
summ_lm <- summary(lm_spec)

# This is the table of estimates with "standard" standard errors.
adj_FE_coef_table(coef_lm = summ_lm$coefficients,
                  resid_lm = summ_lm$residuals,
                  num_obs = saaq_data[, sum(num)],
                  num_rows = nrow(saaq_data),
                  num_vars = length(var_list),
                  num_FE = num_drivers)



################################################################################
# Adjust for Cluster Robust Standard Errors
################################################################################

# Calculate a matrix of the weighted product of residuals
# and covariates for calculation of the cluster-robust variance estimator.
CRVE_dt <- calc_CRVE_tab(saaq_data = saaq_data, # Should pass shallow copy with pointer.
                         weights = summ_lm$weights,
                         resids = summ_lm$residuals,
                         curr_pts_grp_list = curr_pts_grp_list)

# colnames(CRVE_dt)
# summary(CRVE_dt)

# Drop any omitted variables. For example, in the real data, the
# 31-150 points group category is zero for all pre-policy days.
# var_col_names <- var_col_names[var_col_names != 'curr_pts_31_150']


# Calculate matrix aggregated by seq.
# First curr_pts_grp category is dropped.
curr_pts_grp_est_list <- curr_pts_grp_list[2:length(curr_pts_grp_list)]
var_col_names <- c(sprintf('curr_pts_%s',
                           gsub('-', '_', curr_pts_grp_est_list)),
                   sprintf('curr_pts_%s_policy',
                           gsub('-', '_', curr_pts_grp_est_list)))
# Add columns for intercept and policy indicator.
var_col_names <- c('(Intercept)', 'dev_policy', var_col_names)
# Verify that columns are aligned.
var_col_names == colnames(CRVE_dt)[1:28]
# Now aggregate by individual.
CRVE_by_seq <- CRVE_dt[, lapply(.SD, sum), by = seq, .SDcols = var_col_names]

# summary(CRVE_by_seq)

# Calculate the inner matrix of the CRVE sandwich estimator
# using the weighted product of residuals and covariates.
CRVE_meat <- calc_CRVE_meat(CRVE_by_seq = CRVE_by_seq,
                            var_col_names = var_col_names)

# Verify that columns are aligned.
var_col_names == colnames(CRVE_meat)


# Now get the bread of the sandwich from the regression model.
# Make an adjustment to back out the standard error.
CRVE_bread <- vcov(lm_spec, complete = FALSE)/summ_lm$sigma^2
# This should be X-transpose-X-inverse.
# And the order of columns must be the same.
colnames(CRVE_bread)
# Verify that columns are aligned.
var_col_names == colnames(CRVE_bread)
colnames(CRVE_bread) == colnames(CRVE_meat)



# Calculate the standard errors from the CRVE sandwich estimator.
CRVE_SE <- calc_CRVE_FE_SE(CRVE_bread = CRVE_bread,
                           CRVE_meat = CRVE_meat,
                           num_ind = nrow(CRVE_by_seq), # = num_seq
                           num_obs = sum(summ_lm$weights), # = num_sub
                           num_vars = length(var_col_names))


# Adjust the table of coefficients
# for adjusted standard errors in the fixed effects model.
coef_adj <- adj_FE_coef_SE(coef_orig = summ_lm$coefficients,
                           se_adj = CRVE_SE,
                           # num_obs = saaq_data[, sum(num)],
                           num_obs = num_drivers,
                           num_vars = length(var_list),
                           # num_FE = num_drivers,
                           num_FE = 0)
# Notice that the degrees of freedom depend on the
# number of clusters (drivers) and not the number of observations.
# Thus, the number of variables does not include the fixed effects.



################################################################################
# End
################################################################################
