
################################################################################
#
# Investigation of SAAQ Excessive Speeding Laws
#
# Produces a dataset for comparison of results with the gold standard,
# the one and only software you would ever want to use,
# for every type of analysis, ever envisioned by humanity.
# Yes, you guessed it: Stata.
#
# Fixed effects regressions where only the policy and points group interactions
# remain in the model.
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
# The second aggregated dataset compresses the dataset by a factor of 1000.
#
#
#
# Lee Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# August 1, 2021
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
# # Set thresholds for determining number of observations in each age group.
# age_group_probs <- c(0.10, 0.30, 0.30, 0.20, ...)
# Just cycle through all categories for the simulation.

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
# sample_end <- '2006-04-15'
sample_end <- '2010-03-31'
day_1 <- as.numeric(as.Date(sample_beg))
day_T <- as.numeric(as.Date(sample_end))
# day_T <- as.numeric(as.Date('2013-01-01')) # Allows for all points to expire.
date_list <- as.Date(seq(day_1, day_T), origin = as.Date('1970-01-01'))

num_dates <- length(date_list)
min(date_list)
max(date_list)



# Set sequence of index numbers for drivers.
num_drivers_each_age <- 100
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
# saaq_data[, sex := factor(sex, levels = c('M', 'F'))]
saaq_data[, age_grp := factor(age_grp, levels = age_group_list)]
saaq_data[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]


# Define the indicator for the policy change.
saaq_data[, policy := date >= april_fools_date]


# Inspection of covariates.
head(saaq_data, 50)
tail(saaq_data, 50)


saaq_data[, .N, by = 'age_grp']
saaq_data[, .N, by = 'curr_pts_grp']


# Generate probability of event.
beta_0 <- 0.005
beta_age <- 0.001*c(1.0, 7.5, 10, 7.5, 5.0, 2.0, 1.0)
beta_pts <- 0.0001*c(seq(0, 10), 12.0, 15.0, 20.0)
beta_pts_policy <- - 0.0001*c(seq(0, 10), 12.0, 15.0, 20.0)/2


# Generate probability of event.
saaq_data[, prob := beta_0]
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




################################################################################
# Select Data for Output to Stata
################################################################################


colnames(saaq_data)
check_var_names <- c('date_stata', 'seq', 'age_grp', 'curr_pts_grp',
                     'policy_int', 'events_int')

write.csv(saaq_data[, check_var_names, with = FALSE],
          file = out_file_name, row.names = FALSE)


################################################################################
# Analysis using algorithms in R
################################################################################



################################################################################
# End
################################################################################





