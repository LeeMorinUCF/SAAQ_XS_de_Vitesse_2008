################################################################################
#
# Investigation of SAAQ Excessive Speeding Laws
#
# Construction of a series of numbers of tickets awarded by the
# number of points per ticket.
# Datasets hold observations for sets of sequential id codes.
# Aggregate data by age and sex categories.
# Join with non-event data from total licensees on SAAQ webpage.
# Output an aggregate dataset suitable for regression analysis.
#
#
#
# Lee Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# June 19, 2021
#
################################################################################
#
# Load data from traffic violations, license suspensions and licensee data.
# Aggregate by demerit point value for each date.
#
# This version calculates cumulative point totals and statistics from past
# driving behaviour.
#
# This version is also trimmed to load the point category aggregation
# from another instance.
#
# This version also includes an extra category for pre-policy change
# points balances.
# 
# This version adds an individual-specific observation of zeros, 
# which is used for individual fixed effects and cluster-robust standard errors, 
# clustered on the individual drivers. 
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

# Set name of file with records of tickets. 
# tickets_file_name <- 'saaq_tickets.csv' # Tickets only.
tickets_file_name <- 'saaq_tickets_balances.csv' # Tickets and balances.

# Set name of file with counts of drivers without tickets.
# Driver population includes drivers with no past tickets or current points.
# no_tickets_file_name <- 'saaq_no_tickets.csv'
driver_counts_file_name <- 'SAAQ_drivers_daily.csv'


# Set name of output file for point totals.
# pts_out_file_name <- 'saaq_pts.csv'
pts_bal_file_name <- 'saaq_point_balances.csv'

# Set name of output file for training, testing and estimation samples.
out_train_file_name <- 'saaq_train.csv'
out_test_file_name <- 'saaq_test.csv'
out_estn_file_name <- 'saaq_estn.csv'


set.seed(42)


################################################################################
# Set Parameters for variables
################################################################################

# Parameters for dividing data into samples.
sample_beg <- '2006-04-01'
sample_end <- '2010-03-31'

pct_train <- 0.70
pct_test <- 0.30
# pct_estn <- 0.30
pct_estn <- max(1 - pct_train - pct_test, 0)


# Create rows for list of dates.
# day_1 <- as.numeric(as.Date('1998-01-01'))
# day_T <- as.numeric(as.Date('2010-12-31'))
# Restrict date range to focus on sample. 
day_1 <- as.numeric(as.Date(sample_beg))
day_T <- as.numeric(as.Date(sample_end))
# day_T <- as.numeric(as.Date('2013-01-01')) # Allows for all points to expire.
date_list <- as.Date(seq(day_1, day_T), origin = as.Date('1970-01-01'))

length(date_list)
min(date_list)
max(date_list)


# Age group categories for defining factors.
# Original finer grouping to match driver counts: 
# age_group_list <- c('0-15', '16-19', '20-24', '25-34', '35-44', '45-54',
#                     '55-64', '65-74', '75-84', '85-89', '90-199')
# Coarser grouping to merge less-populated age groups:
age_group_list <- c('0-19', 
                    '20-24', '25-34', '35-44', '45-54',
                    '55-64', '65-199')

# Current points group categories for defining factors.
curr_pts_grp_list <- c(seq(0,10), '11-20', '21-30', '31-150')


# Current version has current points groups and indicator
# for pre-policy-change mid-to-high points balance:
# agg_var_list <- c('date', 'sex', 'age_grp', 'past_active', 
#                   'curr_pts_grp', 'points')

# List of variables for joined datasets.
join_var_list <- c('date', 'seq', 'sex', 'age_grp', 'past_active', 
                   'curr_pts_grp', 'points', 'num')
train_var_list <- c('date', 'seq', 'sex', 'age_grp', 'past_active', 
                    'curr_pts_grp', 'points', 'train_num')
test_var_list <- c('date', 'seq', 'sex', 'age_grp', 'past_active', 
                   'curr_pts_grp', 'points', 'test_num')
estn_var_list <- c('date', 'seq', 'sex', 'age_grp', 'past_active', 
                   'curr_pts_grp', 'points', 'estn_num')



################################################################################
# Load Datasets
################################################################################

#-------------------------------------------------------------------------------
# Load Daily Driver Counts
#-------------------------------------------------------------------------------

# Driver population includes all drivers, 
# including those with no past tickets or current points.
in_path_file_name <- sprintf('%s/%s', data_in_path, driver_counts_file_name)
driver_counts <- fread(in_path_file_name)

# Restrict sample to date range. 
driver_counts <- driver_counts[date >= sample_beg & 
                                 date <= sample_end, ]

# Need to add empty column of curr_pts_grp to
# default population with no tickets.
driver_counts[, curr_pts_grp := 0]

# Also need to add empty column of past_active to
# default population with no tickets.
driver_counts[, past_active := FALSE]

# Standardize date variable.
# driver_counts[, date := dinf]
# Changed name in source file.


# Define categorical variables as factors.
driver_counts[, sex := factor(sex, levels = c('M', 'F'))]
driver_counts[age_grp %in% c('0-15', '16-19'), age_grp := '0-19']
driver_counts[age_grp %in% c('65-74', '75-84', '85-89', '90-199'), age_grp := '65-199']
driver_counts[, age_grp := factor(age_grp, levels = age_group_list)]
driver_counts <- unique(driver_counts[, num := sum(num), by = c('date', 'age_grp', 'sex')])
driver_counts[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]


# summary(driver_counts)
colnames(driver_counts)
lapply(driver_counts, class)

summary(driver_counts[, c(join_var_list), with = FALSE])



# Count observations.
nrow(driver_counts)/length(date_list)

table(driver_counts[, age_grp], driver_counts[, sex])
# Every sex-age_grp combination.
# All have zero curr_pts and no seq.





#-------------------------------------------------------------------------------
# Load Points Balances
#-------------------------------------------------------------------------------


# # Read a dataset tabulated elsewhere.
# counts_version <- 3
# in_file_name <- sprintf('saaq_past_counts_%d_%s_%s_v%d.csv',
#                          ptsVersion,
#                          1998, 2010,
#                          counts_version)
# data_count_path <- 'SAAQ_counts/'

in_path_file_name <- sprintf('%s/%s', data_in_path, pts_bal_file_name)
saaq_past_counts <- fread(file = in_path_file_name)


nrow(saaq_past_counts)
colnames(saaq_past_counts)
lapply(saaq_past_counts, class)

# Restrict sample to date range. 
saaq_past_counts <- saaq_past_counts[, sample_sel := date >= sample_beg & 
                                       date <= sample_end]

# Add variables to match other datasets.
saaq_past_counts[, points := 0]
saaq_past_counts[, num := N]
saaq_past_counts[, N := NULL]
saaq_past_counts[, seq := 0]


# Define categorical variables as factors.
saaq_past_counts[, sex := factor(sex, levels = c('M', 'F'))]
saaq_past_counts[age_grp %in% c('0-15', '16-19'), age_grp := '0-19']
saaq_past_counts[age_grp %in% c('65-74', '75-84', '85-89', '90-199'), age_grp := '65-199']
saaq_past_counts[, age_grp := factor(age_grp, levels = age_group_list)]
saaq_past_counts <- unique(saaq_past_counts[, num := sum(num), 
                                            by = c('date', 'age_grp', 'sex', 
                                                   'past_active', 'curr_pts_grp')])
saaq_past_counts[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]



summary(saaq_past_counts[, c(join_var_list), with = FALSE])
# Recall that negative values are drivers swapping in from 
# the zero-point category. 
# These will be canceled out later, when driver counts are added in. 
# Update: These are already canceled out by initializing the balances. 
# They are removed from the counts of inactive drivers below.


#-------------------------------------------------------------------------------
# Load data from records of tickets.
#-------------------------------------------------------------------------------


in_path_file_name <- sprintf('%s/%s', data_in_path, tickets_file_name)
# saaq_tickets <- data.table(read.csv(file = in_path_file_name))
saaq_tickets <- fread(file = in_path_file_name)

# Restrict sample to date range. 
saaq_tickets <- saaq_tickets[, sample_sel := date >= sample_beg & 
                               date <= sample_end]


# Standardize date variable.
# Note that "date" is lagged one day to calculate point-balance counts.
# The variable "dinf" refers to the date of infraction. 
saaq_tickets[, date_bal := date]
saaq_tickets[, date := dinf]


# Adjuste balance category variable.
# For balances, status moves from prev_pts to curr_pts, 
# during a day, as a driver gets a ticket (or not). 
# FOr conditioning on past behaviour, prev_pts is the relevant balance category
# for driver's status before getting a ticket.
saaq_tickets[, curr_pts_bal := curr_pts]
saaq_tickets[, curr_pts_grp_bal := curr_pts_grp]
saaq_tickets[, curr_pts := prev_pts]
saaq_tickets[, curr_pts_grp := prev_pts_grp]


# Each unique ticket event happens once.
saaq_tickets[, num := 1]
saaq_tickets[, train_num := 1]
saaq_tickets[, test_num := 1]
saaq_tickets[, estn_num := 1]


colnames(saaq_tickets)
lapply(saaq_tickets, class)


# Define categorical variables as factors.
saaq_tickets[, sex := factor(sex, levels = c('M', 'F'))]
saaq_tickets[age_grp %in% c('0-15', '16-19'), age_grp := '0-19']
saaq_tickets[age_grp %in% c('65-74', '75-84', '85-89', '90-199'), age_grp := '65-199']
saaq_tickets[, age_grp := factor(age_grp, levels = age_group_list)]
saaq_tickets[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]


# Previous data prep has joined in curr_pts_grp, past_active.
summary(saaq_tickets[, c(join_var_list), with = FALSE])


################################################################################
# Adjust counts of licensed drivers for distribution across
# point-balance categories
################################################################################

# Obtain counts of drivers who left the curr_pts_grp == 0 category.
# That is, all drivers who got a ticket at some point.
# These are already contained in saaq_past_counts
# (even if they are sometime in curr_pts_grp == 0)
# and should be removed from driver_counts to avoid double-counting these drivers.
non_zero_counts <- unique(saaq_tickets[, c('seq', 'sex', 'age_grp')])
non_zero_counts <- unique(non_zero_counts[, .N, by = c('sex', 'age_grp')])
# non_zero_counts[, curr_pts_grp := 0]
# Change column order to match.
# non_zero_counts <- non_zero_counts[, c('sex', 'age_grp', 'curr_pts_grp', 'N')]
# non_zero_counts <- non_zero_counts[, c('sex', 'age_grp', 'N')]
# Skip adding the zeros for now.
# non_zero_counts[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]
# non_zero_counts <- non_zero_counts[order(sex, age_grp, curr_pts_grp)]
non_zero_counts <- non_zero_counts[order(sex, age_grp)]


# Subtract these active drivers from the total count of drivers.
for (row in 1:nrow(non_zero_counts)) {
  sex_sel <- non_zero_counts[row, sex]
  age_grp_sel <- non_zero_counts[row, age_grp]
  num_non_zero <- non_zero_counts[row, N]
  
  # Note that all inactive drivers have zero points, curr_pts_grp == 0,  
  # past_active == FALSE and seq == 0. 
  driver_counts[sex == sex_sel & age_grp == age_grp_sel, num := num - num_non_zero]
}

# Only the inactive drivers will remain.

summary(driver_counts)


################################################################################
# Join Daily Driver Events with Full Dataset of Non-events
################################################################################

#-------------------------------------------------------------------------------
# Verify compatibility of all three datasets
#-------------------------------------------------------------------------------



# The positive point balances come from here.
summary(saaq_past_counts[, c(join_var_list), with = FALSE])
# In earlier versions, there were negative counts for the zero categories.
# These were accumulated as drivers swapped from zero to positive balances. 
summary(saaq_past_counts[num < 0, num, by = curr_pts_grp])
# No more negative counts remain in the zero categories.
summary(saaq_past_counts[curr_pts_grp != 0, min(num), by = curr_pts_grp])
# Confirmed. 


# The full count of drivers with zero point balances come from here.
summary(driver_counts[, c(join_var_list), with = FALSE])


# The ticket events are contained in here.
summary(saaq_tickets[, c(join_var_list), with = FALSE])




# Sort them all in the same order.
saaq_past_counts <- saaq_past_counts[order(date, sex, age_grp, past_active, curr_pts_grp), ]
driver_counts <- driver_counts[order(date, sex, age_grp, past_active, curr_pts_grp), ]
saaq_tickets <- saaq_tickets[order(date, sex, age_grp, past_active, curr_pts_grp), ]



#-------------------------------------------------------------------------------
# Divide data into samples.
#-------------------------------------------------------------------------------

# Couts of drivers in point balance categories.
saaq_past_counts[, train_num := round(num*pct_train)]
saaq_past_counts[, test_num := round(num*pct_test)]
saaq_past_counts[, estn_num := round(num*pct_estn)]

# Keep current points group if counts are initialized. 
# saaq_past_counts[, sample_sel := curr_pts_grp != 0 & 
#                    date >= sample_beg & 
#                    date <= sample_end]
saaq_past_counts[, sample_sel := date >= sample_beg & 
                   date <= sample_end]



# Counts of licensed drivers. 
driver_counts[, train_num := round(num*pct_train)]
driver_counts[, test_num := round(num*pct_test)]
driver_counts[, estn_num := round(num*pct_estn)]

driver_counts[, sample_sel := date >= sample_beg & 
                date <= sample_end]



# Select drivers to allocate tickets to samples. 
seq_list <- unique(saaq_tickets[, c('seq'), with = FALSE])[order(seq)]
seq_list <- seq_list[, seq]
seq_sel_rand <- runif(n = length(seq_list))

seq_sel_train <- seq_list[seq_sel_rand <= pct_train]
seq_sel_test <- seq_list[seq_sel_rand > pct_train & 
                           seq_sel_rand <= pct_train + pct_test]
seq_sel_estn <- seq_list[seq_sel_rand > 1 - pct_estn]

# Every driver is allocated to one and only one dataset.
length(unique(c(seq_sel_train, seq_sel_test, seq_sel_estn))) == length(seq_list)
length(c(seq_sel_train, seq_sel_test, seq_sel_estn)) == length(seq_list)


# Select sample within specified dates.
saaq_tickets[, sample_sel := date >= sample_beg & 
               date <= sample_end]


# Set indicator for allocation to samples.
saaq_tickets[, train_sel := seq %in% seq_sel_train & sample_sel == TRUE]
saaq_tickets[, test_sel := seq %in% seq_sel_test & sample_sel == TRUE]
saaq_tickets[, estn_sel := seq %in% seq_sel_estn & sample_sel == TRUE]

table(saaq_tickets[, train_sel], useNA = 'ifany')
table(saaq_tickets[, test_sel], useNA = 'ifany')
table(saaq_tickets[, estn_sel], useNA = 'ifany')


table(saaq_tickets[, train_sel], 
      saaq_tickets[, test_sel], useNA = 'ifany')


#-------------------------------------------------------------------------------
# Join the datasets by stacking
#-------------------------------------------------------------------------------

# Training dataset.
saaq_train <- rbind(saaq_past_counts[sample_sel == TRUE, c(train_var_list), with = FALSE], 
                    driver_counts[sample_sel == TRUE, c(train_var_list), with = FALSE], 
                    saaq_tickets[train_sel == TRUE, c(train_var_list), with = FALSE])
colnames(saaq_train) <- join_var_list
saaq_train <- saaq_train[order(date, seq, sex, age_grp, past_active, curr_pts_grp, points), ]
# Aggregate counts of zero balances from two data sources.
saaq_train <- unique(saaq_train[, num := sum(num), 
                                by = list(date, seq, sex, age_grp, past_active, curr_pts_grp, points)])


# Testing dataset.
saaq_test <- rbind(saaq_past_counts[sample_sel == TRUE, c(test_var_list), with = FALSE], 
                   driver_counts[sample_sel == TRUE, c(test_var_list), with = FALSE], 
                   saaq_tickets[test_sel == TRUE, c(test_var_list), with = FALSE])
colnames(saaq_test) <- join_var_list
saaq_test <- saaq_test[order(date, seq, sex, age_grp, past_active, curr_pts_grp, points), ]
# Aggregate counts of zero balances from two data sources.
saaq_test <- unique(saaq_test[, num := sum(num), 
                              by = list(date, seq, sex, age_grp, past_active, curr_pts_grp, points)])


# Estimation dataset.
saaq_estn <- rbind(saaq_past_counts[sample_sel == TRUE, c(estn_var_list), with = FALSE], 
                   driver_counts[sample_sel == TRUE, c(estn_var_list), with = FALSE], 
                   saaq_tickets[estn_sel == TRUE, c(estn_var_list), with = FALSE])
colnames(saaq_estn) <- join_var_list
saaq_estn <- saaq_estn[order(date, seq, sex, age_grp, past_active, curr_pts_grp, points), ]
# Aggregate counts of zero balances from two data sources.
saaq_estn <- unique(saaq_estn[, num := sum(num), 
                              by = list(date, seq, sex, age_grp, past_active, curr_pts_grp, points)])




# # All three datasets.
# nrow(saaq_tickets) + nrow(saaq_past_counts) + nrow(driver_counts)
# 
# # Removing overlap from zero point-balance category.
# nrow(saaq_tickets) + nrow(saaq_past_counts)
# 
# # Includes observations after 2010, when tickets continue to expire.
# nrow(saaq_tickets) + nrow(saaq_past_counts) + nrow(driver_counts)
# 
# nrow(saaq_full)



################################################################################
# Output Training, Testing and Estimation Datasets
################################################################################


# Training dataset.
out_path_file_name <- sprintf('%s/%s', data_out_path, out_train_file_name)
write.csv(x = saaq_train, file = out_path_file_name, row.names = FALSE)


# Testing dataset.
out_path_file_name <- sprintf('%s/%s', data_out_path, out_test_file_name)
write.csv(x = saaq_test, file = out_path_file_name, row.names = FALSE)


# Estimation dataset.
if (pct_estn > 0) {
  out_path_file_name <- sprintf('%s/%s', data_out_path, out_estn_file_name)
  write.csv(x = saaq_estn, file = out_path_file_name, row.names = FALSE) 
}



################################################################################
# End
################################################################################
