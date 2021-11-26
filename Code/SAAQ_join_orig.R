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
# This version uses the original method of joining
# the data, by stacking datasets and without
# division into subsamples.
#
################################################################################


################################################################################
# Clearing Workspace and Declaring Packages
################################################################################

# Clear workspace, if running interactively.
rm(list=ls(all=TRUE))

# Load package for importing datasets in proprietary formats.
library(foreign)

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

# Set name of output file for full dataset.
out_file_name <- 'saaq_full.csv'
# In past version, dataset was not divided.



set.seed(42)


################################################################################
# Set Parameters for variables
################################################################################

# Create rows for list of dates.
day_1 <- as.numeric(as.Date('1998-01-01'))
day_T <- as.numeric(as.Date('2010-12-31'))
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
agg_var_list <- c('date', 'sex', 'age_grp', 'past_active',
                  'curr_pts_grp', 'points')



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

summary(driver_counts[, c(agg_var_list, 'num'), with = FALSE])



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




summary(saaq_past_counts[, c(agg_var_list, 'num'), with = FALSE])
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


# Standardize date variable.
# Note that "date" is lagged one day to calculate point-balance counts.
# The variable "dinf" refers to the date of infraction.
saaq_tickets[, date_bal := date]
saaq_tickets[, date := dinf]


# Adjust balance category variable.
# For balances, status moves from prev_pts to curr_pts,
# during a day, as a driver gets a ticket (or not).
# For conditioning on past behaviour, prev_pts is the relevant balance category
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
summary(saaq_tickets[, c(agg_var_list), with = FALSE])





################################################################################
# Join Daily Driver Events with Full Dataset of Non-events
################################################################################


#-------------------------------------------------------------------------------
# First join drivers with points balances
# to the driver counts without tickets
#-------------------------------------------------------------------------------

# The positive point balances come from here.
summary(saaq_past_counts[, c(agg_var_list, 'num'), with = FALSE])
# Notice the negative counts for the zero categories.
# These were accumulated as drivers swapped from zero to positive balances.
summary(saaq_past_counts[N < 0, N, by = curr_pts_grp])
# All negative counts are in the zero categories.


# The drivers with zero point balances come from here.
summary(driver_counts[, c(agg_var_list, 'num'), with = FALSE])

# First put them both in the same order.
saaq_past_counts <- saaq_past_counts[order(date, sex, age_grp, past_active, curr_pts_grp), ]
driver_counts <- driver_counts[order(date, sex, age_grp, past_active, curr_pts_grp), ]



# These categories might have negative balances to offset driver counts.
summary(saaq_past_counts[curr_pts_grp == 0 &
                           past_active == FALSE, num])
summary(saaq_past_counts[curr_pts_grp != 0 |
                           past_active != FALSE, num])
saaq_past_counts[(curr_pts_grp != 0 |
                    past_active != FALSE) &
                   num < 0, ]
# Note that there are zeros on some past_actives as well.
saaq_past_counts[curr_pts_grp != 0  & num < 0, ]
# But no negative counts other than those with zero balances.


# Need to combine the datasets and aggregate on the sum of drivers.
colnames(saaq_past_counts)
colnames(driver_counts)

# Final inspection of datasets before joining.
summary(saaq_past_counts[, join_var_list, with = FALSE])
summary(driver_counts[, join_var_list, with = FALSE])


# Stack datasets and aggregate the counts of drivers.
all_driver_counts <- rbind(driver_counts[, join_var_list, with = FALSE],
                           saaq_past_counts[, join_var_list, with = FALSE])
all_driver_counts <- all_driver_counts[, num := sum(num),
                                       by = list(date, seq,
                                                 sex, age_grp, past_active,
                                                 curr_pts_grp, points)]
all_driver_counts <- unique(all_driver_counts)
# Note: same number of rows as original:
nrow(all_driver_counts)
nrow(saaq_past_counts)


# Moment of truth:
summary(all_driver_counts[, ])
# Some negatives remain. Split by past_active.
summary(all_driver_counts[past_active == FALSE, ])
# Some negatives in the past active category.
summary(all_driver_counts[past_active == TRUE, ])


# Count the remaining negatives.
all_driver_counts[past_active == FALSE & num < 0, .N]
all_driver_counts[past_active == TRUE & num < 0, .N]
all_driver_counts[past_active == FALSE & num < 0, sum(num)]
all_driver_counts[past_active == TRUE & num < 0, sum(num)]


# See who they are.
all_driver_counts[num < 0, .N, by = list(sex, age_grp, past_active)]
all_driver_counts[num < 0, sum(num), by = list(sex, age_grp, past_active)]
all_driver_counts[num < 0, mean(num),
                  by = list(sex, age_grp, past_active)][order(sex, past_active, age_grp)]


# Check within sample period only.
all_driver_counts[num < 0 &
                    date >= '2006-01-01' &
                    date <= '2010-12-31', mean(num),
                  by = list(sex, age_grp, past_active)][order(sex, past_active, age_grp)]
# In the relevant sample, only the past actives are negative.
# This makes sense, since I didn't allocate from the drivers with no tickets.
# Check again by aggregating over past_active.
all_driver_counts[num < 0 &
                    date >= '2006-01-01' &
                    date <= '2010-12-31', mean(num),
                  by = list(sex, age_grp)][order(sex, age_grp)]
# Still have negative values in the tens of thousands.
all_driver_counts[num < 0 &
                    date >= '2008-01-01' &
                    date <= '2010-12-31', mean(num),
                  by = list(sex, age_grp)][order(sex, age_grp)]




# Take average without past_active.
all_driver_counts[num < 0 &
                    date >= '2006-01-01' &
                    date <= '2010-12-31', mean(num),
                  by = list(sex, age_grp)][order(sex, age_grp)]
# Restrict to zero points balance.
all_driver_counts[curr_pts_grp != 0 &
                    date >= '2006-01-01' &
                    date <= '2010-12-31', mean(num),
                  by = list(sex, age_grp)][order(sex, age_grp)]
# The average numbers in the zero category is only up to the thousands.


# One problem is that the counts do not represent the same drivers:
# drivers enter and leave Quebec
# and new drivers get their license and others stop driving.


# The best solution is to add the categories from both datasets.


#-------------------------------------------------------------------------------
# Next join driver counts and point-balance counts
# to the record of tickets.
#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
# Aggregate data (if necessary).
#-------------------------------------------------------------------------------


saaq_agg <- saaq_tickets[, .N, by = agg_var_list]

colnames(saaq_agg) <- c(agg_var_list, 'num')
colnames(saaq_agg)

head(saaq_agg, 50)
tail(saaq_agg, 50)

summary(saaq_agg)



# Change type to factor.
# saaq_agg[, curr_pts_grp := as.factor(curr_pts_grp)]
saaq_agg[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]



#--------------------------------------------------------------------------------
# Final inspection
#--------------------------------------------------------------------------------


# Select columns from saaq_agg.
summary(saaq_agg[, c(agg_var_list, 'num'), with = FALSE])


# Select columns from saaq_no_tickets_full in same order as saaq_agg.
# summary(saaq_no_tickets_full[, c(agg_var_list, 'num'), with = FALSE])


# For previous joins:
# Select columns from driver_counts in same order as saaq_agg.
summary(driver_counts[, c(agg_var_list, 'num'), with = FALSE])

# Select columns from saaq_past_counts_sum in same order as saaq_agg.
summary(saaq_past_counts_sum[, c(agg_var_list, 'num'), with = FALSE])



#--------------------------------------------------------------------------------
# Join Daily Driver Events with Full Dataset of Non-events
#--------------------------------------------------------------------------------


# Stack the data frames with properly ordered columns.

# First version without point balances.
# saaq_agg <- rbind(saaq_agg[, c(agg_var_list, 'num')],
#                   no_tickets_df[, c(agg_var_list, 'num')])

# Second version without non-event correction.
saaq_agg_out <- rbind(saaq_agg[, c(agg_var_list, 'num'), with = FALSE],
                      driver_counts[, c(agg_var_list, 'num'), with = FALSE],
                      saaq_past_counts_sum[, c(agg_var_list, 'num'), with = FALSE])

# Optional version with non-event correction.
# saaq_agg_out <- rbind(saaq_agg[, c(agg_var_list, 'num'), with = FALSE],
#                       driver_counts[, c(agg_var_list, 'num'), with = FALSE],
#                       saaq_past_counts_sum[, c(agg_var_list, 'num'), with = FALSE])


colnames(saaq_agg_out)
nrow(saaq_agg_out)


summary(saaq_agg_out)


# saaq_agg_out <- saaq_agg_out[order(dinf, sex, age_grp, curr_pts_grp, points), ]
saaq_agg_out <- saaq_agg_out[order(dinf, sex, age_grp, curr_pts_grp, past_active, points), ]


head(saaq_agg_out, 50)
tail(saaq_agg_out, 50)




################################################################################
# Output Daily Driver Counts
################################################################################

# Current version has separate tag for aggregated file with
# new variable for past points indicator.
out_path_file_name <- sprintf('%s%s', data_out_path, out_file_name)
write.csv(x = saaq_agg_out, file = out_path_file_name, row.names = FALSE)






################################################################################
# End
################################################################################
