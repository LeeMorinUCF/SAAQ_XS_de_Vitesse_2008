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
# This version calculates point balances for each driver
# and maintains a driver-specific total number of days, 
# in each category, even for days without tickets. 
#
################################################################################


################################################################################
# Clearing Workspace and Declaring Packages
################################################################################

# Clear workspace, if running interactively.
rm(list=ls(all=TRUE))

# Load package for importing datasets in proprietary formats.
# library(foreign)

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

# Set name of file with records of tickets. 
tickets_in_file_name <- 'saaq_tickets.csv'


# The data of counts of licensed drivers are also stored in 'Data/'.
data_out_path <- 'Data'

# Set name of output file with additional variables appended to
# tickets dataset. 
# tickets_out_file_name <- 'saaq_tickets_balances.csv'


# Set name of output file for point totals.
# balance_out_file_name <- 'saaq_point_balances.csv'

# Set name of output file for both tickets and balances, 
# with separate observations by driver.
out_file_name <- 'saaq_tickets_balances_by_seq.csv'


################################################################################
# Set parameters for variable definitions.
################################################################################

#--------------------------------------------------------------------------------
# Set parameters related to dates
#--------------------------------------------------------------------------------

# Set date of policy change.
april_fools_2008 <- '2008-04-01'
# No joke: policy change on April Fool's Day!


# Restrict date range to focus on sample. 
sample_beg <- '2006-04-01'
# Need a two-year lead to calculate two-year point balances.
sample_lead <- '2004-04-01'
# But note that this will also require an additional blank event 
# at the first date of the sample, to properly count days
# from the beginning of the sample. 
sample_end <- '2010-03-31'


# Create rows for list of dates.
# day_1 <- as.numeric(as.Date('1998-01-01'))
# # day_T <- as.numeric(as.Date('2010-12-31'))
# day_T <- as.numeric(as.Date('2013-01-01')) # Allows for all points to expire.

# # Restrict date range to focus on sample. 
# day_1 <- as.numeric(as.Date(sample_beg))
# day_T <- as.numeric(as.Date(sample_end))
# date_list <- as.Date(seq(day_1, day_T), origin = as.Date('1970-01-01'))
# 
# length(date_list)
# min(date_list)
# max(date_list)


#--------------------------------------------------------------------------------
# Set parameters for age and point categories.
#--------------------------------------------------------------------------------

# Age categories. 
# Originals. 
# age_group_list <- c('0-15', '16-19', '20-24', '25-34', '35-44', '45-54',
#                     '55-64', '65-74', '75-84', '85-89', '90-199')
# age_cut_points <- c(0, 15.5, 19.5, seq(24.5, 84.5, by = 10), 89.5, 199)
# Consolidate some date ranges.
age_group_list <- c('0-19', '20-24', '25-34', '35-44', '45-54',
                    '55-64', '65-199')
age_cut_points <- c(0, 19.5, seq(24.5, 64.5, by = 10), 199)


# Point balance categories.
# curr_pts_grp_list <- c(seq(0,10), '11-20', '21-30', '30-150') # was 30, in error.
curr_pts_grp_list <- c(seq(0,10), '11-20', '21-30', '31-150')
curr_pts_cut_points <- c(seq(-0.5, 10.5, by = 1), 20.5, 30.5, 150)



# Create a list of active drivers before the policy change.
past_active_pts_list <- c('6', '7', '8', '9', '10')


################################################################################
# Load data from records of tickets.
################################################################################


# Start with the dataset of tickets. 
in_path_file_name <- sprintf('%s/%s', data_in_path, tickets_in_file_name)
saaq_point_hist <- fread(file = in_path_file_name)

colnames(saaq_point_hist)


# Create alternate name of date variable (since not all will be infractions).
saaq_point_hist[, date := dinf]
# colnames(saaq_point_hist)[6] <- 'date'
# The column "dinf" refers to the date of an infraction, 
# and is used to keep track of all events, both tickets and expiries.
# The column "date" is used to keep track of the counts of 
# drivers at each point balance level.

# Eventually (at the end of this script and in the modeling to follow),
# all events are time-stamped by the same "date" variable.



# Restrict sample to date range. 
# saaq_point_hist <- saaq_point_hist[date >= sample_beg & 
#                                      date <= sample_end, ]
saaq_point_hist <- saaq_point_hist[date >= sample_lead & 
                                     date <= sample_end, ]


#--------------------------------------------------------------------------------
# Convert categorical variables to factors. 
#--------------------------------------------------------------------------------

# Sex is first. 
saaq_point_hist[, sex := factor(sex, levels = c('M', 'F'))]
# table(saaq_point_hist[, 'sex'], useNA = 'ifany')
saaq_point_hist[, .N, by = 'sex']


# Now redefine age categories.
saaq_point_hist[, 'age_grp'] <- cut(saaq_point_hist[, age], breaks = age_cut_points,
                            labels = age_group_list)

saaq_point_hist[, age_grp := factor(age_grp, levels = age_group_list)]

# Verify definitions of age categories.
summary(saaq_point_hist[age_grp == '0-15', 'age'])
summary(saaq_point_hist[age_grp == '55-64', 'age'])
summary(saaq_point_hist[age_grp == '65-199', 'age'])


saaq_point_hist[, .N, by = 'age_grp']
# table(saaq_point_hist[, 'age_grp'], useNA = 'ifany')


# Drop unnecessary variables and reorder.
colnames(saaq_point_hist)
keep_col_names <- c("seq", "date", "dinf", "sex", "age_grp", "points" )
saaq_point_hist <- saaq_point_hist[, keep_col_names, with = FALSE]

summary(saaq_point_hist)



################################################################################
# Generate new variables to calculate violation history.
################################################################################


#--------------------------------------------------------------------------------
# Add events for removal of points after expiry two years later.
#--------------------------------------------------------------------------------


# Sort by date and seq.
saaq_point_hist <- saaq_point_hist[order(seq, date), ]
head(saaq_point_hist, 10)

# Create a data table to calculate cumulative points balances.
# Stack two copies of point events.
# One is the original, when points are added.
# The other is a copy, two years later, when points are removed.


# Calculate cumulative points total by driver.
# Copy the dataset to calculate a record of expiries of tickets.
saaq_point_hist_exp <- copy(saaq_point_hist)
# By default, data.table makes a shallow copy. 
# We need a deep copy, since we truly want a duplicate table
# but want to lead the dates 2 years and reverse the demerit points, 
# without changing the original with positive point values.


# Aggregate points over dates, since some drivers get multiple tickts per day.
saaq_point_hist_exp <- saaq_point_hist_exp[, points := sum(points), 
                                           by = c("seq", "date", "dinf", 
                                                  "sex", "age_grp")]
saaq_point_hist_exp <- unique(saaq_point_hist_exp)




# Translate into the drops in points two years later.
# Notice that this uses the "date" date variable, 
# which is used to keep track of drivers at different point balances.
saaq_point_hist_exp[, date := as.Date(date + 730)]
saaq_point_hist[, date := as.Date(date)] # Change original date to match class.

# Negative points two years later will subtract
# expiring points from two-year balance.
saaq_point_hist_exp[, points := - points]


# Verify accuracy of points. 
head(saaq_point_hist, 10)
head(saaq_point_hist_exp, 10)


# Append to the original observations, then sort.
saaq_point_hist <- rbind(saaq_point_hist, saaq_point_hist_exp)

saaq_point_hist <- saaq_point_hist[order(seq,
                         date,
                         points)]
head(saaq_point_hist, 10)

# Check for some drivers with multiple tickets in one day. 
saaq_point_hist[seq == 68306, ]
saaq_point_hist[seq == 847237, ]
saaq_point_hist[seq == 3526906, ]

# Verify that all points eventually expire.
saaq_point_hist[, sum(points)]


# Remove the duplicate, which is no longer needed after joining. 
rm(saaq_point_hist_exp)

summary(saaq_point_hist)


#--------------------------------------------------------------------------------
# Add blank events to hold balances the day after a ticket.
#--------------------------------------------------------------------------------

saaq_point_hist_bal <- copy(saaq_point_hist)

# Copy only the point events (not the expiries). 
saaq_point_hist_bal <- saaq_point_hist_bal[points > 0, ]

# Set date to the day after.
saaq_point_hist_bal[, date := as.Date(date + 1)]

# No points on this day (unless in another record).
saaq_point_hist_bal[, points := 0]

# Eliminate duplicate events from multiple tickets in one day.
saaq_point_hist_bal <- unique(saaq_point_hist_bal)

summary(saaq_point_hist_bal)



# Append to the original observations, then sort.
saaq_point_hist <- rbind(saaq_point_hist, saaq_point_hist_bal)

saaq_point_hist <- saaq_point_hist[order(seq,
                                         date,
                                         points)]
head(saaq_point_hist, 10)

# Check for some drivers with multiple tickets in one day. 
saaq_point_hist[seq == 68306, ]
saaq_point_hist[seq == 847237, ]
saaq_point_hist[seq == 3526906, ]

# Verify that all points eventually expire.
saaq_point_hist[, sum(points)]


# Remove the duplicate, which is no longer needed after joining. 
rm(saaq_point_hist_bal)


#--------------------------------------------------------------------------------
# Add blank events for checkpoints to calculate balances. 
# Main checkpoints are for sample beginning and end.
# Sample beginning includes two-year lead period for calculating initial balances.
# Also add a date to separate points before and after april_fools_2008. 
#--------------------------------------------------------------------------------

# colnames(saaq_point_hist)
saaq_point_hist_ends <- copy(saaq_point_hist)
# Blank events have no point changes.
saaq_point_hist_ends[, points := 0]

# Append blanks events at sample beginning.
saaq_point_hist_ends[, date := as.Date(sample_beg)]
saaq_point_hist_ends[, dinf := as.Date(sample_beg)]

saaq_point_hist_ends <- unique(saaq_point_hist_ends)

# Confirm one row per driver.
length(unique(saaq_point_hist[, seq])) == nrow(saaq_point_hist_ends)


# Append to the history.
saaq_point_hist <- rbind(saaq_point_hist, saaq_point_hist_ends)


# Append blanks events at beginning of two-year sample lead time.
saaq_point_hist_ends[, date := as.Date(sample_lead)]
saaq_point_hist_ends[, dinf := as.Date(sample_lead)]

# Append to the history.
saaq_point_hist <- rbind(saaq_point_hist, saaq_point_hist_ends)



# Append blanks events at the date of the policy change.
saaq_point_hist_ends[, date := as.Date(april_fools_2008)]
saaq_point_hist_ends[, dinf := as.Date(april_fools_2008)]

# Append to the history.
saaq_point_hist <- rbind(saaq_point_hist, saaq_point_hist_ends)


# Append blanks events at sample end.
saaq_point_hist_ends[, date := as.Date(sample_end)]
saaq_point_hist_ends[, dinf := as.Date(sample_end)]

# Append to the history.
saaq_point_hist <- rbind(saaq_point_hist, saaq_point_hist_ends)


# Sort complete history of events.
saaq_point_hist <- saaq_point_hist[order(seq,
                                         date,
                                         points)]

head(saaq_point_hist, 10)

# Check for some drivers with multiple tickets in one day. 
saaq_point_hist[seq == 68306, ]
saaq_point_hist[seq == 847237, ]
saaq_point_hist[seq == 3526906, ]

# Verify that all points eventually expire.
saaq_point_hist[, sum(points)]


# Remove the duplicate, which is no longer needed after joining. 
rm(saaq_point_hist_ends)

summary(saaq_point_hist)


#--------------------------------------------------------------------------------
# Generate counts of cumulative point balances.
#--------------------------------------------------------------------------------


# Calculate point balances.
saaq_point_hist[, curr_pts := cumsum(points)]
head(saaq_point_hist, 20)

# Check for some drivers with multiple tickets in one day. 
saaq_point_hist[seq == 68306, ]
saaq_point_hist[seq == 847237, ]
saaq_point_hist[seq == 3526906, ]


# Then drop the duplicate values.
# saaq_point_hist <- saaq_point_hist[points > 0, ]
# Not with this version: we don't need the total history, 
# only the two-year history, including the negative points.

# Then compare with saaq to verify accuracy.
# summary(saaq)
# summary(saaq_point_hist)
# summary(saaq_point_hist_exp)



# # In addition:
# # Calculate cumulative points total (entire history) by driver.
# saaq_point_hist[, cum_pts := cumsum(points)]
# # Need to lag cumulative points to remove past driver's total points.
# saaq_point_hist[, 'cum_pts_lag'] <- c(0, saaq_point_hist[-nrow(saaq_point_hist), cum_pts])
# head(saaq_point_hist, 20)
# tail(saaq_point_hist, 20)

# # Subtract lowest value for each driver to obtain
# # cumulative balance for each driver
# # (starting at zero for each driver).
# saaq_point_hist[, beg_pts := min(cum_pts_lag), by = seq]
# saaq_point_hist[, hist_pts := cum_pts - beg_pts]

# head(saaq_point_hist[, c('seq', 'date', 'points', 'cum_pts', 'beg_pts', 'hist_pts', 'curr_pts')], 20)
# summary(saaq_point_hist[, c('seq', 'date', 'points', 'cum_pts', 'beg_pts', 'hist_pts', 'curr_pts')])



# # Closer look at comparison of different point counts.
# head(saaq_point_hist[, c('seq', 'date', 'points', 'hist_pts', 'curr_pts')], 100)


# Look correct.
# The only remaining adjustment is to remove the current point
# so that the units represent past points history.
# saaq_point_hist[, hist_pts := hist_pts - points]
# saaq_point_hist[, curr_pts := curr_pts - points]

# For recording the transitions between point balance categories, 
# current points include today's ticket(s),
# previous points include all point changes up to midnight yesterday night. 
saaq_point_hist[, prev_pts := curr_pts - points]


# Check one last time.
# head(saaq_point_hist[, c('seq', 'date', 'points', 'hist_pts', 'curr_pts')], 100)
head(saaq_point_hist[, c('seq', 'date', 'points', 'curr_pts', 'prev_pts')], 100)

# Check for some drivers with multiple tickets in one day. 
saaq_point_hist[seq == 68306, ]
saaq_point_hist[seq == 847237, ]
saaq_point_hist[seq == 3526906, ]


#--------------------------------------------------------------------------------
# Calculate number of days at each point level.
#--------------------------------------------------------------------------------

# Create dates before and after.
# saaq_point_hist[!(date == sample_beg & points == 0), 
#                 last_date := shift(date, n = 1, type = 'lag')]
# saaq_point_hist[!(date == sample_end & points == 0), 
#                 next_date := shift(date, n = 1, type = 'lead')]
saaq_point_hist[, last_date := shift(date, n = 1, type = 'lag')]
# Start dates are easy to identify:
# a blank event on the start date. 
# saaq_point_hist[(date == sample_beg & points == 0),
#                 last_date := NA]
# Even better: check that the driver ID is the same.
saaq_point_hist[, last_seq := shift(seq, n = 1, type = 'lag')]
saaq_point_hist[seq != last_seq, last_date := NA]



head(saaq_point_hist[, c('seq', 'last_seq', 'date', 'last_date')], 20)


# Calculate date of next event (for same person). 
saaq_point_hist[, next_date := shift(date, n = 1, type = 'lead')]
# saaq_point_hist[(next_date == sample_beg & points <= 0) | 
#                   (date > sample_end & points <= 0),
#                 next_date := NA]
# saaq_point_hist[(next_date == sample_beg & points <= 0) | 
#                   (date >= sample_end & points <= 0),
#                 next_date := NA]
# The logic can get complicated for all the possible cases:
# ticket on last day, expiry on last day, 
# multiple events on last day, etc. 
# saaq_point_hist[next_date == sample_beg & points <= 0 & 
#                   date == sample_end,
#                 next_date := NA]
# saaq_point_hist[date >= sample_end & points <= 0,
#                 next_date := NA]

# Instead, check that the driver ID is the same.
saaq_point_hist[, next_seq := shift(seq, n = 1, type = 'lead')]
saaq_point_hist[seq != next_seq, next_date := NA]

head(saaq_point_hist[, c('seq', 'next_seq', 'date', 'next_date')], 20)


# Calculate number of days at zero points at the beginning of the sample. 
# saaq_point_hist[(date == sample_beg & points == 0), 
#                 num := as.integer(next_date - date)]

# Calculate number of days to the next event.
# saaq_point_hist[!(date == sample_end & points == 0), 
#                 num := as.integer(next_date - date)]
saaq_point_hist[, num := as.integer(next_date - date)]

# Any zero days on tickets correspond to multiple tickets in one day.
saaq_point_hist[points > 0, num := 1]
# Every ticket means that a new trial occurred.



head(saaq_point_hist[, c('seq', 'next_seq', 'date', 'next_date', 'points', 'num')], 20)

head(saaq_point_hist[, c('seq', 'next_seq', 'date', 'next_date', 
                         'points', 'curr_pts', 'num')], 20)


saaq_point_hist[seq == 847237, c('seq', 'next_seq', 'date', 'next_date', 
                                 'points', 'curr_pts', 'num')]

# Verify that all point events are classified as one day.
table(saaq_point_hist[points > 0, num], useNA = 'ifany')
# Any zeros correspond to multiple tickets in one day, 
# which were accounted for above.


# Check that duplicate tickets on sample_beg are not lost.
saaq_point_hist[points > 0 & is.na(num)]
saaq_point_hist[seq == 232526, c('seq', 'next_seq', 'date', 'next_date', 
                                 'points', 'curr_pts', 'num')]
# Pattern of point levels proceeds as expected. 

# Check that sum of days is a constant 6 years per driver.
as.Date(sample_lead) - as.Date(sample_end)
summary(saaq_point_hist[,  sum(num, na.rm = TRUE), by = 'seq'])
# Some drivers have balances that extend up to two years after
# the end of the sample: expiring demerit points. 
head(saaq_point_hist[,  sum(num, na.rm = TRUE), by = 'seq'])


# Extra days from the tickets that expire after the sample period.
summary(saaq_point_hist[date == sample_end,  sum(num, na.rm = TRUE), by = 'seq'])
# Up to two years, for those who got tickets on the last day of the sample. 


# Consider only those occurring during the sample period.
summary(saaq_point_hist[date < sample_end,  sum(num, na.rm = TRUE), by = 'seq'])
# One person got 26 tickets in one day. Check.





# Check drivers with a different number of days.
# saaq_point_hist[,  sum_num := sum(num, na.rm = TRUE), by = 'seq']
# saaq_point_hist[sum_num != 1460, c('seq', 'date', 'next_date', 'points', 'num', 'sum_num')]
# length(unique(saaq_point_hist[sum_num != 1460, seq]))
# [1] 31470
# ...which is the number of drivers who got multiple tickets in a day.
# This is ok because it is a reasonably defined trial. 




# Inspection after driving days calculated. 
summary(saaq_point_hist)




################################################################################
# Final sample selection
################################################################################

# Drop the negative points for expiries. 
# Keep only the tickets and the event placeholders.
saaq_point_hist <- saaq_point_hist[points >= 0, ]

summary(saaq_point_hist)


# Number of drivers. 
length(unique(saaq_point_hist[, seq]))
# Each one is missing the last date on the first observation. Check. 


saaq_point_hist[seq == 232526, c('seq', 'next_seq', 'date', 'next_date', 
                                 'points', 'curr_pts', 'num')]

# num is only missing on the observations missing the next_date:
# these are drivers with no demerit points by the end of the sample. 
# Drop the observations after the sample period. 
summary(saaq_point_hist[date == '2010-03-31' & points == 0, ])
# This accounts for all the missings. 
summary(saaq_point_hist[!(date == '2010-03-31' & points == 0), ])
nrow(saaq_point_hist[!(date == '2010-03-31' & points == 0), ])
# Drop these non-events, since the balances that are measured
# should not be included, since they go beyond the sample period. 


saaq_point_hist <- saaq_point_hist[!(date == '2010-03-31' & points == 0), ]


summary(saaq_point_hist)


#--------------------------------------------------------------------------------
# Categorization of point total balances
#--------------------------------------------------------------------------------

# Define current points as cumulative sum to date
# (in between tickets, on days without new points), 
# so keep as is. 

# Define current points as the point balance up to the moment a driver gets a ticket, 
# which is defined by prev_pts up to now. 
saaq_point_hist[points > 0, curr_pts := prev_pts]

# Categories:
# 0-10 separately, for granularity.
# 11-20 for next category.
# 21-30 for next category.
# 31+ for last category.

# Classify by current points group.
saaq_point_hist[, 'curr_pts_grp'] <- cut(saaq_point_hist[, curr_pts], 
                                         breaks = curr_pts_cut_points,
                                         labels = curr_pts_grp_list)

saaq_point_hist[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]

#--------------------------------------------------------------------------------
# Create an indicator for highest point category before policy change.
# Use it to determine if the bad guys change their habits.
#--------------------------------------------------------------------------------

saaq_point_hist[, pre_policy := date < as.Date(april_fools_2008)]
table(saaq_point_hist[, pre_policy], useNA = 'ifany')
# Pre-policy period is longer with an asymmetric window. 

# Create a list of active drivers before the policy change.
# With separate observations by date, balance category variables 
# are already defined appropriately for this classification.
past_active_list <- unique(saaq_point_hist[curr_pts_grp %in% past_active_pts_list &
                                             pre_policy == TRUE, seq])
# past_active_list <- unique(saaq_point_hist[prev_pts_grp %in% past_active_pts_list &
#                                              pre_policy == TRUE, seq])
# Note that this variable is not used for prediction.
# It is used for classifying a subsample.

# Allocate drivers to the list of those with an active past. 
saaq_point_hist[, past_active := seq %in% past_active_list]



length(unique(saaq_point_hist[, seq]))
# [1] 3369249 # Full sample.
# [1] 2421228 # Sample from 2004-2010.
length(past_active_list)
# [1] 795169 # Current
# [1] 723606 # Previous
# [1] 411228 # With selection by seq on 2004-2010.
# About a quarter of the sample of drivers.
# Good: Not too many. Not too few.

table(saaq_point_hist[, past_active], useNA = 'ifany')
# FALSE     TRUE 
# 10749201  4772372 


#--------------------------------------------------------------------------------
# Final variables selection
#--------------------------------------------------------------------------------


# Keep selected variables.
saaq_point_hist <- saaq_point_hist[, c('seq', 'date', 'dinf', 
                                       'sex', 'age_grp', 'past_active', 
                                       'curr_pts', 'curr_pts_grp', 
                                       'points', 'num'), with = FALSE]


summary(saaq_point_hist)


# # Still some NA's?
# summary(saaq_point_hist[is.na(num), ])
# 
# 
# # All were concentrated on the boundary dates:
# saaq_point_hist[is.na(num) & date == '2006-04-01', .N]
# saaq_point_hist[is.na(num) & date == '2010-03-31', .N]
# saaq_point_hist[is.na(num) & date == '2010-04-01', .N]
# # > 976+2055434+3580
# # [1] 2059990
# saaq_point_hist[is.na(num), .N]

# # Compare with number of distinct drivers. 
# length(unique(saaq_point_hist[, seq]))
# 
# saaq_point_hist[is.na(num) & date %in% c('2006-04-01', '2010-03-31', '2010-04-01'), .N]




################################################################################
# Output datasets
################################################################################


#--------------------------------------------------------------------------------
# Output dataset of tickets with balance variables.
#--------------------------------------------------------------------------------


out_path_file_name <- sprintf('%s/%s', data_out_path, out_file_name)
# write.csv(x = saaq_past_counts_sum, file = out_path_file_name, row.names = FALSE)
write.csv(x = saaq_point_hist, file = out_path_file_name, row.names = FALSE)






################################################################################
# End
################################################################################
