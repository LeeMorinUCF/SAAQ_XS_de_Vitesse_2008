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
# June 9, 2021
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
tickets_file_name <- 'saaq_tickets.csv'


# The data of counts of licensed drivers are also stored in 'Data/'.
data_out_path <- 'Data'

# Set name of file with counts of drivers without tickets.
# Driver population includes drivers with no past tickets or current points.
# no_tickets_file_name <- 'saaq_no_tickets.csv'


# Set name of output file for point totals.
out_file_name <- 'saaq_point_balances.csv'

# Set name of output file for aggregated dataset.
# agg_out_file_name <- 'saaq_agg.csv'




################################################################################
# Load data from records of tickets.
################################################################################



in_path_file_name <- sprintf('%s/%s', data_in_path, tickets_file_name)
# saaq_dt <- data.table(read.csv(file = in_path_file_name))
saaq_dt <- fread(file = in_path_file_name)

# colnames(saaq)
colnames(saaq_dt)






################################################################################
# Generate new variables for violation history.
################################################################################

# Create a new driver table to record points history.
# colnames(drivers)
# driver_hist <- drivers[, 'seq']

# Add a new variable for the date.
# length(unique(saaq[, 'dinf']))
# No. Would make too many permutations.

# Instead, make a table with dates as rows and columns as categories.


#--------------------------------------------------------------------------------
# Generate date index
#--------------------------------------------------------------------------------

# Create rows for list of dates.
day_1 <- as.numeric(as.Date('1998-01-01'))
day_T <- as.numeric(as.Date('2010-12-31'))
date_list <- as.Date(seq(day_1, day_T), origin = as.Date('1970-01-01'))

length(date_list)
min(date_list)
max(date_list)


#--------------------------------------------------------------------------------

# Create columns for the sex and age combinations.
# and point categories as well.

# colnames(saaq)
colnames(saaq_dt)
summary(saaq_dt)


# Sex and age variables listed as character: change to factors.
saaq_dt[, sex := factor(sex, levels = c('M', 'F'))]



age_group_list <- c('0-15', '16-19', '20-24', '25-34', '35-44', '45-54',
                    '55-64', '65-74', '75-84', '85-89', '90-199')
age_cut_points <- c(0, 15.5, 19.5, seq(24.5, 84.5, by = 10), 89.5, 199)


#--------------------------------------------------------------------------------
# To test, keep ages constant to limit state space. 
summary(saaq_dt[, age])
summary(saaq_dt[, sqrt(var(age)), by = c('seq')])
saaq_dt[, age := round(mean(age)), by = c('seq')]
summary(saaq_dt[, age])
summary(saaq_dt[, sqrt(var(age)), by = c('seq')])

# Now redefine age categories.
saaq_dt[, 'age_grp'] <- cut(saaq_dt[, age], breaks = age_cut_points,
                         labels = age_group_list)

# Verify definitions of age categories.
summary(saaq_dt[age_grp == '0-15', 'age'])

summary(saaq_dt[age_grp == '55-64', 'age'])

summary(saaq_dt[age_grp == '90-199', 'age'])

#--------------------------------------------------------------------------------


saaq_dt[, age_grp := factor(age_grp, levels = age_group_list)]


table(saaq_dt[, 'sex'], useNA = 'ifany')
table(saaq_dt[, 'age_grp'], useNA = 'ifany')


# table(saaq[, 'sex'], saaq[, 'age_grp'], useNA = 'ifany')
# table(saaq_dt[, 'sex'], saaq_dt[, 'age_grp'], useNA = 'ifany')


#--------------------------------------------------------------------------------
# Generate counts of cumulative point totals.
#--------------------------------------------------------------------------------

# Note that these are historic totals with no expiry date.

# Sort by date and seq.
# saaq <- saaq[order(saaq$seq, saaq$dinf), ]
# head(saaq, 10)
saaq_dt <- saaq_dt[order(seq, dinf), ]
head(saaq_dt, 10)

# Create a data table to calculate cumulative points balances.
# saaq_dt <- data.table(saaq)
# Join with leading dataset with negative points balances.
# Already read in as a data table.

# Stack two copies of point events.
# One is the original, when points are added.
# The other is a copy, two years later, when points are removed.


# Calculate cumulative points total by driver.
# saaq_dt <- data.table(saaq)
saaq_dt_2 <- copy(saaq_dt)
# By default, data.table makes a shallow copy. 
# We need a deep copy, since we truly want a duplicate table
# but want to lead the dates 2 years and reverse the demerit points.


# Translate into the drops in points two years later.
saaq_dt_2[, dinf := as.Date(dinf + 730)]
saaq_dt[, dinf := as.Date(dinf)] # Change original date to match class.

# Only if age category not fixed.
# saaq_dt_2[, age := age + 2]
# summary(saaq_dt_2[, sqrt(var(age)), by = c('seq')])

saaq_dt_2[, points := - points]
head(saaq_dt_2, 10)
head(saaq_dt, 10)


# Append the original observations, then sort.
saaq_dt <- rbind(saaq_dt_2, saaq_dt)

# saaq_dt <- saaq_dt[order(saaq_dt$seq,
#                          saaq_dt$dinf,
#                          saaq_dt$points)]
saaq_dt <- saaq_dt[order(seq,
                         dinf,
                         points)]
head(saaq_dt, 10)



# Calculate point balances.
saaq_dt[, curr_pts := cumsum(points)]
head(saaq_dt, 20)


# Then drop the duplicate values.
saaq_dt <- saaq_dt[points > 0, ]


# Then compare with saaq to verify accuracy.
# summary(saaq)
summary(saaq_dt)
summary(saaq_dt_2)



# In addition:
# Calculate cumulative points total (entire history) by driver.
saaq_dt[, cum_pts := cumsum(points)]
# Need to lag cumulative points to remove past driver's total points.
saaq_dt[, 'cum_pts_lag'] <- c(0, saaq_dt[-nrow(saaq_dt), cum_pts])
head(saaq_dt, 20)
tail(saaq_dt, 20)

# Subtract lowest value for each driver to obtain
# cumulative balance for each driver
# (starting at zero for each driver).
saaq_dt[, beg_pts := min(cum_pts_lag), by = seq]
saaq_dt[, hist_pts := cum_pts - beg_pts]

head(saaq_dt[, c('seq', 'dinf', 'points', 'cum_pts', 'beg_pts', 'hist_pts', 'curr_pts')], 20)
summary(saaq_dt[, c('seq', 'dinf', 'points', 'cum_pts', 'beg_pts', 'hist_pts', 'curr_pts')])



# Closer look at comparison of different point counts.
head(saaq_dt[, c('seq', 'dinf', 'points', 'hist_pts', 'curr_pts')], 100)


# Look correct.
# The only remaining adjustment is to remove the current point
# so that the units represent past points history.
saaq_dt[, hist_pts := hist_pts - points]
saaq_dt[, curr_pts := curr_pts - points]


# Check one last time.
head(saaq_dt[, c('seq', 'dinf', 'points', 'hist_pts', 'curr_pts')], 100)



#--------------------------------------------------------------------------------
# Categorization of point total balances
#--------------------------------------------------------------------------------

# Categories:
# 0-10 separately, for granularity.
# 11-20 for next category.
# 21-30 for next category.
# 31+ for last category.

# saaq_past_pts[, curr_pts_grp := as.factor(NA, levels = c(seq(0,10), '11-20', '21-30', '31-150'))]
saaq_dt[, curr_pts_grp := '-99']
head(saaq_dt, 20)
saaq_dt[curr_pts <= 10, curr_pts_grp := as.character(curr_pts)]
saaq_dt[curr_pts > 10 & curr_pts <= 20,
              curr_pts_grp := '11-20']
saaq_dt[curr_pts > 20 & curr_pts <= 30,
              curr_pts_grp := '21-30']
saaq_dt[curr_pts > 30,
              curr_pts_grp := '30-150']

# Change type to factor.
saaq_dt[, curr_pts_grp := as.factor(curr_pts_grp)]

table(saaq_dt[, curr_pts_grp])


#--------------------------------------------------------------------------------
# Create an indicator for highest point category before policy change.
# Use it to determine if the bad guys change their habits.
#--------------------------------------------------------------------------------

# First run the script to check everything without.
# Check. Now include the extra category.
colnames(saaq_dt)
saaq_dt[, pre_policy := dinf < as.Date('2008-04-01')]

# Create a list of active drivers before the policy change.
table(saaq_dt[, curr_pts_grp], useNA = 'ifany')
past_active_pts_list <- c('6', '7', '8', '9', '10')
past_active_list <- unique(saaq_dt[curr_pts_grp %in% past_active_pts_list &
                                     pre_policy == TRUE, seq])

length(past_active_list)

saaq_dt[, past_active := seq %in% past_active_list]

table(saaq_dt[, past_active], useNA = 'ifany')
# About a third of the sample.
# Good: Not too many. Not too few.


#--------------------------------------------------------------------------------
# Analysis of point total balances
#--------------------------------------------------------------------------------

# hist(saaq_dt[, 'past_pts'])
# Fail. But anyway, this is the missing column.
# plot(saaq_dt[, 'past_pts'])
# Too many to plot practically.

# # List the values instead.
# past_pts_list <- unique(saaq_dt[, past_pts])
# past_pts_list <- past_pts_list[order(past_pts_list)]
# # Every number up to 162 occurs, then 164, 165, 167 and 168.
#
#
# # Instead, sort the past points into past points categories.
# # List the quantiles to choose the categories.
# quantile(saaq_dt[, past_pts], probs = seq(0, 1, by = 0.1))
# # 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
# # 0    0    0    0    2    3    5    8   11   18  168
# quantile(saaq_dt[, past_pts], probs = seq(0.3, 1, by = 0.05))
# # 30%  35%  40%  45%  50%  55%  60%  65%  70%  75%  80%  85%  90%  95% 100%
# # 0    2    2    3    3    4    5    6    8    9   11   14   18   25  168
# quantile(saaq_dt[, past_pts], probs = seq(0.8, 1, by = 0.01))
# # 80%  81%  82%  83%  84%  85%  86%  87%  88%  89%  90%  91%  92%  93%  94%  95%  96%
# # 11   12   12   13   13   14   15   15   16   17   18   19   20   22   23   25   27
# # 97%  98%  99% 100%
# #   30   35   43  168
#
# # Integers 0 to 10 get us to 80%.
# # Next increments of 5 get us the next two vigintiles.
# #
#
# # Problem: counts will keep growing over time.
# # Last half of dataset will have the highest point balances.


#
# #--------------------------------------------------------------------------------
# # Generate counts of cumulative two-year point totals.
# #--------------------------------------------------------------------------------
#
# # Keep above saaq_dt table with ticket events.
#
# # # Repeat the calculation with two-year balances.
# # saaq_dt <- NULL
# #
# # # Sort by date and seq.
# # saaq <- saaq[order(saaq$seq, saaq$dinf), ]
# # head(saaq, 10)
#
#
# # Stack two copies of point events.
# # One is the original, when points are added.
# # The other is a copy, two years later, when points are removed.
#
#

# Calculate cumulative points total by driver.
# saaq_past_pts <- data.table(saaq[, c('seq', 'sex', 'age', 'dinf', 'points')])
# saaq_past_pts <- copy(saaq_dt[, c('seq', 'sex', 'age', 'dinf', 'points')])
saaq_past_pts <- copy(saaq_dt[, c('seq', 'sex', 'age', 'dinf', 'points', 'past_active')])
# Translate into the drops in points two years later.
saaq_past_pts[, dinf := as.Date(dinf + 730)]
# saaq_past_pts[, age := age + 2] #not with fixed age categories.
saaq_past_pts[, points := - points]
head(saaq_past_pts, 10)
# Append the original observations, then sort.
# saaq_past_pts <- rbind(saaq_past_pts,
#                        data.table(saaq[, c('seq', 'sex', 'age', 'dinf', 'points')]))
# saaq_past_pts <- rbind(saaq_past_pts,
#                        saaq_dt[, c('seq', 'sex', 'age', 'dinf', 'points')])
saaq_past_pts <- rbind(saaq_past_pts,
                       saaq_dt[, c('seq', 'sex', 'age', 'dinf', 'points', 'past_active')])
saaq_past_pts <- saaq_past_pts[order(saaq_past_pts$seq,
                                     saaq_past_pts$dinf,
                                     saaq_past_pts$points)]
head(saaq_past_pts, 10)



# Calculate point balances.
saaq_past_pts[, curr_pts := cumsum(points)]
head(saaq_past_pts, 100)

# Calculate previous point balance category (to model transitions).
saaq_past_pts[, prev_pts := curr_pts - points]
head(saaq_past_pts, 100)



# Not necessary to reset to zero for each individual,
# once points are removed later.
# saaq_past_pts[, beg_pts := min(cum_pts), by = seq]
# saaq_past_pts[, past_pts := cum_pts - beg_pts]

# The only remaining adjustment is to remove the current point
# so that the units represent past points history.
# saaq_past_pts[points > 0, curr_pts := curr_pts - points]
# head(saaq_past_pts, 100)

# No. This is for defining the rest of the population, not the ticket-getters.
# These figures should be lagged one day, instead.

saaq_past_pts[points > 0, dinf := as.Date(dinf + 1)]

summary(saaq_past_pts)




#--------------------------------------------------------------------------------
# Analysis of two-year point total balances
#--------------------------------------------------------------------------------


# List the possible values.
past_pts_list <- unique(saaq_past_pts[, curr_pts])
past_pts_list <- past_pts_list[order(past_pts_list)]
# Every number up to 110, then more sparse up to 150.

# Inspect the distribution to choose categories.
quantile(saaq_past_pts[, curr_pts], probs = seq(0, 1, by = 0.1))
# 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
# 0    0    0    1    2    3    3    4    6    8  150

quantile(saaq_past_pts[, curr_pts], probs = seq(0.9, 1, by = 0.01))
# 90%  91%  92%  93%  94%  95%  96%  97%  98%  99% 100%
# 8    9    9   10   10   11   12   13   15   18  150

quantile(saaq_past_pts[, curr_pts], probs = seq(0.99, 1, by = 0.001))
# 99% 99.1% 99.2% 99.3% 99.4% 99.5% 99.6% 99.7% 99.8% 99.9%  100%
# 18    18    19    20    21    22    23    25    27    32   150


# 0-10 gets up to the 95 percentile.
# 15 gets to 98th percentile.
# 20 gets inside 99th percentile.
# 30 gets to 99.9%.



#--------------------------------------------------------------------------------
# Categorization of point total balances
#--------------------------------------------------------------------------------

# Categories:
# 0-10 separately, for granularity.
# 11-20 for next category.
# 21-30 for next category.
# 31+ for last category.

# saaq_past_pts[, curr_pts_grp := as.factor(NA, levels = c(seq(0,10), '11-20', '21-30', '31-150'))]
saaq_past_pts[, curr_pts_grp := '-99']
head(saaq_past_pts, 20)
saaq_past_pts[curr_pts <= 10, curr_pts_grp := as.character(curr_pts)]
saaq_past_pts[curr_pts > 10 & curr_pts <= 20,
              curr_pts_grp := '11-20']
saaq_past_pts[curr_pts > 20 & curr_pts <= 30,
              curr_pts_grp := '21-30']
saaq_past_pts[curr_pts > 30,
              curr_pts_grp := '30-150']


table(saaq_past_pts[, curr_pts_grp], useNA = 'ifany')

curr_pts_grp_list <- c(as.character(seq(0, 10)), '11-20', '21-30', '30-150')


# Repeat for previous points group.
saaq_past_pts[, prev_pts_grp := '-99']
head(saaq_past_pts, 20)
saaq_past_pts[prev_pts <= 10, prev_pts_grp := as.character(prev_pts)]
saaq_past_pts[prev_pts > 10 & prev_pts <= 20,
              prev_pts_grp := '11-20']
saaq_past_pts[prev_pts > 20 & prev_pts <= 30,
              prev_pts_grp := '21-30']
saaq_past_pts[prev_pts > 30,
              prev_pts_grp := '30-150']

table(saaq_past_pts[, prev_pts_grp], useNA = 'ifany')


#--------------------------------------------------------------------------------
# Categorization of age groups
#--------------------------------------------------------------------------------


# Now calculate age groups as before.

# Already defined.
# age_group_list <- c('0-15', '16-19', '20-24', '25-34', '35-44', '45-54',
#                     '55-64', '65-74', '75-84', '85-89', '90-199')
# age_cut_points <- c(0, 15.5, 19.5, seq(24.5, 84.5, by = 10), 89.5, 199)
# saaq[, 'age_grp'] <- factor(levels = age_group_list)


saaq_past_pts[, age_grp := cut(age, breaks = age_cut_points,
                         labels = age_group_list)]

summary(saaq_past_pts[age_grp == '0-15', age])
summary(saaq_past_pts[age_grp == '55-64', age])
summary(saaq_past_pts[age_grp == '90-199', age])

head(saaq_past_pts, 20)


# Verify that categories are static (takes too long).
# table(saaq_past_pts[, min(past_active), by = c('seq')],
#       saaq_past_pts[, max(past_active), by = c('seq')], useNA = 'ifany')



# Change name of date variable (since not all are infractions).
saaq_past_pts[, date := dinf]


# Now this dataset can be used to calculate counts by category.
# Not quite! Some drivers have several tickets in one day.
# These drivers are counted several times over. 
# Need to select current and previous category for each driver-day. 

# Add indicator for event. 
colnames(saaq_past_pts)
saaq_past_pts[, 'event_id'] <- seq(nrow(saaq_past_pts))

# Each driver-day, record the first id and last id.
saaq_past_pts[, first_id := min(event_id), by = list(seq, date)]
saaq_past_pts[, last_id := max(event_id), by = list(seq, date)]

# Create true full-day transition from first balance
# category to last balance category. 
saaq_past_pts[event_id == first_id, prev_pts_trim := prev_pts]
saaq_past_pts[event_id == last_id, curr_pts_trim := curr_pts]

# Assign full-day balance categories.
saaq_past_pts[, curr_pts_trim := mean(curr_pts_trim, na.rm = TRUE), by = list(seq, date)]
saaq_past_pts[, prev_pts_trim := mean(prev_pts_trim, na.rm = TRUE), by = list(seq, date)]

# Now calculate the corresponding points groups.
saaq_past_pts[, curr_pts_grp_trim := '-99']
saaq_past_pts[curr_pts_trim <= 10, curr_pts_grp_trim := as.character(curr_pts_trim)]
saaq_past_pts[curr_pts_trim > 10 & curr_pts_trim <= 20,
              curr_pts_grp_trim := '11-20']
saaq_past_pts[curr_pts_trim > 20 & curr_pts_trim <= 30,
              curr_pts_grp_trim := '21-30']
saaq_past_pts[curr_pts_trim > 30,
              curr_pts_grp_trim := '30-150']

saaq_past_pts[, prev_pts_grp_trim := '-99']
saaq_past_pts[prev_pts_trim <= 10, prev_pts_grp_trim := as.character(prev_pts_trim)]
saaq_past_pts[prev_pts_trim > 10 & prev_pts_trim <= 20,
              prev_pts_grp_trim := '11-20']
saaq_past_pts[prev_pts_trim > 20 & prev_pts_trim <= 30,
              prev_pts_grp_trim := '21-30']
saaq_past_pts[prev_pts_trim > 30,
              prev_pts_grp_trim := '30-150']


head(saaq_past_pts)
tail(saaq_past_pts)


# Verify that it is working for the drivers with multiple tickets
# in one day.
saaq_past_pts[seq == 68306, ]
saaq_past_pts[seq == 847237, ]
saaq_past_pts[seq == 3526906, ]


# Now this dataset can be used to calculate counts by category.


# Create an aggregated version to streamline counting.
# saaq_pts_chgs <- saaq_past_pts[, .N, by = c('date', 'sex', 'age_grp', 'past_active', # Fixed categories.
#                                             'prev_pts_grp', 'curr_pts_grp')]         # Transition category. 
# Eliminate all but the first record per driver day. 
saaq_pts_chgs <- saaq_past_pts[event_id == first_id, .N, 
                               by = c('date', 'sex', 'age_grp', 'past_active', # Fixed categories.
                                      'prev_pts_grp_trim', 'curr_pts_grp_trim')]   # Transition category. 


# Replace the column name as if drivers only got one ticket per day. 
colnames(saaq_pts_chgs) <- c('date', 'sex', 'age_grp', 'past_active', 
                             'prev_pts_grp', 'curr_pts_grp', 'N')


head(saaq_pts_chgs, 20)

saaq_pts_chgs <- saaq_pts_chgs[order(date, sex, age_grp, past_active, 
                                     prev_pts_grp, curr_pts_grp)]

summary(saaq_pts_chgs)

#--------------------------------------------------------------------------------
# Daily categorization of point total balances across age and sex categories
#--------------------------------------------------------------------------------

# This creates a time series of counts by point-age-sex categories.


################################################################################
################################################################################
# New version with daily balances updated with new events.
################################################################################
################################################################################


# Start with a dataset of all possible permutations of the categories, each day.
# Initialize with zeros for the combinations that didn't happen.
saaq_past_counts <- data.table(expand.grid(date = date_list,
                                           sex = c('M', 'F'),
                                           age_grp = age_group_list,
                                           past_active = c(FALSE, TRUE), 
                                           curr_pts_grp = curr_pts_grp_list,
                                           N = 0L))
saaq_past_counts <- saaq_past_counts[order(date, sex, age_grp, past_active, curr_pts_grp)]


# Create a single-day list for padding.
saaq_zero_curr <- data.table(expand.grid(sex = c('M', 'F'),
                                         age_grp = age_group_list,
                                         past_active = c(FALSE, TRUE), 
                                         curr_pts_grp = curr_pts_grp_list,
                                         N = 0L))
saaq_zero_curr <- saaq_zero_curr[order(sex, age_grp, past_active, curr_pts_grp)]
# Create another for padding deductions from previous counts.
saaq_zero_prev <- saaq_zero_curr
colnames(saaq_zero_prev)[4] <- 'prev_pts_grp'


# Set date range.
# The range 2004-2010 is sufficient for a dataset covering
# a two-year window around 2008 and two years of leading data 
# for initial points balances.
# beg_date <- '2004-01-01'
# Now that it is efficient, we can start from the beginning, 
# leaving one day for the lagged counts.
beg_date <- date_list[2]
beg_date_num <- which(date_list == beg_date)
# end_date <- '2004-12-31' # Test with 1-year sample.
end_date <- '2010-12-31'
end_date_num <- which(date_list == end_date)
# Note that this leaves many dates at zero, outside of the range. 
date_num_list <- beg_date_num:end_date_num



# Loop on dates and calculate the totals.
# date_num <- beg_date_num
# date_num <- date_num + 1
for (date_num in date_num_list) {
  
  # Select row for current date.
  date_curr <- date_list[date_num]
  if (date_num == date_num_list[1]) {
    date_prev <- NULL
  } else {
    date_prev <- date_list[date_num - 1]
  }
  
  # Print progress report.
  if (wday(date_curr) == 1) {
    print(sprintf('Now tabulating for date %s.', as.character(date_curr)))
  }
  
  # Pull all point changes on this date. 
  point_changes <- saaq_pts_chgs[date == date_curr, ]
  # Only need drivers who actually changed categories.
  # Some stay in 11-20, 21-30, or 31-150.
  point_changes <- point_changes[curr_pts_grp != prev_pts_grp]
  
  # Deduct from count of drivers in the previous categories.
  
  deduct_counts <- point_changes[, c('sex', 'age_grp', 'past_active', 
                                     'prev_pts_grp', 'N')]
  
  # Pad list of changes with zero entries.
  deduct_counts <- rbind(deduct_counts, saaq_zero_prev)
  
  # Aggregate and sort.
  deduct_counts <- deduct_counts[, N := sum(N), 
                                 by = c('sex', 'age_grp', 'past_active', 
                                        'prev_pts_grp')]
  deduct_counts <- unique(deduct_counts)
  deduct_counts <- deduct_counts[order(sex, age_grp, past_active, prev_pts_grp)]
  
  # Now this list should be in the same order as the date selection.
  
  # if (date_num > date_num_list[1] | beg_date_num == 2) {
  if (date_num == date_num_list[1]) {
    saaq_past_counts[date == date_curr, 'N'] <- - deduct_counts[, 'N']
  } else {
    saaq_past_counts[date == date_curr, 'N'] <- 
      saaq_past_counts[date == date_prev, 'N'] - 
      deduct_counts[, 'N']
  }
  
  
  # Add to count of drivers in the current categories.
  add_counts <- point_changes[, c('sex', 'age_grp', 'past_active', 
                                     'curr_pts_grp', 'N')]
  
  # Pad list of changes with zero entries.
  add_counts <- rbind(add_counts, saaq_zero_curr)
  
  # Aggregate and sort.
  add_counts <- add_counts[, N := sum(N), 
                                 by = c('sex', 'age_grp', 'past_active', 
                                        'curr_pts_grp')]
  add_counts <- unique(add_counts)
  add_counts <- add_counts[order(sex, age_grp, past_active, curr_pts_grp)]
  
  # Now this list should be in the same order as the date selection.
  saaq_past_counts[date == date_curr, 'N'] <- 
    saaq_past_counts[date == date_curr, 'N'] + 
    add_counts[, 'N']
  

}

# Sum on each date should be zero.
# Drivers all swap in from zero points and swap to positive points.
saaq_past_counts[, sum(N), by = c('date')] 
summary(saaq_past_counts[, sum(N), by = c('date')])

summary(saaq_past_counts[N > 0, sum(N), by = c('date')])
# Check the total for day 1:
saaq_pts_chgs[date == date_list[beg_date_num], ]
saaq_pts_chgs[date == date_list[beg_date_num], sum(N)]
# Sum is 146, half of 292.
# Was missing a minus sign. Fixed. 

# Check the first day.
saaq_past_counts[date == date_list[beg_date_num], ]


head(saaq_past_counts[N > 0, sum(N), by = c('date')])
tail(saaq_past_counts[N > 0, sum(N), by = c('date')])
saaq_past_counts[N > 0, sum(N), by = c('date')] 
# Last date:
# 4747: 2010-12-31 3065987

# Compare to the counts of drivers.
length(unique(saaq_dt[, seq]))
# [1] 3369249

# # First check:
# # 3369249 (from the original data table) vs 3600227 (from the counts)
# 3369249 - 3600227
# # -230978
# # This doesn't match.
# # This suggests that some 230978 are being added.
# # Compare to the list of points. 
# length(unique(saaq_past_pts[, seq]))
# # 3369249 (which matches the original data table)

# Second check:
# 3369249 (from the original data table) vs 3065987 (from the counts)
3369249 - 3065987
# [1] 303262
# This doesn't match.
# This suggests that some 303262 are getting lost.
# Compare to the list of points. 
length(unique(saaq_past_pts[, seq]))
# 3369249 (which matches the original data table)
# [1] 3369249


# Drivers then move around from there (possibly back to zero). 
# Total positive population should be the count of drivers who have received
# any ticket up to that date. 
colnames(saaq_past_counts)
colnames(point_changes)
colnames(saaq_past_pts)
first_ticket <- saaq_past_pts[, min(date), by = seq]
summary(first_ticket)
driver_count <- table(first_ticket[, V1])
summary(driver_count)
head(driver_count)
plot(driver_count)
plot(cumsum(driver_count))

driver_count_2 <- saaq_past_counts[N > 0 & curr_pts_grp != 0, 
                                   sum(N), by = list(date)]
head(driver_count_2, 100)
plot(driver_count_2[, V1])


head(cumsum(driver_count), 10)
head(driver_count_2, 10)
tail(cumsum(driver_count), 10)
tail(driver_count_2, 10)
# Looks like the count is missing some drivers.
# But at least it matches up to the points table. 

count_diff <- cumsum(driver_count) - c(0, driver_count_2[, V1])


head(count_diff)
tail(count_diff)
plot(count_diff)
summary(count_diff)
# The accumulated difference is quite large
# and it grows over the sample--first up, then down. 





# Troubleshooting surprise negative counts.
summary(saaq_past_counts[date == date_curr, 'N'])
print(saaq_past_counts[date == date_curr & 
                         N != 0, ])
print(saaq_past_counts[date == date_curr & 
                         N < 0, ])
print(saaq_past_counts[date == date_curr & 
                         N != 0 & 
                         curr_pts_grp == 0, ])
print(saaq_past_counts[date == date_curr & 
                         N < 0 & 
                         curr_pts_grp != 0, ])
# 146-149 non-zero categories with negative counts.
summary(saaq_past_counts[date == date_curr & 
                         N < 0 & 
                         curr_pts_grp != 0, ])
# From -1 to -17230.


# Check for negative balances on particular days. 
# All zero on day 1 (from lag).
# All good on day 2.
# date_num_sel <- 3 # One negative from 3 to 7.
# The driver got two tickets in one day.
# None on day 4 and 5; other drivers swapped in. 
date_num_sel <- 5
date_sel <- date_list[date_num_sel]
summary(saaq_past_counts[date == date_sel, ])
print(saaq_past_counts[date == date_sel & 
                         N < 0, ])
print(saaq_past_counts[date == date_sel & 
                         N != 0 & 
                         curr_pts_grp == 0, ])
print(saaq_past_counts[date == date_sel & 
                         N < 0 & 
                         curr_pts_grp != 0, ])



# How did this happen on day 3?
print(saaq_past_counts[date == date_list[3] & 
                         N < 0 & 
                         curr_pts_grp != 0, ])

# Must be the 3 to 7 guy:
saaq_pts_chgs[date == date_list[3] & 
                prev_pts_grp != 0, ]

colnames(saaq_dt)
# saaq_dt[dinf == date_list[3] & points == 4 & curr_pts == 7, ]
colnames(saaq_past_pts)
saaq_past_pts[date == date_list[3] & 
                prev_pts_grp == 3 & 
                curr_pts_grp == 7, ]
# seq      sex age       dinf points past_active curr_pts prev_pts curr_pts_grp prev_pts_grp age_grp       date
# 1: 68306   M  39 1998-01-03      4        TRUE        7        3            7            3   35-44 1998-01-03

# Check his record.
saaq_dt[seq == 68306, ]
# Aha! The driver got two tickets in one day.

# How often does this happen?
driver_day_num_tickets <- saaq_dt[, .N, by = list(seq, dinf)]
summary(driver_day_num_tickets)
summary(driver_day_num_tickets[N > 1, ])
summary(driver_day_num_tickets[, N > 1])
driver_day_num_tickets[N > 1, sum(N)]
# Mostly two tickets in a day. 
# One driver got 26 tickets in one day:
driver_day_num_tickets[N > 25, ]
saaq_dt[seq == 847237, ]
# Confirmed. 

# A few got 16-18 tickets. 
driver_day_num_tickets[N > 15, ]
saaq_dt[seq == 3526906, ]
# Confirmed. 

# Check these in the individual record of transitions.
saaq_past_pts[seq == 68306, ]
saaq_past_pts[seq == 847237, ]
saaq_past_pts[seq == 3526906, ]
saaq_past_counts[date == '1999-07-31' & curr_pts_grp == '30-150', ]
saaq_past_counts[date == '1999-08-01' & curr_pts_grp == '30-150', ]
# The 47-point day does not show up.
# They are lost somewhere in between. 

saaq_pts_chgs[date == '1999-07-31' & curr_pts_grp == '30-150', ]
saaq_pts_chgs[date == '1999-07-31' & 
                sex == 'M' & age_grp == '20-24' & past_active == TRUE, ]



# # Evaluate the counts.
# summary(saaq_past_counts[date >= date_list[beg_date_num] & 
#                            date <= date_list[end_date_num], ])
# # Notice that this does not include the drivers with no tickets.
# # Negative counts indicate drivers who have swapped in by getting a ticket.
# 
# saaq_past_counts[date == date_curr, ]
# head(saaq_past_counts[date == date_curr, ], 100)
# 
# # Note that no drivers are recorded outside the date range.
# summary(saaq_past_counts[date < date_list[beg_date_num], ])
# summary(saaq_past_counts[date > date_list[end_date_num], ])


# Check counts of drivers by point balances.
table(saaq_past_counts[, curr_pts_grp])
summary(saaq_past_counts[curr_pts_grp == 0, N])
summary(saaq_past_counts[curr_pts_grp == 1, N])
summary(saaq_past_counts[curr_pts_grp == 2, N])
summary(saaq_past_counts[curr_pts_grp == 3, N])
summary(saaq_past_counts[curr_pts_grp == 4, N])
summary(saaq_past_counts[curr_pts_grp == 5, N])
summary(saaq_past_counts[curr_pts_grp == 6, N])
summary(saaq_past_counts[curr_pts_grp == 7, N])
summary(saaq_past_counts[curr_pts_grp == 8, N])
summary(saaq_past_counts[curr_pts_grp == 9, N])
summary(saaq_past_counts[curr_pts_grp == 10, N])
summary(saaq_past_counts[curr_pts_grp == '11-20', N])
summary(saaq_past_counts[curr_pts_grp == '21-30', N])
summary(saaq_past_counts[curr_pts_grp == '30-150', N])



# 


################################################################################
################################################################################
# Initial version with daily juggling of the dataset
################################################################################
################################################################################

# #--------------------------------------------------------------------------------
# # Daily categorization of point total balances across age and sex categories
# #--------------------------------------------------------------------------------
# 
# # This creates a time series of counts by point-age-sex categories.
# 
# 
# # Check maximum number of rows for each day.
# # nrow(expand.grid(sex = c('M', 'F'),
# #                  age_grp = age_group_list,
# #                  curr_pts_grp = curr_pts_grp_list))
# 
# 
# 
# # Start with a dataset of all possible permutations of the categories, each day.
# saaq_past_counts <- data.table(expand.grid(date = date_list,
#                                            sex = c('M', 'F'),
#                                            age_grp = age_group_list,
#                                            curr_pts_grp = curr_pts_grp_list))
# # Only a million rows or so.
# # Initialize with zeros for the combinations that didn't happen.
# saaq_past_counts[, N := -99L]
# last_row <- 0
# 
# # Initialize a data table to store the counts.
# # saaq_past_counts <- NULL
# past_counts <- NULL
# 
# # Set date range.
# # The range 2004-2010 is sufficient for a dataset covering
# # a two-year window around 2008 and two years of leading data 
# # for initial points balances.
# beg_date <- '2004-01-01'
# beg_date_num <- which(date_list == beg_date)
# end_date <- '2010-12-31'
# # end_date <- '2004-12-31' #Test with 1-year sample.
# end_date_num <- which(date_list == end_date)
# 
# # Loop on dates and calculate the totals.
# # date_num <- 2
# # date_num_list <- 2:length(date_list)
# # date_num_list <- 2:100
# date_num_list <- beg_date_num:end_date_num
# 
# 
# 
# 
# for (date_num in date_num_list) {
# 
#   # Select up to previous date.
#   date_count <- date_list[date_num]
#   date_last <- date_list[date_num - 1]
# 
#   # Print progress report.
#   if (TRUE | (wday(date_count) == 1)) {
#     print(sprintf('Now tabulating for date %s.', as.character(date_count)))
#   }
# 
#   # Each month, pull a subset of the data for easier daily pulls.
#   if (date_num == date_num_list[1] | mday(date_count) == 1) {
# 
#     print(sprintf('Resetting subset of data for date %s...', as.character(date_count)))
# 
#     saaq_past_pts_sub <- saaq_past_pts[year(dinf) == year(date_count) &
#                                          month(dinf) == month(date_count) |
#                                          year(dinf) == year(date_last) &
#                                          month(dinf) == month(date_last),
#                                        c('dinf', 'seq', 'sex', 'age_grp', 'curr_pts_grp')]
# 
#     print(sprintf('Finished resetting subset of data for date %s.', as.character(date_count)))
# 
#   }
# 
#   # Obtain most recent point blance for each driver (those who got a ticket).
#   # Keep the most recent and append any new observations.
#   # past_counts <- rbind(past_counts,
#   #                      saaq_past_pts[dinf == date_last,
#   #                                    c('dinf', 'seq', 'sex', 'age_grp', 'curr_pts_grp')])
#   # Pull from subset for efficiency.
#   past_counts <- rbind(past_counts[,
#                                    c('dinf', 'seq', 'sex', 'age_grp', 'curr_pts_grp')],
#                        saaq_past_pts_sub[dinf == date_last, ])
# 
# 
#   # Obtain the last date for each driver.
#   past_counts[, most_recent_date := max(dinf), by = seq]
#   # Obtain data from only the last date for each driver.
#   # past_counts <- past_counts[dinf == most_recent_date,
#   #                            c('dinf', 'seq', 'sex', 'age_grp', 'curr_pts_grp')]
#   past_counts <- past_counts[dinf == most_recent_date, ] # All columns included.
#   # This will drop any stale observations that were updated.
# 
# 
#   # Tabulate counts in each category.
#   past_counts_tab <- past_counts[, .N, by = c('sex', 'age_grp', 'curr_pts_grp')]
# 
#   # Append the current date.
#   past_counts_tab[, date:= date_count]
# 
# 
#   # Append the new totals to the data table of counts.
#   # saaq_past_counts <- rbind(saaq_past_counts, past_counts_tab)
#   # Appending becomes slower as the table grows.
#   # Better to select particular rows.
#   saaq_past_counts[(last_row + 1) :
#                      (last_row + nrow(past_counts_tab)), ] <-
#     past_counts_tab[, c('date', 'sex', 'age_grp', 'curr_pts_grp', 'N')]
# 
# 
#   # Update for last row populated.
#   last_row <- last_row + nrow(past_counts_tab)
# }

################################################################################
################################################################################
# End of Initial version with daily juggling of the dataset
################################################################################
################################################################################



################################################################################
################################################################################
# Previous version saved here.
################################################################################
################################################################################

# # Save for later.
# counts_version <- 1
# # counts_version <- 3
# ptsVersion <- 4
# mid_out_file_name <- sprintf('saaq_past_counts_temp_%d_%s_%s_v%d.csv',
#                          ptsVersion,
#                          substr(beg_date, 1, 4), substr(end_date, 1, 4),
#                          counts_version)
# # out_path_file_name <- sprintf('%s%s', dataInPath, out_file_name)
# out_path_file_name <- sprintf('%s/%s', data_out_path, mid_out_file_name)
# write.csv(x = saaq_past_counts, file = out_path_file_name, row.names = FALSE)



################################################################################
################################################################################
# Do more calculations and save later
################################################################################
################################################################################

# No need to read the data: it's already in memory.

# Read a dataset tabulated elsewhere.
# counts_version <- 3 # Before adding past_active
# counts_version <- 4 # After adding past_active
# in_file_name <- sprintf('saaq_past_counts_temp_%d_%s_%s_v%d.csv',
#                          ptsVersion,
#                         1998, 2010, # The full monty.
#                         # 2004, 2004, # The test with one year of history.
#                          counts_version)

# data_count_path <- 'SAAQ_counts/'
# Back to in path.
# data_count_path <- 'SAAQdata_full/'
# in_path_file_name <- sprintf('%s%s', data_count_path, in_file_name)
# in_path_file_name <- sprintf('%s/%s', data_count_path, pts_out_file_name)
# saaq_past_counts <- data.table(read.csv(file = in_path_file_name))
# 
# # Adjust dataset to the original state.
# summary(saaq_past_counts)
# saaq_past_counts[, date := as.Date(date)]


# # Read a past version of this dataset.
# check_file_name <- 'saaq_past_counts_temp_2_1998_2010_v4.csv'
# in_path_file_name <- sprintf('%s/%s', data_in_path, check_file_name)
# saaq_past_counts_check <- fread(file = in_path_file_name)
# 
# # Compare with saaq_past_counts calculated here. 
# colnames(saaq_past_counts)
# colnames(saaq_past_counts_check)
# 
# # Zeros replaced with -99L in previous version.
# saaq_past_counts_check[N == -99, N := 0]
# 
# summary(saaq_past_counts)
# summary(saaq_past_counts_check)

# Seems as though past version was over=counting.
# That might solve the problem with the apparent excess of 
# tickets awarded to young males.


################################################################################
################################################################################
# Continue from here.
################################################################################
################################################################################



################################################################################
################################################################################
# Not needed with revised calculation.
################################################################################
################################################################################


# # Append rows with zeros to make size predictable.
# 
# # Previous version without past_active:
# # saaq_past_zero <- data.table(expand.grid(date = date_list,
# #                                          sex = c('M', 'F'),
# #                                          age_grp = age_group_list,
# #                                          curr_pts_grp = curr_pts_grp_list))
# 
# # Later version with past_active:
# saaq_past_zero <- data.table(expand.grid(date = date_list,
#                                          sex = c('M', 'F'),
#                                          age_grp = age_group_list,
#                                          curr_pts_grp = curr_pts_grp_list,
#                                          past_active = c(FALSE, TRUE)))
# 
# 
# # Only a million rows or so.
# # Or maybe three million rows or so.
# # Initialize with zeros for the combinations that didn't happen.
# saaq_past_zero[, N := 0L]
# 
# # Verify that columns match.
# colnames(saaq_past_counts)
# colnames(saaq_past_zero)
# 
# # Append these blank rows, dropping unpopulated rows.
# saaq_past_counts <- rbind(saaq_past_counts[N >= 0, ], saaq_past_zero)
# 
# #--------------------------------------------------
# # First version without past active.
# 
# # # Sum again to square off points categories.
# # saaq_past_counts[, num := sum(N), by = c('date', 'sex', 'age_grp', 'curr_pts_grp')]
# # # Drop duplicate point values.
# # saaq_past_counts_sum <- unique(saaq_past_counts[, c('date', 'sex', 'age_grp', 'curr_pts_grp', 'num')])
# # # Sort in same order.
# # saaq_past_counts_sum <- saaq_past_counts_sum[order(date, sex, age_grp, curr_pts_grp), ]
# 
# 
# #--------------------------------------------------
# # Later version with past active.
# 
# # Sum again to square off points categories.
# saaq_past_counts[, num := sum(N), by = c('date', 'sex', 'age_grp', 'curr_pts_grp', 'past_active')]
# # Drop duplicate point values.
# saaq_past_counts_sum <- unique(saaq_past_counts[, c('date', 'sex', 'age_grp', 'curr_pts_grp', 'past_active', 'num')])
# # Sort in same order.
# saaq_past_counts_sum <- saaq_past_counts_sum[order(date, sex, age_grp, curr_pts_grp, past_active), ]
# 
# #--------------------------------------------------
# 
# # Result should have same number of rows as saaq_past_zero.
# print('Checking that rows match:')
# nrow(saaq_past_zero)
# nrow(saaq_past_counts) # Includes other combinations.
# nrow(saaq_past_counts_sum)
# 
# # First version without past active.
# # nrow(unique(saaq_past_counts[, c('date', 'sex', 'age_grp', 'curr_pts_grp')]))
# # Later version with past active.
# nrow(unique(saaq_past_counts[, c('date', 'sex', 'age_grp', 'curr_pts_grp', 'past_active')]))
# 
# summary(saaq_past_counts)
# summary(saaq_past_counts_sum)
# 
# 
# # Data checks.
# saaq_past_counts_sum[date == as.Date('1998-01-03'), sum(num)]
# saaq_dt[dinf == as.Date('1998-01-01'), sum(points > 0)]
# saaq_past_counts_sum[date == as.Date('2010-12-31') &
#                        curr_pts_grp != 0, sum(num)]
# saaq_dt[dinf >= as.Date('2009-01-01') &
#           dinf <= as.Date('2010-12-31'), .N]
# # Close but ok since some drivers get duplicates.
# saaq_past_counts_sum[date == as.Date('2010-12-31'), sum(num)]
# saaq_past_counts_sum[, sum(num)]/length(date_list)
# # Figures appear plausible.



#--------------------------------------------------------------------------------
# Plot evolution of population over time.
#--------------------------------------------------------------------------------


# Plot a time series of counts.
# plot(saaq_past_counts_sum[, sum(num), by = c('date')])
plot(saaq_past_counts[, sum(N), by = c('date')])
# Counts up to number of drivers (approximately),
# because every driver has had an event by the end of the dataset.

# Plot for point groups.
# table(saaq_past_counts_sum[, curr_pts_grp])
# plot(saaq_past_counts_sum[curr_pts_grp == 0, sum(num), by = c('date')])
# plot(saaq_past_counts_sum[curr_pts_grp == 1, sum(num), by = c('date')])
# plot(saaq_past_counts_sum[curr_pts_grp == 2, sum(num), by = c('date')])
# plot(saaq_past_counts_sum[curr_pts_grp == 3, sum(num), by = c('date')])
table(saaq_past_counts[, curr_pts_grp])
plot(saaq_past_counts[curr_pts_grp == 0, sum(N), by = c('date')])
plot(saaq_past_counts[curr_pts_grp == 1, sum(N), by = c('date')])
plot(saaq_past_counts[curr_pts_grp == 2, sum(N), by = c('date')])
plot(saaq_past_counts[curr_pts_grp == 3, sum(N), by = c('date')])

plot(saaq_past_counts[curr_pts_grp == '11-20', sum(N), by = c('date')])
plot(saaq_past_counts[curr_pts_grp == '30-150', sum(N), by = c('date')])


# Check for past_active.
# plot(saaq_past_counts_sum[past_active == TRUE, sum(num), by = c('date')])
plot(saaq_past_counts[past_active == TRUE, sum(N), by = c('date')])



# Plot population with particular point balances over time.
# All point categories together.
fig_file_name <- '~/Research/SAAQ/SAAQ_counts/Counts_1_150.png'
# png(file = fig_file_name)
color_list <- rainbow(length(curr_pts_grp_list))
first_color_num <- 2
color_num <- first_color_num
plot(saaq_past_counts[curr_pts_grp == curr_pts_grp_list[color_num],
                          sum(N)/1000, by = c('date')],
     # saaq_past_counts_sum[curr_pts_grp == curr_pts_grp_list[color_num],
     #                      sum(num)/1000, by = c('date')],
     col = color_list[color_num],
     lwd = 3,
     main = 'Number of Drivers with Selected Current Point Balances',
     xlab = 'Date',
     ylab = 'Frequency (thousands)',
     type = 'l',
     ylim = c(0, 500))
for (color_num in (color_num + 1):length(curr_pts_grp_list)) {
  lines(saaq_past_counts[curr_pts_grp == curr_pts_grp_list[color_num],
                             sum(N)/1000, by = c('date')],
        # saaq_past_counts_sum[curr_pts_grp == curr_pts_grp_list[color_num],
        #                      sum(num)/1000, by = c('date')],
        col = color_list[color_num],
       lwd = 3)
}
legend(x = 'topleft',
       legend = curr_pts_grp_list[first_color_num:length(curr_pts_grp_list)],
       col = color_list[first_color_num:length(curr_pts_grp_list)],
       lwd = 3) # ,
       # y.intersp = 1.25,
       # cex = 1.0,
       # seg.len = 0.5)
# dev.off()


# # Plot for higher point balance categories.
# fig_file_name <- '~/Research/SAAQ/SAAQ_counts/Counts_4_150.png'
# # png(file = fig_file_name)
# color_list <- rainbow(length(curr_pts_grp_list))
# first_color_num <- 5
# color_num <- first_color_num
# plot(saaq_past_counts_sum[curr_pts_grp == curr_pts_grp_list[color_num],
#                           sum(num)/1000, by = c('date')],
#      col = color_list[color_num],
#      lwd = 3,
#      main = 'Number of Drivers with Selected Current Point Balances',
#      xlab = 'Date',
#      ylab = 'Frequency (thousands)',
#      type = 'l',
#      ylim = c(0, 125))
# for (color_num in (color_num + 1):length(curr_pts_grp_list)) {
#   lines(saaq_past_counts_sum[curr_pts_grp == curr_pts_grp_list[color_num],
#                              sum(num)/1000, by = c('date')],
#         col = color_list[color_num],
#         lwd = 3)
# }
# legend(x = 'topleft',
#        legend = curr_pts_grp_list[first_color_num:length(curr_pts_grp_list)],
#        col = color_list[first_color_num:length(curr_pts_grp_list)],
#        lwd = 3) # ,
# # y.intersp = 1.25,
# # cex = 1.0,
# # seg.len = 0.5)
# # dev.off()
# 
# 
# 
# # Plot for highest point balance categories.
# fig_file_name <- '~/Research/SAAQ/SAAQ_counts/Counts_7_150.png'
# # png(file = fig_file_name)
# color_list <- rainbow(length(curr_pts_grp_list))
# first_color_num <- 8
# color_num <- first_color_num
# plot(saaq_past_counts_sum[curr_pts_grp == curr_pts_grp_list[color_num],
#                           sum(num)/1000, by = c('date')],
#      col = color_list[color_num],
#      lwd = 3,
#      main = 'Number of Drivers with Selected Current Point Balances',
#      xlab = 'Date',
#      ylab = 'Frequency (thousands)',
#      type = 'l',
#      ylim = c(0, 60))
# for (color_num in (color_num + 1):length(curr_pts_grp_list)) {
#   lines(saaq_past_counts_sum[curr_pts_grp == curr_pts_grp_list[color_num],
#                              sum(num)/1000, by = c('date')],
#         col = color_list[color_num],
#         lwd = 3)
# }
# legend(x = 'topleft',
#        legend = curr_pts_grp_list[first_color_num:length(curr_pts_grp_list)],
#        col = color_list[first_color_num:length(curr_pts_grp_list)],
#        lwd = 3) # ,
# # y.intersp = 1.25,
# # cex = 1.0,
# # seg.len = 0.5)
# # dev.off()



# # Look for dupes. FOUND!
# # length(table(saaq_past_counts[, c('date', 'sex', 'age_grp', 'curr_pts_grp')]))
# summary(saaq_past_counts[, c('date')])
# summary(table(saaq_past_counts[, c('date')]))
# sum(!(saaq_past_counts[, c('date', 'sex', 'age_grp', 'curr_pts_grp')] %in%
#         saaq_past_zero[, c('date', 'sex', 'age_grp', 'curr_pts_grp')]))
#
# # Be more careful:
# nrow(unique(saaq_past_counts[, c('date', 'sex', 'age_grp', 'curr_pts_grp', 'num')]))
#
# head(saaq_past_counts_sum, 20)

# # Problem was curr_pts_grp_list did not have all categories.
# table(saaq_past_counts_sum[num > 0, curr_pts_grp])
# table(saaq_past_counts_sum[num == 0, curr_pts_grp])
# table(saaq_past_counts_sum[, curr_pts_grp])


# # Save result for total counts by point level-age-sex categories.
# counts_version <- 1
# # counts_version <- 3
# out_file_name <- sprintf('saaq_past_counts_%d_%s_%s_v%d.csv',
#                          ptsVersion,
#                          substr(beg_date, 1, 4), substr(end_date, 1, 4),
#                          counts_version)
# # out_path_file_name <- sprintf('%s/%s', dataInPath, in_file_name)
# out_path_file_name <- sprintf('%s%s', dataInPath, pts_out_file_name)
# # Yes, keep it in dataInPath since it is yet to be joined.
# write.csv(x = saaq_past_counts_sum, file = out_path_file_name, row.names = FALSE)


# # Read a dataset tabulated elsewhere.
# counts_version <- 3
# in_file_name <- sprintf('saaq_past_counts_%d_%s_%s_v%d.csv',
#                          ptsVersion,
#                          1998, 2010,
#                          counts_version)
# data_count_path <- 'SAAQ_counts/'
# # in_path_file_name <- sprintf('%s%s', data_count_path, in_file_name)
# out_path_file_name <- sprintf('%s%s', dataInPath, pts_out_file_name)
# saaq_past_counts_2 <- data.table(read.csv(file = in_path_file_name))


# nrow(saaq_past_counts_2)
# nrow(saaq_past_counts)



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################






################################################################################
# Output Point Balances by Driver
################################################################################

# Current version has separate tag for aggregated file with
# new variable for past points indicator.
out_path_file_name <- sprintf('%s/%s', data_out_path, out_file_name)
# write.csv(x = saaq_past_counts_sum, file = out_path_file_name, row.names = FALSE)
write.csv(x = saaq_past_counts, file = out_path_file_name, row.names = FALSE)






################################################################################
# End
################################################################################
