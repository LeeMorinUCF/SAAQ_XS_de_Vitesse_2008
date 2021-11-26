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
tickets_out_file_name <- 'saaq_tickets_balances.csv'


# Set name of output file for point totals.
balance_out_file_name <- 'saaq_point_balances.csv'



################################################################################
# Set parameters for variable definitions.
################################################################################

#--------------------------------------------------------------------------------
# Set parameters related to dates
#--------------------------------------------------------------------------------

# Set date of policy change.
april_fools_2008 <- '2008-04-01'
# No joke: policy change on April Fool's Day!


# Create rows for list of dates.
day_1 <- as.numeric(as.Date('1998-01-01'))
# day_T <- as.numeric(as.Date('2010-12-31'))
day_T <- as.numeric(as.Date('2013-01-01')) # Allows for all points to expire.
date_list <- as.Date(seq(day_1, day_T), origin = as.Date('1970-01-01'))

length(date_list)
min(date_list)
max(date_list)


#--------------------------------------------------------------------------------
# Set parameters for age and point categories.
#--------------------------------------------------------------------------------

# Age categories.
age_group_list <- c('0-15', '16-19', '20-24', '25-34', '35-44', '45-54',
                    '55-64', '65-74', '75-84', '85-89', '90-199')
age_cut_points <- c(0, 15.5, 19.5, seq(24.5, 84.5, by = 10), 89.5, 199)


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


#--------------------------------------------------------------------------------
# Convert categorical variables to factors.
#--------------------------------------------------------------------------------

# Sex is first.
saaq_point_hist[, sex := factor(sex, levels = c('M', 'F'))]

table(saaq_point_hist[, 'sex'], useNA = 'ifany')


# Now redefine age categories.
saaq_point_hist[, 'age_grp'] <- cut(saaq_point_hist[, age], breaks = age_cut_points,
                            labels = age_group_list)

saaq_point_hist[, age_grp := factor(age_grp, levels = age_group_list)]

# Verify definitions of age categories.
summary(saaq_point_hist[age_grp == '0-15', 'age'])
summary(saaq_point_hist[age_grp == '55-64', 'age'])
summary(saaq_point_hist[age_grp == '90-199', 'age'])


table(saaq_point_hist[, 'age_grp'], useNA = 'ifany')



summary(saaq_point_hist)



################################################################################
# Generate new variables for violation history.
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


# Append the original observations, then sort.
saaq_point_hist <- rbind(saaq_point_hist, saaq_point_hist_exp)

saaq_point_hist <- saaq_point_hist[order(seq,
                         date,
                         points)]
head(saaq_point_hist, 10)

# Remove the duplicate, which is no longer needed after joining.
rm(saaq_point_hist_exp)



#--------------------------------------------------------------------------------
# Generate counts of cumulative point balances.
#--------------------------------------------------------------------------------


# Calculate point balances.
saaq_point_hist[, curr_pts := cumsum(points)]
head(saaq_point_hist, 20)


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



#--------------------------------------------------------------------------------
# Categorization of point total balances
#--------------------------------------------------------------------------------

# Categories:
# 0-10 separately, for granularity.
# 11-20 for next category.
# 21-30 for next category.
# 31+ for last category.

saaq_point_hist[, 'curr_pts_grp'] <- cut(saaq_point_hist[, curr_pts],
                                              breaks = curr_pts_cut_points,
                                              labels = curr_pts_grp_list)

saaq_point_hist[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]



# Allocate previous point balances to the same categories.
saaq_point_hist[, 'prev_pts_grp'] <- cut(saaq_point_hist[, prev_pts],
                                         breaks = curr_pts_cut_points,
                                         labels = curr_pts_grp_list)

saaq_point_hist[, prev_pts_grp := factor(prev_pts_grp, levels = curr_pts_grp_list)]


table(saaq_point_hist[, curr_pts_grp], useNA = 'ifany')
table(saaq_point_hist[, prev_pts_grp], useNA = 'ifany')
# Note that they match:
# Every point balance that occurred, has occurred both previously and currently.
# In particular, the zero balances are either at the beginnings or the ends.


#--------------------------------------------------------------------------------
# Create an indicator for highest point category before policy change.
# Use it to determine if the bad guys change their habits.
#--------------------------------------------------------------------------------

# colnames(saaq_point_hist)
# saaq_point_hist[, pre_policy := date < as.Date('2008-04-01')]
saaq_point_hist[, pre_policy := date < as.Date(april_fools_2008)]
# saaq_point_hist[, pre_policy := dinf < as.Date(april_fools_2008)]
table(saaq_point_hist[, pre_policy], useNA = 'ifany')
# Pre-policy period is much longer with an asymmetric window.

# Create a list of active drivers before the policy change.
# past_active_list <- unique(saaq_point_hist[curr_pts_grp %in% past_active_pts_list &
#                                              pre_policy == TRUE, seq])
past_active_list <- unique(saaq_point_hist[prev_pts_grp %in% past_active_pts_list &
                                             pre_policy == TRUE, seq])
# Note that this variable is not used for prediction.
# It is used for classifying a subsample.

# Allocate drivers to the list of those with an active past.
saaq_point_hist[, past_active := seq %in% past_active_list]



length(unique(saaq_point_hist[, seq]))
# [1] 3369249
length(past_active_list)
# [1] 795169 # Based on current points (including today's)
# [1] 723606 # Based on previous points
# About a quarter of the sample of drivers.
# Good: Not too many. Not too few.

table(saaq_point_hist[, past_active], useNA = 'ifany')
# Based on current points (including today's):
# FALSE     TRUE
# 10358098 10732848
# Based on previous points:
# FALSE     TRUE
# 10972352 10118594




#--------------------------------------------------------------------------------
# Generate counts of cumulative two-year point totals.
#--------------------------------------------------------------------------------

# Requires only certain variables to proceed.
# saaq_point_hist <- saaq_point_hist[, c('seq', 'sex', 'age', 'date', 'points', 'past_active')]
saaq_point_hist <- saaq_point_hist[, c('seq', 'sex', 'age_grp', 'past_active',
                                       'dinf', 'date',
                                       'points', 'curr_pts', 'prev_pts',
                                       'curr_pts_grp', 'prev_pts_grp')]



# # Change name of date variable (since not all are infractions).
# saaq_point_hist[, date := date]
# # Later versions will do this earlier and keep the variable called "date" throughout.
# Moved above.

# Note that this is for defining the rest of the population, not the ticket-getters.
# These figures should be lagged one day.
# Their ticket balance is the value they start with tomorrow morning.

saaq_point_hist[points > 0, date := as.Date(date + 1)]

summary(saaq_point_hist)




#--------------------------------------------------------------------------------
# Analysis of two-year point total balances
#--------------------------------------------------------------------------------


# List the possible values.
past_pts_list <- unique(saaq_point_hist[, curr_pts])
past_pts_list <- past_pts_list[order(past_pts_list)]
# Every number up to 110, then more sparse up to 150.

# Inspect the distribution to choose categories.
quantile(saaq_point_hist[, curr_pts], probs = seq(0, 1, by = 0.1))
# 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
# 0    0    0    1    2    3    3    4    6    8  150

quantile(saaq_point_hist[, curr_pts], probs = seq(0.9, 1, by = 0.01))
# 90%  91%  92%  93%  94%  95%  96%  97%  98%  99% 100%
# 8    9    9   10   10   11   12   13   15   18  150

quantile(saaq_point_hist[, curr_pts], probs = seq(0.99, 1, by = 0.001))
# 99% 99.1% 99.2% 99.3% 99.4% 99.5% 99.6% 99.7% 99.8% 99.9%  100%
# 18    18    19    20    21    22    23    25    27    32   150


# 0-10 gets up to the 95 percentile.
# 15 gets to 98th percentile.
# 20 gets inside 99th percentile.
# 30 gets to 99.9%.

# This analysis inspired the definition of the balance categories.


#--------------------------------------------------------------------------------
# Categorization of balances by full-day activity
# since some drivers have multiple tickets per day.
#--------------------------------------------------------------------------------


# Now this dataset can be used to calculate counts by category.
# But one more adjustment is required: some drivers have several tickets in one day.
# These drivers are counted several times over.
# Need to select current and previous category for each driver-day.

# Add indicator for event.
# colnames(saaq_point_hist)
saaq_point_hist[, 'event_id'] <- seq(nrow(saaq_point_hist))

# Each driver-day, record the first id and last id.
saaq_point_hist[, first_id := min(event_id), by = list(seq, date)]
saaq_point_hist[, last_id := max(event_id), by = list(seq, date)]

# Create true full-day transition from first balance
# category to last balance category.
# Values at the end of the day or the beginning of the day.
saaq_point_hist[event_id == last_id, curr_pts_EOD := curr_pts]
saaq_point_hist[event_id == first_id, prev_pts_BOD := prev_pts]

# Assign full-day balance categories.
saaq_point_hist[, curr_pts_EOD := mean(curr_pts_EOD, na.rm = TRUE), by = list(seq, date)]
saaq_point_hist[, prev_pts_BOD := mean(prev_pts_BOD, na.rm = TRUE), by = list(seq, date)]


# Allocate them to balance categories.
saaq_point_hist[, 'curr_pts_grp_EOD'] <- cut(saaq_point_hist[, curr_pts_EOD],
                                         breaks = curr_pts_cut_points,
                                         labels = curr_pts_grp_list)

saaq_point_hist[, curr_pts_grp_EOD := factor(curr_pts_grp_EOD, levels = curr_pts_grp_list)]



# Allocate previous point balances to the same categories.
saaq_point_hist[, 'prev_pts_grp_BOD'] <- cut(saaq_point_hist[, prev_pts_BOD],
                                         breaks = curr_pts_cut_points,
                                         labels = curr_pts_grp_list)

saaq_point_hist[, prev_pts_grp_BOD := factor(prev_pts_grp_BOD, levels = curr_pts_grp_list)]


table(saaq_point_hist[, curr_pts_grp_EOD], useNA = 'ifany')
table(saaq_point_hist[, prev_pts_grp_BOD], useNA = 'ifany')
# These do differ because the end of day values do not necessarily appear at the
# beginning of another day.

# head(saaq_point_hist)
# tail(saaq_point_hist)


# Verify that it is working for the drivers with multiple tickets
# in one day.
saaq_point_hist[seq == 68306, ]
saaq_point_hist[seq == 847237, ]
saaq_point_hist[seq == 3526906, ]


# This is one of the datasets that will be output below.
colnames(saaq_point_hist)


#--------------------------------------------------------------------------------
# Calculate counts of drivers by category.
#--------------------------------------------------------------------------------


# Create an aggregated version to streamline counting.
# saaq_pts_chgs <- saaq_past_pts[, .N, by = c('date', 'sex', 'age_grp', 'past_active', # Fixed categories.
#                                             'prev_pts_grp', 'curr_pts_grp')]         # Transition category.
# Eliminate all but the first record per driver day (with full-day transitions).
saaq_pts_chgs <- saaq_point_hist[event_id == first_id, .N,
                               by = c('date', 'sex', 'age_grp', 'past_active', # Fixed categories.
                                      'prev_pts_grp_BOD', 'curr_pts_grp_EOD')]   # Transition category.


# Replace the column name as if drivers only got one ticket per day.
colnames(saaq_pts_chgs) <- c('date', 'sex', 'age_grp', 'past_active',
                             'prev_pts_grp', 'curr_pts_grp', 'N')

summary(saaq_pts_chgs)

# Points groups need to be converted to factors.
# And levels must be defined in order, so that counts of transitions match in rows.
saaq_pts_chgs[, prev_pts_grp := factor(prev_pts_grp, levels = curr_pts_grp_list)]
saaq_pts_chgs[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]

summary(saaq_pts_chgs)



table(saaq_pts_chgs[, 'prev_pts_grp'])
table(saaq_pts_chgs[, 'curr_pts_grp'])



head(saaq_pts_chgs, 20)

saaq_pts_chgs <- saaq_pts_chgs[order(date, sex, age_grp, past_active,
                                     prev_pts_grp, curr_pts_grp)]

summary(saaq_pts_chgs)


# # Inspect a sample to check the order.
# saaq_pts_chgs[date == '1998-01-22' &
#                 sex == 'M' &  age_grp == '65-74' &  past_active == TRUE, ]


#--------------------------------------------------------------------------------
# Daily categorization of point total balances across age and sex categories
#--------------------------------------------------------------------------------

# This creates a time series of counts by point-age-sex-past_active categories.

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
# beg_date <- date_list[2]
beg_date <- date_list[1]
beg_date_num <- which(date_list == beg_date)
# end_date <- '2004-12-31' # Test with 1-year sample.
# end_date <- '2010-12-31'
end_date <- '2013-01-01' # Allows all points to expire.
end_date_num <- which(date_list == end_date)
# Note that this leaves many dates at zero, outside of the range.
date_num_list <- beg_date_num:end_date_num


# Initialize counts at zero point group.
# Initialize counts with number of drivers in each category combination.
date_curr <- date_list[beg_date_num]
add_counts <- unique(saaq_point_hist[event_id == first_id,
                                     c('seq', 'sex', 'age_grp', 'past_active')])
add_counts <- unique(add_counts[, .N, by = c('sex', 'age_grp', 'past_active')])
add_counts[, curr_pts_grp := 0]
# Change column order to match.
add_counts <- add_counts[, c('sex', 'age_grp', 'past_active',
                                'curr_pts_grp', 'N')]
# Pad list of changes with zero entries.
add_counts <- rbind(add_counts, saaq_zero_curr)
# Aggregate and sort.
add_counts <- add_counts[, N := sum(N),
                         by = c('sex', 'age_grp', 'past_active',
                                'curr_pts_grp')]
add_counts <- unique(add_counts)
add_counts[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]
add_counts <- add_counts[order(sex, age_grp, past_active, curr_pts_grp)]
# Add to initial balances in curr_pts_grp zero.
saaq_past_counts[date == date_curr, 'N'] <- add_counts[, 'N']


# Loop on dates and calculate the totals.
# date_num <- 22
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
  deduct_counts[, prev_pts_grp := factor(prev_pts_grp, levels = curr_pts_grp_list)]
  deduct_counts <- deduct_counts[order(sex, age_grp, past_active, prev_pts_grp)]

  # Now this list should be in the same order as the date selection.
  # But let's make sure.
  # deduct_counts[sex == 'M' & age_grp == '65-74' & past_active == TRUE, ]
  # saaq_past_counts[date == date_curr &
  #                    sex == 'M' & age_grp == '65-74' & past_active == TRUE, ]

  # Whay are they going out of order?
  # table(deduct_counts[, prev_pts_grp])
  # class(point_changes[, prev_pts_grp])
  # class(deduct_counts[, prev_pts_grp])
  # class(saaq_pts_chgs[, prev_pts_grp])
  # levels(point_changes[, prev_pts_grp])
  # levels(deduct_counts[, prev_pts_grp])
  # levels(saaq_pts_chgs[, prev_pts_grp])
  # It could be because they are not factors.



  # if (date_num > date_num_list[1] | beg_date_num == 2) {
  if (date_num == date_num_list[1]) {
    saaq_past_counts[date == date_curr, 'N'] <-
      saaq_past_counts[date == date_curr, 'N'] -
      deduct_counts[, 'N']
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
  add_counts[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]
  add_counts <- add_counts[order(sex, age_grp, past_active, curr_pts_grp)]

  # Now this list should be in the same order as the date selection.
  # But let's make sure.
  # add_counts[sex == 'M' & age_grp == '65-74' & past_active == TRUE, ]
  # saaq_past_counts[date == date_curr &
  #                    sex == 'M' & age_grp == '65-74' & past_active == TRUE, ]



  saaq_past_counts[date == date_curr, 'N'] <-
    saaq_past_counts[date == date_curr, 'N'] +
    add_counts[, 'N']


}

# Sum on each date should be zero (plus number of unique drivers = 3369249).
# Drivers all swap in from zero points and swap to positive points.
saaq_past_counts[, sum(N)]
saaq_past_counts[, sum(N), by = c('date')]
summary(saaq_past_counts[, sum(N), by = c('date')])

# How many drivers remain at zero (none, now).
saaq_past_counts[N < 0, sum(N)]
saaq_past_counts[N < 0, sum(N), by = c('date')]
saaq_past_counts[N > 0, sum(N), by = c('date')]
# Initial version skipped initialization to track changes.

# Now counts are initialized at number of drivers,
# so the total count stays at a constant population.


summary(saaq_past_counts[N > 0, sum(N), by = c('date')])
# Check the total for day 1:
saaq_pts_chgs[date == date_list[beg_date_num], ]
saaq_pts_chgs[date == date_list[beg_date_num], sum(N)]
# Sum is 146, half of 292.
# Was missing a minus sign. Fixed.
# Now it is 145, after removing some double-counting.

# Check the first day.
saaq_past_counts[date == date_list[beg_date_num], ]


# Look at the cumulative driver counts.
head(saaq_past_counts[N > 0, sum(N), by = c('date')])
tail(saaq_past_counts[N > 0, sum(N), by = c('date')])
saaq_past_counts[N > 0, sum(N), by = c('date')]
# Last date:
# 4747: 2010-12-31 3065987
# Now it is this:
# 4747: 2010-12-31 3040992
# Now it is this:
# 4747: 2010-12-31 1319592
# That's really low.
# But it makes sense if most drivers are getting swapped back to zero.

# Now counts are initialized at number of drivers,
# so the total count stays at a constant population.


# # Compare to the drivers with tickets in the last two years.
# # saaq_point_hist[date >= '2008-01-01', ]
# # summary(saaq_point_hist)
# saaq_point_hist[date >= '2008-01-01' & first_id == event_id & points > 0, .N]
# # [1] 2888758
#
#
# # Compare to the counts of drivers.
# length(unique(saaq_point_hist[, seq]))
# # [1] 3369249
#
# # # First check:
# # # 3369249 (from the original data table) vs 3600227 (from the counts)
# # 3369249 - 3600227
# # # -230978
# # # This doesn't match.
# # # This suggests that some 230978 are being added.
# # # Compare to the list of points.
# # length(unique(saaq_point_hist[, seq]))
# # # 3369249 (which matches the original data table)
#
# # Second check:
# # 3369249 (from the original data table) vs 3065987 (from the counts)
# 3369249 - 3065987
# # [1] 303262
# # This doesn't match.
# # This suggests that some 303262 are getting lost.
# # Compare to the list of points.
# length(unique(saaq_point_hist[, seq]))
# # 3369249 (which matches the original data table)
# # [1] 3369249
#
#
# # Third check:
# # 3369249 (from the original data table) vs 3040992 (from the counts)
# 3369249 - 3040992
# # [1] 328257
# # This doesn't match.
# # This suggests that some 328257 are getting lost.
# # Compare to the list of points.
# length(unique(saaq_past_pts[, seq]))
# # 3369249 (which matches the original data table)
# # [1] 3369249
#
#
# # Drivers then move around from there (possibly back to zero).
# # Total positive population should be the count of drivers who have received
# # any ticket up to that date.
# colnames(saaq_past_counts)
# colnames(point_changes)
# colnames(saaq_past_pts)
# first_ticket <- saaq_point_hist[, min(date), by = seq]
# summary(first_ticket)
# # driver_count <- table(first_ticket[, V1])
# driver_count <- first_ticket[, .N, by = list(V1)][order(V1), ]
# summary(driver_count)
# head(driver_count)
# tail(driver_count)
# plot(driver_count)
# plot(cumsum(driver_count[, N]))
# tail(driver_count[, cumsum(N)])
#
# driver_count_2 <- saaq_past_counts[N > 0 & curr_pts_grp != 0,
#                                    sum(N), by = list(date)]
# head(driver_count_2, 100)
# plot(driver_count_2[, V1])
#
#
# head(cumsum(driver_count[, N]), 10)
# head(driver_count_2, 10)
# tail(cumsum(driver_count[, N]), 10)
# tail(driver_count_2, 10)
# # Looks like the count is missing some drivers.
# # But at least it matches up to the points table.
# # Now there is a large number missing.
# # Maybe these have gone back to zero.
#
# count_diff <- cumsum(driver_count[, N]) - c(0, driver_count_2[, V1])
#
#
# head(count_diff)
# tail(count_diff)
# plot(count_diff)
# summary(count_diff)
# # The accumulated difference is quite large
# # and it grows over the sample--first up, then down
# # and levels off somewhat.
# # Now it starts out flat and then grows linearly with some plateaus.
#
#
#
#
# # Troubleshooting surprise negative counts.
# summary(saaq_past_counts[date == date_curr, 'N'])
# print(saaq_past_counts[date == date_curr &
#                          N != 0, ])
# print(saaq_past_counts[date == date_curr &
#                          N < 0, ])
# print(saaq_past_counts[date == date_curr &
#                          N != 0 &
#                          curr_pts_grp == 0, ])
# print(saaq_past_counts[date == date_curr &
#                          N < 0 &
#                          curr_pts_grp != 0, ])
# # 146-149 non-zero categories with negative counts.
# # Now down to 124.
# summary(saaq_past_counts[date == date_curr &
#                          N < 0 &
#                          curr_pts_grp != 0, ])
# # From -1 to -17230.
# # Now from -1 to -15158.
# # Now down to zero.
# # None left.


# Check for negatives on all days.
saaq_past_counts[N < 0, .N, by = curr_pts_grp]
saaq_past_counts[N < 0, .N, by = list(sex, curr_pts_grp)]
saaq_past_counts[N < 0, .N, by = list(sex, past_active, curr_pts_grp)]
saaq_past_counts[N < 0, .N,
                 by = list(sex, age_grp, past_active, curr_pts_grp)][order(sex, age_grp, past_active, curr_pts_grp)]
# Problem solved.


# # Check for negative balances on particular days.
# # All zero on day 1 (from lag).
# # All good on day 2.
# # date_num_sel <- 3 # One negative from 3 to 7.
# # The driver got two tickets in one day.
# # None on day 4 and 5; other drivers swapped in.
#
# # After removing multiple tickets,
# # the negatives happen less often.
# # date_num_sel <- 10
# for (date_num_sel in 20:30) {
#   date_sel <- date_list[date_num_sel]
#   print(sprintf("Negative counts with non-zero balances on %s", date_sel))
#   summary(saaq_past_counts[date == date_sel, ])
#   # print(saaq_past_counts[date == date_sel &
#   #                          N < 0, ])
#   # print(saaq_past_counts[date == date_sel &
#   #                          N != 0 &
#   #                          curr_pts_grp == 0, ])
#   print(saaq_past_counts[date == date_sel &
#                            N < 0 &
#                            curr_pts_grp != 0, ])
#
# }
#
#
#
# #--------------------------------------------------------------------------------
# # Detecting problem with negatives appearing.
# #--------------------------------------------------------------------------------
#
# date_list[21]
# print(saaq_past_counts[date == date_list[21] &
#                          N < 0 &
#                          curr_pts_grp != 0, ])
#
# saaq_pts_chgs[date == date_list[21] &
#                 prev_pts_grp != 0, ]
#
# saaq_pts_chgs[date == date_list[21] &
#                 sex == 'F' & age_grp == '55-64' & past_active == TRUE, ]
#
# saaq_point_hist[date == date_list[21] &
#                 sex == 'F' & age_grp == '55-64' & past_active == TRUE, ]#  &
#                 # prev_pts_grp == 2 &
#                 # curr_pts_grp == 4, ]
#
# saaq_point_hist[seq == 2255414, ]
# # Nothing unusual here.
#
#
# # Try another one.
# date_list[22]
# print(saaq_past_counts[date == date_list[22] &
#                          N < 0 &
#                          curr_pts_grp != 0, ])
#
# saaq_pts_chgs[date == date_list[22] &
#                 prev_pts_grp != 0, ]
#
# saaq_pts_chgs[date == date_list[22] &
#                 sex == 'M' & age_grp == '65-74' & past_active == TRUE, ]
#
# saaq_point_hist[date == date_list[22] &
#                 sex == 'M' & age_grp == '65-74' & past_active == TRUE, ]#  &
# # prev_pts_grp == 2 &
# # curr_pts_grp == 4, ]
#
# saaq_point_hist[seq == 788702, ]
#
#
#
#
# #--------------------------------------------------------------------------------
# # Detecting problem with multiple tickets:
# #--------------------------------------------------------------------------------
#
# # How did this happen on day 3?
# print(saaq_past_counts[date == date_list[3] &
#                          N < 0 &
#                          curr_pts_grp != 0, ])
#
# # Must be the 3 to 7 guy:
# saaq_pts_chgs[date == date_list[3] &
#                 prev_pts_grp != 0, ]
#
# colnames(saaq_point_hist)
# # saaq_point_hist[date == date_list[3] & points == 4 & curr_pts == 7, ]
# colnames(saaq_point_hist)
# saaq_point_hist[date == date_list[3] &
#                 prev_pts_grp == 3 &
#                 curr_pts_grp == 7, ]
# # seq      sex age       date points past_active curr_pts prev_pts curr_pts_grp prev_pts_grp age_grp       date
# # 1: 68306   M  39 1998-01-03      4        TRUE        7        3            7            3   35-44 1998-01-03
#
# # Check his record.
# saaq_point_hist[seq == 68306, ]
# # Aha! The driver got two tickets in one day.
#
# # How often does this happen?
# driver_day_num_tickets <- saaq_point_hist[, .N, by = list(seq, date)]
# summary(driver_day_num_tickets)
# summary(driver_day_num_tickets[N > 1, ])
# summary(driver_day_num_tickets[, N > 1])
# driver_day_num_tickets[N > 1, sum(N)]
# # Mostly two tickets in a day.
# # One driver got 26 tickets in one day:
# driver_day_num_tickets[N > 25, ]
# saaq_point_hist[seq == 847237, ]
# # Confirmed.
#
# # A few got 16-18 tickets.
# driver_day_num_tickets[N > 15, ]
# saaq_point_hist[seq == 3526906, ]
# # Confirmed.
#
# # Check these in the individual record of transitions.
# saaq_point_hist[seq == 68306, ]
# saaq_point_hist[seq == 847237, ]
# saaq_point_hist[seq == 3526906, ]
# saaq_past_counts[date == '1999-07-31' & curr_pts_grp == '30-150', ]
# saaq_past_counts[date == '1999-08-01' & curr_pts_grp == '30-150', ]
# # The 47-point day does not show up.
# # They are lost somewhere in between.
#
# saaq_pts_chgs[date == '1999-07-31' & curr_pts_grp == '30-150', ]
# saaq_pts_chgs[date == '1999-07-31' &
#                 sex == 'M' & age_grp == '20-24' & past_active == TRUE, ]
#

#--------------------------------------------------------------------------------




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
summary(saaq_past_counts[curr_pts_grp == '31-150', N])



#


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
# Plot evolution of population over time.
################################################################################


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
plot(saaq_past_counts[curr_pts_grp == '31-150', sum(N), by = c('date')])


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
# legend(x = 'topleft',
#        legend = curr_pts_grp_list[first_color_num:length(curr_pts_grp_list)],
#        col = color_list[first_color_num:length(curr_pts_grp_list)],
#        lwd = 3) # ,
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
# Output datasets
################################################################################


#--------------------------------------------------------------------------------
# Output dataset of tickets with balance variables.
#--------------------------------------------------------------------------------


# Drop the negative points for expiries.
# Keep only the tickets.
saaq_point_hist <- saaq_point_hist[points > 0, ]

summary(saaq_point_hist)


out_path_file_name <- sprintf('%s/%s', data_out_path, tickets_out_file_name)
# write.csv(x = saaq_past_counts_sum, file = out_path_file_name, row.names = FALSE)
write.csv(x = saaq_point_hist, file = out_path_file_name, row.names = FALSE)




#--------------------------------------------------------------------------------
# Output Counts of Drivers by Point Balances
#--------------------------------------------------------------------------------

# Current version has separate tag for aggregated file with
# new variable for past points indicator.
out_path_file_name <- sprintf('%s/%s', data_out_path, balance_out_file_name)
# write.csv(x = saaq_past_counts_sum, file = out_path_file_name, row.names = FALSE)
write.csv(x = saaq_past_counts, file = out_path_file_name, row.names = FALSE)






################################################################################
# End
################################################################################
