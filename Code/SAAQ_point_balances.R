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
# rm(list=ls(all=TRUE))

# Load package for importing datasets in proprietary formats.
# library(foreign)

# Load data table package for quick selection on seq.
library(data.table)


################################################################################
# Set parameters for file IO
################################################################################

# Set working directory, if running interactively.
# drive_path <- 'C:/Users/le279259/OneDrive - University of Central Florida/Documents'
# git_path <- 'Research/SAAQ/SAAQspeeding/SAAQ_XS_de_Vitesse_2008'
# wd_path <- sprintf('%s/%s',drive_path, git_path)
# setwd(wd_path)

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
saaq_dt_2[, age := age + 2]
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
saaq_past_pts <- copy(saaq_dt[, c('seq', 'sex', 'age', 'dinf', 'points')])
# Translate into the drops in points two years later.
saaq_past_pts[, dinf := as.Date(dinf + 730)]
saaq_past_pts[, age := age + 2]
saaq_past_pts[, points := - points]
head(saaq_past_pts, 10)
# Append the original observations, then sort.
# saaq_past_pts <- rbind(saaq_past_pts,
#                        data.table(saaq[, c('seq', 'sex', 'age', 'dinf', 'points')]))
saaq_past_pts <- rbind(saaq_past_pts,
                       saaq_dt[, c('seq', 'sex', 'age', 'dinf', 'points')])
saaq_past_pts <- saaq_past_pts[order(saaq_past_pts$seq,
                                     saaq_past_pts$dinf,
                                     saaq_past_pts$points)]
head(saaq_past_pts, 10)



# Calculate point balances.
saaq_past_pts[, curr_pts := cumsum(points)]
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

# Now this dataset can be used to calculate counts by category.


#--------------------------------------------------------------------------------
# Daily categorization of point total balances across age and sex categories
#--------------------------------------------------------------------------------

# This creates a time series of counts by point-age-sex categories.


# Check maximum number of rows for each day.
# nrow(expand.grid(sex = c('M', 'F'),
#                  age_grp = age_group_list,
#                  curr_pts_grp = curr_pts_grp_list))



# Start with a dataset of all possible permutations of the categories, each day.
saaq_past_counts <- data.table(expand.grid(date = date_list,
                                           sex = c('M', 'F'),
                                           age_grp = age_group_list,
                                           curr_pts_grp = curr_pts_grp_list))
# Only a million rows or so.
# Initialize with zeros for the combinations that didn't happen.
saaq_past_counts[, N := -99L]
last_row <- 0

# Initialize a data table to store the counts.
# saaq_past_counts <- NULL
past_counts <- NULL

# Set date range.
beg_date <- '2004-01-01'
beg_date_num <- which(date_list == beg_date)
end_date <- '2010-12-31'
# end_date <- '2004-12-31'
end_date_num <- which(date_list == end_date)

# Loop on dates and calculate the totals.
# date_num <- 2
# date_num_list <- 2:length(date_list)
# date_num_list <- 2:100
date_num_list <- beg_date_num:end_date_num
for (date_num in date_num_list) {

  # Select up to previous date.
  date_count <- date_list[date_num]
  date_last <- date_list[date_num - 1]

  # Print progress report.
  if (TRUE | (wday(date_count) == 1)) {
    print(sprintf('Now tabulating for date %s.', as.character(date_count)))
  }

  # Each month, pull a subset of the data for easier daily pulls.
  if (date_num == date_num_list[1] | mday(date_count) == 1) {

    print(sprintf('Resetting subset of data for date %s...', as.character(date_count)))

    saaq_past_pts_sub <- saaq_past_pts[year(dinf) == year(date_count) &
                                         month(dinf) == month(date_count) |
                                         year(dinf) == year(date_last) &
                                         month(dinf) == month(date_last),
                                       c('dinf', 'seq', 'sex', 'age_grp', 'curr_pts_grp')]

    print(sprintf('Finished resetting subset of data for date %s.', as.character(date_count)))

  }

  # Obtain most recent point blance for each driver (those who got a ticket).
  # Keep the most recent and append any new observations.
  # past_counts <- rbind(past_counts,
  #                      saaq_past_pts[dinf == date_last,
  #                                    c('dinf', 'seq', 'sex', 'age_grp', 'curr_pts_grp')])
  # Pull from subset for efficiency.
  past_counts <- rbind(past_counts[,
                                   c('dinf', 'seq', 'sex', 'age_grp', 'curr_pts_grp')],
                       saaq_past_pts_sub[dinf == date_last, ])


  # Obtain the last date for each driver.
  past_counts[, most_recent_date := max(dinf), by = seq]
  # Obtain data from only the last date for each driver.
  # past_counts <- past_counts[dinf == most_recent_date,
  #                            c('dinf', 'seq', 'sex', 'age_grp', 'curr_pts_grp')]
  past_counts <- past_counts[dinf == most_recent_date, ] # All columns included.
  # This will drop any stale observations that were updated.


  # Tabulate counts in each category.
  past_counts_tab <- past_counts[, .N, by = c('sex', 'age_grp', 'curr_pts_grp')]

  # Append the current date.
  past_counts_tab[, date:= date_count]


  # Append the new totals to the data table of counts.
  # saaq_past_counts <- rbind(saaq_past_counts, past_counts_tab)
  # Appending becomes slower as the table grows.
  # Better to select particular rows.
  saaq_past_counts[(last_row + 1) :
                     (last_row + nrow(past_counts_tab)), ] <-
    past_counts_tab[, c('date', 'sex', 'age_grp', 'curr_pts_grp', 'N')]


  # Update for last row populated.
  last_row <- last_row + nrow(past_counts_tab)
}



################################################################################
################################################################################
# Previous version saved here.
################################################################################
################################################################################

# # Save for later.
# counts_version <- 1
# # counts_version <- 3
# out_file_name <- sprintf('saaq_past_counts_temp_%d_%s_%s_v%d.csv',
#                          ptsVersion,
#                          substr(beg_date, 1, 4), substr(end_date, 1, 4),
#                          counts_version)
# # out_path_file_name <- sprintf('%s%s', dataInPath, out_file_name)
# out_path_file_name <- sprintf('%s%s', dataInPath, pts_out_file_name)
# # Yes, keep it in dataInPath since it is yet to be joined.
# write.csv(x = saaq_past_counts, file = out_path_file_name, row.names = FALSE)
#


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


################################################################################
################################################################################
# Continue from here.
################################################################################
################################################################################



# Append rows with zeros to make size predictable.

# Previous version without past_active:
# saaq_past_zero <- data.table(expand.grid(date = date_list,
#                                          sex = c('M', 'F'),
#                                          age_grp = age_group_list,
#                                          curr_pts_grp = curr_pts_grp_list))

# Later version with past_active:
saaq_past_zero <- data.table(expand.grid(date = date_list,
                                         sex = c('M', 'F'),
                                         age_grp = age_group_list,
                                         curr_pts_grp = curr_pts_grp_list,
                                         past_active = c(FALSE, TRUE)))


# Only a million rows or so.
# Or maybe three million rows or so.
# Initialize with zeros for the combinations that didn't happen.
saaq_past_zero[, N := 0L]

# Append these blank rows, dropping unpopulated rows.
saaq_past_counts <- rbind(saaq_past_counts[N >= 0, ], saaq_past_zero)

#--------------------------------------------------
# First version without past active.

# # Sum again to square off points categories.
# saaq_past_counts[, num := sum(N), by = c('date', 'sex', 'age_grp', 'curr_pts_grp')]
# # Drop duplicate point values.
# saaq_past_counts_sum <- unique(saaq_past_counts[, c('date', 'sex', 'age_grp', 'curr_pts_grp', 'num')])
# # Sort in same order.
# saaq_past_counts_sum <- saaq_past_counts_sum[order(date, sex, age_grp, curr_pts_grp), ]


#--------------------------------------------------
# Later version with past active.

# Sum again to square off points categories.
saaq_past_counts[, num := sum(N), by = c('date', 'sex', 'age_grp', 'curr_pts_grp', 'past_active')]
# Drop duplicate point values.
saaq_past_counts_sum <- unique(saaq_past_counts[, c('date', 'sex', 'age_grp', 'curr_pts_grp', 'past_active', 'num')])
# Sort in same order.
saaq_past_counts_sum <- saaq_past_counts_sum[order(date, sex, age_grp, curr_pts_grp, past_active), ]

#--------------------------------------------------

# Result should have same number of rows as saaq_past_zero.
print('Checking that rows match:')
nrow(saaq_past_zero)
nrow(saaq_past_counts) # Includes other combinations.
nrow(saaq_past_counts_sum)

# First version without past active.
# nrow(unique(saaq_past_counts[, c('date', 'sex', 'age_grp', 'curr_pts_grp')]))
# Later version with past active.
nrow(unique(saaq_past_counts[, c('date', 'sex', 'age_grp', 'curr_pts_grp', 'past_active')]))

summary(saaq_past_counts)
summary(saaq_past_counts_sum)


# Data checks.
saaq_past_counts_sum[date == as.Date('1998-01-03'), sum(num)]
saaq_dt[dinf == as.Date('1998-01-01'), sum(points > 0)]
saaq_past_counts_sum[date == as.Date('2010-12-31') &
                       curr_pts_grp != 0, sum(num)]
saaq_dt[dinf >= as.Date('2009-01-01') &
          dinf <= as.Date('2010-12-31'), .N]
# Close but ok since some drivers get duplicates.
saaq_past_counts_sum[date == as.Date('2010-12-31'), sum(num)]
saaq_past_counts_sum[, sum(num)]/length(date_list)
# Figures appear plausible.

#--------------------------------------------------------------------------------
# Plot evolution of population over time.
#--------------------------------------------------------------------------------


# Plot a time series of counts.
plot(saaq_past_counts_sum[, sum(num), by = c('date')])
# Counts up to number of drivers (approximately),
# because every driver has had an event by the end of the dataset.

# Plot for point groups.
table(saaq_past_counts_sum[, curr_pts_grp])
plot(saaq_past_counts_sum[curr_pts_grp == 0, sum(num), by = c('date')])
plot(saaq_past_counts_sum[curr_pts_grp == 1, sum(num), by = c('date')])
plot(saaq_past_counts_sum[curr_pts_grp == 2, sum(num), by = c('date')])
plot(saaq_past_counts_sum[curr_pts_grp == 3, sum(num), by = c('date')])


# Check for past_active.
plot(saaq_past_counts_sum[past_active == TRUE, sum(num), by = c('date')])



# Plot population with particular point balances over time.
# All point categories together.
fig_file_name <- '~/Research/SAAQ/SAAQ_counts/Counts_1_150.png'
# png(file = fig_file_name)
color_list <- rainbow(length(curr_pts_grp_list))
first_color_num <- 2
color_num <- first_color_num
plot(saaq_past_counts_sum[curr_pts_grp == curr_pts_grp_list[color_num],
                          sum(num)/1000, by = c('date')],
     col = color_list[color_num],
     lwd = 3,
     main = 'Number of Drivers with Selected Current Point Balances',
     xlab = 'Date',
     ylab = 'Frequency (thousands)',
     type = 'l',
     ylim = c(0, 500))
for (color_num in (color_num + 1):length(curr_pts_grp_list)) {
  lines(saaq_past_counts_sum[curr_pts_grp == curr_pts_grp_list[color_num],
                             sum(num)/1000, by = c('date')],
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


# Plot for higher point balance categories.
fig_file_name <- '~/Research/SAAQ/SAAQ_counts/Counts_4_150.png'
# png(file = fig_file_name)
color_list <- rainbow(length(curr_pts_grp_list))
first_color_num <- 5
color_num <- first_color_num
plot(saaq_past_counts_sum[curr_pts_grp == curr_pts_grp_list[color_num],
                          sum(num)/1000, by = c('date')],
     col = color_list[color_num],
     lwd = 3,
     main = 'Number of Drivers with Selected Current Point Balances',
     xlab = 'Date',
     ylab = 'Frequency (thousands)',
     type = 'l',
     ylim = c(0, 125))
for (color_num in (color_num + 1):length(curr_pts_grp_list)) {
  lines(saaq_past_counts_sum[curr_pts_grp == curr_pts_grp_list[color_num],
                             sum(num)/1000, by = c('date')],
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



# Plot for highest point balance categories.
fig_file_name <- '~/Research/SAAQ/SAAQ_counts/Counts_7_150.png'
# png(file = fig_file_name)
color_list <- rainbow(length(curr_pts_grp_list))
first_color_num <- 8
color_num <- first_color_num
plot(saaq_past_counts_sum[curr_pts_grp == curr_pts_grp_list[color_num],
                          sum(num)/1000, by = c('date')],
     col = color_list[color_num],
     lwd = 3,
     main = 'Number of Drivers with Selected Current Point Balances',
     xlab = 'Date',
     ylab = 'Frequency (thousands)',
     type = 'l',
     ylim = c(0, 60))
for (color_num in (color_num + 1):length(curr_pts_grp_list)) {
  lines(saaq_past_counts_sum[curr_pts_grp == curr_pts_grp_list[color_num],
                             sum(num)/1000, by = c('date')],
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
write.csv(x = saaq_past_counts_sum, file = out_path_file_name, row.names = FALSE)






################################################################################
# End
################################################################################
