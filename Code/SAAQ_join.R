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
# Note that these totals affect aggregation in that nonzero past violations
# must be subtracted from the counts of drivers without tickets each day.
# This is complicated by the sex and age dimensions.
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

# Clear workspace.
rm(list=ls(all=TRUE))

# Load package for importing datasets in proprietary formats.
library(foreign)

# Load data table package for quick selection on seq.
library(data.table)


################################################################################
# Set parameters for file IO
################################################################################

# Set working directory.
# setwd('/home/ec2-user/saaq')
setwd('~/Research/SAAQ/')

# The original data are stored in 'SAAQdata_full/'.
dataInPath <- 'SAAQdata_full/'

# The data of demerit point counts are stored in 'SAAQdata/seqData/'.
dataOutPath <- 'SAAQspeeding/SAAQspeeding/'

# Set version of output file.
# ptsVersion <- 1 # Original version with only the present ticket counts.
ptsVersion <- 2 # Modified version with both past and present ticket counts.


# Set version of output file.
# agg_out_version <- 1 # Original version with only the present ticket counts.
# agg_out_version <- 2 # Modified version with both past and present ticket counts.
agg_out_version <- 3 # Modified to include pre-policy points indicator.


# Set the list of years for inclusion in dataset.
# yearList <- seq(1999,2000) # First two years.
# yearList <- seq(1999,2003) # First few years.
yearList <- seq(1998,2010) # Entire list.

# Set parameters for tables.
april_fools_2008 <- '2008-04-01'
# No joke: policy change on April Fool's Day!



################################################################################
# Contents of available datasets
################################################################################


# The 'csYYYY.dta' files contain traffic violations.
#   Observations cover years 1998-2010, with 550,000-1,050,000 obsns/yr.,
#   with 10,545,473 osns in total.
#   pddobt is the number of points.
#   dinf is the YYYY-MM-DD date of infraction.
#   dcon is the YYYY-MM-DD date of conviction.
#   seq is an id code for licensees, numbered 1 to 3,911,743.
# The 'saYYYY.dta' files contain licence revocations.
#   Observations cover years 1999-2010, with 10,000-30,000 obsns/yr.,
#   with 238,091 osns in total.
#   orig is a character array, either 'PIP' or 'PDI'.
#     PDI refers to a regular license revocation (15 pts).
#     PIP refers to a learner license revocation (4 pts).
#   dvig is the YYYY-MM-DD date of revocation.
#   seq is an id code for licensees, numbered 1 to 3,911,743.
# The file 'seq.dta' contains licensee data for all 3,911,743 individuals.
#   sxz is either 1.0 (male) 2.0 (female), an indicator for gender.
#   an is the YY year of birth.
#   mois is the MM month of birth.
#   day is the DD day of birth.
#   seq is an id code for licensees, numbered 1 to 3,911,743.
# The file 'seq3.dta' contains licensee data for all 3,911,743 individuals,
#   with the seq equal to the row number (and stripped from the dataset).
#   sxz is either 1.0 2.0, an indicator for male or female.
#   dob is an integer for dates of birth, from 1960-01-01.




################################################################################
# Assemble and Join Tickets with Licensee Data
################################################################################

# Load Licensee Data
fileName <- sprintf('%s%s.dta', dataInPath, 'seq')
drivers <- read.dta(fileName)

head(drivers)
tail(drivers)


# Verify that seq match rownumbers.
sum(drivers[, 'seq'] != seq(nrow(drivers)))
# Check.


# Initialize dataset with the first year of violations.
yr <- yearList[1]
fileName <- sprintf('%s%s%d.dta', dataInPath, 'cs', yr)
tickets <- read.dta(fileName)

# Reorder by date (one year at a time).
tickets <- tickets[order(tickets$dinf), ]

colnames(tickets)
head(tickets)

# Join driver data.
tickets <- cbind(tickets, drivers[tickets[, 'seq'], ])
# Replace sex with factor.
tickets[, 'sex'] <- factor(levels = c('M', 'F'))
tickets[tickets[, 'sxz'] == 1, 'sex'] <- 'M'
tickets[tickets[, 'sxz'] == 2, 'sex'] <- 'F'

colnames(tickets)
head(tickets)
tail(tickets)


# Create new variables and variable names.
saaq <- tickets[, c('seq', 'sex', 'an', 'mois', 'jour', 'dinf', 'pddobt', 'dcon')]
colnames(saaq) <- c('seq', 'sex', 'dob_yr', 'dob_mo', 'dob_day', 'dinf', 'points', 'dcon')


# Check format.
colnames(saaq)
head(saaq)
tail(saaq)


# Join tickets and driver data for remaining years.
# yr <- yearList[2]
for (yr in yearList[2:length(yearList)]) {

  # Print progress report.
  print(sprintf('Now loading data for year %d', yr))

  # Load the next set of violations for this yr.
  fileName <- sprintf('%s%s%d.dta', dataInPath, 'cs', yr)
  tickets <- read.dta(fileName)


  # Reorder by date (one year at a time).
  tickets <- tickets[order(tickets$dinf), ]


  # Join driver data.
  tickets <- cbind(tickets, drivers[tickets[, 'seq'], ])
  # Replace sex with factor.
  tickets[, 'sex'] <- factor(levels = c('M', 'F'))
  tickets[tickets[, 'sxz'] == 1, 'sex'] <- 'M'
  tickets[tickets[, 'sxz'] == 2, 'sex'] <- 'F'


  # Create new variables and variable names.
  saaq_yr <- tickets[, c('seq', 'sex', 'an', 'mois', 'jour', 'dinf', 'pddobt', 'dcon')]
  colnames(saaq_yr) <- c('seq', 'sex', 'dob_yr', 'dob_mo', 'dob_day', 'dinf', 'points', 'dcon')


  # Append to current dataset.
  saaq <- rbind(saaq, saaq_yr)


}

# Check format.
colnames(saaq)
head(saaq)
tail(saaq)


# Clean up intermediate datasets.
saaq_yr <- NULL
tickets <- NULL

################################################################################
# Generate data By age group
################################################################################

# Age in years by date of infraction.
saaq[, 'age'] <- as.integer(substr(saaq[, 'dinf'], 1, 4)) - (1900 + saaq[, 'dob_yr'])

summary(saaq[, 'age'])
sum(saaq[, 'age'] < 10)
saaq[saaq[, 'age'] < 10, ]

# One person of age zero is actually 100 years old (born in 1899).
saaq[saaq[, 'age'] < 10, 'age'] <- 100
saaq[saaq[, 'age'] == 100, ]

# Any others?
sum(saaq[, 'age'] > 90)
sum(saaq[, 'age'] < 12)
sum(saaq[, 'age'] < 13)
# 3 12-year-olds (or 112-year-olds?).

sum(saaq[, 'age'] < 16)

# Sort into age groups to match SAAQ categories.

# Moins de 16 ans
# 16-19 ans
# 20-24 ans
# 25-34 ans
# 35-44 ans
# 45-54 ans
# 55-64 ans
# 65-74 ans
# 75-84 ans
# 85-89 ans
# 90 ans ou plus

age_group_list <- c('0-15', '16-19', '20-24', '25-34', '35-44', '45-54',
                    '55-64', '65-74', '75-84', '85-89', '90-199')
age_cut_points <- c(0, 15.5, 19.5, seq(24.5, 84.5, by = 10), 89.5, 199)
# saaq[, 'age_grp'] <- factor(levels = age_group_list)
saaq[, 'age_grp'] <- cut(saaq[, 'age'], breaks = age_cut_points,
                         labels = age_group_list)

# Verify definitions of age categories.
summary(saaq[saaq[, 'age_grp'] == '0-15', 'age'])

summary(saaq[saaq[, 'age_grp'] == '55-64', 'age'])

summary(saaq[saaq[, 'age_grp'] == '90-199', 'age'])


################################################################################
# Summary Stats for  Tickets with Licensee Data
################################################################################

# # Demographics
# table(saaq[, 'age_grp'], useNA = 'ifany')
# table(saaq[, 'sex'], saaq[, 'age_grp'], useNA = 'ifany')
#
# table(saaq[, 'points'], saaq[, 'age_grp'], useNA = 'ifany')
#
#
#
# # Tabulate point distribution for subgroups.
#
# # Before policy change.
# sel_obs <- saaq[, 'dinf'] <= april_fools_2008
# table(saaq[sel_obs, 'points'], saaq[sel_obs, 'age_grp'], useNA = 'ifany')
#
# # Males before policy change.
# sel_obs <- saaq[, 'sex'] == 'M' & saaq[, 'dinf'] <= april_fools_2008
# table(saaq[sel_obs, 'points'], saaq[sel_obs, 'age_grp'], useNA = 'ifany')
#
# # Females before policy change.
# sel_obs <- saaq[, 'sex'] == 'F' & saaq[, 'dinf'] <= april_fools_2008
# table(saaq[sel_obs, 'points'], saaq[sel_obs, 'age_grp'], useNA = 'ifany')
#
#
#
# # After policy change.
# sel_obs <- saaq[, 'dinf'] > april_fools_2008
# table(saaq[sel_obs, 'points'], saaq[sel_obs, 'age_grp'], useNA = 'ifany')
#
# # Males after policy change.
# sel_obs <- saaq[, 'sex'] == 'M' & saaq[, 'dinf'] > april_fools_2008
# table(saaq[sel_obs, 'points'], saaq[sel_obs, 'age_grp'], useNA = 'ifany')
#
# # Females after policy change.
# sel_obs <- saaq[, 'sex'] == 'F' & saaq[, 'dinf'] > april_fools_2008
# table(saaq[sel_obs, 'points'], saaq[sel_obs, 'age_grp'], useNA = 'ifany')
#
#
# # Compare males and females by age group (better to compare with woman on top).
# # Select subset of offenses that apply to females.
# sel_obs_num <- saaq[, 'sex'] == 'F' &
#   saaq[, 'dinf'] > april_fools_2008 &
#   saaq[, 'points'] %in% c(seq(1, 7), 9, 10, 12, 14, 18, 24)
# table(saaq[sel_obs_num, 'points'], saaq[sel_obs_num, 'age_grp'], useNA = 'ifany')
# sel_obs_denom <- saaq[, 'sex'] == 'M' &
#   saaq[, 'dinf'] > april_fools_2008 &
#   saaq[, 'points'] %in% c(seq(1, 7), 9, 10, 12, 14, 18, 24)
# table(saaq[sel_obs_denom, 'points'], saaq[sel_obs_denom, 'age_grp'], useNA = 'ifany')
#
# table(saaq[sel_obs_num, 'points'], saaq[sel_obs_num, 'age_grp'], useNA = 'ifany') /
#   table(saaq[sel_obs_denom, 'points'], saaq[sel_obs_denom, 'age_grp'], useNA = 'ifany')
#
#
# # Repeat for violations before policy change.
#
# # Compare males and females by age group (better to compare with woman on top).
# # Select subset of offenses that apply to females.
# sel_obs_num <- saaq[, 'sex'] == 'F' &
#   saaq[, 'dinf'] <= april_fools_2008 &
#   saaq[, 'points'] %in% c(seq(1, 7), 9, 10, 12, 14, 15)
# table(saaq[sel_obs_num, 'points'], saaq[sel_obs_num, 'age_grp'], useNA = 'ifany')
# sel_obs_denom <- saaq[, 'sex'] == 'M' &
#   saaq[, 'dinf'] <= april_fools_2008 &
#   saaq[, 'points'] %in% c(seq(1, 7), 9, 10, 12, 14, 15)
# table(saaq[sel_obs_denom, 'points'], saaq[sel_obs_denom, 'age_grp'], useNA = 'ifany')
#
# table(saaq[sel_obs_num, 'points'], saaq[sel_obs_num, 'age_grp'], useNA = 'ifany') /
#   table(saaq[sel_obs_denom, 'points'], saaq[sel_obs_denom, 'age_grp'], useNA = 'ifany')
#


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
# But wait! There are many point categories as well.

table(saaq[, 'sex'], saaq[, 'age_grp'], useNA = 'ifany')


# Ok. For real. Add a new column to the event dataset.
colnames(saaq)



#--------------------------------------------------------------------------------
# Generate counts of cumulative point totals.
#--------------------------------------------------------------------------------

# Note that these are historic totals with no expiry date.

# Sort by date and seq.
saaq <- saaq[order(saaq$seq, saaq$dinf), ]
head(saaq, 10)

# Create a data table to calculate cumulative points balances.
# saaq_dt <- data.table(saaq)
# Join with leading dataset with negative points balances.


# Stack two copies of point events.
# One is the original, when points are added.
# The other is a copy, two years later, when points are removed.


# Calculate cumulative points total by driver.
saaq_dt <- data.table(saaq)
# Translate into the drops in points two years later.
saaq_dt[, dinf := as.Date(dinf + 730)]
saaq_dt[, age := age + 2]
saaq_dt[, points := - points]
head(saaq_dt, 10)
# Append the original observations, then sort.
saaq_dt <- rbind(saaq_dt, data.table(saaq))
saaq_dt <- saaq_dt[order(saaq_dt$seq,
                         saaq_dt$dinf,
                         saaq_dt$points)]
head(saaq_dt, 10)



# Calculate point balances.
saaq_dt[, curr_pts := cumsum(points)]
head(saaq_dt, 20)


# Then drop the duplicate values.
saaq_dt <- saaq_dt[points > 0, ]


# Then compare with saaq to verify accuracy.
summary(saaq)
summary(saaq_dt)



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
#
# # Calculate cumulative points total by driver.
# saaq_past_pts <- data.table(saaq[, c('seq', 'sex', 'age', 'dinf', 'points')])
# # Translate into the drops in points two years later.
# saaq_past_pts[, dinf := as.Date(dinf + 730)]
# saaq_past_pts[, age := age + 2]
# saaq_past_pts[, points := - points]
# head(saaq_past_pts, 10)
# # Append the original observations, then sort.
# saaq_past_pts <- rbind(saaq_past_pts,
#                        data.table(saaq[, c('seq', 'sex', 'age', 'dinf', 'points')]))
# saaq_past_pts <- saaq_past_pts[order(saaq_past_pts$seq,
#                                      saaq_past_pts$dinf,
#                                      saaq_past_pts$points)]
# head(saaq_past_pts, 10)
#
#
#
# # Calculate point balances.
# saaq_past_pts[, curr_pts := cumsum(points)]
# head(saaq_past_pts, 100)
#
#
# # Not necessary to reset to zero for each individual,
# # once points are removed later.
# # saaq_past_pts[, beg_pts := min(cum_pts), by = seq]
# # saaq_past_pts[, past_pts := cum_pts - beg_pts]
#
# # The only remaining adjustment is to remove the current point
# # so that the units represent past points history.
# # saaq_past_pts[points > 0, curr_pts := curr_pts - points]
# # head(saaq_past_pts, 100)
#
# # No. This is for defining the rest of the population, not the ticket-getters.
# # These figures should be lagged one day, instead.
#
# saaq_past_pts[points > 0, dinf := as.Date(dinf + 1)]
#
# summary(saaq_past_pts)
#
#
#
#
# #--------------------------------------------------------------------------------
# # Analysis of two-year point total balances
# #--------------------------------------------------------------------------------
#
#
# # # List the possible values.
# # past_pts_list <- unique(saaq_past_pts[, curr_pts])
# # past_pts_list <- past_pts_list[order(past_pts_list)]
# # # Every number up to 110, then more sparse up to 150.
# #
# # # Inspect the distribution to choose categories.
# # quantile(saaq_past_pts[, curr_pts], probs = seq(0, 1, by = 0.1))
# # # 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
# # # 0    0    0    1    2    3    3    4    6    8  150
# #
# # quantile(saaq_past_pts[, curr_pts], probs = seq(0.9, 1, by = 0.01))
# # # 90%  91%  92%  93%  94%  95%  96%  97%  98%  99% 100%
# # # 8    9    9   10   10   11   12   13   15   18  150
# #
# # quantile(saaq_past_pts[, curr_pts], probs = seq(0.99, 1, by = 0.001))
# # # 99% 99.1% 99.2% 99.3% 99.4% 99.5% 99.6% 99.7% 99.8% 99.9%  100%
# # # 18    18    19    20    21    22    23    25    27    32   150
# #
# #
# # # 0-10 gets up to the 95 percentile.
# # # 15 gets to 98th percentile.
# # # 20 gets inside 99th percentile.
# # # 30 gets to 99.9%.
#
#
#
# #--------------------------------------------------------------------------------
# # Categorization of point total balances
# #--------------------------------------------------------------------------------
#
# # Categories:
# # 0-10 separately, for granularity.
# # 11-20 for next category.
# # 21-30 for next category.
# # 31+ for last category.
#
# # saaq_past_pts[, curr_pts_grp := as.factor(NA, levels = c(seq(0,10), '11-20', '21-30', '31-150'))]
# saaq_past_pts[, curr_pts_grp := '-99']
# head(saaq_past_pts, 20)
# saaq_past_pts[curr_pts <= 10, curr_pts_grp := as.character(curr_pts)]
# saaq_past_pts[curr_pts > 10 & curr_pts <= 20,
#               curr_pts_grp := '11-20']
# saaq_past_pts[curr_pts > 20 & curr_pts <= 30,
#               curr_pts_grp := '21-30']
# saaq_past_pts[curr_pts > 30,
#               curr_pts_grp := '30-150']
#
#
# table(saaq_past_pts[, curr_pts_grp], useNA = 'ifany')
#
curr_pts_grp_list <- c(as.character(seq(0, 10)), '11-20', '21-30', '30-150')
#
#
# #--------------------------------------------------------------------------------
# # Categorization of age groups
# #--------------------------------------------------------------------------------
#
#
# # Now calculate age groups as before.
#
# age_group_list <- c('0-15', '16-19', '20-24', '25-34', '35-44', '45-54',
#                     '55-64', '65-74', '75-84', '85-89', '90-199')
# age_cut_points <- c(0, 15.5, 19.5, seq(24.5, 84.5, by = 10), 89.5, 199)
# # saaq[, 'age_grp'] <- factor(levels = age_group_list)
# saaq_past_pts[, age_grp := cut(age, breaks = age_cut_points,
#                          labels = age_group_list)]
#
# summary(saaq_past_pts[age_grp == '0-15', age])
# summary(saaq_past_pts[age_grp == '55-64', age])
# summary(saaq_past_pts[age_grp == '90-199', age])
#
# head(saaq_past_pts, 20)
#
# # Now this dataset can be used to calculate counts by category.
#
#
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
# beg_date <- '2004-01-01'
# beg_date_num <- which(date_list == beg_date)
# # end_date <- '2010-12-31'
# end_date <- '2004-12-31'
# end_date_num <- which(date_list == end_date)
#
# # Loop on dates and calculate the totals.
# # date_num <- 2
# # date_num_list <- 2:length(date_list)
# # date_num_list <- 2:100
# date_num_list <- beg_date_num:end_date_num
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
#
#
#
#
#
# # Save for later.
# counts_version <- 1
# # counts_version <- 3
# out_file_name <- sprintf('saaq_past_counts_temp_%d_%s_%s_v%d.csv',
#                          ptsVersion,
#                          substr(beg_date, 1, 4), substr(end_date, 1, 4),
#                          counts_version)
# out_path_file_name <- sprintf('%s%s', dataInPath, out_file_name)
# # Yes, keep it in dataInPath since it is yet to be joined.
# write.csv(x = saaq_past_counts, file = out_path_file_name, row.names = FALSE)
#

# Read a dataset tabulated elsewhere.
# counts_version <- 3 # Before adding past_active
counts_version <- 4 # After adding past_active
in_file_name <- sprintf('saaq_past_counts_temp_%d_%s_%s_v%d.csv',
                         ptsVersion,
                        1998, 2010, # The full monty.
                        # 2004, 2004, # The test with one year of history.
                         counts_version)
# data_count_path <- 'SAAQ_counts/'
# Back to in path.
data_count_path <- 'SAAQdata_full/'
in_path_file_name <- sprintf('%s%s', data_count_path, in_file_name)
saaq_past_counts <- data.table(read.csv(file = in_path_file_name))

# Adjust dataset to the original state.
summary(saaq_past_counts)
saaq_past_counts[, date := as.Date(date)]


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
# out_path_file_name <- sprintf('%s/%s', dataInPath, in_file_name)
# # Yes, keep it in dataInPath since it is yet to be joined.
# write.csv(x = saaq_past_counts_sum, file = out_path_file_name, row.names = FALSE)


# # Read a dataset tabulated elsewhere.
# counts_version <- 3
# in_file_name <- sprintf('saaq_past_counts_%d_%s_%s_v%d.csv',
#                          ptsVersion,
#                          1998, 2010,
#                          counts_version)
# data_count_path <- 'SAAQ_counts/'
# in_path_file_name <- sprintf('%s%s', data_count_path, in_file_name)
# saaq_past_counts_2 <- data.table(read.csv(file = in_path_file_name))


# nrow(saaq_past_counts_2)
# nrow(saaq_past_counts)



################################################################################
# Aggregate by sex and age and point categories.
################################################################################

colnames(saaq)
colnames(saaq_dt)

# Original with no points groups:
# agg_var_list <- c('dinf', 'sex', 'age_grp', 'points')
# saaq[, 'num'] <- 1

# Next version with current points groups:
# agg_var_list <- c('dinf', 'sex', 'age_grp', 'curr_pts_grp', 'points')

# Next version with current points groups and indicator
# for pre-policy-change mid-to-high points balance:
agg_var_list <- c('dinf', 'sex', 'age_grp', 'curr_pts_grp', 'past_active', 'points')



# Reverse order of columns (easier to reorder).
# saaq_agg <- aggregate(num ~ points + age_grp + sex + dinf,
#                       data = saaq[, c(agg_var_list, 'num')],
#                       FUN = sum)

saaq_agg <- saaq_dt[, .N, by = agg_var_list]

colnames(saaq_agg) <- c(agg_var_list, 'num')
colnames(saaq_agg)

head(saaq_agg, 50)
tail(saaq_agg, 50)

summary(saaq_agg)



# Change type to factor.
saaq_agg[, curr_pts_grp := as.factor(curr_pts_grp)]


################################################################################
# Load Daily Driver Counts
################################################################################

# Driver population includes drivers with no past tickets or current points.


no_tickets_ptsVersion <- 1
in_file_name <- sprintf('saaq_no_tickets_%d.csv', no_tickets_ptsVersion)
in_path_file_name <- sprintf('%s/%s', dataInPath, in_file_name)
# Yes, keep it in dataInPath since it is yet to be joined.
# write.csv(x = no_tickets_df, file = out_path_file_name, row.names = FALSE)
no_tickets_df <- read.csv(file = in_path_file_name)


# Need to add empty column of curr_pts_grp to
# default population with no tickets.
no_tickets_df[, 'curr_pts_grp'] <- 0

# Also need to add empty column of past_active to
# default population with no tickets.
no_tickets_df[, 'past_active'] <- FALSE


colnames(saaq_agg)
colnames(no_tickets_df)


# Change it to a data table.
no_tickets_dt <- data.table(no_tickets_df[, c(agg_var_list, 'num')])

# Count observations.
nrow(no_tickets_dt)/length(date_list)

table(no_tickets_dt[, age_grp], no_tickets_dt[, sex])
# Every sex-age_grp combination.
# All have zero curr_pts and no seq.

# Coerce dinf to date format.
no_tickets_dt[, dinf := as.Date(dinf)]
levels(saaq_past_counts_sum[, curr_pts_grp])
no_tickets_dt[, curr_pts_grp := as.factor(curr_pts_grp)]


################################################################################
# Revise Daily Driver Counts
################################################################################


#--------------------------------------------------------------------------------
# Subtract drivers with point balances from general population
# Avoid double-counting them in the no-event-ever population
#--------------------------------------------------------------------------------

# Drivers with current points must be subtracted from
# population of drivers.


colnames(no_tickets_dt)
summary(no_tickets_dt)


colnames(saaq_past_counts_sum)
summary(saaq_past_counts_sum)

# Be careful to exclude zero current points from the subtraction.
summary(saaq_past_counts_sum[curr_pts_grp != 0, ])


# Create aggregated version by curr_pts_grp to subtract from no_tickets_dt.
past_agg_var_list <- c("date", "sex", "age_grp")
saaq_past_counts_no_tickets <-
  saaq_past_counts_sum[curr_pts_grp != 0,
                       sum(num),  by = past_agg_var_list]

colnames(saaq_past_counts_no_tickets) <- c(past_agg_var_list, 'num')
colnames(saaq_past_counts_no_tickets)


# Need to sort them in the same order before subracting.
# Before past_active:
# no_tickets_dt <- no_tickets_dt[order(dinf, sex, age_grp, curr_pts_grp), ]
# After past_active:
no_tickets_dt <- no_tickets_dt[order(dinf, sex, age_grp, curr_pts_grp, past_active), ]

head(no_tickets_dt, 20)
tail(no_tickets_dt, 20)

saaq_past_counts_no_tickets <- saaq_past_counts_no_tickets[order(date, sex, age_grp), ]

head(saaq_past_counts_no_tickets, 50)
tail(saaq_past_counts_no_tickets, 50)

# Compare to make sure rows are aligned.
nrow(no_tickets_dt)
nrow(saaq_past_counts_no_tickets)

# Compare row by row before subtracting.
summary(no_tickets_dt[, c('dinf', 'sex', 'age_grp')])
summary(saaq_past_counts_no_tickets[, c('date', 'sex', 'age_grp')])


# Compare data types.
sapply(no_tickets_dt, class)
sapply(saaq_past_counts_no_tickets, class)


# Count differences.
summary(no_tickets_dt[, c('dinf', 'sex', 'age_grp')] ==
          saaq_past_counts_no_tickets[, c('date', 'sex', 'age_grp')])


# Check that the totals are compatible.
summary(no_tickets_dt[, num] - saaq_past_counts_no_tickets[, num])
sum(no_tickets_dt[, num] < saaq_past_counts_no_tickets[, num])

# Some were not compatible when zeros were included.
summary(no_tickets_dt[no_tickets_dt[, num] < saaq_past_counts_no_tickets[, num], ])
# All of these were males aged 20-24.
summary(no_tickets_dt[no_tickets_dt[, num] < saaq_past_counts_no_tickets[, num], num] -
          saaq_past_counts_no_tickets[no_tickets_dt[, num] < saaq_past_counts_no_tickets[, num], num])
# On many days, there were several more males 20-24
# who have gotten tickets
# than there were males 20-24 who are licenced in Quebec.


# Submit the hooligans to closer inspection.
summary(saaq_past_counts_no_tickets[no_tickets_dt[, num] < saaq_past_counts_no_tickets[, num], ])
summary(saaq_past_counts_no_tickets[
  saaq_past_counts_no_tickets[, date] >= '2006-04-01' &
    saaq_past_counts_no_tickets[, date] <= '2010-03-31' &
    no_tickets_dt[, num] < saaq_past_counts_no_tickets[, num], ])


# Compare the fractions of driver categories with tickets.
# i.e. check that the ratios are sensible.
summary(saaq_past_counts_no_tickets[, num] / no_tickets_dt[, num])
# Penis effect shows up in nearly 60% event rate.
summary(no_tickets_dt[saaq_past_counts_no_tickets[, num] /
                        no_tickets_dt[, num] > 0.5, ])
# Again, males 20-24, of course.
# Makes sense.
summary(no_tickets_dt[saaq_past_counts_no_tickets[, num] /
                        no_tickets_dt[, num] > 0.4, ])
summary(no_tickets_dt[saaq_past_counts_no_tickets[, num] /
                        no_tickets_dt[, num] > 0.28, ])
# Who are these 14 female categories with 28% experience rate?
no_tickets_dt[saaq_past_counts_no_tickets[, num] /
                no_tickets_dt[, num] > 0.28 & sex == 'F', ]
# Females 20-24, only in the summer of 2008.

# At least two factors are at play:
# 1. Drivers are getting tickets without a license.
# 2. Visitors to Quebec are getting tickets during their travels.

# In addition to the third:
# 3. Male 20-24 drivers who had tickets are
# recorded in the saaq_past_pts data table, even though
# their tickets are no longer current.

# However, there should be one last check after imposing
# the aggregate sums excluding zero curr_pts.

# Decision:
# Either: Take raw sum of drivers in all three tables.
# Or: Remove zeros and keep as is.


# Finally, subtract counts from no_tickets.
# Actually perform the subtraction, if this option chosen.
summary(no_tickets_dt[, num] - saaq_past_counts_no_tickets[, num])
no_tickets_dt[, 'num'] <- no_tickets_dt[, num] -
  saaq_past_counts_no_tickets[, num]
summary(no_tickets_dt)

# Essentially, change them from "no tickets ever"
# to "no tickets right now"

#--------------------------------------------------------------------------------
# Subtract drivers with ticket events from population .
# Goal is to obtain an accurate measurement of the population without events.
#--------------------------------------------------------------------------------


# Drivers with tickets must be subtracted from entire population,
# depending on their current point balances.



################################################################################
# Join Daily Driver Non-events
################################################################################


# First, join all the no ticket observations together.
# This includes both drivers with current point balances
# and those with no tickets ever.


#--------------------------------------------------------------------------------
# Verify and correct for compatibility before joining
#--------------------------------------------------------------------------------


colnames(no_tickets_dt)
colnames(saaq_past_counts_no_tickets)
colnames(saaq_past_counts_sum)
colnames(saaq_agg)

nrow(no_tickets_dt)
nrow(saaq_past_counts_sum)

# Need to add zero point observation (non-ticket,  non-events).
saaq_past_counts_sum[, points := 0]


# Verify compatibility.
summary(no_tickets_dt)
summary(saaq_past_counts_sum)

# Rearrange columns and rename date.
saaq_past_counts_sum[, dinf := date]


#--------------------------------------------------------------------------------
# Join all population data into a single table.
#--------------------------------------------------------------------------------


# Select columns from no_tickets_dt in same order as saaq_agg.
summary(no_tickets_dt[, c(agg_var_list, 'num'), with = FALSE])

# Select columns from saaq_past_counts_sum in same order as saaq_agg.
summary(saaq_past_counts_sum[, c(agg_var_list, 'num'), with = FALSE])





saaq_no_tickets_full <- rbind(no_tickets_dt[, c(agg_var_list, 'num'), with = FALSE],
                              saaq_past_counts_sum[, c(agg_var_list, 'num'), with = FALSE])

# Number of rows is just straight adding.
nrow(no_tickets_dt)
nrow(saaq_past_counts_sum)
nrow(saaq_no_tickets_full)
nrow(no_tickets_dt) + nrow(saaq_past_counts_sum)

# Record population sum to make sure.
saaq_no_tickets_full[curr_pts_grp == 0, sum(num), by = c('sex', 'age_grp')]
# sex age_grp         V1
# 1:   F    0-15    9760511
# 2:   F   16-19  266737812
# 3:   F   20-24  759024486
# 4:   F   25-34 1892535101
# 5:   F   35-44 2426706330
# 6:   F   45-54 2476404143
# 7:   F   55-64 1730072906
# 8:   F   65-74  911016741
# 9:   F   75-84  323325061
# 10:   F   85-89   24019742
# 11:   F  90-199    2746683
# 12:   M    0-15   32904554
# 13:   M   16-19  265529310
# 14:   M   20-24  684785694
# 15:   M   25-34 1844236521
# 16:   M   35-44 2450028151
# 17:   M   45-54 2559199649
# 18:   M   55-64 1932198963
# 19:   M   65-74 1184065465
# 20:   M   75-84  530249986
# 21:   M   85-89   56979787
# 22:   M  90-199    9539596
# sex age_grp         V1



# Aggregate observations from each table with curr_pts_grp == 0.
# Some of these come from both tables.
summary(saaq_past_counts_sum[curr_pts_grp == 0, c(agg_var_list, 'num'), with = FALSE])

saaq_no_tickets_full <- saaq_no_tickets_full[, num := sum(num), by = agg_var_list]


summary(saaq_no_tickets_full)
# There are now double the observations in the zero category,
# one from each table.
saaq_no_tickets_full <- unique(saaq_no_tickets_full)


# Expect the number of rows to be equal again.
nrow(saaq_past_counts_sum)
nrow(saaq_no_tickets_full)
nrow(saaq_no_tickets_full) - nrow(saaq_past_counts_sum)


# Expect the population totals to match the above for curr_pts_grp == 0.
saaq_no_tickets_full[curr_pts_grp == 0, sum(num), by = c('sex', 'age_grp')]



#--------------------------------------------------------------------------------
# Now subtract event counts from non-events.
#--------------------------------------------------------------------------------

# Verify alignment of columns and rows.
colnames(saaq_agg)
colnames(saaq_no_tickets_full)

nrow(saaq_agg)
nrow(saaq_no_tickets_full)

# Need to aggregate population across point events.
# saaq_agg_pop <- unique(saaq_agg[, sum(num), by = c(agg_var_list[1:4])])
# colnames(saaq_agg_pop) <- c(agg_var_list[1:4], 'num')
# All but the last variable: points.
saaq_agg_pop <- unique(saaq_agg[, sum(num), by = c(agg_var_list[-length(agg_var_list)])])
colnames(saaq_agg_pop) <- c(agg_var_list[-length(agg_var_list)], 'num')

summary(saaq_agg_pop)
nrow(saaq_agg_pop)

nrow(saaq_no_tickets_full)
# Ticket events occur in about half of all possible categories.

# Merge to compare counts side-by-side.
# All but the last variable: points.
# saaq_agg_pop <- merge(saaq_no_tickets_full, saaq_agg_pop,
#                       by = c(agg_var_list[1:4]), all = TRUE)

# All but the last variable: points.
saaq_agg_pop <- merge(saaq_no_tickets_full, saaq_agg_pop,
                      by = c(agg_var_list[-length(agg_var_list)]), all = TRUE)

# Number of rows should match full population from no_tickets.
nrow(saaq_agg_pop)
# Overwrite missing categories with zero.
saaq_agg_pop[is.na(num.y), num.y := 0]


# Count differences.
summary(saaq_agg_pop)
saaq_agg_pop[, num := num.x - num.y]
summary(saaq_agg_pop)



# A sizeable number of zeros.
summary(saaq_agg_pop[num == 0, ])

# A small number of negatives (34 to be exact).
summary(saaq_agg_pop[num < 0, ])
# Waaaaaaayyyy more after adding past_active.
# In the thousands 10,289: not crazy, given the sample size.

saaq_agg_pop[num < 0, ]
# This should not be possible.
# Even though it occurrs for a very small number of observations.
# But there are unlicensed drivers and travelers increasing the counts of
# drivers with points.

# Hold off on this subtraction for now.
# Join


################################################################################
# Join Daily Driver Events with Full Dataset of Non-events
################################################################################

#--------------------------------------------------------------------------------
# Verify and correct for compatibility before joining
#--------------------------------------------------------------------------------

# Component datasets (for reference).
# colnames(no_tickets_dt)
# colnames(saaq_past_counts_no_tickets)
# colnames(saaq_past_counts_sum)

# Events and non-events (to be joined here).
colnames(saaq_agg)
colnames(saaq_no_tickets_full)

# Count rows.
# nrow(no_tickets_dt)
# nrow(saaq_past_counts_sum)
nrow(saaq_agg)
nrow(saaq_no_tickets_full)


# Verify compatibility.
summary(saaq_agg)
summary(saaq_no_tickets_full)
# Note that some categories of non-events have no occurrances.
summary(saaq_no_tickets_full[num == 0, ])
# Everyone has to take a day off once in a while.
table(saaq_no_tickets_full[num == 0 & dinf > as.Date('2000-01-01'), curr_pts_grp],
      saaq_no_tickets_full[num == 0 & dinf > as.Date('2000-01-01'), sex])
table(saaq_no_tickets_full[num == 0 & dinf > as.Date('2000-01-01'), curr_pts_grp],
      saaq_no_tickets_full[num == 0 & dinf > as.Date('2000-01-01'), age_grp])
# Makes sense! They are all either the crazy high point balance or old age categories!
# Good. No obvious flaw in the data. Sensible characteristics prevail.

# Rearrange columns and rename date.
# saaq_past_counts_sum[, dinf := date]
# Should have been done already.


# Change type to factor.
# saaq_agg[, curr_pts_grp := as.factor(curr_pts_grp)]
# Should have been done with creation of saaq_agg.


#--------------------------------------------------------------------------------
# Final inspection and join
#--------------------------------------------------------------------------------


# Select columns from saaq_agg.
summary(saaq_agg[, c(agg_var_list, 'num'), with = FALSE])


# Select columns from saaq_no_tickets_full in same order as saaq_agg.
summary(saaq_no_tickets_full[, c(agg_var_list, 'num'), with = FALSE])


# For previous joins:
# Select columns from no_tickets_dt in same order as saaq_agg.
summary(no_tickets_dt[, c(agg_var_list, 'num'), with = FALSE])

# Select columns from saaq_past_counts_sum in same order as saaq_agg.
summary(saaq_past_counts_sum[, c(agg_var_list, 'num'), with = FALSE])


# Stack the data frames with properly ordered columns.

# First version without point balances.
# saaq_agg <- rbind(saaq_agg[, c(agg_var_list, 'num')],
#                   no_tickets_df[, c(agg_var_list, 'num')])

# Second version without non-event correction.
saaq_agg_out <- rbind(saaq_agg[, c(agg_var_list, 'num'), with = FALSE],
                      no_tickets_dt[, c(agg_var_list, 'num'), with = FALSE],
                      saaq_past_counts_sum[, c(agg_var_list, 'num'), with = FALSE])

# Optional version with non-event correction.
# saaq_agg_out <- rbind(saaq_agg[, c(agg_var_list, 'num'), with = FALSE],
#                       no_tickets_dt[, c(agg_var_list, 'num'), with = FALSE],
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

# Past iterations had same tag for intermediate file and output.
# out_file_name <- sprintf('saaq_agg_%d.csv', ptsVersion)


# Check that new output matches the previous version.
# prev_file_name <- sprintf('saaq_agg_%d.csv', ptsVersion)
# prev_path_file_name <- sprintf('%s%s', dataInPath, prev_file_name)
# saaq_agg_prev <- fread(prev_path_file_name,
#                        colClasses = c(dinf = "Date", sex = "factor",
#                                       age_grp = "factor", curr_pts_grp = "factor"))
# summary(saaq_agg_prev)
# summary(saaq_agg_out)
# Checks out.


# Current version has separate tag for aggregated file with
# new variable for past points indicator.
out_file_name <- sprintf('saaq_agg_%d.csv', agg_out_version)
out_path_file_name <- sprintf('%s%s', dataInPath, out_file_name)
# Yes, keep it in dataInPath since it is yet to be joined.
# write.csv(x = saaq_agg_out, file = out_path_file_name, row.names = FALSE)






################################################################################
# End
################################################################################
