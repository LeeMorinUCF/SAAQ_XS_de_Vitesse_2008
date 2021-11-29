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
library(foreign)

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


# The data of counts of licensed drivers are also stored in 'Data/'.
data_out_path <- 'Data'


# Set name of output file for aggregated dataset.
out_file_name <- 'saaq_tickets.csv'


################################################################################
# Set parameters for file IO
################################################################################


# Set version of output file.
# ptsVersion <- 1 # Original version with only the present ticket counts.
# ptsVersion <- 2 # Modified version with both past and present ticket counts.


# Set version of output file.
# agg_out_version <- 1 # Original version with only the present ticket counts.
# agg_out_version <- 2 # Modified version with both past and present ticket counts.
# agg_out_version <- 3 # Modified to include pre-policy points indicator.


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
fileName <- sprintf('%s/%s.dta', data_in_path, 'seq')
drivers <- read.dta(fileName)

head(drivers)
tail(drivers)


# Verify that seq match rownumbers.
sum(drivers[, 'seq'] != seq(nrow(drivers)))
# Check.


# Initialize dataset with the first year of violations.
yr <- yearList[1]
fileName <- sprintf('%s/%s%d.dta', data_in_path, 'cs', yr)
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
  fileName <- sprintf('%s/%s%d.dta', data_in_path, 'cs', yr)
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

# For original LPM and logistic regressions,
# we defined age in years by date of infraction.
# saaq[, 'age'] <- as.integer(substr(saaq[, 'dinf'], 1, 4)) - (1900 + saaq[, 'dob_yr'])

# For fixed effects models, redefine ages to create a fixed age category.
# This makes estimation computationally efficient wit hthe FWL theorem.
# Define age as age in years at time of policy change.
saaq[, 'age'] <- 2008 - (1900 + saaq[, 'dob_yr'])
# This way, the only variable with transitions is the point balances.
# It makes for a one-dimensional state space for the (quasi-) first-order Markov model
# (quasi- because demerit points expire after two years).

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
# Output Dataset of Tickets
################################################################################

# Current version has separate tag for aggregated file with
# new variable for past points indicator.
out_path_file_name <- sprintf('%s/%s', data_out_path, out_file_name)
write.csv(x = saaq, file = out_path_file_name, row.names = FALSE)






################################################################################
# End
################################################################################
