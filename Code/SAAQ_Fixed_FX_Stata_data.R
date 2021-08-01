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
#
#
#
# Lee Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# July 31, 2021
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

# Load PRROC package for calculating area under the ROC curve.
library(PRROC)

# Load xtable library to create tex scripts for tables.
library(xtable)

# Load scales library because it has a function
# to display large numbers in comma format.
library(scales)


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
train_file_name <- 'saaq_train_by_seq.csv'
test_file_name <- 'saaq_test_by_seq.csv'


# Set name of output file for full dataset.
# out_file_name <- 'saaq_out.csv'


set.seed(42)



# Set directory for storing figures.
fig_dir <- 'Figures'

# Set directory for storing tables.
tab_dir <- 'Tables'

# Set directory for storing text output.
text_dir <- 'Text'

# Specify samples in terms of driver activity.
# file_tag <- 'high_pts'
# file_tag <- 'all_pts'
# file_tag_list <- c('all_pts', 'high_pts')
file_tag_list <- c('high_pts')

# Specify subsamples for either male, female, or all drivers
sex_sel_list <- c('A', 'M', 'F')


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

# Weekday indicators can't be used in this version.
# Time dimension is not included in the datasets with entire driver histories.
# # Weekday indicators.
# weekday_list <- c('Sunday',
#                   'Monday',
#                   'Tuesday',
#                   'Wednesday',
#                   'Thursday',
#                   'Friday',
#                   'Saturday')

# Set date of policy change.
april_fools_date <- '2008-04-01'
# No joke: policy change on April Fool's Day!


################################################################################
# Load Datasets
################################################################################

#-------------------------------------------------------------------------------
# Load Training Dataset
#-------------------------------------------------------------------------------

# Dataset for in-sample model fitting.
in_path_file_name <- sprintf('%s/%s', data_in_path, train_file_name)
saaq_train <- fread(in_path_file_name)

summary(saaq_train)




# summary(saaq_train[, .N, by = date])
# head(saaq_train, 394)

# table(saaq_train[, sex], useNA = 'ifany')
#
# table(saaq_train[, age_grp], useNA = 'ifany')
#
# table(saaq_train[, past_active], useNA = 'ifany')
#
# table(saaq_train[, past_active], saaq_train[, sex], useNA = 'ifany')
#
# table(saaq_train[, curr_pts_grp], saaq_train[, past_active], useNA = 'ifany')


# length(unique(saaq_train[, date]))
# # [1] 1461 days of driving.
#
# 2*length(age_group_list)*2*length(curr_pts_grp_list)
# # [1] 392 combinations of categories per day.
#
# # Observations added with observed tickets.
# nrow(saaq_train) - 2*length(age_group_list)*2*length(curr_pts_grp_list)*1826


# Tabulate the points, which are the events to be predicted.
# saaq_train[date >= sample_beg & date <= sample_end,
#                 sum(as.numeric(num)), by = points][order(points)]
saaq_train[, sum(as.numeric(num)), by = points][order(points)]


# #-------------------------------------------------------------------------------
# # Load Testing Dataset
# #-------------------------------------------------------------------------------
#
# # Dataset for out-of-sample model testing.
# in_path_file_name <- sprintf('%s/%s', data_in_path, test_file_name)
# saaq_test <- fread(in_path_file_name)
#
# summary(saaq_test)
#
# # summary(saaq_test[, .N, by = date])
# # head(saaq_test, 394)
# #
# # table(saaq_test[, sex], useNA = 'ifany')
# #
# # table(saaq_test[, age_grp], useNA = 'ifany')
# #
# # table(saaq_test[, past_active], useNA = 'ifany')
# #
# # table(saaq_test[, past_active], saaq_test[, sex], useNA = 'ifany')
# #
# # table(saaq_test[, curr_pts_grp], saaq_test[, past_active], useNA = 'ifany')
#
#
#
# # length(unique(saaq_test[, date]))
# # # [1] 1461 days of driving.
# #
# # 2*length(age_group_list)*2*length(curr_pts_grp_list)
# # # [1] 392 combinations of categories per day.
# #
# # # Observations added with observed tickets.
# # nrow(saaq_test) - 2*length(age_group_list)*2*length(curr_pts_grp_list)*1826
#
#
# # Tabulate the points, which are the events to be predicted.
# # saaq_test[date >= sample_beg & date <= sample_end,
# #           sum(as.numeric(num)), by = points][order(points)]
# saaq_test[, sum(as.numeric(num)), by = points][order(points)]
#

################################################################################
# Stack the datasets and label by sample
################################################################################

saaq_train[, sample := 'train']
# saaq_test[, sample := 'test']
# saaq_data <- rbind(saaq_train, saaq_test)
# rm(saaq_train, saaq_test)

# Testing sample not needed.
saaq_data <- saaq_train

rm(saaq_train)


# Check the craziest driver.
# This one should have 26 tickets in one day.
saaq_data[seq == 2868161, ]



# saaq_data[date >= sample_beg & date <= sample_end,
#           sum(as.numeric(num)), by = points][order(points)]
saaq_data[, sum(as.numeric(num)), by = points][order(points)]


################################################################################
# Define additional variables
################################################################################

# Define categorical variables as factors.
saaq_data[, sex := factor(sex, levels = c('M', 'F'))]
saaq_data[, age_grp := factor(age_grp, levels = age_group_list)]
saaq_data[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]

# With individual fixed effects, the data are aggregated by individual
# but not by date.
# # Define new variables for seasonality.
# # Numeric indicator for month.
# # saaq_data[, 'month'] <- substr(saaq_data[, 'date'], 6, 7)
# saaq_data[, month := substr(date, 6, 7)]
# month_list <- unique(saaq_data[, month])
# month_list <- month_list[order(month_list)]
# saaq_data[, month := factor(month, levels = month_list)]
# table(saaq_data[, 'month'], useNA = "ifany")
#
# # Weekday indicator.
# saaq_data[, weekday := weekdays(date)]
# saaq_data[, weekday := factor(weekday, levels = weekday_list)]
# table(saaq_data[, 'weekday'], useNA = "ifany")

# Define the indicator for the policy change.
saaq_data[, policy := date >= april_fools_date]

# Individual-specific variables will be adjusted out by the
# individual intercept.

# # Create some additional indicators for categories.
#
# # There is more traffic on weekdays.
# # People get more sensible, rational tickets on weekdays.
# # People get more crazy, irrational tickets on weekends.
# saaq_data[, weekend := weekday %in% c('Sunday', 'Saturday')]
# # table(saaq_data[, 'weekday'], saaq_data[, 'weekend'], useNA = "ifany")
# saaq_data[, .N, by = c('weekday', 'weekend')]
#
# # Drivers get fewer tickets in December to January.
# saaq_data[, winter := month %in% c('01', '12')]
# saaq_data[, .N, by = c('month', 'winter')]
#
#
# # Indicators for drivers with no points and many points.
# saaq_data[, zero_curr_pts := curr_pts_grp %in% c('0')]
# saaq_data[, .N, by = c('curr_pts_grp', 'zero_curr_pts')]
# saaq_data[, high_curr_pts := curr_pts_grp %in% c('11-20', '21-30', '31-150')]
# saaq_data[, .N, by = c('curr_pts_grp', 'high_curr_pts')]
#
# # Indicators for the younger or middle age groups.
# # age_group_list
# saaq_data[, young_age := age_grp %in% c('0-19', '20-24')]
# saaq_data[, .N, by = c('age_grp', 'young_age')]
# saaq_data[, mid_age := age_grp %in% c('25-34', '35-44')]
# saaq_data[, .N, by = c('age_grp', 'mid_age')]

#
#
# # Select observations based on past activity.
# if (file_tag == 'all_pts') {
#   saaq_data[, sel_obsn := sample == 'train']
# } else if (file_tag == 'high_pts') {
#   saaq_data[, sel_obsn := past_active == TRUE & sample == 'train']
# } else {
#   stop(sprintf("file_tag '%s' not recognized.", file_tag))
# }
#

# saaq_data[date >= sample_beg & date <= sample_end,
#           sum(as.numeric(num)), by = points][order(points)]



################################################################################
# First stage regressions for fixed effects.
################################################################################


# Each driver has 1461 days of driving.
# num_days <- length(unique(saaq_data[, date]))
# But the number of events differs slightly if they get multiple tickets.
# Use individual-level counts of days in the projections of the driver dummies.

# Calculate denominators by driver for regressions on driver dummies.
# saaq_data[, num_by_seq := sum(num), by = 'seq']
# saaq_data[, num_policy_by_seq := sum(num*policy), by = 'seq']
# head(saaq_data[, c('seq', 'num_by_seq', 'num_policy_by_seq')])
# head(saaq_data[seq > 0, c('seq', 'num_by_seq', 'num_policy_by_seq')])
# summary(saaq_data[, c('seq', 'num_by_seq', 'num_policy_by_seq')])
# summary(saaq_data[seq > 0, c('seq', 'num_by_seq', 'num_policy_by_seq')])



# Define dependent variable:
# All violations combined.
saaq_data[, events := points > 0]

# Notice that dataset of tickets is aggregated.
summary(saaq_data[events == 1, num])
saaq_data[events == 1 & num > 1, .N]
saaq_data[events == 1 & num > 1, ]
# Make sure to weight by number of drivers.

saaq_data[events == 1 & num > 1, sum(num)]


################################################################################
# Convert variables for useable format in Stata
################################################################################

# Convert policy and event variables to binary integers instead of logical.
saaq_data[, policy_int := as.integer(policy)]
saaq_data[, events_int := as.integer(events)]

# Convert seq for non-events to have separate dates by seq.
head(unique(saaq_data[, 'seq']))
tail(unique(saaq_data[, 'seq']))


# The zeros need to be assigned to separate seq numbers by characteristics.
saaq_data[seq == 0, sum(num), by = 'date']
# There are between 1.8 and 2.0 million such drivers throughout the time period.

# But there are only 14 combinations of categories each day.
saaq_data[seq == 0, .N, by = 'date']
summary(saaq_data[seq == 0, .N, by = 'date'])



# Sex is binary in this dataset.
head(saaq_data[, as.integer(sex == 'M')], 20)

# Age groups can be converted to beginning age in each category.
head(saaq_data[, as.numeric(age_grp)], 20)
summary(saaq_data[, as.numeric(age_grp)])
head(saaq_data[, age_group_list[as.numeric(age_grp)]], 20)

head(saaq_data[, age_group_list[as.numeric(age_grp)]], 20)

age_group_beg_list <- as.numeric(gsub("-", "", substring(age_group_list, 1, 2)))



# Initialize a unique identifier for each combination of categories.
saaq_data[, xtseq := seq]



# Make a unique identifier for each combination of categories.
saaq_data[seq == 0, xtseq := 9000000L +
            as.integer(sex == 'M')*100000 +
            age_group_beg_list[as.numeric(age_grp)]*1000]

# Verify the counts of categories.
saaq_data[seq == 0, .N, by = 'xtseq']

head(saaq_data[seq == 0, date, by = 'xtseq'])
tail(saaq_data[seq == 0, date, by = 'xtseq'])

head(saaq_data[xtseq == 9100000, .N, by = 'date'])
tail(saaq_data[xtseq == 9100000, .N, by = 'date'])
summary(saaq_data[xtseq == 9100000, .N, by = 'date'])
# At least each date only appears once for each zero-ticket seq code.

# There are, in fact 1461 days in the sample period.
head(as.Date(seq(as.numeric(as.Date('2006-04-01')),
                 as.numeric(as.Date('2010-03-31'))),
             origin = '1970-01-01'))
tail(as.Date(seq(as.numeric(as.Date('2006-04-01')),
                 as.numeric(as.Date('2010-03-31'))),
             origin = '1970-01-01'))
# There are, in fact 1461 days in the sample period.
length(as.Date(seq(as.numeric(as.Date('2006-04-01')),
                   as.numeric(as.Date('2010-03-31'))),
               origin = '1970-01-01'))





#--------------------------------------------------------------------------------

# Now look at individual drivers with tickets.
# Note that drivers with a ticket have only 1460 dates.
saaq_data[, sum(num), by = 'seq']

# Look at drivers with tickets alone:
summary(saaq_data[seq != 0, sum(num), by = 'seq'])
# Some have 26 more "days," meaning they have multiple tickets on a given day.

# Consider one driver.
saaq_data[seq == 7,]
# Sum of num is 1460.

# Account for multiple tickets in a day by aggregating the num count.
head(saaq_data[seq != 0, sum(num), by = 'seq'][V1 > 1460, seq])
tail(saaq_data[seq != 0, sum(num), by = 'seq'][V1 > 1460, seq])
length(saaq_data[seq != 0, sum(num), by = 'seq'][V1 > 1460, seq])
length(unique(saaq_data[seq != 0, sum(num), by = 'seq'][V1 > 1460, seq]))
# More than 20000 drivers with multiple tickets.


# Most have at most two tickets in a day.
# Some have more than twenty.
saaq_data[seq != 0, sum(num), by = 'seq'][V1 > 1460, .N, by = 'V1']

saaq_data[seq != 0, sum(num), by = 'seq'][V1 > 1460, .N, by = 'V1'][, sum(N)]


# Who is this with 1486 days?
saaq_data[seq != 0, sum(num), by = 'seq'][V1 == 1486, ]
#        seq   V1
# 1: 2868161 1486

# Increase count of drivers with these tickets on each date.
# summary(saaq_data[seq != 0 & events == 1, sum(num), by = c('seq', 'date')])

# Instead: modify the date variable to include time.





# # Select variables for output.
# colnames(saaq_data)
# out_var_list <-  c('date', 'xtseq', 'sex', 'age_grp', 'past_active',
#                    'curr_pts_grp',
#                    'num', 'policy_int', 'events_int',
#                    'sample')
#
# saaq_out <- saaq_data[, out_var_list, with = FALSE]
# # summary(saaq_out)
#
#
# # Aggregate counts by all variables for drivers with multiple tickets.
# saaq_out <- saaq_out[sample == 'train',
#                      num := sum(num), by = c('date', 'xtseq', 'sex', 'age_grp', 'past_active',
#                                              'curr_pts_grp',
#                                              'policy_int', 'events_int',
#                                              'sample')]
#
# # Remove duplicates.
# saaq_out <- unique(saaq_out)
# summary(saaq_out)
#
# # Verify that date and xtseq cmbinations are unique.
# saaq_out[, .N, by = c('xtseq', 'date')]
# summary(saaq_out[, .N, by = c('xtseq', 'date')])
#
#
# saaq_out[, .N, by = c('xtseq', 'date')][, .N, by = 'N']
#
# # Inspect one.
# saaq_out[, .N, by = c('xtseq', 'date')][N == 8, ]
# saaq_out[xtseq == 2304109, ]
# # Problem: curr_pts_grp change throughout the day.
# # So, multiple date combinations are still distinct.



# Need to adjust dates to create distinct dates.
# Make a new format with more granular date variables.

# Of course, there is that one guy with 26 tickets,
# so it is not as simple as tagging the tickets with hours.

# Sort the data by xtseq, then date.
saaq_data <- saaq_data[order(xtseq, date), ]
head(saaq_data)
tail(saaq_data)


# Now generate a cumulative count of tickets.
saaq_data[, cum_tickets := seq(nrow(saaq_data))]
saaq_data[, min_cum_tickets := min(cum_tickets), by = c('xtseq', 'date')]

# Calculate the ticket number per driver day.
# saaq_data[, day_ticket_count := cum_tickets - min_cum_tickets + 1]
saaq_data[, day_ticket_num := cum_tickets - min_cum_tickets + 1]

# Calculate the full count of tickets each driver day.
saaq_data[, day_ticket_count := max(day_ticket_num), by = c('xtseq', 'date')]


# Check counts:
saaq_data[, .N, by = 'day_ticket_num']
# Why only up to 9 tickets in a day?
# And why are there more with 2 tickets than before?
# Need to remove a num := sum(num) aggregation in data prep.


# Inspect a file with multiple tickets in a day.
saaq_data[xtseq == 2304109, ]

# This one should have 26 tickets in one day.
saaq_data[xtseq == 2868161, ]



# Find the driver with 26 tickets.
saaq_data[, .N, by = c('xtseq', 'date')][N == 26, ]
#     xtseq       date  N
# 1: 847237 2007-11-20 26
saaq_data[, .N, by = c('xtseq', 'date')][N == 9, ]






#--------------------------------------------------------------------------------
# Create time-of-day variable from day_ticket_num.
#--------------------------------------------------------------------------------

# Spread tickets evenly over the day from 1:00 AM to 11:00 PM
# (just in case rounding is a problem).
saaq_data[, day_ticket_time_num := 1 + day_ticket_num/day_ticket_count*22]

saaq_data[, day_ticket_time_num]


# Now create a string for the exact time of day.

# Test for the hour of the ticket with arbitrary number of minutes.
saaq_data[, day_ticket_time := sprintf('%d:53',
                                       floor(day_ticket_time_num))]
head(saaq_data[day_ticket_time != '23:53', day_ticket_time], 20)
tail(saaq_data[day_ticket_time != '23:53', day_ticket_time], 20)
table(saaq_data[, day_ticket_time], useNA = 'ifany')


# Test for the minute of the ticket with an arbitrary hour.
# Spread tickets evenly over the hour from X:01 to X:59 PM
# (just in case rounding is a problem).
saaq_data[, day_ticket_time := sprintf('5:%d',
                                       as.integer(round(day_ticket_time_num -
                                                          floor(day_ticket_time_num), 2)*58 + 1))]
table(saaq_data[, day_ticket_time], useNA = 'ifany')

# Inspect number of seconds.
table(saaq_data[, as.integer(round(day_ticket_time_num -
                                     floor(day_ticket_time_num), 2)*58 + 1)], useNA = 'ifany')

class(saaq_data[, as.integer(round(day_ticket_time_num -
                                     floor(day_ticket_time_num), 2)*58 + 1)])


# Generate time of ticket up in hours and seconds.
saaq_data[, day_ticket_time := sprintf('%d:%d',
                                       floor(day_ticket_time_num),
                                       as.integer(round(day_ticket_time_num -
                                                          floor(day_ticket_time_num), 2)*58 + 1))]
table(saaq_data[, day_ticket_time], useNA = 'ifany')


# Finally, pad number of minutes with zeros, when necessary.
saaq_data[, day_ticket_time_hour := floor(day_ticket_time_num)]
saaq_data[, day_ticket_time_mins := sprintf('00%d',
                                            as.integer(round(day_ticket_time_num -
                                                               floor(day_ticket_time_num), 2)*58 + 1))]

saaq_data[, day_ticket_time := sprintf('%d:%s',
                                       day_ticket_time_hour,
                                       substring(day_ticket_time_mins,
                                                 nchar(day_ticket_time_mins) - 1,
                                                 nchar(day_ticket_time_mins)))]
table(saaq_data[, day_ticket_time], useNA = 'ifany')


# Create a variable with both date and time.
saaq_data[, date_time := sprintf('%s %s', date, day_ticket_time)]
head(saaq_data[, date_time], 20)
tail(saaq_data[, date_time], 20)




################################################################################
#
# Prepare data for output.
#
# Fixed Effects Regressions:
# Current points group and policy interaction
# Estimate for subsamples by ticket activity
# and on subsamples for male, female and all drivers
#
################################################################################


# file_tag <- 'high_pts'
# file_tag <- 'all_pts'
for (file_tag in file_tag_list) {


  #-------------------------------------------------------------------------------
  # Select data for sample of driver type.
  #-------------------------------------------------------------------------------


  # Select observations based on past activity.
  if (file_tag == 'all_pts') {
    saaq_data[, sel_obsn := sample == 'train']
    # saaq_out[, sel_obsn := TRUE]
  } else if (file_tag == 'high_pts') {
    saaq_data[, sel_obsn := past_active == TRUE & sample == 'train']
    # saaq_out[, sel_obsn := past_active == TRUE]
  } else {
    stop(sprintf("file_tag '%s' not recognized.", file_tag))
  }

  #-------------------------------------------------------------------------------
  # Select subsample for either male, female, or all drivers
  #-------------------------------------------------------------------------------
  #
  # sex_sel_list <- c('A', 'M', 'F')
  # sex_sel <- 'A'
  # sex_sel <- 'M'
  # sex_sel <- 'F'
  for (sex_sel in sex_sel_list) {

    print(sprintf('Creating dataset for sex %s drivers in points group %s.',
                  sex_sel, file_tag))


    ################################################################################
    # Fixed Effects Regressions:
    # Current points group and policy interaction
    ################################################################################


    if (sex_sel == 'A') {
      saaq_data[, sub_sel_obsn := sel_obsn == TRUE]
      # saaq_out[, sub_sel_obsn := sel_obsn == TRUE]
    } else if (sex_sel == 'M') {
      saaq_data[, sub_sel_obsn := sex == 'M' & sel_obsn == TRUE]
      # saaq_out[, sub_sel_obsn := sex == 'M' & sel_obsn == TRUE]
    } else if (sex_sel == 'F') {
      saaq_data[, sub_sel_obsn := sex == 'F' & sel_obsn == TRUE]
      # saaq_out[, sub_sel_obsn := sex == 'F' & sel_obsn == TRUE]
    } else {
      stop(sprintf('Sample selection sex_sel = %s not recognized.', sex_sel))
    }



    # Save the selected dataset for validation.
    out_file_name <- sprintf('%s/saaq_check_FE_%s_%s.csv',
                             data_out_path, sex_sel, file_tag)


    # check_var_names <- c('date', 'xtseq', 'sex', 'age_grp', 'curr_pts_grp',
    #                      'num', 'policy_int', 'events_int')
    check_var_names <- c('date_time', 'xtseq', 'sex', 'age_grp', 'curr_pts_grp',
                         'num', 'policy_int', 'events_int')
    # print(summary(saaq_out[sub_sel_obsn == TRUE, check_var_names, with = FALSE]))
    print(summary(saaq_data[sub_sel_obsn == TRUE, check_var_names, with = FALSE]))


    # write.csv(saaq_out[sub_sel_obsn == TRUE, check_var_names, with = FALSE],
    #           file = out_file_name, row.names = FALSE)
    write.csv(saaq_data[sub_sel_obsn == TRUE, check_var_names, with = FALSE],
              file = out_file_name, row.names = FALSE)





  }



}


# # Check output dataset.
# saaq_check <- saaq_out[sub_sel_obsn == TRUE, check_var_names, with = FALSE]
#
#
# saaq_check[, .N, by = c('xtseq', 'date')]
# summary(saaq_check[, .N, by = c('xtseq', 'date')])



