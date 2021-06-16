################################################################################
# 
# Investigation of SAAQ Excessive Speeding Laws
# 
# Construction of a series of numbers of drivers in sex and age categories
# who were NOT awarded tickets.
# Dataset is used for joining non-events to ticket events.
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
# Load data from licensee data on SAAQ webpage.
# 
################################################################################


################################################################################
# Clearing Workspace and Declaring Packages
################################################################################

# Clear workspace, if running interactively.
# rm(list=ls(all=TRUE))

# Load data table package for quick selection on seq.
# It also includes dependencies for dealing with dates (i.e. lubridate).
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

# The file of annual counts of drivers from SAAQ Website.
annual_file_name <- 'SAAQ_drivers_annual.csv'


# The data of counts of licensed drivers are also stored in 'Data/'.
data_out_path <- 'Data'

# Set name of output file.
# out_file_name <- 'saaq_no_tickets.csv'
out_file_name <- 'SAAQ_drivers_daily.csv'



################################################################################
# Load Annual Driver Counts
################################################################################

# Totals are based on the number of driver's licenses outstanding
# for each category as of June 1 of each year. 

annual_path_file_name <- sprintf('%s/%s', data_in_path, annual_file_name)
annual <- read.csv(file = annual_path_file_name)

colnames(annual)
sapply(annual, class)


################################################################################
# Construct Daily Driver Counts
################################################################################

# Construct a date series from dates in main dataset. 

# Data limits from dataset with tickets (events).
# > min(saaq[, 'dinf'])
# [1] "1998-01-01"
# > max(saaq[, 'dinf'])
# [1] "2010-12-31"
# > 

day_1 <- as.numeric(as.Date('1998-01-01'))
day_T <- as.numeric(as.Date('2010-12-31'))

date_list <- as.Date(seq(day_1, day_T), origin = as.Date('1970-01-01'))


length(date_list)
min(date_list)
max(date_list)


# Get dimensions from product of sex and age groups. 
age_group_list <- c('0-15', '16-19', '20-24', '25-34', '35-44', '45-54', 
                    '55-64', '65-74', '75-84', '85-89', '90-199')

num_rows <- length(date_list)*length(age_group_list)*2


# Define columns as in main saaq dataset of tickets: 
# > colnames(saaq)
# [1] "seq"     "sex"     "dob_yr"  "dob_mo"  "dob_day" "dinf"    "points" 
# [8] "dcon"    "age"     "age_grp"
# > 

# Initialize a data frame for all the drivers without tickets each day. 
no_tickets_df <- data.frame(dinf = rep(date_list, 
                                       each = length(age_group_list)*2),
                            seq = rep(0, num_rows),
                            age_grp = rep(age_group_list, 
                                          length(date_list)*2),
                            sex = rep(rep(c('F', 'M'), 
                                          each = length(age_group_list)), 
                                      length(date_list)),
                            points = rep(0, num_rows),
                            num = rep(NA, num_rows))

summary(no_tickets_df)
head(no_tickets_df, 25)
tail(no_tickets_df, 25)


# Populate the number of licensed drivers by category. 


# Initialize counts with year 2000 totals for previous years. 
this_year <- 2000
next_june_counts <- annual[annual[, 'age_group'] != 'Total' & 
                              annual[, 'sex'] != 'T', 
                            sprintf('yr_%d', this_year)]
last_june_counts <- next_june_counts

next_june_date <- date_list[year(date_list) == this_year &
                              month(date_list) == 6 &
                              mday(date_list) == 1]
last_june_date <- next_june_date

for (date_num in 1:length(date_list)) {
  
  this_date <- date_list[date_num]
  this_year <- year(this_date)
  this_month <- month(this_date)
  this_day <- mday(this_date)
  
  # Print a progress report.
  if (this_day == 1 & this_month == 1) {
    print(sprintf('Now counting drivers in year %d.', this_year))
  }
  
  # Select rows to be modified. 
  row_nums <- seq((date_num - 1)*length(age_group_list)*2 + 1, 
                  date_num*length(age_group_list)*2)
  
  # Constant totals at earliest recorded date. 
  if (this_date <= date_list[year(date_list) == 2000 &
                             month(date_list) == 6 &
                             mday(date_list) == 1]) { # Before "2000-06-01"

    no_tickets_df[row_nums, 'num'] <- next_june_counts

  } else {
    
    # Calculate weighted average of day. 
    next_june_wt <- as.numeric(this_date - last_june_date) / 
      as.numeric(next_june_date - last_june_date)
    
    # Calculate the weighted average of counts.
    this_date_counts <- next_june_wt*next_june_counts + 
      (1 - next_june_wt)*last_june_counts
    
    no_tickets_df[row_nums, 'num'] <- round(this_date_counts)
    
  }
  
  
  # Evey June 1, refresh the totals. 
  if (this_month == 6 & this_day == 1 & this_year >= 2000) {
    
    last_june_counts <- next_june_counts
    
    next_june_counts <- annual[annual[, 'age_group'] != 'Total' & 
                                  annual[, 'sex'] != 'T', 
                                sprintf('yr_%d', (this_year + 1))]
    
    # last_june_date <- this_date
    last_june_date <- next_june_date # More robust to date. 
    if (this_year == 2010) {
      # Next june not in date_list.
      next_june_date <- as.Date('2011-06-01')
      
    } else {
      next_june_date <- date_list[year(date_list) == (this_year + 1) &
                                    month(date_list) == 6 &
                                    mday(date_list) == 1]
    }
    
  }
  
  
  
}



################################################################################
# Validation
################################################################################


# # Now do some tests to verify accuracy. 
# 
# # Check at some June 1 dates.
# no_tickets_df[no_tickets_df[, 'dinf'] == '2000-06-01', ]
# no_tickets_df[no_tickets_df[, 'dinf'] == '2000-06-30', ]
# no_tickets_df[no_tickets_df[, 'dinf'] == '2002-06-01', ]
# no_tickets_df[no_tickets_df[, 'dinf'] == '2002-06-30', ]
# no_tickets_df[no_tickets_df[, 'dinf'] == '2010-06-01', ]
# 
# # Check two consecutive days. 
# no_tickets_df[no_tickets_df[, 'dinf'] == '2010-05-31', ]
# 
# no_tickets_df[no_tickets_df[, 'dinf'] == '2010-06-30', ]
# 
# 
# 
# 
# # Plot the age group counts over time. 
# # sel_obsn <- no_tickets_df[, 'age_grp'] == '0-15' & 
# #   no_tickets_df[, 'sex'] == 'F'
# # sel_obsn <- no_tickets_df[, 'age_grp'] == '0-15' & 
# #   no_tickets_df[, 'sex'] == 'M'
# # sel_obsn <- no_tickets_df[, 'age_grp'] == '20-24' &
# #   no_tickets_df[, 'sex'] == 'F'
# sel_obsn <- no_tickets_df[, 'age_grp'] == '20-24' &
#   no_tickets_df[, 'sex'] == 'M'
# # sel_obsn <- no_tickets_df[, 'age_grp'] == '90-199' & 
# #   no_tickets_df[, 'sex'] == 'M'
# # sel_obsn <- no_tickets_df[, 'age_grp'] == '90-199' &
# #   no_tickets_df[, 'sex'] == 'F'
# 
# # Plot a time series for this selection. 
# new_year_dates <- seq(sum(sel_obsn))[month(no_tickets_df[sel_obsn, 'dinf']) == 1 & 
#   mday(no_tickets_df[sel_obsn, 'dinf']) == 1]
# new_year_labels <- year(no_tickets_df[sel_obsn, 'dinf'][new_year_dates])
# 
# 
# # Plot for the selected age group.
# plot(no_tickets_df[sel_obsn, 'num'],
#      xaxt = 'n')
# 
# axis(1, at = new_year_dates, 
#      labels = new_year_labels)


# Note the kinks on June 1, every year. 


################################################################################
# Output Daily Driver Counts
################################################################################

out_path_file_name <- sprintf('%s/%s', data_out_path, out_file_name)
write.csv(x = no_tickets_df, file = out_path_file_name, row.names = FALSE)




################################################################################
# End
################################################################################

