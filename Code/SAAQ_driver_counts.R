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


#--------------------------------------------------------------------------------
# Make some adjustments to make the data more realistic. 
#--------------------------------------------------------------------------------

# Add an allowance for unlicensed drivers. 

# Unlicensed drivers represent about 5% of drivers who get tickets.
unl_pct <- 0.05

# The majority are males (about 90%).
unl_pct_M <- 0.90

# About one third are aged 20 or under.
unl_pct_teen <- 0.333

# The majority (50%) are aged 21-34.
unl_pct_adult <- 0.50

# The remaining 16.6% are aged 35 and above.
unl_pct_mid_sr <- 0.167

# Allocate these across the population accordingly. 


# Gross up population by 5% to account for entering and leaving the province. 
# It's not the same set of drivers each year. 
turnover_multiplier <- 0.05


# year <- 2000
for (year in seq(2000, 2018)) {
  col_name <- sprintf('yr_%d', year)
  
  # Gross up the population for turnover. 
  annual[, col_name] <- as.integer(round(annual[, col_name]*(1 + turnover_multiplier)))
  
  # Increase counts for predicted number of unlicensed drivers.
  unl_num <- round(unl_pct*annual[nrow(annual), col_name])
  
  # Allocate these across different age groups. 
  
  # Young males.
  row_sel <- annual[, 'age_group'] %in% c("0-15", "16-19") & annual[, 'sex'] == 'M'
  unl_wt <- annual[row_sel, col_name]/sum(annual[row_sel, col_name])
  annual[row_sel, col_name] <- annual[row_sel, col_name] + 
    round(unl_wt*unl_pct_M*unl_pct_teen*unl_num)
  
  # Young females.
  row_sel <- annual[, 'age_group'] %in% c("0-15", "16-19") & annual[, 'sex'] == 'F'
  unl_wt <- annual[row_sel, col_name]/sum(annual[row_sel, col_name])
  annual[row_sel, col_name] <- annual[row_sel, col_name] + 
    round(unl_wt*(1 - unl_pct_M)*unl_pct_teen*unl_num)
  
  # Adult males.
  row_sel <- annual[, 'age_group'] %in% c("20-24", "25-34") & annual[, 'sex'] == 'M'
  unl_wt <- annual[row_sel, col_name]/sum(annual[row_sel, col_name])
  annual[row_sel, col_name] <- annual[row_sel, col_name] + 
    round(unl_wt*unl_pct_M*unl_pct_adult*unl_num)
  
  # Adult females.
  row_sel <- annual[, 'age_group'] %in% c("20-24", "25-34") & annual[, 'sex'] == 'F'
  unl_wt <- annual[row_sel, col_name]/sum(annual[row_sel, col_name])
  annual[row_sel, col_name] <- annual[row_sel, col_name] + 
    round(unl_wt*(1 - unl_pct_M)*unl_pct_adult*unl_num)
  
  # Middle aged or senior males.
  row_sel <- annual[, 'age_group'] %in% c("20-24", "25-34") & annual[, 'sex'] == 'M'
  unl_wt <- annual[row_sel, col_name]/sum(annual[row_sel, col_name])
  annual[row_sel, col_name] <- annual[row_sel, col_name] + 
    round(unl_wt*unl_pct_M*unl_pct_mid_sr*unl_num)
  
  # Middle aged or senior females.
  row_sel <- annual[, 'age_group'] %in% c("20-24", "25-34") & annual[, 'sex'] == 'F'
  unl_wt <- annual[row_sel, col_name]/sum(annual[row_sel, col_name])
  annual[row_sel, col_name] <- annual[row_sel, col_name] + 
    round(unl_wt*(1 - unl_pct_M)*unl_pct_mid_sr*unl_num)
  
  
  
}




################################################################################
# Construct Daily Driver Counts
################################################################################

# Construct a date series from dates in main dataset. 

# Data limits from dataset with tickets (events).
# > min(saaq[, 'date'])
# [1] "1998-01-01"
# > max(saaq[, 'date'])
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
# [1] "seq"     "sex"     "dob_yr"  "dob_mo"  "dob_day" "date"    "points" 
# [8] "dcon"    "age"     "age_grp"
# > 

# Initialize a data frame for all the drivers without tickets each day. 
driver_counts <- data.frame(date = rep(date_list, 
                                       each = length(age_group_list)*2),
                            seq = rep(0, num_rows),
                            age_grp = rep(age_group_list, 
                                          length(date_list)*2),
                            sex = rep(rep(c('F', 'M'), 
                                          each = length(age_group_list)), 
                                      length(date_list)),
                            points = rep(0, num_rows),
                            num = rep(NA, num_rows))

summary(driver_counts)
head(driver_counts, 25)
tail(driver_counts, 25)


# Populate the number of licensed drivers by category. 


# Initialize counts with year 2000 totals for previous years. 
# Data are only available for the years 2000 and beyond.
# This doesn't matter for the two-year window around 2008.
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

    driver_counts[row_nums, 'num'] <- next_june_counts

  } else {
    
    # Calculate weighted average of day. 
    next_june_wt <- as.numeric(this_date - last_june_date) / 
      as.numeric(next_june_date - last_june_date)
    
    # Calculate the weighted average of counts.
    this_date_counts <- next_june_wt*next_june_counts + 
      (1 - next_june_wt)*last_june_counts
    
    driver_counts[row_nums, 'num'] <- round(this_date_counts)
    
  }
  
  
  # Every June 1, refresh the totals. 
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
# driver_counts[driver_counts[, 'date'] == '2000-06-01', ]
# driver_counts[driver_counts[, 'date'] == '2000-06-30', ]
# driver_counts[driver_counts[, 'date'] == '2002-06-01', ]
# driver_counts[driver_counts[, 'date'] == '2002-06-30', ]
# driver_counts[driver_counts[, 'date'] == '2010-06-01', ]
# 
# # Check two consecutive days. 
# driver_counts[driver_counts[, 'date'] == '2010-05-31', ]
# 
# driver_counts[driver_counts[, 'date'] == '2010-06-30', ]
# 
# 
# 
# 
# # Plot the age group counts over time. 
# # sel_obsn <- driver_counts[, 'age_grp'] == '0-15' & 
# #   driver_counts[, 'sex'] == 'F'
# # sel_obsn <- driver_counts[, 'age_grp'] == '0-15' & 
# #   driver_counts[, 'sex'] == 'M'
# # sel_obsn <- driver_counts[, 'age_grp'] == '20-24' &
# #   driver_counts[, 'sex'] == 'F'
# sel_obsn <- driver_counts[, 'age_grp'] == '20-24' &
#   driver_counts[, 'sex'] == 'M'
# # sel_obsn <- driver_counts[, 'age_grp'] == '90-199' & 
# #   driver_counts[, 'sex'] == 'M'
# # sel_obsn <- driver_counts[, 'age_grp'] == '90-199' &
# #   driver_counts[, 'sex'] == 'F'
# 
# # Plot a time series for this selection. 
# new_year_dates <- seq(sum(sel_obsn))[month(driver_counts[sel_obsn, 'date']) == 1 & 
#   mday(driver_counts[sel_obsn, 'date']) == 1]
# new_year_labels <- year(driver_counts[sel_obsn, 'date'][new_year_dates])
# 
# 
# # Plot for the selected age group.
# plot(driver_counts[sel_obsn, 'num'],
#      xaxt = 'n')
# 
# axis(1, at = new_year_dates, 
#      labels = new_year_labels)


# Note the kinks on June 1, every year. 


################################################################################
# Output Daily Driver Counts
################################################################################

out_path_file_name <- sprintf('%s/%s', data_out_path, out_file_name)
write.csv(x = driver_counts, file = out_path_file_name, row.names = FALSE)




################################################################################
# End
################################################################################

