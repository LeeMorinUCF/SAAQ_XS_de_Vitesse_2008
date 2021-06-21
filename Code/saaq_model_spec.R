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
################################################################################

################################################################################
# Clearing Workspace and Declaring Packages
################################################################################

# Clear workspace, if running interactively.
rm(list=ls(all=TRUE))

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

# Set name of output file for training, testing and estimation samples.
train_file_name <- 'saaq_train.csv'
test_file_name <- 'saaq_test.csv'


# Set name of output file for full dataset.
# out_file_name <- 'saaq_out.csv'


set.seed(42)


################################################################################
# Set Parameters for variables
################################################################################



# Age group categories for defining factors.
age_group_list <- c('0-15', '16-19', '20-24', '25-34', '35-44', '45-54',
                    '55-64', '65-74', '75-84', '85-89', '90-199')

# Current points group categories for defining factors.
curr_pts_grp_list <- c(seq(0,10), '11-20', '21-30', '31-150')

# Weekday indicators.
weekday_list <- c('Sunday',
                  'Monday',
                  'Tuesday',
                  'Wednesday',
                  'Thursday',
                  'Friday',
                  'Saturday')


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

summary(saaq_train[, .N, by = date])
head(saaq_train, 618)


table(saaq_train[, sex], useNA = 'ifany')
# 50-50.
table(saaq_train[, age_grp], useNA = 'ifany')
# 11 equal groups.
table(saaq_train[, past_active], useNA = 'ifany')
# 50-50.
table(saaq_train[, past_active], saaq_train[, sex], useNA = 'ifany')
# Equally divided by sex.
table(saaq_train[, curr_pts_grp], saaq_train[, past_active], useNA = 'ifany')
# Equally divided across current points categories. 


length(unique(saaq_train[, date]))
# [1] 1826 days of driving.

2*length(age_group_list)*2*length(curr_pts_grp_list)
# [1] 616 combinations of categories per day.

# All observations accounted for.
nrow(saaq_train) == 2*length(age_group_list)*2*length(curr_pts_grp_list)*1826


#-------------------------------------------------------------------------------
# Load Testing Dataset
#-------------------------------------------------------------------------------

# Dataset for out-of-sample model testing. 
in_path_file_name <- sprintf('%s/%s', data_in_path, test_file_name)
saaq_test <- fread(in_path_file_name)

summary(saaq_test)

summary(saaq_test[, .N, by = date])
head(saaq_test, 618)


table(saaq_test[, sex], useNA = 'ifany')
# 50-50.
table(saaq_test[, age_grp], useNA = 'ifany')
# 11 equal groups.
table(saaq_test[, past_active], useNA = 'ifany')
# 50-50.
table(saaq_test[, past_active], saaq_test[, sex], useNA = 'ifany')
# Equally divided by sex.
table(saaq_test[, curr_pts_grp], saaq_test[, past_active], useNA = 'ifany')
# Equally divided across current points categories. 


length(unique(saaq_test[, date]))
# [1] 1826 days of driving.

2*length(age_group_list)*2*length(curr_pts_grp_list)
# [1] 616 combinations of categories per day.

# All observations accounted for.
nrow(saaq_test) == 2*length(age_group_list)*2*length(curr_pts_grp_list)*1826



################################################################################
# Stack the datasets and label by sample
################################################################################

saaq_train[, sample := 'train']
saaq_test[, sample := 'test']
saaq_data <- rbind(saaq_train, saaq_test)

rm(list(saaq_train, saaq_test))


################################################################################
# Define additional variables
################################################################################

# Define categorical variables as factors.
saaq_data[, sex := factor(sex, levels = c('M', 'F'))]
saaq_data[, age_grp := factor(age_grp, levels = age_group_list)]
saaq_data[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]

# Define new variables for seasonality.
saaq_data[, 'month'] <- substr(saaq_data[, 'date'], 6, 7)
table(saaq_data[, 'month'], useNA = "ifany")

# Weekday indicator.
saaq_data[, 'weekday'] <- weekdays(saaq_data[, 'dinf'])
table(saaq_data[, 'weekday'], useNA = "ifany")
class(saaq_data[, 'weekday'])
saaq_data[, 'weekday'] <- factor(saaq_data[, 'weekday'],
                                 levels = weekday_list)
class(saaq_data[, 'weekday'])


################################################################################
# Fit a series of decision trees with increasing depth
################################################################################

#-------------------------------------------------------------------------------
# First version with all variables
#-------------------------------------------------------------------------------


# Fit a series of models.

# Calculate AUROC in-sample and out-of-sample.

# Output table of results.


#-------------------------------------------------------------------------------
# Second version after projection off key variables
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Separate trees for males and females
#-------------------------------------------------------------------------------




################################################################################
# Fit a series of models with higher-order interactions
################################################################################

# Fit a series of models.

# Calculate AUROC in-sample and out-of-sample.

# Output table of results.



################################################################################
# End
################################################################################

