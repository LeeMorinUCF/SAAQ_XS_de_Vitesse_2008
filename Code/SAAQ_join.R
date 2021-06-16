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

# Set name of file with records of tickets. 
tickets_file_name <- 'saaq_tickets.csv'

# Set name of file with counts of drivers without tickets.
# Driver population includes drivers with no past tickets or current points.
# no_tickets_file_name <- 'saaq_no_tickets.csv'
drivers_file_name <- 'SAAQ_drivers_daily.csv'


# Set name of output file for point totals.
# pts_out_file_name <- 'saaq_pts.csv'
pts_bal_file_name <- 'saaq_point_balances.csv'

# Set name of output file for aggregated dataset.
# agg_out_file_name <- 'saaq_agg.csv'

# Set name of output file for full dataset.
out_file_name <- 'saaq_full.csv'






################################################################################
# Load Datasets
################################################################################



#-------------------------------------------------------------------------------
# Load data from records of tickets.
#-------------------------------------------------------------------------------



in_path_file_name <- sprintf('%s%s', data_in_path, tickets_file_name)
saaq_dt <- data.table(read.csv(file = in_path_file_name))

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


#-------------------------------------------------------------------------------
# Load Points Balances
#-------------------------------------------------------------------------------


# # Read a dataset tabulated elsewhere.
# counts_version <- 3
# in_file_name <- sprintf('saaq_past_counts_%d_%s_%s_v%d.csv',
#                          ptsVersion,
#                          1998, 2010,
#                          counts_version)
# data_count_path <- 'SAAQ_counts/'

# out_path_file_name <- sprintf('%s%s', dataInPath, pts_out_file_name)
in_path_file_name <- sprintf('%s%s', data_in_path, pts_bal_file_name)
saaq_past_counts_2 <- data.table(read.csv(file = in_path_file_name))


# nrow(saaq_past_counts_2)

#-------------------------------------------------------------------------------
# Load Daily Driver Counts
#-------------------------------------------------------------------------------

# Driver population includes all drivers, 
# including those with no past tickets or current points.
in_path_file_name <- sprintf('%s/%s', data_in_path, no_tickets_file_name)

# Need this?
# no_tickets_df <- read.csv(in_path_file_name)
# no_tickets_df <- fread(in_path_file_name)


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
# Join Daily Driver Events with Full Dataset of Non-events
################################################################################


#--------------------------------------------------------------------------------
# Final inspection
#--------------------------------------------------------------------------------


# Select columns from saaq_agg.
summary(saaq_agg[, c(agg_var_list, 'num'), with = FALSE])


# Select columns from saaq_no_tickets_full in same order as saaq_agg.
# summary(saaq_no_tickets_full[, c(agg_var_list, 'num'), with = FALSE])


# For previous joins:
# Select columns from no_tickets_dt in same order as saaq_agg.
summary(no_tickets_dt[, c(agg_var_list, 'num'), with = FALSE])

# Select columns from saaq_past_counts_sum in same order as saaq_agg.
summary(saaq_past_counts_sum[, c(agg_var_list, 'num'), with = FALSE])



#--------------------------------------------------------------------------------
# Join Daily Driver Events with Full Dataset of Non-events
#--------------------------------------------------------------------------------


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

# Current version has separate tag for aggregated file with
# new variable for past points indicator.
out_path_file_name <- sprintf('%s%s', data_out_path, out_file_name)
write.csv(x = saaq_agg_out, file = out_path_file_name, row.names = FALSE)






################################################################################
# End
################################################################################
