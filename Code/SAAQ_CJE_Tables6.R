################################################################################
#
# Investigation of SAAQ Traffic Ticket Violations
#
# Logistic and linear probability models of numbers of tickets awarded by the
# number of points per ticket.
#
#
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# December 10, 2021
#
################################################################################
#
# Analyzed data from traffic violations, and licensee data.
# Aggregated data by demerit point value for each date, sex and age category.
# Estimate logistic and linear probability models for sets of offenses.
# Identify discontinuity from policy change on April 1, 2008.
# Excessive speeding offenses were assigned double demerit points.
#
# This version includes a number of modifications for a revise and resubmit decision.
# It contains the full estimation results to appear in the manuscript.
#
# Previous versions also created a function for each table to enable
# quick replacement of tables for sensitivity analysis.
# Previous versions added marginal effects to the logistic regressions.
# A previous version adds tables for separate regressions by age.
# This version separates the function library and
# calls only the main function.
#
################################################################################


################################################################################
# Clearing Workspace and Declaring Packages
################################################################################

# Clear workspace.
# rm(list=ls(all=TRUE))

# The scales package can print large sample sizes in comma format.
library(scales)


################################################################################
# Set parameters for file IO
################################################################################



# Set working directory, if running interactively.
drive_path <- 'C:/Users/le279259/OneDrive - University of Central Florida/Documents'
git_path <- 'Research/SAAQ/SAAQspeeding/SAAQ_XS_de_Vitesse_2008'
wd_path <- sprintf('%s/%s',drive_path, git_path)
setwd(wd_path)

# Set methodology for zero-ticket population count:
# adj (unadjusted zero counts, intended for stacked join) or
# zero_count_method <- 'adj'
# unadj (adjusted zero counts, intended for differenced join)
zero_count_method <- 'unadj'

# Set join methodology:
# all (stacked, intended for unadjusted zero counts) or
# join_method <- 'all'
# net (differenced, intended for adjusted zero counts)
# join_method <- 'net'
# Original join method, like 'all' but with balances
# calculated in a way that accurately records the aging
# of drivers throughout the sample.
# This original version merges younger age categories.
# join_method <- 'orig'
# This original version keeps the same age categories.
join_method <- 'orig_agg'


# Set version of input files.
# data_in_method <- 'all_unadj'
data_in_method <- sprintf('%s_%s', join_method, zero_count_method)


# Set path for estimation results.
estn_path <- 'Estn'
estn_dir <- sprintf("%s/results_%s", estn_path, data_in_method)


# Set directory for results in GitHub repo.
# md_dir <- sprintf("%s/results", git_path)






# Set directory for Tables.
# tab_dir <- sprintf("%s/Tables", git_path)
tab_dir <- sprintf("%s/tables", estn_dir)

# Read library of functions for generating LaTeX tables.
tab_lib_path <- "Code/SAAQ_Tab_Lib1.R"
source(tab_lib_path)


################################################################################
# Load Estimates for Tables.
################################################################################



#------------------------------------------------------------
# Pooled regressions with separation by age group.
#------------------------------------------------------------

# spec_group <- 'pooled'
#
# estn_version <- 11
# estn_file_name <- sprintf('estimates_v%d_%s.csv', estn_version, spec_group)
# estn_file_path <- sprintf('%s/%s', estn_dir, estn_file_name)
# estn_results_by_age <- read.csv(file = estn_file_path)
# summary(estn_results_by_age)


#------------------------------------------------------------
# Specification: All Drivers with Monthly and weekday seasonality
#------------------------------------------------------------

spec_group <- 'all'

estn_version <- 12
estn_file_name <- sprintf('estimates_v%d_%s.csv', estn_version, spec_group)
estn_file_path <- sprintf('%s/%s', estn_dir, estn_file_name)
estn_results_full <- read.csv(file = estn_file_path)
summary(estn_results_full)



#------------------------------------------------------------
# Sensitivity Analysis: High-point drivers.
# (with monthly and weekday seasonality)
#------------------------------------------------------------

spec_group <- 'high_pts'

estn_version <- 13
estn_file_name <- sprintf('estimates_v%d_%s.csv', estn_version, spec_group)
estn_file_path <- sprintf('%s/%s', estn_dir, estn_file_name)
estn_results_high <- read.csv(file = estn_file_path)
summary(estn_results_high)


#------------------------------------------------------------
# Sensitivity Analysis: Placebo regression.
# (with monthly and weekday seasonality)
#------------------------------------------------------------


spec_group <- 'placebo'

estn_version <- 14
estn_file_name <- sprintf('estimates_v%d_%s.csv', estn_version, spec_group)
estn_file_path <- sprintf('%s/%s', estn_dir, estn_file_name)
estn_results_placebo <- read.csv(file = estn_file_path)
summary(estn_results_placebo)


#------------------------------------------------------------
# Specification: REAL event study with seasonality
#------------------------------------------------------------

spec_group <- 'events'

estn_version <- 15
estn_file_name <- sprintf('estimates_v%d_%s.csv', estn_version, spec_group)
estn_file_path <- sprintf('%s/%s', estn_dir, estn_file_name)
estn_results_events <- read.csv(file = estn_file_path)
summary(estn_results_events)



#------------------------------------------------------------
# Dataset with sample sizes.
#------------------------------------------------------------

# # Set version of output file.
# estn_version <- 1
# estn_file_name <- sprintf('sampl_sizes_v%d.csv', estn_version)
# estn_file_path <- sprintf('%s/%s', md_dir, estn_file_name)
# sampl_sizes <- read.csv(file = estn_file_path)



################################################################################
# Define lists required for tables
################################################################################

# List to divide sample into males and females.
sex_list <- c('All', 'Male', 'Female')

# Create list of age categories for regressions by age group.
age_grp_list <- unique(estn_results_full[substr(estn_results_full[, 'Variable'], 1, 7) ==
                                               'age_grp', 'Variable'])
age_grp_list <- substr(age_grp_list, 8, nchar(age_grp_list))


# Create list of labels for policy*age interactions.
if (join_method == 'orig_agg') {
  age_group_list <- c('0-15', '16-19',
                      '20-24', '25-34', '35-44', '45-54',
                      '55-64', '65-199')
  age_int_var_list <- sprintf('policyTRUE:age_grp%s', age_group_list)
  age_int_label_list <- data.frame(Variable = age_int_var_list,
                                   Label = c('Age 0-15 * policy',
                                             'Age 16-19 * policy',
                                             'Age 20-24 * policy',
                                             'Age 25-34 * policy',
                                             'Age 35-44 * policy',
                                             'Age 45-54 * policy',
                                             'Age 55-64 * policy',
                                             'Age 65+ * policy'))


} else {
  # Coarser grouping to merge less-populated age groups:
  age_group_list <- c('0-19',
                      '20-24', '25-34', '35-44', '45-54',
                      '55-64', '65-199')
  age_int_var_list <- sprintf('policyTRUE:age_grp%s', age_group_list)
  age_int_label_list <- data.frame(Variable = age_int_var_list,
                                   Label = c('Age 16-19 * policy',
                                             'Age 20-24 * policy',
                                             'Age 25-34 * policy',
                                             'Age 35-44 * policy',
                                             'Age 45-54 * policy',
                                             'Age 55-64 * policy',
                                             'Age 65+ * policy'))


}




# Create list of labels for indicators by point value for the dependent variable.
# points_var_list <- unique(estn_results_full[, 'pts_target'])
pts_target_list <- c('all',
                     '1', '2', '3', '4', '5', '7',
                     '9+')
points_label_list <- data.frame(Variable = pts_target_list,
                                Label = c('All point values',
                                          '1 point',
                                          '2 points',
                                          '3 points',
                                          '4 points',
                                          '5 points',
                                          '7 points',
                                          '9 or more points'))


# Create list of labels for policy*month interactions.
# event_month_sel <- substr(estn_results_events[, 'Variable'], 1, 12) == 'policy_month'
# event_month_list <- unique(estn_results_events[event_month_sel, 'Variable'])
event_month_list <- sprintf('policy_monthpolicy%s',
                            c(sprintf('0%d', seq(9)), c('10', '11', '12')))
events_label_list <- data.frame(Variable = event_month_list,
                                Label = sprintf('Month %d',
                                                as.numeric(substr(event_month_list, 19, 20))))





############################################################
# Generate Logit vs LPM Tables.
############################################################


# Set parameters for TeX file for Table.
tab_tag <- 'seas_Logit_vs_LPMx100K'
header_spec <- 'Seasonal Logit and LPM x 100K'

# Set parameters for model selection.
season_incl <- 'mnwk'

# Set parameters for display formatting.
num_fmt <- 'x100K'

# Set captions for tables.
Logit_LPM_description <- c(
  "For each regression, the dependent variable is an indicator that a driver has committed ",
  "any offence on a particular day. ",
  "All regressions contain age category and demerit point category controls,",
  "as well as monthly and weekday indicator variables.",
  "The baseline age category comprises drivers under the age of 16.",
  "The heading ``Sig.\'\' is an abbreviation for statistical significance, with",
  "the symbol * denoting statistical significance at the 0.1\\% level",
  "and ** the 0.001\\% level.",
  "Marginal effects, as well as linear probability model coefficients and standard errors, are ",
  "in scientific notation. ",
  "The linear probability model uses heteroskedasticity-robust standard errors."
)

Logit_LPM_pts_description <- c(
  "The dependent variable in each regression is equal to one ",
  "if a driver receives a ticket with a particular point value  ",
  "(that of the first column for a particular row) on that day, ",
  "and is otherwise equal to zero.",
  "The categories of tickets with 3, 5 and 7 points includes tickets ",
  "with 6, 10 and 14 points after the policy change, respectively, ",
  # "The 3-point category of tickets includes 6-point tickets after the policy change, ",
  # "The 5-point category of tickets includes 10-point tickets after the policy change, ",
  # "the 7-point category includes 14-point tickets after the policy change, ",
  "and the category with 9 or more points includes tickets ",
  "with all corresponding doubled values after the policy change.",
  "All regressions contain age category and demerit point category controls,",
  "as well as monthly and weekday indicator variables.",
  "The baseline age category comprises drivers under the age of 16.",
  "The heading ``Sig.\'\' is an abbreviation for statistical significance, with",
  "the symbol * denoting statistical significance at the 0.1\\% level",
  "and ** the 0.001\\% level.",
  "Marginal effects, as well as linear probability model coefficients and standard errors, are ",
  "in scientific notation. ",
  "The linear probability model uses heteroskedasticity-robust standard errors. "
)



source(tab_lib_path)

# Generate tables.
SAAQ_Logit_vs_LPM_2MFX_table_gen(tab_dir, tab_tag, header_spec,
                                 season_incl, num_fmt,
                                 estn_results_by_age = NULL,
                                 estn_results_full, estn_results_high,
                                 estn_results_placebo, estn_results_events,
                                 age_grp_list, age_int_label_list,
                                 points_label_list, sex_list,
                                 orig_description = Logit_LPM_description,
                                 orig_pts_description = Logit_LPM_pts_description,
                                 incl_mfx = TRUE,
                                 est_pooled = FALSE)


################################################################################
# End
################################################################################
