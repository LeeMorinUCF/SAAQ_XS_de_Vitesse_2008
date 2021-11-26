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
# College of Business Administration
# University of Central Florida
#
# March 5, 2021
#
################################################################################
#
# Load data from traffic violations, and licensee data.
# Aggregated data by demerit point value for each date, sex and age category.
# Estimate linear probability models for sets of offenses.
# Identify discontinuity from policy change on April 1, 2008.
# Excessive speeding offenses were assigned double demerit points.
#
# This version includes a number of modifications for a revise and resubmit decision.
# It contains the full estimation results to appear in the manuscript.
# This version calculates marginal effects for logistic regressions.
# This version calculates those marginal effects by using the formula for the
# derivative but uses the sample average prediction as the relevant probability.
# The average probability is averaged across policy = TRUE and policy = FALSE
# to have a symmetric sample of observations in the average prediction.
# This matters because the events are rare and the policy effect is big.
#
################################################################################


################################################################################
# Clearing Workspace and Declaring Packages
################################################################################

# Clear workspace.
rm(list=ls(all=TRUE))


################################################################################
# Set parameters for file IO
################################################################################

# Set working directory.
# setwd('/home/ec2-user/saaq')
# setwd('~/Research/SAAQ/')

# The original data are stored in 'SAAQdata/origData/'.
# dataInPath <- 'SAAQdata_full/'
# data_in_path <- '~/Research/SAAQ/SAAQdata_full/'
data_in_path <- 'C:/Users/le279259/Documents/Research/SAAQ/SAAQdata_full/'

# The data of demerit point counts are stored in 'SAAQdata/seqData/'.
# dataOutPath <- 'SAAQspeeding/SAAQspeeding/'
# data_out_path <- '~/Research/SAAQ/SAAQspeeding/SAAQspeeding/'
data_out_path <- 'C:/Users/le279259/Documents/Research/SAAQ/SAAQspeeding/SAAQspeeding/'

# Set directory for results in GitHub repo.
# git_path <- "~/Research/SAAQ/SAAQspeeding/Hidden_Comp_Risks/R_and_R"
git_path <- "C:/Users/le279259/Documents/Research/SAAQ/SAAQspeeding/Hidden_Comp_Risks/R_and_R"
md_dir <- sprintf("%s/results", git_path)

# Read script for calculating marginal effects.
mfx_path <- sprintf("%s/Regs_w_2MFX/SAAQ_MFX_lib1.R", git_path)
source(mfx_path)


# Set version of input file.
# ptsVersion <- 2 # With current points but not past active.
pts_version <- 3 # With current points and past active.


# Set version of output file.
estn_version <- 99
estn_file_name <- sprintf('estimates_v%d.csv', estn_version)
estn_file_path <- sprintf('%s/%s', md_dir, estn_file_name)


################################################################################
# Load Daily Driver Counts and Events
################################################################################


in_file_name <- sprintf('saaq_agg_%d.csv', pts_version)
in_path_file_name <- sprintf('%s%s', data_in_path, in_file_name)
# Yes, keep it in dataInPath since it is yet to be joined.
saaq_data <- read.csv(file = in_path_file_name)

colnames(saaq_data)

sapply(saaq_data, class)

# Rewrite dinf as date format.
saaq_data[, 'dinf'] <- as.Date(saaq_data[, 'dinf'])

# Order the current points categories for better interpretability.
table(saaq_data[, 'curr_pts_grp'])

curr_pts_grp_list <- c(as.character(seq(0, 10)), '11-20', '21-30', '30-150')
saaq_data[, 'curr_pts_grp'] <- factor(saaq_data[, 'curr_pts_grp'],
                                      levels <- curr_pts_grp_list)

table(saaq_data[, 'curr_pts_grp'])


summary(saaq_data)

#--------------------------------------------------------------------------------
# Create new factors by consolidating some categories
#--------------------------------------------------------------------------------

# Age groups.
table(saaq_data[, 'age_grp'], useNA = 'ifany')


# age_grp_list <- levels(saaq_data[, 'age_grp'])
orig_age_grp_list <- unique(saaq_data[, 'age_grp'])
saaq_data[, 'age_grp_orig'] <- saaq_data[, 'age_grp']
age_grp_list <- c(orig_age_grp_list[seq(7)], '65-199')

saaq_data[, 'age_grp'] <- as.factor(NA)
levels(saaq_data[, 'age_grp']) <- age_grp_list
age_group_sel <- saaq_data[, 'age_grp_orig'] %in% orig_age_grp_list[seq(7)]
saaq_data[age_group_sel, 'age_grp'] <- saaq_data[age_group_sel, 'age_grp_orig']
saaq_data[!age_group_sel, 'age_grp'] <- age_grp_list[8]


# Trust but verify.
table(saaq_data[, 'age_grp'],
      saaq_data[, 'age_grp_orig'], useNA = 'ifany')
# Check.


# Current point balance groups.
table(saaq_data[, 'curr_pts_grp'], useNA = 'ifany')

# Consolidate categories of current points balances.
# curr_pts_grp_list <- levels(saaq_data[, 'curr_pts_grp'])
saaq_data[, 'curr_pts_grp_orig'] <- saaq_data[, 'curr_pts_grp']
new_curr_pts_grp_list <- c('0', '1-3', '4-6', '7-9', '10-150')

# Create the new factor.
saaq_data[, 'curr_pts_grp'] <- as.factor(NA)
levels(saaq_data[, 'curr_pts_grp']) <- new_curr_pts_grp_list

# Add the zero points group first.
curr_pts_grp_sel <- saaq_data[, 'curr_pts_grp_orig'] %in% curr_pts_grp_list[1]
saaq_data[curr_pts_grp_sel, 'curr_pts_grp'] <- saaq_data[curr_pts_grp_sel, 'curr_pts_grp_orig']
# Add groups 1-3.
curr_pts_grp_sel <- saaq_data[, 'curr_pts_grp_orig'] %in% curr_pts_grp_list[2:4]
saaq_data[curr_pts_grp_sel, 'curr_pts_grp'] <- new_curr_pts_grp_list[2]
# Add groups 4-6.
curr_pts_grp_sel <- saaq_data[, 'curr_pts_grp_orig'] %in% curr_pts_grp_list[5:7]
saaq_data[curr_pts_grp_sel, 'curr_pts_grp'] <- new_curr_pts_grp_list[3]
# Add groups 7-9.
curr_pts_grp_sel <- saaq_data[, 'curr_pts_grp_orig'] %in% curr_pts_grp_list[8:10]
saaq_data[curr_pts_grp_sel, 'curr_pts_grp'] <- new_curr_pts_grp_list[4]
# Add the rest: 10-150.
curr_pts_grp_sel <- saaq_data[, 'curr_pts_grp_orig'] %in% curr_pts_grp_list[11:14]
saaq_data[curr_pts_grp_sel, 'curr_pts_grp'] <- new_curr_pts_grp_list[5]


# Trust but verify.
table(saaq_data[, 'curr_pts_grp'],
      saaq_data[, 'curr_pts_grp_orig'], useNA = 'ifany')
# Check.


################################################################################
# Define Functions
################################################################################


lpm_neg_check <- function(lm_model) {
  # Checking for negative LPM predictions.
  saaq_data[, 'pred'] <- NA
  saaq_data[sel_obs , 'pred'] <- predict(lm_model)
  print('Observations with negative predictions:')
  print(unique(saaq_data[sel_obs & saaq_data[, 'pred'] < 0,
                         c('sex', 'age_grp', 'curr_pts_grp', 'policy', 'pred')]))
  print('Summary of predictions:')
  print(summary(saaq_data[sel_obs , 'pred']))
  print('Number of negative predictions:')
  pct_neg_pred <- sum(saaq_data[sel_obs , 'pred'] < 0)/sum(sel_obs)
  print(pct_neg_pred)
}


# Load functions for regressions with aggregated data.
agg_reg_path <- "C:/Users/le279259/Documents/Research/aggregress/aggregress/R"
agg_reg_file <- sprintf("%s/aggregress.R", agg_reg_path)
source(agg_reg_file)
agg_reg_het_file <- sprintf("%s/aggregress_het.R", agg_reg_path)
source(agg_reg_het_file)


################################################################################
# Generate New Variables to be defined within loop
################################################################################


# Generate variables for regressions.
saaq_data[, 'policy'] <- NA
saaq_data[, 'window'] <- NA
saaq_data[, 'events'] <- NA


# First version of models with seasonality.
# saaq_data[, 'month'] <- month(saaq_data[, 'dinf'])
saaq_data[, 'month'] <- substr(saaq_data[, 'dinf'], 6, 7)
table(saaq_data[, 'month'], useNA = "ifany")


# Second version has monthly seasonality and weekday indicator.
saaq_data[, 'weekday'] <- weekdays(saaq_data[, 'dinf'])
table(saaq_data[, 'weekday'], useNA = "ifany")
class(saaq_data[, 'weekday'])
saaq_data[, 'weekday'] <- factor(saaq_data[, 'weekday'],
                                 levels = c('Sunday',
                                            'Monday',
                                            'Tuesday',
                                            'Wednesday',
                                            'Thursday',
                                            'Friday',
                                            'Saturday'))
class(saaq_data[, 'weekday'])


################################################################################
# Estimation
################################################################################


# Set the combinations of model specifications to be estimated.

# These results are stored in different folders.
# past_pts_list <- c('all', 'high')
past_pts_list <- c('all')
# window_list <- c('4 yr.', 'Placebo')
# window_list <- c('4 yr.')
# seasonality_list <- c('included', 'excluded')
# seasonality_list <- c('excluded')

# These are stored in different files within the folders.
reg_list <- c('LPM', 'Logit')
# reg_list <- c('LPM')
# reg_list <- c('Logit')
# sex_list <- c('Both Sexes', 'Males', 'Females')
sex_list <- c('All', 'Male', 'Female')

# These combination are explored within a file.
pts_target_list <- c('all',
                     '1', '2', '3', '4', '5', '7',
                     '9+')
age_int_list <- c('no', 'with') # ..  age interactions


# Specify headings for each point level.
pts_headings <- data.frame(pts_target = pts_target_list,
                           heading = NA)
pts_headings[1, 'heading'] <- 'All violations combined'
pts_headings[2, 'heading'] <- 'One-point violations (for speeding 11-20 over)'
pts_headings[3, 'heading'] <- 'Two-point violations (speeding 21-30 over or 7 other violations)'
pts_headings[4, 'heading'] <- 'Three-point violations (speeding 31-60 over or 9 other violations)'
pts_headings[5, 'heading'] <- 'Four-point violations (speeding 31-45 over or 9 other violations)'
pts_headings[6, 'heading'] <- 'Five-point violations (speeding 46-60 over or a handheld device violation)'
pts_headings[7, 'heading'] <- 'Seven-point violations (speeding 61-80 over or combinations)'
pts_headings[8, 'heading'] <- 'All pairs of infractions 9 or over (speeding 81 or more and 10 other offences)'



############################################################
# Set List of Regression Specifications
############################################################

# Pick any of the four specification groups for the analysis.
# spec_group <- 'pooled'
spec_group <- 'all' # 8 NAs w QMLE
# spec_group <- 'high_pts' # 3 NAs w QMLE
# spec_group <- 'placebo'
# spec_group <- 'events'


if (spec_group == 'pooled') {

  #------------------------------------------------------------
  # Pooled regressions with separation by age group.
  #------------------------------------------------------------

  estn_version <- 11
  estn_file_name <- sprintf('estimates_v%d_%s.csv', estn_version, spec_group)
  estn_file_path <- sprintf('%s/%s', md_dir, estn_file_name)

  # Set the full list of model specification combinations.
  model_list <- expand.grid(past_pts = c('all'),
                            window = c('4 yr.'),
                            seasonality = c('mnwk'),
                            age_int = c('no', age_grp_list),
                            pts_target = c('all'),
                            sex = c('All'),
                            reg_type = c('LPM', 'Logit'))





} else if (spec_group == 'all') {

  #------------------------------------------------------------
  # Specification: All Drivers with Monthly and weekday seasonality
  #------------------------------------------------------------

  estn_version <- 12
  estn_file_name <- sprintf('estimates_v%d_%s.csv', estn_version, spec_group)
  estn_file_path <- sprintf('%s/%s', md_dir, estn_file_name)

  # Set the full list of model specification combinations.
  model_list <- expand.grid(past_pts = c('all'),
                            window = c('4 yr.'),
                            seasonality = c('mnwk'),
                            age_int = age_int_list,
                            pts_target = pts_target_list,
                            # sex = c('Male', 'Female'),
                            sex = sex_list,
                            reg_type = reg_list)

} else if (spec_group == 'high_pts') {

  #------------------------------------------------------------
  # Sensitivity Analysis: High-point drivers.
  # (with monthly and weekday seasonality)
  #------------------------------------------------------------

  # Set file name for alternate estimation.
  estn_version <- 13
  estn_file_name <- sprintf('estimates_v%d_%s.csv', estn_version, spec_group)
  estn_file_path <- sprintf('%s/%s', md_dir, estn_file_name)

  # Set the partial list of model specification combinations.
  model_list <- expand.grid(past_pts = c('high'),
                            # past_pts = c('all'),
                            window = c('4 yr.'),
                            seasonality = c('mnwk'),
                            age_int = age_int_list,
                            # pts_target = c('all'),
                            pts_target = pts_target_list,
                            sex = c('Male', 'Female'),
                            # sex = sex_list,
                            reg_type = reg_list)


} else if (spec_group == 'placebo') {

  #------------------------------------------------------------
  # Sensitivity Analysis: Placebo regression.
  # (with monthly and weekday seasonality)
  #------------------------------------------------------------

  # # Set file name for alternate estimation.
  estn_version <- 14
  estn_file_name <- sprintf('estimates_v%d_%s.csv', estn_version, spec_group)
  estn_file_path <- sprintf('%s/%s', md_dir, estn_file_name)

  # Set the partial list of model specification combinations.
  model_list <- expand.grid(past_pts = c('all'),
                            window = c('Placebo'),
                            # window = c('4 yr.'),
                            seasonality = c('mnwk'),
                            age_int = age_int_list,
                            pts_target = c('all'),
                            # pts_target = pts_target_list,
                            sex = c('Male', 'Female'),
                            # sex = sex_list,
                            reg_type = reg_list)

} else if (spec_group == 'events') {

  #------------------------------------------------------------
  # Specification: REAL event study with seasonality
  #------------------------------------------------------------

  estn_version <- 15
  estn_file_name <- sprintf('estimates_v%d_%s.csv', estn_version, spec_group)
  estn_file_path <- sprintf('%s/%s', md_dir, estn_file_name)

  # Set the full list of model specification combinations.
  model_list <- expand.grid(past_pts = c('all'),
                            window = c('Monthly 4 yr.'),
                            seasonality = c('mnwk'),
                            age_int = age_int_list,
                            # pts_target = pts_target_list,
                            pts_target = 'all',
                            sex = c('Male', 'Female'),
                            # sex = sex_list,
                            reg_type = reg_list)


} else {
  stop('Regression specification not recognized. ')
}


#------------------------------------------------------------
# Set parameters for marginal effects
#------------------------------------------------------------


# Categories of variables for marginal effects.
# mfx_month_list <- unique(saaq_data[, 'month'])
# mfx_weekday_list <- unique(saaq_data[, 'weekday'])
# mfx_curr_pts_list <- unique(saaq_data[, 'curr_pts_grp'])
# mfx_age_list <- unique(saaq_data[, 'age_grp'])

# Set parameters for typical driver who gets tickets.
# unique(saaq_data[, 'weekday'])
# table(saaq_data[, 'curr_pts_grp'])
# mfx_data_MER <- data.frame(TRUE, 'M', '20-24', '4-6', '07', 'Monday', 1)
# colnames(mfx_data_MER) <- c("policy", "sex", "age_grp",
#                             "curr_pts_grp", "month", "weekday", "num")
mfx_data_MER <- data.frame(TRUE, 'policyFALSE', 'M', '20-24', '4-6', '07', 'Monday', 1)
colnames(mfx_data_MER) <- c("policy", "policy_month", "sex", "age_grp",
                            "curr_pts_grp", "month", "weekday", "num")


# Calculate sandwich SE estimator for QMLE.
# est_QMLE_SEs <- TRUE
est_QMLE_SEs <- FALSE

#------------------------------------------------------------
# Run estimation in a loop on the model specifications.
#------------------------------------------------------------


# Initialize data frame to store estimation results.
estn_results <- NULL


# Initialize path.
md_path_last <- "empty"
# Sample block of code for inserting after data prep.
# estn_num <- 1
# estn_num <- 2
# estn_num <- 10
# estn_num <- 91
# model_list[estn_num, ]
# for (estn_num in 51:nrow(model_list)) {
for (estn_num in 1:nrow(model_list)) {

  # Extract parameters for this estimated model.
  past_pts_sel <- model_list[estn_num, 'past_pts']
  window_sel <- model_list[estn_num, 'window']
  season_incl <- model_list[estn_num, 'seasonality']
  reg_type <- model_list[estn_num, 'reg_type']
  sex_sel <- model_list[estn_num, 'sex']
  pts_target <- model_list[estn_num, 'pts_target']
  age_int <- model_list[estn_num, 'age_int']

  # Create the path and file name for the markdown file.
  md_sub_dir <- sprintf('spec_%s_past_pts_%s_%s_window_seas_%s',
                        spec_group,
                        past_pts_sel,
                        substr(window_sel, 1, 1),
                        substr(season_incl, 1, 4))
  md_file_name <- sprintf('results_%s_%s.md',
                          reg_type,
                          # substr(sex_sel, 1, 1),
                          sex_sel)
  md_path <- sprintf('%s/%s/%s', md_dir, md_sub_dir, md_file_name)


  # Create a message to describe this model.
  out_msg <- sprintf("Estimating model for %s drivers, %s point tickets.",
                     sex_sel, pts_target)

  # If a new path is created, open a new output file.
  if (md_path != md_path_last) {

    print(sprintf("Estimating for folder %s.", md_sub_dir))
    print(sprintf("Estimating for file %s.", md_file_name))

    # title_str <- sprintf('%s Estimates for %s Drivers',
    #                      reg_type, sex_sel)
    if (reg_type == 'LPM') {
      cat(sprintf('# Linear Probability Models - %s Drivers\n\n', sex_sel),
          file = md_path, append = FALSE)
      cat('## Linear Regression Results (Standard Errors with HCCME) \n\n',
          file = md_path, append = TRUE)
    } else if (reg_type == 'Logit') {
      cat(sprintf('# Logistic Regression Models - %s Drivers\n\n', sex_sel),
          file = md_path, append = FALSE)
      cat('## Logistic Regression Results \n\n',
          file = md_path, append = TRUE)
    }

    if (window_sel == 'Placebo') {
      cat('## Placebo Regressions \n\n',
          file = md_path, append = TRUE)
    } else if (window_sel == 'Monthly 4 yr.') {
      cat('## Regressions with Monthly Policy Dummies \n\n',
          file = md_path, append = TRUE)
    }

    if (age_int == 'no') {
      cat('## Regressions for Full Sample \n\n',
          file = md_path, append = TRUE)
    } else if (age_int %in% age_grp_list) {
      cat(sprintf('## Regressions for Drivers Aged %s \n\n', age_int),
          file = md_path, append = TRUE)
    }

  }
  md_path_last <- md_path
  print(out_msg)


  #--------------------------------------------------
  # Set event window around policy indicator.
  #--------------------------------------------------
  if (window_sel == '4 yr.' | window_sel == 'Monthly 4 yr.') {

    # Select symmetric window around the policy change.
    saaq_data[, 'window'] <- saaq_data[, 'dinf'] >= '2006-04-01' &
      saaq_data[, 'dinf'] <= '2010-03-31'

    # Set date of policy change.
    april_fools_date <- '2008-04-01'
    # No joke: policy change on April Fool's Day!

  } else if (window_sel == '2 yr.') {

    # Select two-year symmetric window around the policy change.
    saaq_data[, 'window'] <- saaq_data[, 'dinf'] >= '2007-04-01' &
      saaq_data[, 'dinf'] <= '2009-03-31'

    # Set date of policy change.
    april_fools_date <- '2008-04-01'
    # No joke: policy change on April Fool's Day!

  } else if (window_sel == 'Placebo') {

    # Select symmetric window around the placebo change.
    # Year before (2007):
    saaq_data[, 'window'] <- saaq_data[, 'dinf'] >= '2006-04-01' &
      saaq_data[, 'dinf'] <= '2008-03-31'

    # Set date of placebo policy change.
    april_fools_date <- '2007-04-01'
    # No joke: policy change on April Fool's Day!

  } else {
    stop(sprintf("Window setting '%s' not recognized.", window_sel))
  }
  # Generate the indicator for the policy change.
  saaq_data[, 'policy'] <- saaq_data[, 'dinf'] >= april_fools_date


  # Generate monthly indicators after the policy change (for learning rate).
  if (window_sel == 'Monthly 4 yr.') {
    # Two definitions:
    saaq_data[, 'policy_month'] <- NA
    saaq_data[saaq_data[, 'dinf'] < april_fools_date, 'policy_month'] <- "policyFALSE"
    saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
                saaq_data[, 'dinf'] >= as.Date('2009-04-01'), 'policy_month'] <- "policyFALSE"
    # First definition: Policy and month of year (yes, confusing but easy).
    # saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
    #             saaq_data[, 'dinf'] < as.Date('2009-04-01'), 'policy_month'] <-
    #   sprintf("policy%s", saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
    #                                   saaq_data[, 'dinf'] < as.Date('2009-04-01'), 'month'])
    # Second definition: Policy and month of year (yes, confusing but easy).
    saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
                saaq_data[, 'dinf'] < as.Date('2009-04-01') &
                saaq_data[, 'month'] == '04', 'policy_month'] <- 'policy01'
    saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
                saaq_data[, 'dinf'] < as.Date('2009-04-01') &
                saaq_data[, 'month'] == '05', 'policy_month'] <- 'policy02'
    saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
                saaq_data[, 'dinf'] < as.Date('2009-04-01') &
                saaq_data[, 'month'] == '06', 'policy_month'] <- 'policy03'
    saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
                saaq_data[, 'dinf'] < as.Date('2009-04-01') &
                saaq_data[, 'month'] == '07', 'policy_month'] <- 'policy04'
    saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
                saaq_data[, 'dinf'] < as.Date('2009-04-01') &
                saaq_data[, 'month'] == '08', 'policy_month'] <- 'policy05'
    saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
                saaq_data[, 'dinf'] < as.Date('2009-04-01') &
                saaq_data[, 'month'] == '09', 'policy_month'] <- 'policy06'
    saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
                saaq_data[, 'dinf'] < as.Date('2009-04-01') &
                saaq_data[, 'month'] == '10', 'policy_month'] <- 'policy07'
    saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
                saaq_data[, 'dinf'] < as.Date('2009-04-01') &
                saaq_data[, 'month'] == '11', 'policy_month'] <- 'policy08'
    saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
                saaq_data[, 'dinf'] < as.Date('2009-04-01') &
                saaq_data[, 'month'] == '12', 'policy_month'] <- 'policy09'
    saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
                saaq_data[, 'dinf'] < as.Date('2009-04-01') &
                saaq_data[, 'month'] == '01', 'policy_month'] <- 'policy10'
    saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
                saaq_data[, 'dinf'] < as.Date('2009-04-01') &
                saaq_data[, 'month'] == '02', 'policy_month'] <- 'policy11'
    saaq_data[saaq_data[, 'dinf'] >= april_fools_date &
                saaq_data[, 'dinf'] < as.Date('2009-04-01') &
                saaq_data[, 'month'] == '03', 'policy_month'] <- 'policy12'
    # In either case, transform it into factor.
    saaq_data[, 'policy_month'] <- factor(saaq_data[, 'policy_month'],
                                          levels = c('policyFALSE',
                                                     sprintf('policy0%d', 1:9),
                                                     sprintf('policy%d', 10:12)))
  }



  #--------------------------------------------------
  # Default event window around policy change
  # and impose any sample selection.
  #--------------------------------------------------
  if (past_pts_sel == 'all') {

    # All relevant observations.
    saaq_data[, 'sel_window'] <- saaq_data[, 'window']

  } else if (past_pts_sel == 'high') {

    # Additional subsetting for drivers with past
    # point balances between 6 and 10 points
    # in the pre-policy-change period.
    saaq_data[, 'sel_window'] <- saaq_data[, 'window'] &
      saaq_data[, 'past_active']

  } else {
    stop(sprintf("Past points setting '%s' not recognized.", past_pts_sel))
  }


  #--------------------------------------------------
  # Select subset of observations by sex.
  #--------------------------------------------------
  if (sex_sel == 'All') {

    saaq_data[, 'sel_obsn'] <- saaq_data[, 'sel_window']

  } else if (sex_sel %in% c('Male', 'Female')) {

    saaq_data[, 'sel_obsn'] <- saaq_data[, 'sex'] == substr(sex_sel, 1, 1) &
      saaq_data[, 'sel_window']

  } else {
    stop(sprintf("Sex selection '%s' not recognized.", sex_sel))
  }

  #--------------------------------------------------
  # Select subset of observations by age.
  #--------------------------------------------------
  if (age_int %in% age_grp_list) {
    saaq_data[, 'sel_obsn'] <- saaq_data[, 'sel_obsn'] &
      saaq_data[, 'age_grp'] %in% age_int
  }


  sel_obs <- saaq_data[, 'sel_obsn']


  #--------------------------------------------------
  # Define event as a combination of point balances.
  #--------------------------------------------------
  if (pts_target == 'all') {

    # All violations combined.
    saaq_data[, 'events'] <- saaq_data[, 'points'] > 0

  } else if (pts_target == '1') {

    # One point violations.
    saaq_data[, 'events'] <- saaq_data[, 'points'] == 1

  } else if (pts_target == '2') {

    # Two point violations.
    saaq_data[, 'events'] <- saaq_data[, 'points'] == 2

  } else if (pts_target == '3') {

    # Three point violations
    # (or 6-point violations that used to be 3-point violations).
    saaq_data[, 'events'] <- saaq_data[, 'points'] == 3 |
      saaq_data[, 'policy'] & saaq_data[, 'points'] == 6

  } else if (pts_target == '4') {

    # Four point violations.
    saaq_data[, 'events'] <- saaq_data[, 'points'] == 4

  } else if (pts_target == '5') {

    # Five point violations.
    # (or 10-point violations that used to be 5-point violations).
    saaq_data[, 'events'] <- saaq_data[, 'points'] == 5 |
      saaq_data[, 'policy'] & saaq_data[, 'points'] == 10

  } else if (pts_target == '7') {

    # Seven and fourteen point violations.
    # Seven point violations.
    # (or 14-point violations that used to be 7-point violations).
    saaq_data[, 'events'] <- saaq_data[, 'points'] == 7 |
      saaq_data[, 'policy'] & saaq_data[, 'points'] == 14

  } else if (pts_target == '9+') {

    # Nine point speeding violations and up (excluding the 10s and 14s above).
    saaq_data[, 'events'] <- saaq_data[, 'points'] %in% c(9, 12, 15, 18, 21,
                                                          24, 30, 36)

  } else {
    stop(sprintf("Point balance target '%s' not recognized.", pts_target_list))
  }


  #--------------------------------------------------
  # Set formula for regression model
  #--------------------------------------------------

  if (sex_sel == 'All') {
    # If sex variables included.
    # var_list <- c('policy', 'sex', 'sex*policy')
    # If same model estimated as that with subsamples separated by sex.
    var_list <- c('policy')
  } else if (sex_sel %in% c('Male', 'Female')) {
    var_list <- c('policy')
  } else {
    stop(sprintf("Sex selection '%s' not recognized.", sex_sel))
  }
  if (window_sel == 'Monthly 4 yr.') {
    var_list <- c(var_list, 'policy_month')
  }
  if (age_int == 'with') {
    # var_list <- c(var_list, 'policy*age_grp')
    var_list <- c(var_list, 'age_grp', 'policy*age_grp')
  } else if (age_int == 'no') {
    # no variables added, except age.
    # var_list <- var_list
    var_list <- c(var_list, 'age_grp')
  } else if (age_int %in% age_grp_list) {
    # no variables added.
    var_list <- var_list
  } else {
    stop(sprintf("Age indicator selector '%s' not recognized.", age_int))
  }
  # var_list <- c(var_list, 'age_grp', 'curr_pts_grp')
  var_list <- c(var_list, 'curr_pts_grp')
  if (season_incl == 'monthly') {
    var_list <- c(var_list, 'month')
  } else if (season_incl == 'mnwk') {
    var_list <- c(var_list, 'month', 'weekday')
  } else if (season_incl == 'excluded') {
    # No variables added.
    var_list <- var_list
  } else {
    stop(sprintf("Seasonality indicator selector '%s' not recognized.", season_incl))
  }

  fmla <- as.formula(sprintf('events ~ %s',
                             paste(var_list, collapse = " + ")))



  #--------------------------------------------------
  # Run regressions
  #--------------------------------------------------
  if (reg_type == 'LPM') {

    # Estimating a Linear Probability Model


    # Estimate the model accounting for the aggregated nature of the data.
    agg_lm_model_1 <- agg_lm(data = saaq_data[sel_obs, ], weights = num,
                             formula = fmla, x = TRUE)
    summ_agg_lm <- summary_agg_lm(agg_lm_model_1)

    # Adjust standard errors for heteroskedasticity.
    agg_lpm_hccme_1 <- white_hccme_med(agg_lm_model_1)
    summ_model <- agg_lpm_hccme_1
    # print(agg_lpm_hccme_1$coef_hccme)

    est_coefs <- summ_model$coef_hccme

    # # Checking for negative LPM predictions.
    # lpm_neg_check(lm(data = saaq_data[sel_obs, ], weights = num,
    #                  formula = chosen_model))

  } else if (reg_type == 'Logit') {

    # Estimate logistic regression model.
    log_model_1 <- glm(data = saaq_data[sel_obs, ], weights = num,
                       formula = fmla,
                       family = 'binomial')
    summ_model <- summary(log_model_1)

    est_coefs <- summ_model$coefficients

    # Calculate sandwich SE estimator for QMLE.
    if (est_QMLE_SEs == TRUE) {

      # The MLE covariance estimator is the inverse Hessian matrix.
      # This is the bread of the sandwich.
      V = vcov(log_model_1)

      # Now calculate OPG.
      y <- as.integer(saaq_data[sel_obs, 'events'])
      num_weights <- saaq_data[sel_obs, 'num']
      num_obs <- sum(num_weights)
      p <- fitted(log_model_1)

      # Now calculate a weighting matrix.
      g <- (y-p) * sqrt(num_weights)
      # Note that the outer product will get back the weights.

      # The full design matrix:
      X <- model.matrix(log_model_1)
      kX <- ncol(X)
      Xg <- X * matrix(rep(g, each = kX), ncol = kX, byrow = TRUE)

      # This is the meat of the sandwich.
      # XppX <- t(Xg) %*% X
      XppX <- t(Xg) %*% Xg


      # Now make a sandwich.
      VS <- V %*% XppX %*% V

      # The standard errors are the diagonals.
      est_coefs[, 'Std. Error'] <- sqrt(diag(VS))

      # Recalculate z-statistic and p-values.
      est_coefs[, 'z value'] <- est_coefs[, 'Estimate'] /
        est_coefs[, 'Std. Error']
      est_coefs[, 'Pr(>|z|)'] <- 2*pt(- abs(est_coefs[, 'z value']),
                                      df = num_obs - kX - 1)

    }



  } else {
    stop(sprintf("Model type '%s' not recognized.", reg_type))
  }


  #--------------------------------------------------
  # Calculate marginal effects, if appropriate.
  #--------------------------------------------------


  # Only meaningful for logit regressions.
  if (reg_type == 'Logit') {

    # Set selected variables for MER calculation.
    # mfx_data_MER_adj <- mfx_data_MER[, c(var_list, "num")]
    # Extra columns don't matter.
    mfx_data_MER_adj <- mfx_data_MER
    # Adjust sex variables, if necessary.
    if ("sex" %in% var_list) {
      if (sex_sel %in% c('All', 'Males')) {
        mfx_data_MER_adj[1, 'sex'] <- 'M'
      } else if (sex_sel == 'F') {
        mfx_data_MER_adj[1, 'sex'] <- 'F'
      } else {
        stop('Selected sex not recognized.')
      }
    }


    # Case depends on whether there are interaction terms.
    if ((age_int %in% c('no', age_grp_list)) &
        !(window_sel == 'Monthly 4 yr.')) {
      #------------------------------------------------------------
      # Only policy MFX required for tables.
      # But add MFX for age groups.
      #------------------------------------------------------------
      # mfx_mat = data.frame(AME = NA, MER = NA)
      # rownames(mfx_mat) <- 'policyTRUE'
      mfx_mat = data.frame(AME = rep(NA, length(age_grp_list)),
                           MER = rep(NA, length(age_grp_list)))
      rownames(mfx_mat) <- c('policyTRUE',
                             sprintf('age_grp%s',
                                     age_grp_list[2:length(age_grp_list)]))

      #------------------------------------------------------------
      # Average marginal effect.
      #------------------------------------------------------------

      # Create dataset for calculation sample of predictions.
      mfx_fmla_list <- var_list
      mfx_fmla <- as.formula(sprintf('num ~ %s',
                                     paste(mfx_fmla_list, collapse = ' + ')))
      saaq_data_pred <- aggregate(formula = mfx_fmla,
                                  data = saaq_data[sel_obs, ],
                                  FUN = sum)

      # Assign difference to AME.
      mfx_mat['policyTRUE', 'AME'] <- mfx_AME_diff(saaq_data_pred, log_model_1)


      #------------------------------------------------------------
      # Marginal effect at representative values.
      #------------------------------------------------------------

      # Single-row dataset for calculation of predictions.
      saaq_data_pred <- mfx_data_MER_adj

      # Assign difference to MER.
      mfx_mat['policyTRUE', 'MER'] <- mfx_AME_diff(saaq_data_pred, log_model_1)

      # Set row number for age_grp MFX.
      last_mfx_row <- 1


    } else if ((age_int == 'with') &
               !(window_sel == 'Monthly 4 yr.')) {
      #------------------------------------------------------------
      # Both policy and policy-age MFX required for tables.
      # But add MFX for age groups.
      #------------------------------------------------------------
      mfx_mat = data.frame(AME = rep(NA, 2*length(age_grp_list) - 1),
                           MER = rep(NA, 2*length(age_grp_list) - 1))
      rownames(mfx_mat) <- c('policyTRUE',
                             sprintf('policyTRUE:age_grp%s',
                                     age_grp_list[2:length(age_grp_list)]),
                             sprintf('age_grp%s',
                                     age_grp_list[2:length(age_grp_list)]))

      # Create dataset for calculation sample of predictions.
      mfx_fmla_list <- var_list
      mfx_fmla <- as.formula(sprintf('num ~ %s',
                                     paste(mfx_fmla_list, collapse = ' + ')))


      # mfx_age_num <- 1
      # mfx_age_num <- 2
      for (mfx_age_num in 1:length(age_grp_list)) {

        #------------------------------------------------------------
        # Average marginal effect.
        #------------------------------------------------------------

        mfx_age_sel <- age_grp_list[mfx_age_num]
        mfx_sel_obs <- sel_obs & saaq_data[, 'age_grp'] == mfx_age_sel
        saaq_data_pred <- aggregate(formula = mfx_fmla,
                                    data = saaq_data[mfx_sel_obs, ],
                                    FUN = sum)
        # Assign difference to AME.
        if (mfx_age_num == 1) {
          mfx_mat['policyTRUE', 'AME'] <- mfx_AME_diff(saaq_data_pred, log_model_1)
        } else {
          cross_coefficient <- est_coefs[sprintf('policyTRUE:age_grp%s', mfx_age_sel), 'Estimate']
          mfx_mat[mfx_age_num, 'AME'] <- mfx_AME_cross_diff(saaq_data_pred, log_model_1,
                                                            cross_coefficient)
        }

        #------------------------------------------------------------
        # Marginal effect at representative values.
        #------------------------------------------------------------

        # Single-row dataset for calculation of predictions.
        saaq_data_pred <- mfx_data_MER_adj

        # Set age group.
        saaq_data_pred[, 'age_grp'] <- mfx_age_sel

        # Assign difference to MER.
        if (mfx_age_num == 1) {
          mfx_mat['policyTRUE', 'MER'] <- mfx_AME_diff(saaq_data_pred, log_model_1)
        } else {
          # cross_coefficient is the same as for AME.
          mfx_mat[mfx_age_num, 'MER'] <- mfx_AME_cross_diff(saaq_data_pred, log_model_1,
                                                            cross_coefficient)
        }



      }
      # Set row number for age_grp MFX.
      last_mfx_row <- mfx_age_num



    } else if ((age_int == 'no') &
               (window_sel == 'Monthly 4 yr.')) {
      #------------------------------------------------------------
      # Both policy and policy-month MFX required for tables.
      # But add MFX for age groups.
      #------------------------------------------------------------
      mfx_mat = data.frame(AME = rep(NA, (12 + 1) + (length(age_grp_list) - 1)),
                           MER = rep(NA, (12 + 1) + (length(age_grp_list) - 1)))

      policy_num_str <- sprintf('00%d', seq(12))
      policy_num_str <- substr(policy_num_str, (nchar(policy_num_str) - 1), nchar(policy_num_str))
      policy_var_str <- sprintf('policy%s', policy_num_str)
      rownames(mfx_mat) <- c('policyTRUE',
                             sprintf('policy_month%s', policy_var_str),
                             sprintf('age_grp%s',
                                     age_grp_list[2:length(age_grp_list)]))

      # Create dataset for calculation sample of predictions.
      mfx_fmla_list <- var_list
      mfx_fmla <- as.formula(sprintf('num ~ %s',
                                     paste(mfx_fmla_list, collapse = ' + ')))


      # mfx_month_num <- 1
      # mfx_month_num <- 2
      for (mfx_month_num in seq(13)) {

        #------------------------------------------------------------
        # Average marginal effect.
        #------------------------------------------------------------

        if (mfx_month_num == 1) {
          # mfx_sel_obs <- sel_obs & saaq_data[, 'policyTRUE'] == TRUE
          mfx_sel_obs <- sel_obs & saaq_data[, 'policy'] == TRUE
          mfx_month_sel <- 'policyFALSE'
        } else {
          policy_num_str <- sprintf('00%d', mfx_month_num - 1)
          policy_num_str <- substr(policy_num_str, (nchar(policy_num_str) - 1), nchar(policy_num_str))
          mfx_month_sel <- sprintf('policy%s', policy_num_str)
          mfx_sel_obs <- sel_obs & saaq_data[, 'policy_month'] == mfx_month_sel
        }


        saaq_data_pred <- aggregate(formula = mfx_fmla,
                                    data = saaq_data[mfx_sel_obs, ],
                                    FUN = sum)

        # Assign difference to AME.
        if (mfx_month_num == 1) {
          mfx_mat['policyTRUE', 'AME'] <- mfx_AME_diff(saaq_data_pred, log_model_1)
        } else {
          cross_coefficient <- est_coefs[sprintf('policy_month%s', mfx_month_sel), 'Estimate']
          mfx_mat[mfx_month_num, 'AME'] <- mfx_AME_cross_diff(saaq_data_pred, log_model_1,
                                                            cross_coefficient)
        }

        #------------------------------------------------------------
        # Marginal effect at representative values.
        #------------------------------------------------------------

        # Single-row dataset for calculation of predictions.
        saaq_data_pred <- mfx_data_MER_adj

        # Set age group.
        saaq_data_pred[, 'policy_month'] <- mfx_month_sel

        # Assign difference to MER.
        if (mfx_month_num == 1) {
          mfx_mat['policyTRUE', 'MER'] <- mfx_AME_diff(saaq_data_pred, log_model_1)
        } else {
          # cross_coefficient is the same as for AME.
          mfx_mat[mfx_month_num, 'MER'] <- mfx_AME_cross_diff(saaq_data_pred, log_model_1,
                                                            cross_coefficient)
        }


      }
      # Set row number for age_grp MFX.
      last_mfx_row <- mfx_month_num

    }



    #------------------------------------------------------------
    # Calculate MFX for age groups (all models)
    # (unless sample restricted to one age group).
    #------------------------------------------------------------

    if (!(age_int %in% age_grp_list)) {

      # Create dataset for calculation sample of predictions.
      mfx_fmla_list <- var_list
      mfx_fmla <- as.formula(sprintf('num ~ %s',
                                     paste(mfx_fmla_list, collapse = ' + ')))

      # Skip the first age group (the benchmark).
      for (mfx_age_num in 2:length(age_grp_list)) {

        mfx_row <- last_mfx_row + mfx_age_num - 1

        #------------------------------------------------------------
        # Average marginal effect.
        #------------------------------------------------------------

        # Benchmark group is first age category.
        mfx_age_bench <- age_grp_list[1]

        mfx_age_sel <- age_grp_list[mfx_age_num]
        mfx_sel_obs <- sel_obs & saaq_data[, 'age_grp'] == mfx_age_sel
        saaq_data_pred <- aggregate(formula = mfx_fmla,
                                    data = saaq_data[mfx_sel_obs, ],
                                    FUN = sum)

        # Set value of policy variable.
        saaq_data_pred[, 'policy'] <- FALSE

        # Assign difference to AME.
        mfx_mat[mfx_row, 'AME'] <- mfx_AME_diff(saaq_data_pred, log_model_1,
                                                mfx_var = 'age_grp',
                                                before_val = mfx_age_bench,
                                                after_val = mfx_age_sel)

        #------------------------------------------------------------
        # Marginal effect at representative values.
        #------------------------------------------------------------

        # Single-row dataset for calculation of predictions.
        saaq_data_pred <- mfx_data_MER_adj

        # Set value of policy variable.
        saaq_data_pred[, 'policy'] <- FALSE

        # Assign difference to MER.
        mfx_mat[mfx_row, 'MER'] <- mfx_AME_diff(saaq_data_pred, log_model_1,
                                                mfx_var = 'age_grp',
                                                before_val = mfx_age_bench,
                                                after_val = mfx_age_sel)

      }



    }

    #------------------------------------------------------------
    # Gross up to same units as the linear probability model.
    mfx_mat <- mfx_mat*100000
    #------------------------------------------------------------




  }




  #--------------------------------------------------
  # Print the results.
  #--------------------------------------------------

  # Print section headings in README file.
  pts_heading_sel <- pts_headings[
    pts_headings[, 'pts_target'] == pts_target, 'heading']
  cat(sprintf('\n\n### %s\n\n', pts_heading_sel),
      file = md_path, append = TRUE)


  # Print regression output.
  cat('\n```', file = md_path, append = TRUE)
  var_label <- sprintf("%s                         ", "Variable")
  cat(sprintf(" \n%s", substr(var_label, 1, 25)),
      file = md_path, append = TRUE)
  cat(paste(sprintf("     %s    ", colnames(est_coefs)), collapse = ""),
      file = md_path, append = TRUE)
  for (print_row in 1:nrow(est_coefs)) {
    var_label <- sprintf("%s                         ", rownames(est_coefs)[print_row])
    cat(sprintf(" \n%s", substr(var_label, 1, 25)),
        file = md_path, append = TRUE)
    cat(sprintf("     % 9.6g  ", est_coefs[print_row, ]),
        file = md_path, append = TRUE)
  }
  cat('\n```\n', file = md_path, append = TRUE)


  # Print marginal effects, if appropriate.
  if (reg_type == 'Logit') {
    cat('\n\n\n```\n', file = md_path, append = TRUE)
    # cat(mfx_mat, file = md_path, append = TRUE)
    cat(sprintf('%s          %s\n',
                colnames(mfx_mat)[1], colnames(mfx_mat)[2]),
        file = md_path, append = TRUE)
    for (mfx_row in 1:nrow(mfx_mat)) {
      cat(sprintf('%s          %f\n',
                  mfx_mat[mfx_row, 1], mfx_mat[mfx_row, 2]),
          file = md_path, append = TRUE)
    }
    cat('\n```\n\n', file = md_path, append = TRUE)
  }


  #--------------------------------------------------
  # Store the results for tables.
  #--------------------------------------------------

  # Rearrange coefficient matrix to add a column for row names.
  est_coefs_df <- data.frame(est_coefs)
  est_coefs_df[, 'Variable'] <- rownames(est_coefs)
  est_coefs_df <- est_coefs_df[, c(5, 1:4)]
  colnames(est_coefs_df) <- c('Variable', 'Estimate', 'Std_Error',
                              'z_stat', 'p_value')

  estn_results_sub <- cbind(model_list[rep(estn_num, nrow(est_coefs_df)), ],
                            est_coefs_df)

  # Append a column for marginal effects.
  # estn_results_sub[, 'mfx'] <- NA
  estn_results_sub[, 'AME'] <- NA
  estn_results_sub[, 'MER'] <- NA
  # Insert values for marginal effects, if appropriate.

  # Add sample size to data frame for tables.
  estn_results_sub[, 'N'] <- sum(saaq_data[sel_obs, 'num'])

  if (reg_type == 'Logit') {


    # Easy way:
    # Make the rownames match in both tables.
    # estn_results_sub[rownames(mfx_mat), c('AME', 'MER')] <- mfx_mat[, c('AME', 'MER')]
    # But that requires changing the rownames.

    # Instead, assign values by subset.


    # Policy MFX required for all tables.
    estn_results_sub[estn_results_sub[, 'Variable'] == 'policyTRUE',
                     c('AME', 'MER')] <- mfx_mat[1, c('AME', 'MER')]



    if ((age_int %in% c('no', age_grp_list)) &
        !(window_sel == 'Monthly 4 yr.')) {
      # # Only policy MFX required for tables.
      # estn_results_sub[estn_results_sub[, 'Variable'] == 'policyTRUE',
      #                  c('AME', 'MER')] <- mfx_mat[1, c('AME', 'MER')]


    } else if ((age_int == 'with') &
               !(window_sel == 'Monthly 4 yr.')) {
      # # Both policy and policy-age MFX required for tables.
      # estn_results_sub[estn_results_sub[, 'Variable'] == 'policyTRUE',
      #                  c('AME', 'MER')] <- mfx_mat[1, c('AME', 'MER')]
      # estn_results_sub[
      #   substr(estn_results_sub[, 'Variable'], 1, 18) == 'policyTRUE:age_grp',
      #   c('AME', 'MER')] <- mfx_mat[2:nrow(mfx_mat), c('AME', 'MER')]

      # Base it on contents of model and mfx_mat.
      estn_row_sel <-
        substr(estn_results_sub[, 'Variable'], 1, 18) == 'policyTRUE:age_grp'
      mfx_row_sel <- substr(rownames(mfx_mat), 1, 18) == 'policyTRUE:age_grp'
      estn_results_sub[estn_row_sel, c('AME', 'MER')] <-
        mfx_mat[mfx_row_sel, c('AME', 'MER')]


    } else if ((age_int == 'no') &
               (window_sel == 'Monthly 4 yr.')) {
      # # Both policy and policy-month MFX required for tables.
      # estn_results_sub[estn_results_sub[, 'Variable'] == 'policyTRUE',
      #                  c('AME', 'MER')] <- mfx_mat[1, c('AME', 'MER')]
      # estn_results_sub[
      #   substr(estn_results_sub[, 'Variable'], 1, 12) == 'policy_month',
      #   c('AME', 'MER')] <- mfx_mat[2:nrow(mfx_mat), c('AME', 'MER')]

      estn_row_sel <-
        substr(estn_results_sub[, 'Variable'], 1, 12) == 'policy_month'
      mfx_row_sel <- substr(rownames(mfx_mat), 1, 12) == 'policy_month'
      estn_results_sub[estn_row_sel, c('AME', 'MER')] <-
        mfx_mat[mfx_row_sel, c('AME', 'MER')]


    }


    # Age MFX required for all tables.
    estn_row_sel <-
      substr(estn_results_sub[, 'Variable'], 1, 7) == 'age_grp'
    mfx_row_sel <- substr(rownames(mfx_mat), 1, 7) == 'age_grp'
    estn_results_sub[estn_row_sel, c('AME', 'MER')] <-
      mfx_mat[mfx_row_sel, c('AME', 'MER')]

  }

  # Bind it to the full data frame of results.
  estn_results <- rbind(estn_results, estn_results_sub)

}

# Save the data frame of estimates.
write.csv(estn_results, file = estn_file_path)


################################################################################
# End
################################################################################


# Sample saved to test in (ugh!) Stata:

sel_cols <- c("dinf", "sex", "age_grp", "curr_pts_grp",
              "past_active", "points", "num",
              "policy", "events",
              "month", "weekday")

saaq_test_data <- saaq_data[sel_obs, sel_cols]

summary(saaq_test_data)

logical_var_list <- colnames(saaq_test_data)[sapply(saaq_test_data, class) == "logical"]
for (col_name in logical_var_list) {
  saaq_test_data[, col_name] <- as.integer(saaq_test_data[, col_name])
}

summary(saaq_test_data)


table(saaq_test_data[, 'month'], useNA = 'ifany')
table(month.abb[as.integer(saaq_test_data[, 'month'])], useNA = 'ifany')

saaq_test_data[, 'month'] <- month.abb[as.integer(saaq_test_data[, 'month'])]

summary(saaq_test_data)



test_file_name <- sprintf('saaq_agg_%d_sub_test.csv', pts_version)
test_file_name <- sprintf('%s%s', data_in_path, test_file_name)

write.csv(file = test_file_name,
          saaq_test_data, row.names = FALSE)


# Now I have to take a shower and burn my clothing.
