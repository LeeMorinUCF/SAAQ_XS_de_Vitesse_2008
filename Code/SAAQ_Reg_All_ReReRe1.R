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
# November 26, 2021
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

# Set working directory, if running interactively.
drive_path <- 'C:/Users/le279259/OneDrive - University of Central Florida/Documents'
git_path <- 'Research/SAAQ/SAAQspeeding/SAAQ_XS_de_Vitesse_2008'
wd_path <- sprintf('%s/%s',drive_path, git_path)
setwd(wd_path)


# The original data are stored in 'SAAQdata/origData/'.
# dataInPath <- 'SAAQdata_full/'
# data_in_path <- '~/Research/SAAQ/SAAQdata_full/'
# data_in_path <- 'C:/Users/le279259/Documents/Research/SAAQ/SAAQdata_full/'
data_in_path <- 'Data'

# The data of demerit point counts are stored in 'SAAQdata/seqData/'.
# dataOutPath <- 'SAAQspeeding/SAAQspeeding/'
# data_out_path <- '~/Research/SAAQ/SAAQspeeding/SAAQspeeding/'
# data_out_path <- 'C:/Users/le279259/Documents/Research/SAAQ/SAAQspeeding/SAAQspeeding/'
data_out_path <- 'Estn'


# Set directory for results in GitHub repo.
# git_path <- "~/Research/SAAQ/SAAQspeeding/Hidden_Comp_Risks/R_and_R"
# git_path <- "C:/Users/le279259/Documents/Research/SAAQ/SAAQspeeding/Hidden_Comp_Risks/R_and_R"
# md_dir <- sprintf("%s/results", git_path)
md_dir <- sprintf("%s/results", data_out_path)
# md_dir <- "results"

# Read script for calculating marginal effects.
# mfx_path <- sprintf("%s/Code/SAAQ_MFX_lib2.R", wd_path)
mfx_path <- "Code/SAAQ_MFX_lib2.R"
source(mfx_path)

# Read library of functions for regression modeling.
reg_lib_path <- "Code/SAAQ_Reg_lib1.R"
source(reg_lib_path)


# Load functions for regressions with aggregated data.
agg_reg_path <- "Research/aggregress/aggregress/R"
agg_reg_path <- sprintf('%s/%s',drive_path, agg_reg_path)
agg_reg_file <- sprintf("%s/aggregress.R", agg_reg_path)
source(agg_reg_file)
agg_reg_het_file <- sprintf("%s/aggregress_het.R", agg_reg_path)
source(agg_reg_het_file)



# Set version of input files.
data_in_method <- 'all_unadj'

# Set name of input file for training, testing and estimation samples.
train_file_name <- sprintf('saaq_%s_train.csv', data_in_method)
test_file_name <- sprintf('saaq_%s_test.csv', data_in_method)
# estn_file_name <- sprintf('saaq_%s_estn.csv', data_in_method)




# # Set version of output file.
# estn_version <- 99
# estn_file_name <- sprintf('estimates_v%d.csv', estn_version)
# estn_file_path <- sprintf('%s/%s', md_dir, estn_file_name)

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

# Weekday indicators.
weekday_list <- c('Sunday',
                  'Monday',
                  'Tuesday',
                  'Wednesday',
                  'Thursday',
                  'Friday',
                  'Saturday')

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

summary(saaq_train[, .N, by = date])
head(saaq_train, 394)


table(saaq_train[, sex], useNA = 'ifany')

table(saaq_train[, age_grp], useNA = 'ifany')

table(saaq_train[, past_active], useNA = 'ifany')

table(saaq_train[, past_active], saaq_train[, sex], useNA = 'ifany')

table(saaq_train[, curr_pts_grp], saaq_train[, past_active], useNA = 'ifany')



length(unique(saaq_train[, date]))
# [1] 1461 days of driving.

2*length(age_group_list)*2*length(curr_pts_grp_list)
# [1] 392 combinations of categories per day.

# Observations added with observed tickets.
nrow(saaq_train) - 2*length(age_group_list)*2*length(curr_pts_grp_list)*1826


# Tabulate the points, which are the events to be predicted.
# saaq_train[date >= sample_beg & date <= sample_end,
#                 sum(as.numeric(num)), by = points][order(points)]


#-------------------------------------------------------------------------------
# Load Testing Dataset
#-------------------------------------------------------------------------------

# Dataset for out-of-sample model testing.
in_path_file_name <- sprintf('%s/%s', data_in_path, test_file_name)
saaq_test <- fread(in_path_file_name)

summary(saaq_test)

summary(saaq_test[, .N, by = date])
head(saaq_test, 394)


table(saaq_test[, sex], useNA = 'ifany')

table(saaq_test[, age_grp], useNA = 'ifany')

table(saaq_test[, past_active], useNA = 'ifany')

table(saaq_test[, past_active], saaq_test[, sex], useNA = 'ifany')

table(saaq_test[, curr_pts_grp], saaq_test[, past_active], useNA = 'ifany')



length(unique(saaq_test[, date]))
# [1] 1461 days of driving.

2*length(age_group_list)*2*length(curr_pts_grp_list)
# [1] 392 combinations of categories per day.

# Observations added with observed tickets.
nrow(saaq_test) - 2*length(age_group_list)*2*length(curr_pts_grp_list)*1826


# Tabulate the points, which are the events to be predicted.
# saaq_test[date >= sample_beg & date <= sample_end,
#           sum(as.numeric(num)), by = points][order(points)]


################################################################################
# Stack the datasets and label by sample
################################################################################

saaq_train[, sample := 'train']
saaq_test[, sample := 'test']
saaq_data <- rbind(saaq_train, saaq_test)

rm(saaq_train, saaq_test)





saaq_data[, sum(as.numeric(num)), by = points][order(points)]

colnames(saaq_data)

sapply(saaq_data, class)


################################################################################
# Define additional variables
################################################################################

# Define categorical variables as factors.
saaq_data[, sex := factor(sex, levels = c('M', 'F'))]
saaq_data[, age_grp := factor(age_grp, levels = age_group_list)]
saaq_data[, curr_pts_grp := factor(curr_pts_grp, levels = curr_pts_grp_list)]

# Define new variables for seasonality.
# Numeric indicator for month.
# saaq_data[, 'month'] <- substr(saaq_data[, 'date'], 6, 7)
saaq_data[, month := substr(date, 6, 7)]
month_list <- unique(saaq_data[, month])
month_list <- month_list[order(month_list)]
saaq_data[, month := factor(month, levels = month_list)]
table(saaq_data[, 'month'], useNA = "ifany")

# Weekday indicator.
saaq_data[, weekday := weekdays(date)]
saaq_data[, weekday := factor(weekday, levels = weekday_list)]
table(saaq_data[, 'weekday'], useNA = "ifany")

# Define the indicator for the policy change.
saaq_data[, policy := date >= april_fools_date]

summary(saaq_data)

#--------------------------------------------------------------------------------
# Create new factors by consolidating some categories
#--------------------------------------------------------------------------------

# Age groups.
table(saaq_data[, 'age_grp'], useNA = 'ifany')

# Age groups already consolidated in data prep.
class(saaq_data[, age_grp])
age_grp_list <- levels(saaq_data[, age_grp])


# Otherwise, consolidate age group categories.
# age_grp_list <- levels(saaq_data[, 'age_grp'])
# orig_age_grp_list <- unique(saaq_data[, 'age_grp'])
# saaq_data[, 'age_grp_orig'] <- saaq_data[, 'age_grp']
# age_grp_list <- c(orig_age_grp_list[seq(7)], '65-199')
#
# saaq_data[, 'age_grp'] <- as.factor(NA)
# levels(saaq_data[, 'age_grp']) <- age_grp_list
# age_group_sel <- saaq_data[, 'age_grp_orig'] %in% orig_age_grp_list[seq(7)]
# saaq_data[age_group_sel, 'age_grp'] <- saaq_data[age_group_sel, 'age_grp_orig']
# saaq_data[!age_group_sel, 'age_grp'] <- age_grp_list[8]
#
#
# # Trust but verify.
# table(saaq_data[, 'age_grp'],
#       saaq_data[, 'age_grp_orig'], useNA = 'ifany')
# # Check.






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
saaq_data[, curr_pts_grp_sel := curr_pts_grp_orig %in% curr_pts_grp_list[1]]
saaq_data[curr_pts_grp_sel == TRUE, curr_pts_grp := curr_pts_grp_orig]
# Add groups 1-3.
saaq_data[, curr_pts_grp_sel := curr_pts_grp_orig %in% curr_pts_grp_list[2:4]]
saaq_data[curr_pts_grp_sel == TRUE, curr_pts_grp := new_curr_pts_grp_list[2]]
# Add groups 4-6.
saaq_data[, curr_pts_grp_sel := curr_pts_grp_orig %in% curr_pts_grp_list[5:7]]
saaq_data[curr_pts_grp_sel == TRUE, curr_pts_grp := new_curr_pts_grp_list[3]]
# Add groups 7-9.
saaq_data[, curr_pts_grp_sel := curr_pts_grp_orig %in% curr_pts_grp_list[8:10]]
saaq_data[curr_pts_grp_sel == TRUE, curr_pts_grp := new_curr_pts_grp_list[4]]
# Add the rest: 10-150.
saaq_data[, curr_pts_grp_sel := curr_pts_grp_orig %in% curr_pts_grp_list[11:14]]
saaq_data[curr_pts_grp_sel == TRUE, curr_pts_grp := new_curr_pts_grp_list[5]]


# Reset levels of new curr_pts_grp factor.
saaq_data[, curr_pts_grp := factor(curr_pts_grp,
                                   levels = new_curr_pts_grp_list)]
table(saaq_data[, 'curr_pts_grp'], useNA = 'ifany')


# Trust but verify.
table(saaq_data[, curr_pts_grp],
      saaq_data[, curr_pts_grp_orig], useNA = 'ifany')
# Check.



################################################################################
# Generate New Variables to be defined within loop
################################################################################


# Generate variables for regressions.
saaq_data[, 'policy'] <- NA
saaq_data[, 'window'] <- NA
saaq_data[, 'events'] <- NA



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

#------------------------------------------------------------
# Pooled regressions with separation by age group.
# spec_group <- 'pooled'
# estn_version <- 11
#------------------------------------------------------------

#------------------------------------------------------------
# Specification: All Drivers with Monthly and weekday seasonality
spec_group <- 'all'
estn_version <- 12
#------------------------------------------------------------

#------------------------------------------------------------
# Sensitivity Analysis: High-point drivers.
# (with monthly and weekday seasonality)
# spec_group <- 'high_pts'
# estn_version <- 13
#------------------------------------------------------------

#------------------------------------------------------------
# Sensitivity Analysis: Placebo regression.
# spec_group <- 'placebo'
# estn_version <- 14
#------------------------------------------------------------

#------------------------------------------------------------
# Specification: REAL event study with seasonality
# spec_group <- 'events'
# estn_version <- 15
#------------------------------------------------------------

#------------------------------------------------------------
# Specification: Plot by demerit point groups
# spec_group <- 'points'
# estn_version <- 16
#------------------------------------------------------------


# source(reg_lib_path)

model_list <- model_spec(spec_group,
                         sex_list, age_grp_list, age_int_list,
                         pts_target_list, reg_list)

# Set path for printed results.
estn_file_name <- sprintf('estimates_v%d_%s.csv', estn_version, spec_group)
estn_file_path <- sprintf('%s/%s', md_dir, estn_file_name)





############################################################
# Set parameters for marginal effects
############################################################


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
mfx_data_MER <- data.frame(TRUE, 'policyFALSE', 'M', '20-24',
                           '4-6', '07', 'Monday', 1)
colnames(mfx_data_MER) <- c("policy", "policy_month", "sex", "age_grp",
                            "curr_pts_grp", "month", "weekday", "num")


# Calculate sandwich SE estimator for QMLE.
# est_QMLE_SEs <- TRUE
est_QMLE_SEs <- FALSE

#------------------------------------------------------------
# Run estimation in a loop on the model specifications.
#------------------------------------------------------------

# source(reg_lib_path)

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

  # Set paths and headers for regression output to markdown files.
  md_path <- md_headers(md_dir, md_path_last,
                        spec_group,
                        reg_type,
                        sex_sel,
                        age_int,
                        pts_target,
                        past_pts_sel,
                        window_sel,
                        season_incl)
  md_path_last <- md_path

  #--------------------------------------------------
  # Data preparation.
  # Calculate variables specific to the model specification.
  #--------------------------------------------------

  saaq_data <- saaq_data_prep(saaq_data,
                              window_sel,
                              past_pts_sel,
                              sex_sel,
                              age_int, age_grp_list,
                              pts_target)
  # sel_obs <- saaq_data[, 'sel_obsn']


  #--------------------------------------------------
  # Set formula for regression model
  #--------------------------------------------------

  var_list <- reg_var_list(sex_sel,
                           window_sel,
                           age_int, age_grp_list,
                           season_incl)

  fmla <- as.formula(sprintf('events ~ %s',
                             paste(var_list, collapse = " + ")))




  #--------------------------------------------------
  # Run regressions
  #--------------------------------------------------
  if (reg_type == 'LPM') {

    # Estimating a Linear Probability Model

    # Estimate the model accounting for the aggregated nature of the data.
    agg_lm_model_1 <- agg_lm(data = saaq_data[sel_obsn == TRUE, ], weights = num,
                             formula = fmla, x = TRUE)
    summ_agg_lm <- summary_agg_lm(agg_lm_model_1)

    # Adjust standard errors for heteroskedasticity.
    agg_lpm_hccme_1 <- white_hccme_med(agg_lm_model_1)
    summ_model <- agg_lpm_hccme_1
    # print(agg_lpm_hccme_1$coef_hccme)

    est_coefs <- summ_model$coef_hccme

    # # Checking for negative LPM predictions.
    # lpm_neg_check(lm(data = saaq_data[sel_obsn == TRUE, ], weights = num,
    #                  formula = chosen_model))

  } else if (reg_type == 'Logit') {

    # Estimate logistic regression model.
    log_model_1 <- glm(data = saaq_data[sel_obsn == TRUE, ], weights = num,
                       formula = fmla,
                       family = 'binomial')

    # Calculate SE based on chosen method.
    if (est_QMLE_SEs == FALSE) {

      # Standard standard errors.
      summ_model <- summary(log_model_1)
      est_coefs <- summ_model$coefficients

    } else if (est_QMLE_SEs == TRUE) {
      # Calculate sandwich SE estimator for QMLE.

      est_coefs <- est_coefs_QMLE(V,
                                  y = as.integer(saaq_data[sel_obsn == TRUE, 'events']),
                                  num_weights = saaq_data[sel_obsn == TRUE, 'num'],
                                  p = fitted(log_model_1),
                                  X = model.matrix(log_model_1))

    }

  } else {
    stop(sprintf("Model type '%s' not recognized.", reg_type))
  }


  #--------------------------------------------------
  # Calculate marginal effects, if appropriate.
  #--------------------------------------------------

  # Only meaningful for logit regressions.
  if (reg_type == 'Logit') {
    mfx_mat <- mfx_mat_calc(saaq_data,
                            log_model_1,
                            est_coefs,
                            mfx_data_MER,
                            var_list,
                            window_sel,
                            sex_sel,
                            age_int, age_grp_list)
  } else {
    mfx_mat <- NA
  }


  #--------------------------------------------------
  # Print the results.
  #--------------------------------------------------

  md_reg_out(md_path,
             est_coefs,
             reg_type, mfx_mat,
             pts_headings, pts_target)


  #--------------------------------------------------
  # Store the regression results for tables.
  #--------------------------------------------------

  estn_results_sub <- estn_results_table(est_coefs, estn_num,
                                         num_obs = sum(saaq_data[sel_obsn == TRUE, 'num']),
                                         reg_type, mfx_mat,
                                         age_int, age_grp_list, window_sel)


  # Bind it to the full data frame of results.
  estn_results <- rbind(estn_results, estn_results_sub)

}

# Save the data frame of estimates.
write.csv(estn_results, file = estn_file_path)


################################################################################
# End
################################################################################

