

################################################################################
# Define Functions for Regression Modeling
################################################################################

############################################################
# Check for negative predictions from LPM
############################################################

lpm_neg_check <- function(lm_model) {
  # Checking for negative LPM predictions.
  saaq_data[, 'pred'] <- NA
  saaq_data[sel_obsn == TRUE , 'pred'] <- predict(lm_model)
  print('Observations with negative predictions:')
  print(unique(saaq_data[sel_obsn == TRUE & saaq_data[, 'pred'] < 0,
                         c('sex', 'age_grp', 'curr_pts_grp', 'policy', 'pred')]))
  print('Summary of predictions:')
  print(summary(saaq_data[sel_obsn == TRUE , 'pred']))
  print('Number of negative predictions:')
  pct_neg_pred <- sum(saaq_data[sel_obsn == TRUE , 'pred'] < 0) /
    saaq_data[, sum(sel_obsn == TRUE)]
  print(pct_neg_pred)
}


############################################################
# Set List of Regression Specifications
############################################################

model_spec <- function(spec_group,
                       sex_list, age_grp_list, age_int_list,
                       pts_target_list, reg_list) {
  if (spec_group == 'pooled') {

    #------------------------------------------------------------
    # Pooled regressions with separation by age group.
    #------------------------------------------------------------

    # Set the full list of model specification combinations.
    model_list <- expand.grid(past_pts = c('all'),
                              window = c('4 yr.'),
                              seasonality = c('mnwk'),
                              age_int = c('no', age_grp_list),
                              pts_int = 'no', # Extra column.
                              pts_target = c('all'),
                              sex = c('All'),
                              reg_type = c('LPM', 'Logit'))





  } else if (spec_group == 'all') {

    #------------------------------------------------------------
    # Specification: All Drivers with Monthly and weekday seasonality
    #------------------------------------------------------------

    # Set the full list of model specification combinations.
    model_list <- expand.grid(past_pts = c('all'),
                              window = c('4 yr.'),
                              seasonality = c('mnwk'),
                              age_int = age_int_list,
                              pts_int = 'no', # Extra column.
                              pts_target = pts_target_list,
                              sex = c('Male', 'Female'),
                              # sex = sex_list,
                              reg_type = reg_list)

    # Remove some unnecessary combinations.
    model_list <- model_list[model_list[, 'pts_target'] == 'all' |
                               (model_list[, 'pts_target'] != 'all' &
                                  model_list[, 'age_int'] == 'no'), ]

  } else if (spec_group == 'high_pts') {

    #------------------------------------------------------------
    # Sensitivity Analysis: High-point drivers.
    # (with monthly and weekday seasonality)
    #------------------------------------------------------------

    # Set the partial list of model specification combinations.
    model_list <- expand.grid(past_pts = c('high'),
                              # past_pts = c('all'),
                              window = c('4 yr.'),
                              seasonality = c('mnwk'),
                              # age_int = age_int_list,
                              age_int = 'no',
                              pts_int = 'no', # Extra column.
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

    # Set the partial list of model specification combinations.
    model_list <- expand.grid(past_pts = c('all'),
                              window = c('Placebo'),
                              # window = c('4 yr.'),
                              seasonality = c('mnwk'),
                              age_int = age_int_list,
                              pts_int = 'no', # Extra column.
                              pts_target = c('all'),
                              # pts_target = pts_target_list,
                              sex = c('Male', 'Female'),
                              # sex = sex_list,
                              reg_type = reg_list)

  } else if (spec_group == 'events') {

    #------------------------------------------------------------
    # Specification: REAL event study with seasonality
    #------------------------------------------------------------

    # Set the full list of model specification combinations.
    model_list <- expand.grid(past_pts = c('all'),
                              window = c('Monthly 4 yr.'),
                              seasonality = c('mnwk'),
                              # age_int = age_int_list,
                              age_int = 'no',
                              pts_int = 'no', # Extra column.
                              # pts_target = pts_target_list,
                              pts_target = 'all',
                              sex = c('Male', 'Female'),
                              # sex = sex_list,
                              reg_type = reg_list)


  } else if (spec_group == 'points') {

    #------------------------------------------------------------
    # Specification: Separate demerit point groups
    #------------------------------------------------------------

    # Set the full list of model specification combinations.
    model_list <- expand.grid(past_pts = c('all'),
                              window = c('4 yr.'),
                              seasonality = c('mnwk'),
                              # age_int = age_int_list,
                              age_int = 'no',
                              pts_int = 'with', # Extra column.
                              # pts_target = pts_target_list,
                              pts_target = 'all',
                              sex = c('Male', 'Female'),
                              # sex = sex_list,
                              # reg_type = reg_list,
                              reg_type = 'LPM') # MFX not implemented yet.
    # Note the extra column for demerit-point interactions.


  } else {
    stop('Regression specification not recognized. ')
  }

  return(model_list)

}


############################################################
# Set paths and headers for regression output to markdown files.
############################################################


md_headers <- function(md_dir, md_path_last,
                       spec_group,
                       reg_type,
                       sex_sel,
                       age_int,
                       pts_target,
                       past_pts_sel,
                       window_sel,
                       season_incl) {

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
  print(out_msg)


  return(md_path)

}


############################################################
# Print regression results to markdown files.
############################################################

md_reg_out <- function(md_path,
                       est_coefs,
                       reg_type, mfx_mat,
                       pts_headings, pts_target) {

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

}

############################################################
# Data preparation.
# Calculate variables specific to the model specification.
############################################################

saaq_data_prep <- function(saaq_data,
                           window_sel,
                           past_pts_sel,
                           sex_sel,
                           age_int, age_grp_list,
                           pts_target) {

  #--------------------------------------------------
  # Set event window around policy indicator.
  #--------------------------------------------------
  if (window_sel == '4 yr.' | window_sel == 'Monthly 4 yr.') {

    # Select symmetric window around the policy change.
    saaq_data[, 'window'] <- saaq_data[, 'date'] >= '2006-04-01' &
      saaq_data[, 'date'] <= '2010-03-31'

    # Set date of policy change.
    april_fools_date <- '2008-04-01'
    # No joke: policy change on April Fool's Day!

  } else if (window_sel == '2 yr.') {

    # Select two-year symmetric window around the policy change.
    saaq_data[, 'window'] <- saaq_data[, 'date'] >= '2007-04-01' &
      saaq_data[, 'date'] <= '2009-03-31'

    # Set date of policy change.
    april_fools_date <- '2008-04-01'
    # No joke: policy change on April Fool's Day!

  } else if (window_sel == 'Placebo') {

    # Select symmetric window around the placebo change.
    # Year before (2007):
    saaq_data[, 'window'] <- saaq_data[, 'date'] >= '2006-04-01' &
      saaq_data[, 'date'] <= '2008-03-31'

    # Set date of placebo policy change.
    april_fools_date <- '2007-04-01'
    # No joke: policy change on April Fool's Day!

  } else {
    stop(sprintf("Window setting '%s' not recognized.", window_sel))
  }


  #--------------------------------------------------
  # Generate the indicator for the policy change.
  #--------------------------------------------------

  # policy date depends on the model specification (e.g. placebo).
  saaq_data[, 'policy'] <- saaq_data[, 'date'] >= april_fools_date


  #--------------------------------------------------
  # Generate monthly indicators after the policy change (for learning rate).
  #--------------------------------------------------

  if (window_sel == 'Monthly 4 yr.') {
    # Two definitions, one is much more efficient:
    saaq_data[, 'policy_month'] <- NA_character_
    # saaq_data[saaq_data[, 'date'] < april_fools_date, 'policy_month'] <- "policyFALSE"
    saaq_data[date < april_fools_date, policy_month := "policyFALSE"]
    # saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #             saaq_data[, 'date'] >= as.Date('2009-04-01'), 'policy_month'] <- "policyFALSE"
    saaq_data[date >= april_fools_date &
                date >= as.Date('2009-04-01'), policy_month := "policyFALSE"]
    # First definition: Policy and month of year (yes, confusing but easy).
    # saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #             saaq_data[, 'date'] < as.Date('2009-04-01'), 'policy_month'] <-
    #   sprintf("policy%s", saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #                                   saaq_data[, 'date'] < as.Date('2009-04-01'), 'month'])
    # Second definition: Policy and month of year (yes, confusing but easy).
    # saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #             saaq_data[, 'date'] < as.Date('2009-04-01') &
    #             saaq_data[, 'month'] == '04', 'policy_month'] <- 'policy01'
    saaq_data[date >= april_fools_date &
                date < as.Date('2009-04-01') &
                month == '04', policy_month := 'policy01']
    # saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #             saaq_data[, 'date'] < as.Date('2009-04-01') &
    #             saaq_data[, 'month'] == '05', 'policy_month'] <- 'policy02'
    saaq_data[date >= april_fools_date &
                date < as.Date('2009-04-01') &
                month == '05', policy_month := 'policy02']
    # saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #             saaq_data[, 'date'] < as.Date('2009-04-01') &
    #             saaq_data[, 'month'] == '06', 'policy_month'] <- 'policy03'
    saaq_data[date >= april_fools_date &
                date < as.Date('2009-04-01') &
                month == '06', policy_month := 'policy03']
    # saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #             saaq_data[, 'date'] < as.Date('2009-04-01') &
    #             saaq_data[, 'month'] == '07', 'policy_month'] <- 'policy04'
    saaq_data[date >= april_fools_date &
                date < as.Date('2009-04-01') &
                month == '07', policy_month := 'policy04']
    # saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #             saaq_data[, 'date'] < as.Date('2009-04-01') &
    #             saaq_data[, 'month'] == '08', 'policy_month'] <- 'policy05'
    saaq_data[date >= april_fools_date &
                date < as.Date('2009-04-01') &
                month == '08', policy_month := 'policy05']
    # saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #             saaq_data[, 'date'] < as.Date('2009-04-01') &
    #             saaq_data[, 'month'] == '09', 'policy_month'] <- 'policy06'
    saaq_data[date >= april_fools_date &
                date < as.Date('2009-04-01') &
                month == '09', policy_month := 'policy06']
    # saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #             saaq_data[, 'date'] < as.Date('2009-04-01') &
    #             saaq_data[, 'month'] == '10', 'policy_month'] <- 'policy07'
    saaq_data[date >= april_fools_date &
                date < as.Date('2009-04-01') &
                month == '10', policy_month := 'policy07']
    # saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #             saaq_data[, 'date'] < as.Date('2009-04-01') &
    #             saaq_data[, 'month'] == '11', 'policy_month'] <- 'policy08'
    saaq_data[date >= april_fools_date &
                date < as.Date('2009-04-01') &
                month == '11', policy_month := 'policy08']
    # saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #             saaq_data[, 'date'] < as.Date('2009-04-01') &
    #             saaq_data[, 'month'] == '12', 'policy_month'] <- 'policy09'
    saaq_data[date >= april_fools_date &
                date < as.Date('2009-04-01') &
                month == '12', policy_month := 'policy09']
    # saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #             saaq_data[, 'date'] < as.Date('2009-04-01') &
    #             saaq_data[, 'month'] == '01', 'policy_month'] <- 'policy10'
    saaq_data[date >= april_fools_date &
                date < as.Date('2009-04-01') &
                month == '01', policy_month := 'policy10']
    # saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #             saaq_data[, 'date'] < as.Date('2009-04-01') &
    #             saaq_data[, 'month'] == '02', 'policy_month'] <- 'policy11'
    saaq_data[date >= april_fools_date &
                date < as.Date('2009-04-01') &
                month == '02', policy_month := 'policy11']
    # saaq_data[saaq_data[, 'date'] >= april_fools_date &
    #             saaq_data[, 'date'] < as.Date('2009-04-01') &
    #             saaq_data[, 'month'] == '03', 'policy_month'] <- 'policy12'
    saaq_data[date >= april_fools_date &
                date < as.Date('2009-04-01') &
                month == '03', policy_month := 'policy12']
    # In either case, transform it into factor.
    # saaq_data[, 'policy_month'] <- factor(saaq_data[, 'policy_month'],
    #                                       levels = c('policyFALSE',
    #                                                  sprintf('policy0%d', 1:9),
    #                                                  sprintf('policy%d', 10:12)))
    saaq_data[, policy_month := factor(policy_month,
                                       levels = c('policyFALSE',
                                                  sprintf('policy0%d', 1:9),
                                                  sprintf('policy%d', 10:12)))]


    # table(saaq_data[, 'policy_month'], useNA = 'ifany')
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
    stop(sprintf("Point balance target '%s' not recognized.", pts_target))
  }


  #--------------------------------------------------
  return(saaq_data)


}


############################################################
# Set formula for regression model
############################################################

reg_var_list <- function(sex_sel,
                     window_sel,
                     age_int, age_grp_list,
                     pts_int,
                     season_incl) {

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


  if (pts_int == 'with') {
    var_list <- c(var_list, 'curr_pts_reg', 'policy*curr_pts_reg')
  } else if (pts_int == 'no') {
    var_list <- c(var_list, 'curr_pts_grp')
    # Use coarser groupings without interactions.
  } else {
    stop(sprintf("Demerit-points indicator selector '%s' not recognized.", pts_int))
  }



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


  return(var_list)
}



############################################################
# Calculate sandwich SE estimator for QMLE.
############################################################

est_coefs_QMLE <- function(V, y, num_weights, p, X) {

  # The MLE covariance estimator is the inverse Hessian matrix.
  # This is the bread of the sandwich.
  # V = vcov(log_model_1)

  # Now calculate OPG.
  # y <- as.integer(saaq_data[sel_obs, 'events'])
  # num_weights <- saaq_data[sel_obs, 'num']
  num_obs <- sum(num_weights)
  # p <- fitted(log_model_1)

  # Now calculate a weighting matrix.
  g <- (y-p) * sqrt(num_weights)
  # Note that the outer product will get back the weights.

  # The full design matrix:
  # X <- model.matrix(log_model_1)
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

  return(est_coefs)
}


############################################################
# Store the regression results for tables.
############################################################

estn_results_table <- function(est_coefs, estn_num,
                               num_obs,
                               reg_type, mfx_mat,
                               age_int, age_grp_list, window_sel) {

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
  estn_results_sub[, 'N'] <- num_obs


  # Calculate MFX, if appropriate.
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


  return(estn_results_sub)
}


############################################################
# End.
############################################################
