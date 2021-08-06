################################################################################
#
# Investigation of SAAQ Excessive Speeding Laws
#
# A library of functions for estimating the fixed effects model
# and calculating the cluster-robust variance estimator,
# for data that are weighted by frequency,
# when the frequency varies within individuals.
#
# Lee Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# August 3, 2021
#
################################################################################
#
################################################################################

################################################################################
# Declaring Packages
################################################################################

# Load data table package for quick selection on seq.
library(data.table)

################################################################################
# Function Definitions
################################################################################


#--------------------------------------------------------------------------------
# adj_FE_coef_SE adjusts the table of coefficients
# for adjusted standard errors in the fixed effects model.
# Used when the fixed effects model is estimated
# with weighted observations and in the second stage regressions
# using the Frisch-Waugh-Lovell theorem.
#
# Dependencies: None.
#
#--------------------------------------------------------------------------------

adj_FE_coef_SE <- function(coef_orig, se_adj,
                           num_obs, num_vars, num_FE) {

  # Start with the original estimates.
  coef_adj <- coef_orig

  # Adjust the standard errors.
  coef_adj[, 'Std. Error'] <- se_adj
  coef_adj[, 't value'] <- coef_adj[, 'Estimate']/coef_adj[, 'Std. Error']
  coef_adj[, 'Pr(>|t|)'] <- 2*pt(- abs(coef_adj[, 't value']),
                                 df = (num_obs - (num_vars + num_FE + 1)))

  return(coef_adj)
}

#--------------------------------------------------------------------------------
# adj_FE_coef_table adjusts the table of coefficients
# for adjusted standard errors in the fixed effects model.
# It makes the adjustment between
# weighted and unweighted estimates of the fixed effects estimator.
# It recalculates the standard errors and then adjusts the table
# with the new standard error calculation.
# Used when the fixed effects model is estimated
# with weighted observations and in the second stage regressions
# using the Frisch-Waugh-Lovell theorem.
#
# Dependencies: adj_FE_coef_SE
#
#--------------------------------------------------------------------------------

adj_FE_coef_table <- function(coef_lm, resid_lm,
                              num_obs, num_rows, num_vars, num_FE) {

  # s^2 estimate for fixed effects model.
  s2_true <- sum(resid_lm^2) /
    (num_obs - (num_vars + num_FE + 1))

  # s^2 estimate for the weighted model.
  s2_lm <- sum(resid_lm^2) /
    (num_rows - (num_vars + 1))

  # Calculate adjustment ratio for standard errors.
  se_adj_factor <- sqrt(s2_true)/sqrt(s2_lm)

  # Replace the standard errors and other statistics
  # to account for difference in degrees of freedom.
  coef_adj <- adj_FE_coef_SE(coef_orig = coef_lm,
                             se_adj = se_adj_factor*coef_lm[, 'Std. Error'],
                             num_obs = num_obs,
                             num_vars = num_vars,
                             num_FE = num_FE)



  return(coef_adj)
}

#--------------------------------------------------------------------------------
# calc_CRVE_tab calculates a matrix of the weighted product of
# residuals and covariates for calculation of the cluster-robust variance
# estimator in the weighted fixed effects regression from the second stage
# regression estimates following the Frisch-Waugh-Lovell theorem.
#
# Dependencies:
#
# Examples:
# CRVE_dt <- calc_CRVE_tab(saaq_data = saaq_data, # Should pass shallow copy with pointer.
#                          weights = summ_sub$weights,
#                          resids = summ_sub$residuals,
#                          curr_pts_grp_list = curr_pts_grp_list)
#
#--------------------------------------------------------------------------------

calc_CRVE_tab <- function(saaq_data, weights, resids, curr_pts_grp_list) {

  # Calculate columns for cluster-robust variance estimate,
  # clustering on the driver.
  # Number of columns is 2*length(curr_pts_grp_list)
  # because the zero categories are dropped and the
  # intercept and policy indicators are included.
  CRVE_dt <- data.table(matrix(NA,
                               nrow = length(weights),
                               ncol = 2*length(curr_pts_grp_list)))
  # First curr_pts_grp category is dropped.
  curr_pts_grp_est_list <- curr_pts_grp_list[2:length(curr_pts_grp_list)]
  var_col_names <- c(sprintf('curr_pts_%s',
                             gsub('-', '_', curr_pts_grp_est_list)),
                     sprintf('curr_pts_%s_policy',
                             gsub('-', '_', curr_pts_grp_est_list)))
  # Add columns for intercept and policy indicator.
  var_col_names <- c('(Intercept)', 'dev_policy', var_col_names)

  colnames(CRVE_dt) <- var_col_names
  # Add indicator for individuals.
  CRVE_dt[, 'seq'] <- saaq_data[sub_sel_obsn == TRUE, seq]

  # Calculate columns of weighted product of residuals and covariates.
  # Intercept:
  CRVE_dt[, '(Intercept)'] <- sqrt(weights) * resids
  # Policy indicator:
  CRVE_dt[, 'dev_policy'] <- sqrt(weights) * resids *
    saaq_data[sub_sel_obsn == TRUE, 'dev_policy', with = FALSE]


  # Calculate columns of weighted product of residuals and covariates.
  for (curr_pts_level in curr_pts_grp_est_list) {

    print(sprintf('Calculating CRVE column for curr_pts_grp %s', curr_pts_level))

    col_var_name <- sprintf('curr_pts_%s', gsub('-', '_', curr_pts_level))
    CRVE_dt[, col_var_name] <- sqrt(weights) * resids *
      saaq_data[sub_sel_obsn == TRUE, col_var_name, with = FALSE]

    col_var_name <- sprintf('curr_pts_%s_policy', gsub('-', '_', curr_pts_level))
    CRVE_dt[, col_var_name] <- sqrt(weights) * resids *
      saaq_data[sub_sel_obsn == TRUE, col_var_name, with = FALSE]

  }


  return(CRVE_dt)
}


#--------------------------------------------------------------------------------
# calc_CRVE_meat calculates the inner matrix of the CRVE sandwich estimator
# using the weighted product of residuals and covariates
# in the weighted fixed effects regression from the second stage
# regression estimates following the Frisch-Waugh-Lovell theorem.
#
# Dependencies:
#
# Examples:
# CRVE_meat <- calc_CRVE_meat(CRVE_by_seq = CRVE_by_seq,
#                             var_col_names = var_col_names)
#
#--------------------------------------------------------------------------------

calc_CRVE_meat <- function(CRVE_by_seq, var_col_names) {

  # Get dimensions.
  num_vars = length(var_col_names)

  # Calculate a covariance matrix from the matrix aggregated by seq.
  CRVE_meat <- matrix(NA, nrow = num_vars, ncol = num_vars)
  colnames(CRVE_meat) <- var_col_names
  rownames(CRVE_meat) <- var_col_names
  for (var_num_i in 1:num_vars) {
    for (var_num_j in 1:var_num_i) {

      var_col_i <- var_col_names[var_num_i]
      var_col_j <- var_col_names[var_num_j]

      CRVE_meat[var_col_i, var_col_j] <- sum(CRVE_by_seq[, var_col_i, with = FALSE] *
                                               CRVE_by_seq[, var_col_j, with = FALSE])

      CRVE_meat[var_col_j, var_col_i] <- CRVE_meat[var_col_i, var_col_j]
    }
  }


  return(CRVE_meat)
}


#--------------------------------------------------------------------------------
# calc_CRVE_FE_SE calculates the standard errors from the CRVE sandwich estimator
# using the weighted product of residuals and covariates
# in the weighted fixed effects regression from the second stage
# regression estimates following the Frisch-Waugh-Lovell theorem.
#
# Dependencies: None.
#
# Examples:
# CRVE_SE <- calc_CRVE_FE_SE(CRVE_bread = CRVE_bread,
#                            CRVE_meat = CRVE_meat,
#                            num_ind = num_seq, # = nrow(CRVE_by_seq)
#                            num_obs = num_sub,
#                            num_vars = length(var_col_names))
#
#--------------------------------------------------------------------------------

calc_CRVE_FE_SE <- function(CRVE_bread, CRVE_meat,
                            num_ind, num_obs, num_vars) {

  # Now make a sandwich.
  CRVE <- CRVE_bread %*% CRVE_meat %*% CRVE_bread

  # Take the standard errors, multiplied by a degrees of freedom correction.
  # num_seq <- nrow(CRVE_by_seq)
  CRVE <- CRVE*(num_ind/(num_ind-1))*(num_obs - 1)/(num_obs - num_vars)

  # Take the standard errors, as usual.
  CRVE_SE <- sqrt(diag(CRVE))

  return(CRVE_SE)
}



################################################################################
# End
################################################################################

