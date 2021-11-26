
############################################################
# Functions for calculating marginal effects.
# Only meaningful for logit regressions.
############################################################

############################################################
# Calculate marginal effects, if appropriate.
# Only meaningful for logit regressions.
############################################################

mfx_mat_calc <- function(saaq_data,
                         log_model_1,
                         est_coefs,
                         mfx_data_MER,
                         var_list,
                         window_sel,
                         sex_sel,
                         age_int, age_grp_list) {

  sel_obs <- saaq_data[, 'sel_obsn']

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

  return(mfx_mat)
}





############################################################
# Taking differences for AME calculation
############################################################


mfx_AME_diff <- function(saaq_data_pred, log_model_1,
                         mfx_var = 'policy',
                         before_val = FALSE, after_val = TRUE) {

  # Calculate average prediction before policy change.
  # saaq_data_pred[, 'policy'] <- FALSE
  saaq_data_pred[, mfx_var] <- before_val
  saaq_data_pred[, 'pred_prob_before'] <- predict(log_model_1,
                                                  newdata = saaq_data_pred,
                                                  type = "response")
  pred_before <- sum(saaq_data_pred[, 'pred_prob_before'] *
                       saaq_data_pred[, 'num']) / sum(saaq_data_pred[, 'num'])

  # Calculate average prediction after policy change.
  # saaq_data_pred[, 'policy'] <- TRUE
  saaq_data_pred[, mfx_var] <- after_val
  saaq_data_pred[, 'pred_prob_after'] <- predict(log_model_1,
                                                 newdata = saaq_data_pred,
                                                 type="response")
  pred_after <- sum(saaq_data_pred[, 'pred_prob_after'] *
                      saaq_data_pred[, 'num']) / sum(saaq_data_pred[, 'num'])

  # Assign difference to AME.
  mfx <- pred_after - pred_before

  return(mfx)

}


############################################################
# Define logistic transformation for producing probabilities.
############################################################

logit_link <- function(X_beta) {
  return( exp(X_beta) / (1 + exp(X_beta)) )
}


############################################################
# Calculating cross-differences for models with interactions
############################################################

mfx_AME_cross_diff <- function(saaq_data_pred, log_model_1,
                               cross_coefficient) {

  # Keep first-order policy indicator true.
  saaq_data_pred[, 'policy'] <- TRUE


  # Calculate average prediction before policy change.
  # saaq_data_pred[, 'policy'] <- FALSE
  # Measure this in probabilities, as is (type = "response").
  saaq_data_pred[, 'pred_prob_before'] <- predict(log_model_1,
                                                  newdata = saaq_data_pred,
                                                  type = "response")
  pred_before <- sum(saaq_data_pred[, 'pred_prob_before'] *
                       saaq_data_pred[, 'num']) / sum(saaq_data_pred[, 'num'])

  # Calculate average prediction after policy change.
  # saaq_data_pred[, 'policy'] <- TRUE
  # Measure this in the single index level, as is (type = "link").
  saaq_data_pred[, 'pred_prob_after'] <- predict(log_model_1,
                                                 newdata = saaq_data_pred,
                                                 # type = "response",
                                                 type = "link")
  # Now add interaction term for policy effect.
  saaq_data_pred[, 'pred_prob_after'] <-
    saaq_data_pred[, 'pred_prob_after'] + cross_coefficient

  # Transform back into probabilities.
  saaq_data_pred[, 'pred_prob_after'] <-
    logit_link(saaq_data_pred[, 'pred_prob_after'])

  pred_after <- sum(saaq_data_pred[, 'pred_prob_after'] *
                      saaq_data_pred[, 'num']) / sum(saaq_data_pred[, 'num'])

  # Assign difference to AME.
  mfx <- pred_after - pred_before

  return(mfx)
}


############################################################
# End
############################################################

