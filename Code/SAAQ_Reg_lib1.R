

################################################################################
# Define Functions for Regression Modeling
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

