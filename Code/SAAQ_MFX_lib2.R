


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


# Define logistic transformation for producing probabilities.
logit_link <- function(X_beta) {
  return( exp(X_beta) / (1 + exp(X_beta)) )
}


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
