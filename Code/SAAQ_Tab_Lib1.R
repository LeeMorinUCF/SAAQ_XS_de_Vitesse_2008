
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
# A library of functions for generating LaTeX tables.
#
################################################################################



################################################################################
# Define functions and tables required
################################################################################

#--------------------------------------------------
# Auxiliary functions
#--------------------------------------------------


p_val_stars <- function(p_value) {
  if (is.na(p_value)) {
    warning(sprintf("The p-value is NA."))
    star_str <- '???'
  } else {
    if (p_value < 0.00001) {
      star_str <- ' **'
    } else if (p_value < 0.001) {
      star_str <- '  *'
    } else {
      star_str <- '   '
    }
  }
}

#--------------------------------------------------
# Functions for Individual Tables
#--------------------------------------------------

single_point_LPM_logit_2MFX_table <- function(tab_file_path, estn_results_tab,
                                              header, caption, description, label,
                                              sex_list,
                                              age_int_label_list,
                                              obsn_str_list,
                                              num_fmt = 'science',
                                              incl_mfx = FALSE,
                                              pooled = FALSE) {


  cat(sprintf('%% %s \n\n', header),
      file = tab_file_path, append = FALSE)
  cat('\\begin{table}% [ht] \n', file = tab_file_path, append = TRUE)
  cat('\\centering \n', file = tab_file_path, append = TRUE)


  cat('\\begin{tabular}{l r r r r l r r l} \n', file = tab_file_path, append = TRUE)

  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)

  cat(" & \\multicolumn{5}{c}{Logistic Regression} ",
      file = tab_file_path, append = TRUE)
  cat(" & \\multicolumn{3}{c}{Linear Probability Model} \\\\ \n",
      file = tab_file_path, append = TRUE)

  cat('\n \\cmidrule(lr){2-6}\\cmidrule(lr){7-9} \n', file = tab_file_path, append = TRUE)

  cat(" & \\multicolumn{2}{c}{Marginal Effects} & Estimate & Standard & Sig. & Estimate & Standard & Sig. \\\\ \n",
      file = tab_file_path, append = TRUE)
  cat('\n \\cmidrule(lr){2-3} \n', file = tab_file_path, append = TRUE)
  cat(" &   AME &  MER  &          &  Error   &      &          &  Error   &     \\\\ \n",
      file = tab_file_path, append = TRUE)





  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)

  # for (sex_sel in sex_list) {
  # Include pooled results only if specified.
  if (pooled == TRUE) {
    sex_list_beg <- 1
    sex_list_end <- 1
  } else {
    sex_list_beg <- 2
    sex_list_end <- length(sex_list)
  }
  for (sex_sel in sex_list[sex_list_beg:sex_list_end]) {

    # Print sample title in first line.
    if (sex_sel == 'All') {
      row_str <- 'Full sample'
    } else {
      row_str <- sex_sel
    }
    # Print number of observations in header.
    obsn_str <- obsn_str_list[sex_sel]

    cat(sprintf('\\multicolumn{8}{l}{\\textbf{%s Drivers} (%s observations)} \\\\ \n\n',
                row_str, obsn_str), file = tab_file_path, append = TRUE)

    #------------------------------------------------------------
    # Print first row with policy indicator from both regression types.
    # Logit model without age interactions.
    #------------------------------------------------------------
    row_str <- 'Model without age-policy interaction: '
    cat(sprintf('\\hline\n\\multicolumn{8}{l}{%s} \\\\ \n', row_str), file = tab_file_path, append = TRUE)
    cat('Policy                  ', file = tab_file_path, append = TRUE)

    # Pull estimates.
    logit_var_name_sel <- c('Estimate', 'Std_Error', 'p_value')
    # logit_var_name_sel <- c(logit_var_name_sel, 'mfx')
    # logit_var_name_sel <- c(logit_var_name_sel, 'AME')
    logit_var_name_sel <- c(logit_var_name_sel, 'AME', 'MER')

    # Coefficients from Logit model.
    est_se_p <- estn_results_tab[estn_results_tab[, 'sex'] == sex_sel &
                                   estn_results_tab[, 'age_int'] == 'no' &
                                   estn_results_tab[, 'reg_type'] == 'Logit' &
                                   estn_results_tab[, 'Variable'] == 'policyTRUE',
                                 logit_var_name_sel]
    # Print marginal effects in first column, if necessary.
    cat(sprintf(' &  %6.4f      ', est_se_p[4:5]), file = tab_file_path, append = TRUE)

    # Print logit results directly.
    cat(sprintf(' &  %6.4f      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)

    cat(sprintf(' &  %s      ', p_val_stars(p_value = est_se_p[3])), file = tab_file_path, append = TRUE)
    # LPM model without age interactions.
    est_se_p <- estn_results_tab[estn_results_tab[, 'sex'] == sex_sel &
                                   # estn_results_tab[, 'age_int'] == 'with' &
                                   estn_results_tab[, 'age_int'] == 'no' &
                                   estn_results_tab[, 'reg_type'] == 'LPM' &
                                   estn_results_tab[, 'Variable'] == 'policyTRUE',
                                 c('Estimate', 'Std_Error', 'p_value')]
    if (num_fmt == 'science') {
      cat(sprintf(' &  %5.2E      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)
    } else if (num_fmt %in% c('num', 'x100K')) {
      cat(sprintf(' &  %6.4f      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)
    } else {
      stop(sprintf('Number format %s not recognized.', num_fmt))
    }
    cat(sprintf(' &  %s      ', p_val_stars(p_value = est_se_p[3])), file = tab_file_path, append = TRUE)
    cat(' \\\\ \n', file = tab_file_path, append = TRUE)

    #------------------------------------------------------------
    # Print first row with policy indicator from both regression types.
    # Logit model with age interactions.
    #------------------------------------------------------------
    row_str <- 'Model with age-policy interaction: '
    cat(sprintf('\\hline\n\\multicolumn{8}{l}{%s} \\\\ \n', row_str), file = tab_file_path, append = TRUE)
    cat('Policy                  ', file = tab_file_path, append = TRUE)
    est_se_p <- estn_results_tab[estn_results_tab[, 'sex'] == sex_sel &
                                   estn_results_tab[, 'age_int'] == 'with' &
                                   # estn_results_tab[, 'age_int'] == 'no' &
                                   estn_results_tab[, 'reg_type'] == 'Logit' &
                                   estn_results_tab[, 'Variable'] == 'policyTRUE',
                                 logit_var_name_sel]
    # Print marginal effects in first column, if necessary.
    cat(sprintf(' &  %6.4f      ', est_se_p[4:5]), file = tab_file_path, append = TRUE)

    # Print logit results directly.
    cat(sprintf(' &  %6.4f      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)

    cat(sprintf(' &  %s      ', p_val_stars(p_value = est_se_p[3])), file = tab_file_path, append = TRUE)
    # LPM model with age interactions.
    est_se_p <- estn_results_tab[estn_results_tab[, 'sex'] == sex_sel &
                                   estn_results_tab[, 'age_int'] == 'with' &
                                   # estn_results_tab[, 'age_int'] == 'no' &
                                   estn_results_tab[, 'reg_type'] == 'LPM' &
                                   estn_results_tab[, 'Variable'] == 'policyTRUE',
                                 c('Estimate', 'Std_Error', 'p_value')]
    if (num_fmt == 'science') {
      cat(sprintf(' &  %5.2E      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)
    } else if (num_fmt %in% c('num', 'x100K')) {
      cat(sprintf(' &  %6.4f      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)
    } else {
      stop(sprintf('Number format %s not recognized.', num_fmt))
    }
    cat(sprintf(' &  %s      ', p_val_stars(p_value = est_se_p[3])), file = tab_file_path, append = TRUE)
    cat(' \\\\ \n', file = tab_file_path, append = TRUE)


    # Remaining rows show only age interaction.
    # for (age_int_num in 1:nrow(age_int_label_list)) {
    for (age_int_num in 2:nrow(age_int_label_list)) {
      age_int_var <- age_int_label_list[age_int_num, 'Variable']
      age_int_label <- age_int_label_list[age_int_num, 'Label']
      # cat(sprintf('%s           & & & ', age_int_label), file = tab_file_path, append = TRUE)


      #------------------------------------------------------------
      # Logit model with age interactions.
      #------------------------------------------------------------
      cat(sprintf('%s  ', age_int_label), file = tab_file_path, append = TRUE)
      est_se_p <- estn_results_tab[estn_results_tab[, 'sex'] == sex_sel &
                                     estn_results_tab[, 'age_int'] == 'with' &
                                     estn_results_tab[, 'reg_type'] == 'Logit' &
                                     estn_results_tab[, 'Variable'] == age_int_var,
                                   logit_var_name_sel]
      # print(est_se_p)
      # print(sex_sel)
      # print(age_int_var)
      # Print marginal effects in first column, if necessary.
      cat(sprintf(' &  %6.4f      ', est_se_p[4:5]), file = tab_file_path, append = TRUE)

      # Print logit results directly.
      cat(sprintf(' &  %6.4f      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)

      cat(sprintf(' &  %s      ', p_val_stars(p_value = est_se_p[3])), file = tab_file_path, append = TRUE)
      #------------------------------------------------------------


      #------------------------------------------------------------
      # LPM model with age interactions.
      #------------------------------------------------------------
      # cat(sprintf('%s  ', age_int_label), file = tab_file_path, append = TRUE)
      est_se_p <- estn_results_tab[estn_results_tab[, 'sex'] == sex_sel &
                                     estn_results_tab[, 'age_int'] == 'with' &
                                     estn_results_tab[, 'reg_type'] == 'LPM' &
                                     estn_results_tab[, 'Variable'] == age_int_var,
                                   c('Estimate', 'Std_Error', 'p_value')]
      if (num_fmt == 'science') {
        cat(sprintf(' &  %5.2E      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)
      } else if (num_fmt %in% c('num', 'x100K')) {
        cat(sprintf(' &  %6.4f      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)
      } else {
        stop(sprintf('Number format %s not recognized.', num_fmt))
      }
      cat(sprintf(' &  %s      ', p_val_stars(p_value = est_se_p[3])), file = tab_file_path, append = TRUE)
      #------------------------------------------------------------

      cat(' \\\\ \n', file = tab_file_path, append = TRUE)

    }

    # Print divider between subsamples.
    # obsn_str <- obsn_str_list[sex_sel]
    # cat(sprintf('Observations & \\multicolumn{2}{c}{%s} \\\\ \n\n', obsn_str),
    #     file = tab_file_path, append = TRUE)
    cat('\n\\hline \n\n', file = tab_file_path, append = TRUE)

  }

  cat('\\end{tabular} \n', file = tab_file_path, append = TRUE)
  cat(sprintf('\\caption{%s} \n', caption), file = tab_file_path, append = TRUE)
  # cat('All regressions contain age category and demerit point category controls. \n', file = tab_file_path, append = TRUE)
  # cat('The symbol * denotes statistical significance at the 0.1\\% level \n', file = tab_file_path, append = TRUE)
  # cat('and ** the 0.001\\% level. \n', file = tab_file_path, append = TRUE)
  # cat('``Sig.\'\' is an abbreviation for statistical significance. \n', file = tab_file_path, append = TRUE)
  for (desc_row in 1:length(description)) {
    cat(sprintf('%s \n', description[desc_row]), file = tab_file_path, append = TRUE)
  }
  cat(sprintf('\\label{%s} \n', label), file = tab_file_path, append = TRUE)
  cat('\\end{table} \n \n', file = tab_file_path, append = TRUE)

}

multi_group_LPM_logit_2MFX_table <- function(tab_file_path, estn_results_tab,
                                             header, caption, description, label,
                                             # sex_list,
                                             # age_int_label_list,
                                             age_grp_list,
                                             obsn_str_list,
                                             num_fmt = 'science',
                                             incl_mfx = FALSE,
                                             pooled = FALSE) {


  cat(sprintf('%% %s \n\n', header),
      file = tab_file_path, append = FALSE)
  cat('\\begin{table}% [ht] \n', file = tab_file_path, append = TRUE)
  cat('\\centering \n', file = tab_file_path, append = TRUE)


  cat('\\begin{tabular}{l r r r r l r r l} \n', file = tab_file_path, append = TRUE)

  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)

  cat(" & \\multicolumn{5}{c}{Logistic Regression} ",
      file = tab_file_path, append = TRUE)
  cat(" & \\multicolumn{3}{c}{Linear Probability Model} \\\\ \n",
      file = tab_file_path, append = TRUE)

  cat('\n \\cmidrule(lr){2-6}\\cmidrule(lr){7-9} \n', file = tab_file_path, append = TRUE)

  cat(" & \\multicolumn{2}{c}{Marginal Effects} & Estimate & Standard & Sig. & Estimate & Standard & Sig. \\\\ \n",
      file = tab_file_path, append = TRUE)
  cat('\n \\cmidrule(lr){2-3} \n', file = tab_file_path, append = TRUE)
  cat(" &   AME &  MER  &          &  Error   &      &          &  Error   &     \\\\ \n",
      file = tab_file_path, append = TRUE)


  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)


  # Separate models by age group (both sexes pooled).
  sex_sel <- 'All'
  for (age_grp_num in 1:length(age_grp_list)) {

    age_grp_sel <- age_grp_list[age_grp_num]

    # Pooled both sexes by default.
    row_str <- sprintf('Age Group %s', age_grp_sel)

    # Print number of observations in header.
    obsn_str <- obsn_str_list[age_grp_num]

    cat(sprintf('\\multicolumn{8}{l}{\\textbf{Drivers in %s} (%s observations)} \\\\ \n\n',
                row_str, obsn_str), file = tab_file_path, append = TRUE)



    #------------------------------------------------------------
    # Print one row with policy indicator from each age subsample.
    #------------------------------------------------------------


    cat('Policy Indicator       ', file = tab_file_path, append = TRUE)

    # Logit model by age group.

    # Pull estimates.
    logit_var_name_sel <- c('Estimate', 'Std_Error', 'p_value')
    # logit_var_name_sel <- c(logit_var_name_sel, 'mfx')
    # logit_var_name_sel <- c(logit_var_name_sel, 'AME')
    logit_var_name_sel <- c(logit_var_name_sel, 'AME', 'MER')

    # Coefficients from Logit model.
    est_se_p <- estn_results_tab[estn_results_tab[, 'sex'] == sex_sel &
                                   estn_results_tab[, 'age_int'] == age_grp_sel &
                                   # estn_results_tab[, 'age_int'] == 'no' &
                                   estn_results_tab[, 'reg_type'] == 'Logit' &
                                   estn_results_tab[, 'Variable'] == 'policyTRUE',
                                 logit_var_name_sel]
    # Print marginal effects in first column, if necessary.
    cat(sprintf(' &  %6.4f      ', est_se_p[4:5]), file = tab_file_path, append = TRUE)

    # Print logit results directly.
    cat(sprintf(' &  %6.4f      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)

    cat(sprintf(' &  %s      ', p_val_stars(p_value = est_se_p[3])), file = tab_file_path, append = TRUE)

    # LPM model by age group.
    est_se_p <- estn_results_tab[estn_results_tab[, 'sex'] == sex_sel &
                                   estn_results_tab[, 'age_int'] == age_grp_sel &
                                   # estn_results_tab[, 'age_int'] == 'no' &
                                   estn_results_tab[, 'reg_type'] == 'LPM' &
                                   estn_results_tab[, 'Variable'] == 'policyTRUE',
                                 c('Estimate', 'Std_Error', 'p_value')]
    if (num_fmt == 'science') {
      cat(sprintf(' &  %5.2E      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)
    } else if (num_fmt %in% c('num', 'x100K')) {
      cat(sprintf(' &  %6.4f      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)
    } else {
      stop(sprintf('Number format %s not recognized.', num_fmt))
    }
    cat(sprintf(' &  %s      ', p_val_stars(p_value = est_se_p[3])), file = tab_file_path, append = TRUE)
    cat(' \\\\ \n', file = tab_file_path, append = TRUE)

    # Print divider between subsamples.
    cat('\n\\hline \n\n', file = tab_file_path, append = TRUE)


  }

  cat('\\end{tabular} \n', file = tab_file_path, append = TRUE)
  cat(sprintf('\\caption{%s} \n', caption), file = tab_file_path, append = TRUE)
  for (desc_row in 1:length(description)) {
    cat(sprintf('%s \n', description[desc_row]), file = tab_file_path, append = TRUE)
  }
  cat(sprintf('\\label{%s} \n', label), file = tab_file_path, append = TRUE)
  cat('\\end{table} \n \n', file = tab_file_path, append = TRUE)

}


multi_point_LPM_logit_2MFX_table <- function(tab_file_path, estn_results_tab,
                                             header, caption, description, label,
                                             points_label_list,
                                             obsn_str_list,
                                             num_fmt = 'science',
                                             incl_mfx = FALSE) {

  cat(sprintf('%% %s \n\n', header),
      file = tab_file_path, append = FALSE)
  cat('\\begin{table}% [ht] \n', file = tab_file_path, append = TRUE)
  cat('\\centering \n', file = tab_file_path, append = TRUE)

  cat('\\begin{tabular}{l r r r r l r r l} \n', file = tab_file_path, append = TRUE)

  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)

  cat(" & \\multicolumn{5}{c}{Logistic Regression} ",
      file = tab_file_path, append = TRUE)
  cat(" & \\multicolumn{3}{c}{Linear Probability Model} \\\\ \n",
      file = tab_file_path, append = TRUE)

  cat('\n \\cmidrule(lr){2-6}\\cmidrule(lr){7-9} \n', file = tab_file_path, append = TRUE)

  cat(" & \\multicolumn{2}{c}{Marginal Effects} & Estimate & Standard & Sig. & Estimate & Standard & Sig. \\\\ \n",
      file = tab_file_path, append = TRUE)
  cat('\n \\cmidrule(lr){2-3} \n', file = tab_file_path, append = TRUE)
  cat(" &   AME & MER &          &  Error   &      &          &  Error   &     \\\\ \n",
      file = tab_file_path, append = TRUE)



  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)
  for (sex_sel in sex_list[2:length(sex_list)]) {


    # Print sample title in first line.
    if (sex_sel == 'All') {
      row_str <- 'Full sample'
    } else {
      row_str <- sex_sel
    }
    # cat(sprintf('\\textbf{%s Drivers} \\\\ \n\\hline\n', row_str), file = tab_file_path, append = TRUE)

    # Print number of observations in header.
    obsn_str <- obsn_str_list[sex_sel]

    cat(sprintf('\\multicolumn{8}{l}{\\textbf{%s Drivers} (%s observations)} \\\\ \n\n',
                row_str, obsn_str), file = tab_file_path, append = TRUE)


    for (pts_target_num in 1:nrow(points_label_list)) {

      pts_target <- points_label_list[pts_target_num, 'Variable']
      pts_target_label <- points_label_list[pts_target_num, 'Label']
      pts_target_str <- substr(sprintf('%s %s', pts_target_label,
                                       paste(rep(' ', 30), collapse = '')), 1, 30)



      #------------------------------------------------------------
      # Logit model with age interactions.
      #------------------------------------------------------------
      # Print row with policy indicator from both models.
      cat(sprintf('%s ', pts_target_str), file = tab_file_path, append = TRUE)

      # Pull estimates.
      logit_var_name_sel <- c('Estimate', 'Std_Error', 'p_value')

      # logit_var_name_sel <- c(logit_var_name_sel, 'mfx')
      logit_var_name_sel <- c(logit_var_name_sel, 'AME', 'MER')

      est_se_p <- estn_results_tab[estn_results_tab[, 'sex'] == sex_sel &
                                     estn_results_tab[, 'age_int'] == 'no' &
                                     estn_results_tab[, 'reg_type'] == 'Logit' &
                                     estn_results_tab[, 'pts_target'] == pts_target &
                                     estn_results_tab[, 'Variable'] == 'policyTRUE',
                                   logit_var_name_sel]
      # Print marginal effects, if necessary.
      cat(sprintf(' &  %6.4f      ', est_se_p[4:5]), file = tab_file_path, append = TRUE)

      # Print logit results directly.
      cat(sprintf(' &  %6.4f      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)

      cat(sprintf(' &  %s      ', p_val_stars(p_value = est_se_p[3])),
          file = tab_file_path, append = TRUE)
      #------------------------------------------------------------


      #------------------------------------------------------------
      # Linear probability model with age interactions.
      #------------------------------------------------------------
      est_se_p <- estn_results_tab[estn_results_tab[, 'sex'] == sex_sel &
                                     estn_results_tab[, 'age_int'] == 'no' &
                                     estn_results_tab[, 'reg_type'] == 'LPM' &
                                     estn_results_tab[, 'pts_target'] == pts_target &
                                     estn_results_tab[, 'Variable'] == 'policyTRUE',
                                   c('Estimate', 'Std_Error', 'p_value')]
      print(est_se_p)
      if (num_fmt == 'science') {
        cat(sprintf(' &  %5.2E      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)
      } else if (num_fmt %in% c('num', 'x100K')) {
        cat(sprintf(' &  %6.4f      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)
      } else {
        stop(sprintf('Number format %s not recognized.', num_fmt))
      }
      cat(sprintf(' &  %s      ', p_val_stars(p_value = est_se_p[3])), file = tab_file_path, append = TRUE)
      cat(' \\\\ \n', file = tab_file_path, append = TRUE)
      #------------------------------------------------------------

    }

    # Print sample sizes at bottom.
    # obsn_str <- obsn_str_list[sex_sel]
    # cat(sprintf('Observations            & \\multicolumn{2}{c}{%s} \\\\\n         ', obsn_str),
    #     file = tab_file_path, append = TRUE)

    cat('\n\\hline \n\n', file = tab_file_path, append = TRUE)
  }

  cat('\\end{tabular} \n', file = tab_file_path, append = TRUE)
  cat(sprintf('\\caption{%s} \n', caption), file = tab_file_path, append = TRUE)
  # cat('All regressions contain age category and demerit point category controls. \n', file = tab_file_path, append = TRUE)
  # cat('The symbol * denotes statistical significance at the 0.1\\% level \n', file = tab_file_path, append = TRUE)
  # cat('and ** the 0.001\\% level. \n', file = tab_file_path, append = TRUE)
  # cat('``Sig.\'\' is an abbreviation for statistical significance. \n', file = tab_file_path, append = TRUE)
  for (desc_row in 1:length(description)) {
    cat(sprintf('%s \n', description[desc_row]), file = tab_file_path, append = TRUE)
  }
  cat(sprintf('\\label{%s} \n', label), file = tab_file_path, append = TRUE)
  cat('\\end{table} \n \n', file = tab_file_path, append = TRUE)
}


event_study_LPM_logit_2MFX_table <- function(tab_file_path, estn_results_tab,
                                             header, caption, description, label,
                                             events_label_list,
                                             obsn_str_list,
                                             num_fmt = 'science',
                                             incl_mfx = FALSE) {

  cat(sprintf('%% %s \n\n', header),
      file = tab_file_path, append = FALSE)
  cat('\\begin{table}% [ht] \n', file = tab_file_path, append = TRUE)
  cat('\\centering \n', file = tab_file_path, append = TRUE)


  cat('\\begin{tabular}{l r r r r l r r l} \n', file = tab_file_path, append = TRUE)

  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)

  cat(" & \\multicolumn{5}{c}{Logistic Regression} ",
      file = tab_file_path, append = TRUE)
  cat(" & \\multicolumn{3}{c}{Linear Probability Model} \\\\ \n",
      file = tab_file_path, append = TRUE)

  cat('\n \\cmidrule(lr){2-6}\\cmidrule(lr){7-9} \n', file = tab_file_path, append = TRUE)

  cat(" & \\multicolumn{2}{c}{Marginal Effects} & Estimate & Standard & Sig. & Estimate & Standard & Sig. \\\\ \n",
      file = tab_file_path, append = TRUE)
  cat('\n \\cmidrule(lr){2-3} \n', file = tab_file_path, append = TRUE)
  cat(" &   AME & MER &          &  Error   &      &          &  Error   &     \\\\ \n",
      file = tab_file_path, append = TRUE)


  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)
  for (sex_sel in sex_list[2:length(sex_list)]) {


    # Print sample title in first line.
    if (sex_sel == 'All') {
      row_str <- 'Full sample'
    } else {
      row_str <- sex_sel
    }


    # Print number of observations in header.
    obsn_str <- obsn_str_list[sex_sel]

    cat(sprintf('\\multicolumn{7}{l}{\\textbf{%s Drivers} (%s observations)} \\\\ \n\n',
                row_str, obsn_str), file = tab_file_path, append = TRUE)

    #------------------------------------------------------------
    # Print first row with policy indicator.
    # Logit model without age interactions.
    #------------------------------------------------------------
    cat('Policy Indicator         ', file = tab_file_path, append = TRUE)
    # Pull estimates.
    logit_var_name_sel <- c('Estimate', 'Std_Error', 'p_value')

    # logit_var_name_sel <- c(logit_var_name_sel, 'mfx')
    logit_var_name_sel <- c(logit_var_name_sel, 'AME', 'MER')

    est_se_p <- estn_results_tab[estn_results_tab[, 'sex'] == sex_sel &
                                   estn_results_tab[, 'age_int'] == 'no' &
                                   estn_results_tab[, 'reg_type'] == 'Logit' &
                                   estn_results_tab[, 'pts_target'] == 'all' &
                                   estn_results_tab[, 'Variable'] == 'policyTRUE',
                                 logit_var_name_sel]
    # Print marginal effects, if necessary.
    cat(sprintf(' &  %6.4f      ', est_se_p[4:5]), file = tab_file_path, append = TRUE)

    # Print logit results directly.
    cat(sprintf(' &  %6.4f      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)

    cat(sprintf(' &  %s      ', p_val_stars(p_value = est_se_p[3])), file = tab_file_path, append = TRUE)
    # LPM model without age interactions.
    est_se_p <- estn_results_tab[estn_results_tab[, 'sex'] == sex_sel &
                                   # estn_results_tab[, 'age_int'] == 'with' &
                                   estn_results_tab[, 'age_int'] == 'no' &
                                   estn_results_tab[, 'reg_type'] == 'LPM' &
                                   estn_results_tab[, 'pts_target'] == 'all' &
                                   estn_results_tab[, 'Variable'] == 'policyTRUE',
                                 c('Estimate', 'Std_Error', 'p_value')]
    if (num_fmt == 'science') {
      cat(sprintf(' &  %5.2E      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)
    } else if (num_fmt %in% c('num', 'x100K')) {
      cat(sprintf(' &  %6.4f      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)
    } else {
      stop(sprintf('Number format %s not recognized.', num_fmt))
    }
    cat(sprintf(' &  %s      ', p_val_stars(p_value = est_se_p[3])), file = tab_file_path, append = TRUE)
    cat(' \\\\ \n', file = tab_file_path, append = TRUE)


    for (event_month_num in 1:nrow(events_label_list)) {

      event_month <- events_label_list[event_month_num, 'Variable']
      event_month_label <- events_label_list[event_month_num, 'Label']
      event_month_str <- substr(sprintf('%s %s', event_month_label,
                                        paste(rep(' ', 30), collapse = '')), 1, 30)



      #------------------------------------------------------------
      # Logit model with age interactions.
      #------------------------------------------------------------
      # Print row with policy indicator from both models.
      cat(sprintf('%s ', event_month_str), file = tab_file_path, append = TRUE)
      est_se_p <- estn_results_tab[estn_results_tab[, 'sex'] == sex_sel &
                                     estn_results_tab[, 'age_int'] == 'no' &
                                     estn_results_tab[, 'reg_type'] == 'Logit' &
                                     estn_results_tab[, 'pts_target'] == 'all' &
                                     estn_results_tab[, 'Variable'] == event_month,
                                   logit_var_name_sel]
      # Print marginal effects, if necessary.
      cat(sprintf(' &  %6.4f      ', est_se_p[4:5]), file = tab_file_path, append = TRUE)

      # Print logit results directly.
      cat(sprintf(' &  %6.4f      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)

      cat(sprintf(' &  %s      ', p_val_stars(p_value = est_se_p[3])),
          file = tab_file_path, append = TRUE)
      #------------------------------------------------------------


      #------------------------------------------------------------
      # Linear probability model with age interactions.
      #------------------------------------------------------------
      est_se_p <- estn_results_tab[estn_results_tab[, 'sex'] == sex_sel &
                                     estn_results_tab[, 'age_int'] == 'no' &
                                     estn_results_tab[, 'reg_type'] == 'LPM' &
                                     estn_results_tab[, 'pts_target'] == 'all' &
                                     estn_results_tab[, 'Variable'] == event_month,
                                   c('Estimate', 'Std_Error', 'p_value')]
      if (num_fmt == 'science') {
        cat(sprintf(' &  %5.2E      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)
      } else if (num_fmt %in% c('num', 'x100K')) {
        cat(sprintf(' &  %6.4f      ', est_se_p[1:2]), file = tab_file_path, append = TRUE)
      } else {
        stop(sprintf('Number format %s not recognized.', num_fmt))
      }
      cat(sprintf(' &  %s      ', p_val_stars(p_value = est_se_p[3])), file = tab_file_path, append = TRUE)
      cat(' \\\\ \n', file = tab_file_path, append = TRUE)
      #------------------------------------------------------------

    }

    cat('\n\\hline \n\n', file = tab_file_path, append = TRUE)
  }

  cat('\\end{tabular} \n', file = tab_file_path, append = TRUE)
  cat(sprintf('\\caption{%s} \n', caption), file = tab_file_path, append = TRUE)
  for (desc_row in 1:length(description)) {
    cat(sprintf('%s \n', description[desc_row]), file = tab_file_path, append = TRUE)
  }
  cat(sprintf('\\label{%s} \n', label), file = tab_file_path, append = TRUE)
  cat('\\end{table} \n \n', file = tab_file_path, append = TRUE)
}






#--------------------------------------------------
# Main Function for Full Set of Tables
#--------------------------------------------------

SAAQ_Logit_vs_LPM_2MFX_table_gen <-
  function(tab_dir, tab_tag, header_spec, season_incl, num_fmt,
           estn_results_by_age, estn_results_full, estn_results_high,
           estn_results_placebo, estn_results_events,
           age_grp_list, age_int_label_list, points_label_list, sex_list,
           orig_description, orig_pts_description,
           incl_mfx = FALSE,
           est_pooled = TRUE) {


    header_model <- 'Logistic Regression and Linear Probability Models'


    if (num_fmt == 'x100K') {
      if (!is.null(estn_results_by_age)) {
        estn_results_by_age[estn_results_by_age[, 'reg_type'] == 'LPM', c('Estimate', 'Std_Error')] <-
          estn_results_by_age[estn_results_by_age[, 'reg_type'] == 'LPM', c('Estimate', 'Std_Error')]*100000
      }
      if (!is.null(estn_results_full)) {
        estn_results_full[estn_results_full[, 'reg_type'] == 'LPM', c('Estimate', 'Std_Error')] <-
          estn_results_full[estn_results_full[, 'reg_type'] == 'LPM', c('Estimate', 'Std_Error')]*100000
      }
      if (!is.null(estn_results_high)) {
        estn_results_high[estn_results_high[, 'reg_type'] == 'LPM', c('Estimate', 'Std_Error')] <-
          estn_results_high[estn_results_high[, 'reg_type'] == 'LPM', c('Estimate', 'Std_Error')]*100000
      }
      if (!is.null(estn_results_placebo)) {
        estn_results_placebo[estn_results_placebo[, 'reg_type'] == 'LPM', c('Estimate', 'Std_Error')] <-
          estn_results_placebo[estn_results_placebo[, 'reg_type'] == 'LPM', c('Estimate', 'Std_Error')]*100000
      }
      if (!is.null(estn_results_events)) {
        estn_results_events[estn_results_events[, 'reg_type'] == 'LPM', c('Estimate', 'Std_Error')] <-
          estn_results_events[estn_results_events[, 'reg_type'] == 'LPM', c('Estimate', 'Std_Error')]*100000
      }

      num_fmt_tag <- 'multiplied by 100,000'
    } else if (num_fmt == 'science') {
      num_fmt_tag <- 'in scientific notation'
    } else if (num_fmt == 'num') {
      num_fmt_tag <- 'in general number format'
    }

    # Change the number format sentence accordingly.
    # num_fmt_row <- length(orig_description)
    num_fmt_row <- which(orig_description == 'in scientific notation. ')
    orig_description[num_fmt_row] <- sprintf('%s. ', num_fmt_tag)
    # num_fmt_row <- length(orig_pts_description)
    num_fmt_row <- which(orig_pts_description == 'in scientific notation. ')
    orig_pts_description[num_fmt_row] <- sprintf('%s. ', num_fmt_tag)


    #--------------------------------------------------
    # Select information for main regression results
    #--------------------------------------------------


    # Temporary list of fixed numbers of observations.
    obsn_str_list_full <- c('9,675,245,494', '5,335,033,221', '4,340,212,273')
    names(obsn_str_list_full) <- sex_list


    # Collect estimates into table.
    results_sel <- estn_results_full[, 'past_pts'] == 'all' &
      estn_results_full[, 'window'] == '4 yr.' &
      estn_results_full[, 'seasonality'] == season_incl &
      estn_results_full[, 'age_int'] %in% c('no', 'with') &
      estn_results_full[, 'pts_target'] == 'all' &
      # estn_results_full[, 'reg_type'] == reg_type &
      (substr(estn_results_full[, 'Variable'], 1, 6) == 'policy')
    estn_results_tab <- estn_results_full[results_sel, ]


    #--------------------------------------------------
    # Regressions on the Pooled Sample
    #--------------------------------------------------

    if (est_pooled == TRUE) {

      tab_file_name <- sprintf('%s_pooled_only_regs.tex', tab_tag)
      tab_file_path <- sprintf('%s/%s', tab_dir, tab_file_name)

      single_point_LPM_logit_2MFX_table(tab_file_path, estn_results_tab,
                                        header = sprintf('%s: %s Specification for All Drivers by Point Value',
                                                         header_model, header_spec),
                                        # caption = sprintf('Regressions for all drivers (%s)',
                                        #                   header_spec),
                                        caption = 'Pooled Regressions for all offences, male and female drivers',
                                        description = orig_description,
                                        label = sprintf('tab:%s_pooled_regs', tab_tag),
                                        sex_list, # Pool on gender.
                                        # sex_list = c('All'),
                                        age_int_label_list,
                                        obsn_str_list = obsn_str_list_full,
                                        num_fmt,
                                        # incl_mfx = FALSE)
                                        incl_mfx = incl_mfx,
                                        pooled = TRUE) # Pool on gender.


    }


    #--------------------------------------------------
    # Regressions on the Entire Sample by Gender
    #--------------------------------------------------

    # Table selection is the same but only the file names
    # and gender selection are different.

    tab_file_name <- sprintf('%s_regs.tex', tab_tag)
    tab_file_path <- sprintf('%s/%s', tab_dir, tab_file_name)

    single_point_LPM_logit_2MFX_table(tab_file_path, estn_results_tab,
                                      header = sprintf('%s: %s Specification for Male and Female Drivers by Point Value',
                                                       header_model, header_spec),
                                      # caption = sprintf('Regressions for all drivers (%s)',
                                      #                   header_spec),
                                      caption = 'Regressions for all offences',
                                      description = orig_description,
                                      label = sprintf('tab:%s_regs', tab_tag),
                                      sex_list,
                                      age_int_label_list,
                                      obsn_str_list = obsn_str_list_full,
                                      num_fmt,
                                      # incl_mfx = FALSE)
                                      incl_mfx = incl_mfx)



    #--------------------------------------------------
    # Regressions on the Pooled Sample by Age Group
    #--------------------------------------------------

    if (!is.null(estn_results_by_age)) {

      # Collect estimates into table.
      results_sel <- estn_results_by_age[, 'past_pts'] == 'all' &
        estn_results_by_age[, 'window'] == '4 yr.' &
        estn_results_by_age[, 'seasonality'] == season_incl &
        estn_results_by_age[, 'age_int'] %in% age_grp_list &
        # estn_results_by_age[, 'age_int'] %in% c('no', 'with') &
        estn_results_by_age[, 'pts_target'] == 'all' &
        # estn_results_by_age[, 'reg_type'] == reg_type &
        (substr(estn_results_by_age[, 'Variable'], 1, 6) == 'policy')
      estn_results_tab <- estn_results_by_age[results_sel, ]



      # Temporary list of fixed numbers of observations.
      # Pull from file of sample sizes.
      git_path <- "C:/Users/le279259/Documents/Research/SAAQ/SAAQspeeding/Hidden_Comp_Risks/R_and_R"
      md_dir <- sprintf("%s/results", git_path)
      estn_version <- 1
      estn_file_name <- sprintf('sampl_sizes_v%d.csv', estn_version)
      estn_file_path <- sprintf('%s/%s', md_dir, estn_file_name)
      sample_sizes <- read.csv(estn_file_path)


      sample_size_sel <- sample_sizes[, 'past_pts'] == 'all' &
        sample_sizes[, 'window'] == '4 yr.' &
        sample_sizes[, 'seasonality'] == season_incl &
        sample_sizes[, 'age_int'] %in% age_grp_list &
        # estn_results_by_age[, 'age_int'] %in% c('no', 'with') &
        sample_sizes[, 'pts_target'] == 'all' &
        # estn_results_by_age[, 'reg_type'] == reg_type &
        # (substr(sample_sizes[, 'Variable'], 1, 6) == 'policy')
        sample_sizes[, 'reg_type'] == 'LPM'
      # obsn_str_list_by_age <- sample_sizes[sample_size_sel, 'N']
      obsn_str_list_by_age <- comma_format()(sample_sizes[sample_size_sel, 'N'])
      names(obsn_str_list_by_age) <- age_grp_list


      tab_file_name <- sprintf('%s_regs_by_age.tex', tab_tag)
      tab_file_path <- sprintf('%s/%s', tab_dir, tab_file_name)


      multi_group_LPM_logit_2MFX_table(tab_file_path, estn_results_tab,
                                       header = sprintf('%s: %s Specification for All Drivers by Age Group',
                                                        header_model, header_spec),
                                       # caption = sprintf('Regressions for all drivers (%s)',
                                       #                   header_spec),
                                       caption = 'Regressions for all offences, by age group',
                                       description = orig_description,
                                       label = sprintf('tab:%s_regs_by_age', tab_tag),
                                       # sex_list,
                                       # age_int_label_list,
                                       age_grp_list,
                                       obsn_str_list = obsn_str_list_by_age,
                                       num_fmt,
                                       # incl_mfx = FALSE)
                                       incl_mfx = incl_mfx)

    }


    #--------------------------------------------------
    # Regressions by ticket point value
    #--------------------------------------------------

      # Temporary list of fixed numbers of observations.
    obsn_str_list_full <- c('9,675,245,494', '5,335,033,221', '4,340,212,273')
    names(obsn_str_list_full) <- sex_list

    # Collect estimates into table.
    results_sel <- estn_results_full[, 'past_pts'] == 'all' &
      estn_results_full[, 'window'] == '4 yr.' &
      estn_results_full[, 'seasonality'] == season_incl &
      estn_results_full[, 'age_int'] %in% c('no') &
      # estn_results_full[, 'pts_target'] != 'all' &
      # estn_results_full[, 'reg_type'] == reg_type &
      estn_results_full[, 'Variable'] == 'policyTRUE'
    estn_results_tab <- estn_results_full[results_sel, ]


    # Create TeX file for Table.
    tab_file_name <- sprintf('%s_regs_by_points.tex', tab_tag)
    tab_file_path <- sprintf('%s/%s', tab_dir, tab_file_name)


    multi_point_LPM_logit_2MFX_table(tab_file_path, estn_results_tab,
                                     header = sprintf('%s: %s Specification by Point Value',
                                                      header_model, header_spec),
                                     # caption = sprintf('Regressions by ticket-point value (%s)',
                                     #                   header_spec),
                                     caption = 'Regressions by ticket-point value',
                                     description = orig_pts_description,
                                     label = sprintf('tab:%s_regs_by_points', tab_tag),
                                     points_label_list,
                                     obsn_str_list_full,
                                     num_fmt,
                                     # incl_mfx = FALSE)
                                     incl_mfx = incl_mfx)



    #--------------------------------------------------
    # Regressions by ticket point value for high-point drivers
    #--------------------------------------------------

    # Temporary list of fixed numbers of observations.
    # obsn_str_list_high <- c('1,170,426,426', '921,131,812', '249,294,614')
    # Corrected by adding back degrees of freedom for females.
    obsn_str_list_high <- c('1,170,426,426', '921,131,812', '249,294,627')
    names(obsn_str_list_high) <- sex_list


    results_sel <- estn_results_high[, 'past_pts'] == 'high' &
      estn_results_high[, 'window'] == '4 yr.' &
      estn_results_high[, 'seasonality'] == season_incl &
      estn_results_high[, 'age_int'] %in% c('no') &
      # estn_results_full[, 'pts_target'] != 'all' &
      # estn_results_high[, 'reg_type'] == reg_type &
      estn_results_high[, 'Variable'] == 'policyTRUE'
    estn_results_tab <- estn_results_high[results_sel, ]


    # Create TeX file for Table.
    tab_file_name <- sprintf('%s_high_pt_regs_by_points.tex', tab_tag)
    tab_file_path <- sprintf('%s/%s', tab_dir, tab_file_name)


    multi_point_LPM_logit_2MFX_table(tab_file_path, estn_results_tab,
                                     header = sprintf('%s: %s Specification for High-Point Drivers by Point Value',
                                                      header_model, header_spec),
                                     # caption = sprintf('Regressions for high-point drivers by ticket-point value (%s)',
                                     #                   header_spec),
                                     caption = 'Regressions for high-point drivers by ticket-point value',
                                     description = orig_pts_description,
                                     label = sprintf('tab:%s_high_pt_regs_by_points', tab_tag),
                                     points_label_list,
                                     obsn_str_list_high,
                                     num_fmt,
                                     # incl_mfx = FALSE)
                                     incl_mfx = incl_mfx)


    #--------------------------------------------------
    # Placebo regressions
    #--------------------------------------------------

    # Temporary list of fixed numbers of observations.
    obsn_str_list_placebo <- c('4,728,750,336', '2,618,869,394', '2,109,880,942 ')
    # Corrected by adding back degrees of freedom.
    obsn_str_list_placebo <- c('4,728,750,336', '2,618,869,407', '2,109,880,955 ')
    names(obsn_str_list_placebo) <- sex_list


    results_sel <- estn_results_placebo[, 'past_pts'] == 'all' &
      estn_results_placebo[, 'window'] == 'Placebo' &
      estn_results_placebo[, 'seasonality'] == season_incl &
      estn_results_placebo[, 'age_int'] %in% c('no', 'with') &
      estn_results_placebo[, 'pts_target'] == 'all' &
      # estn_results_placebo[, 'reg_type'] == reg_type &
      (substr(estn_results_placebo[, 'Variable'], 1, 6) == 'policy')
    estn_results_tab <- estn_results_placebo[results_sel, ]


    # Create TeX file for Table.
    tab_file_name <- sprintf('%s_placebo_regs.tex', tab_tag)
    tab_file_path <- sprintf('%s/%s', tab_dir, tab_file_name)


    single_point_LPM_logit_2MFX_table(tab_file_path, estn_results_tab,
                                      header = sprintf('%s: %s Placebo Specification',
                                                       header_model, header_spec),
                                      # caption = sprintf('Placebo regressions (%s)', header_spec),
                                      caption = 'Placebo regressions for all offences',
                                      description = orig_description,
                                      label = sprintf('tab:%s_placebo_regs', tab_tag),
                                      sex_list,
                                      age_int_label_list,
                                      obsn_str_list_placebo,
                                      num_fmt,
                                      # incl_mfx = FALSE)
                                      incl_mfx = incl_mfx)



    #--------------------------------------------------
    # Regressions by months since policy change
    #--------------------------------------------------


    # Temporary list of fixed numbers of observations.
    obsn_str_list_full <- c('9,675,245,494', '5,335,033,221', '4,340,212,273')
    names(obsn_str_list_full) <- sex_list


    # Create TeX file for Table.
    tab_file_name <- sprintf('%s_event_month_regs.tex', tab_tag)
    tab_file_path <- sprintf('%s/%s', tab_dir, tab_file_name)



    event_study_LPM_logit_2MFX_table(tab_file_path, estn_results_events,
                                     header = sprintf('%s: %s Specification by Event Month',
                                                      header_model, header_spec),
                                     # caption = sprintf('Regressions with indicators for month since policy change (%s)',
                                     #                   header_spec),
                                     caption = 'Regressions with indicators for month since policy change',
                                     description = orig_description,
                                     label = sprintf('tab:%s_event_month_regs', tab_tag),
                                     events_label_list,
                                     obsn_str_list_full,
                                     num_fmt,
                                     # incl_mfx = FALSE)
                                     incl_mfx = incl_mfx)

  }





################################################################################
# End
################################################################################
