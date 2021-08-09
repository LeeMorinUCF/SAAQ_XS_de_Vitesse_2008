################################################################################
#
# Investigation of SAAQ Excessive Speeding Laws
#
# A library of functions to generate tables of estimated coeffiecients
# and standard errors the fixed effects model
#
# Lee Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# August 9, 2021
#
################################################################################
#
################################################################################

################################################################################
# Declaring Packages
################################################################################


# Load scales library because it has a function
# to display large numbers in comma format.
library(scales)

################################################################################
# Function Definitions
################################################################################


#--------------------------------------------------------------------------------
# FE_reg_table generate tables of estimated coeffiecients
# and standard errors the fixed effects model.
#
# Dependencies: comma_format, from the scales package.
#
#--------------------------------------------------------------------------------

# Call function to generate LaTeX table.
FE_reg_table <- function(FE_est_out, tab_file_path,
                            header, caption, description, label) {
  # Create a flag to indicate successful completion.
  FE_reg_flag <- FALSE



  # Output header.
  cat(sprintf('%% %s \n\n', header),
      file = tab_file_path, append = FALSE)
  cat('\\begin{table}% [ht] \n', file = tab_file_path, append = TRUE)
  cat('\\centering \n', file = tab_file_path, append = TRUE)
  cat('\\begin{tabular}{l r r r r r r} \n', file = tab_file_path, append = TRUE)
  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)

  # Output column headers.
  cat('\nSample \n', file = tab_file_path, append = TRUE)
  for (sample_name in c('All', 'Male', 'Female')) {
    cat(sprintf(" & \\multicolumn{2}{c}{%s  Drivers} ", sample_name),
        file = tab_file_path, append = TRUE)
  }
  cat('  \\\\ \n \n', file = tab_file_path, append = TRUE)

  cat('\n \\cmidrule(lr){1-1}\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7} \n',
      file = tab_file_path, append = TRUE)

  cat('\nEstimate ', file = tab_file_path, append = TRUE)
  for (i in 1:3) {
    cat(' & Coefficient & Std. Error ', file = tab_file_path, append = TRUE)
  }
  cat('  \\\\ \n \n', file = tab_file_path, append = TRUE)
  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)


  # Output table of estimates.

  for (row in 1:nrow(FE_est_out)) {

    if (row == 3) {
      cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)
      cat('\\multicolumn{4}{l}{\\textbf{Demerit points group indicators:}}  \\\\ \n \n',
          file = tab_file_path, append = TRUE)
    }


    # Create variable name for table.
    var_name <- rownames(FE_est_out)[row]
    var_name <- gsub('curr_pts_', '', var_name)
    var_name <- gsub('_policy', '', var_name)
    var_name <- gsub('_', '-', var_name)
    if (var_name == '(Intercept)') {
      var_name <- 'Intercept'
    } else if (var_name == 'dev') {
      var_name <- 'policy'
    } else {
      var_name <- sprintf('%s points', var_name)
    }

    cat(sprintf('%s ', var_name), file = tab_file_path, append = TRUE)
    for (col in 1:ncol(FE_est_out)) {

      cat(sprintf(' & %6.3f ', FE_est_out[row, col]*1000),
          file = tab_file_path, append = TRUE)
    }
    cat('  \\\\ \n \n', file = tab_file_path, append = TRUE)
    # Print divider to report policy-points-group interactions separately.
    if (row == (nrow(FE_est_out) - 2)/2 + 2) {
      cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)
      cat('\\multicolumn{4}{l}{\\textbf{Policy and points group interactions:}}  \\\\ \n \n',
          file = tab_file_path, append = TRUE)
    }

  }
  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)

  # Output summary statistics.
  # FFX_stats

  cat('\nDrivers \n', file = tab_file_path, append = TRUE)
  for (i in 1:3) {
    num_drivers <- comma_format()(FFX_stats[i, 'num_drivers'])
    cat(sprintf(' & \\multicolumn{2}{r}{%s} ', num_drivers),
        file = tab_file_path, append = TRUE)
  }
  cat('  \\\\ \n \n', file = tab_file_path, append = TRUE)

  cat('\nDriver days \n', file = tab_file_path, append = TRUE)
  for (i in 1:3) {
    num_obs <- comma_format()(FFX_stats[i, 'num_obs'])
    cat(sprintf(' & \\multicolumn{2}{r}{%s} ', num_obs),
        file = tab_file_path, append = TRUE)
  }
  cat('  \\\\ \n \n', file = tab_file_path, append = TRUE)

  cat('\nSSR \n', file = tab_file_path, append = TRUE)
  for (i in 1:3) {
    SSR <- comma_format()(FFX_stats[i, 'SSR'])
    cat(sprintf(' & \\multicolumn{2}{r}{%s} ', SSR),
        file = tab_file_path, append = TRUE)
  }
  cat('  \\\\ \n \n', file = tab_file_path, append = TRUE)

  cat('\n\\hline \n \n', file = tab_file_path, append = TRUE)


  # Output closing arguments.
  cat('\\end{tabular} \n', file = tab_file_path, append = TRUE)
  cat(sprintf('\\caption{%s} \n', caption), file = tab_file_path, append = TRUE)
  for (desc_row in 1:length(description)) {
    cat(sprintf('%s \n', description[desc_row]), file = tab_file_path, append = TRUE)
  }
  cat(sprintf('\\label{%s} \n', label), file = tab_file_path, append = TRUE)
  cat('\\end{table} \n \n', file = tab_file_path, append = TRUE)




  # If it reaches here without failing, FE_reg_flag is TRUE.
  FE_reg_flag <- TRUE
  return(FE_reg_flag)
}
