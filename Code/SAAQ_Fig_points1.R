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
# November 28, 2021
#
################################################################################
#
# This script creates figures of the policy effect by demerit point
# balances both with and without age interactions.
#
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


# Set methodology for zero-ticket population count:
# adj (unadjusted zero counts, intended for stacked join) or
zero_count_method <- 'adj'
# unadj (adjusted zero counts, intended for differenced join)
# zero_count_method <- 'unadj'

# Set join methodology:
# all (stacked, intended for unadjusted zero counts) or
join_method <- 'all'
# net (differenced, intended for adjusted zero counts)
# join_method <- 'net'


# Set version of input files.
# data_in_method <- 'all_unadj'
data_in_method <- sprintf('%s_%s', join_method, zero_count_method)


data_out_path <- 'Estn'


# Set directory for results in GitHub repo.
# git_path <- "~/Research/SAAQ/SAAQspeeding/Hidden_Comp_Risks/R_and_R"
# git_path <- "C:/Users/le279259/Documents/Research/SAAQ/SAAQspeeding/Hidden_Comp_Risks/R_and_R"
# md_dir <- sprintf("%s/results", git_path)
# md_dir <- sprintf("%s/results", data_out_path)
md_dir <- sprintf("%s/results_%s", data_out_path, data_in_method)




################################################################################
# Load estimation results.
################################################################################


# Set path for retrieving estimates.
# Specification: Plot by demerit point groups
spec_group <- 'points'
estn_version <- 16
estn_file_name <- sprintf('estimates_v%d_%s.csv', estn_version, spec_group)
estn_file_path <- sprintf('%s/%s', md_dir, estn_file_name)



# Read in the estimation results.
estn_results <- read.csv(file = estn_file_path)


################################################################################
# Model without age interactions
################################################################################

age_int <- 'no'


# sex_sel <- 'Male'
# sex_sel <- 'Female'
sex_sel_list <- c('Male', 'Female')

points_policy <- data.frame(curr_pts = curr_pts_reg_list[2:11],
                            policy_M = numeric(10),
                            std_err_M = numeric(10),
                            policy_F = numeric(10),
                            std_err_F = numeric(10))



for (sex_sel in sex_sel_list) {

  print(sprintf('Calculating predictions for %s Drivers', sex_sel))

  #------------------------------------------------------------
  # Retrieve estimates
  #------------------------------------------------------------

  # Select the rows that correspond to a particular model and subsample.
  estn_sel <- estn_results[, 'sex'] == sex_sel &
    estn_results[, 'age_int'] == age_int
  est_coefs <- estn_results[estn_sel, ]


  #------------------------------------------------------------
  # Retrieve covariance matrix
  #------------------------------------------------------------

  # Set path for retrieving covariance matrix.
  cov_file_name <- sprintf('points_fig/cov_mat_v%d_%s_%s_age_int_%s.csv',
                           estn_version, spec_group, age_int, substr(sex_sel, 1, 1))
  cov_file_path <- sprintf('%s/%s', md_dir, cov_file_name)

  vcov_hccme <- read.csv(file = cov_file_path)

  # Extract matrix with appropriate labels.
  rownames(vcov_hccme) <- vcov_hccme[, 'X']
  vcov_hccme <- vcov_hccme[, 2:ncol(vcov_hccme)]
  vcov_hccme <- as.matrix(vcov_hccme)


  # Verify the diagonal to ensure compatibility.
  # sqrt(diag(vcov_hccme)) - est_coefs[, 'Std_Error']
  # abs(sqrt(diag(vcov_hccme)) - est_coefs[, 'Std_Error']) < 10^(-19)
  print('Covariance matrix check: Number of errors = ')
  print(sum(!abs(sqrt(diag(vcov_hccme)) - est_coefs[, 'Std_Error']) < 10^(-15)))
  # Confirms that the files are compatible.

  #------------------------------------------------------------
  # Obtain the estimates and standard errors.
  #------------------------------------------------------------

  for (curr_pts_num in 1:10) {

    curr_pts_grp_sel <- curr_pts_reg_list[curr_pts_num - 1]

    var_sel <- c('policyTRUE', sprintf('policyTRUE:curr_pts_reg%s', curr_pts_grp_sel))
    var_row_sel <- est_coefs[, 'Variable'] %in% var_sel
    # est_coefs[var_row_sel, 'Variable']
    # est_coefs[var_row_sel, 'Estimate']
    # est_coefs[var_row_sel, 'Std_Error']

    col_name <- sprintf('policy_%s', substr(sex_sel, 1, 1))
    points_policy[curr_pts_num, col_name] <- sum(est_coefs[var_row_sel, 'Estimate'])


    # Calculate standard error of linear combination.
    col_name <- sprintf('std_err_%s', substr(sex_sel, 1, 1))
    # points_policy[curr_pts_num, col_name] <- sqrt(t(var_row_sel) %*% vcov_hccme %*% var_row_sel)
    points_policy[curr_pts_num, col_name] <- sqrt(sum(vcov_hccme[var_row_sel, var_row_sel]))



  }


}






################################################################################
# Model with age interactions
################################################################################

age_int <- 'with'

# Select a subset of young drivers.
age_grp_sel <- '20-24'
# age_grp_sel <- '25-34'

# [36] "policyTRUE:age_grp20-24"
# [37] "policyTRUE:age_grp25-34"
# [38] "policyTRUE:age_grp35-44"
# [39] "policyTRUE:age_grp45-54"
# [40] "policyTRUE:age_grp55-64"
# [41] "policyTRUE:age_grp65-199"


points_policy_w_age <- data.frame(curr_pts = curr_pts_reg_list[2:11],
                                  policy_M = numeric(10),
                                  std_err_M = numeric(10),
                                  policy_F = numeric(10),
                                  std_err_F = numeric(10))



# sex_sel <- 'Male'
for (sex_sel in sex_sel_list) {

  print(sprintf('Calculating predictions for %s Drivers', sex_sel))

  #------------------------------------------------------------
  # Retrieve estimates
  #------------------------------------------------------------

  # Select the rows that correspond to a particular model and subsample.
  estn_sel <- estn_results[, 'sex'] == sex_sel &
    estn_results[, 'age_int'] == age_int
  est_coefs <- estn_results[estn_sel, ]


  #------------------------------------------------------------
  # Retrieve covariance matrix
  #------------------------------------------------------------

  # Set path for retrieving covariance matrix.
  cov_file_name <- sprintf('points_fig/cov_mat_v%d_%s_%s_age_int_%s.csv',
                           estn_version, spec_group, age_int, substr(sex_sel, 1, 1))
  cov_file_path <- sprintf('%s/%s', md_dir, cov_file_name)

  vcov_hccme <- read.csv(file = cov_file_path)

  # Extract matrix with appropriate labels.
  rownames(vcov_hccme) <- vcov_hccme[, 'X']
  vcov_hccme <- vcov_hccme[, 2:ncol(vcov_hccme)]
  vcov_hccme <- as.matrix(vcov_hccme)


  # Verify the diagonal to ensure compatibility.
  # sqrt(diag(vcov_hccme)) - est_coefs[, 'Std_Error']
  # abs(sqrt(diag(vcov_hccme)) - est_coefs[, 'Std_Error']) < 10^(-19)
  print('Covariance matrix check: Number of errors = ')
  print(sum(!abs(sqrt(diag(vcov_hccme)) - est_coefs[, 'Std_Error']) < 10^(-15)))
  # Confirms that the files are compatible.

  #------------------------------------------------------------
  # Obtain the estimates and standard errors.
  #------------------------------------------------------------

  for (curr_pts_num in 1:10) {

    curr_pts_grp_sel <- curr_pts_reg_list[curr_pts_num - 1]

    # For version with age group interaction:
    var_sel <- c('policyTRUE',
                 sprintf('policyTRUE:age_grp%s', age_grp_sel),
                 sprintf('policyTRUE:curr_pts_reg%s', curr_pts_grp_sel))

    var_row_sel <- est_coefs[, 'Variable'] %in% var_sel
    # est_coefs[var_row_sel, 'Variable']
    # est_coefs[var_row_sel, 'Estimate']
    # est_coefs[var_row_sel, 'Std_Error']

    col_name <- sprintf('policy_%s', substr(sex_sel, 1, 1))
    points_policy_w_age[curr_pts_num, col_name] <- sum(est_coefs[var_row_sel, 'Estimate'])


    # Calculate standard error of linear combination.
    col_name <- sprintf('std_err_%s', substr(sex_sel, 1, 1))
    # points_policy[curr_pts_num, col_name] <- sqrt(t(var_row_sel) %*% vcov_hccme %*% var_row_sel)
    points_policy_w_age[curr_pts_num, col_name] <- sqrt(sum(vcov_hccme[var_row_sel, var_row_sel]))



  }


}


points_policy
points_policy_w_age



################################################################################
# Plot results
################################################################################


#------------------------------------------------------------
# Model without age interactions
#------------------------------------------------------------

fig_file_name <- 'points_fig/points_fig_no_age_int.eps'
fig_file_path <- sprintf('%s/%s', md_dir, fig_file_name)


postscript(fig_file_path)
# y_lims <- c(min(points_policy[, 'policy_M'] -
#                   1.96*points_policy[, 'std_err_M'])*100000, 0)
y_lims <- c(-30, 5)
plot(points_policy[, 'policy_M']*100000,
     xlab = 'Demerit-Point Balance',
     ylab = 'Policy Effect',
     ylim = y_lims,
     type = 'l',
     col = 'black',
     lwd = 3)
# Append standard error bands.
lines((points_policy[, 'policy_M'] +
         1.96*points_policy[, 'std_err_M'])*100000,
      col = 'black',
      lty = 'dashed',
      lwd = 3)
lines((points_policy[, 'policy_M'] -
         1.96*points_policy[, 'std_err_M'])*100000,
      col = 'black',
      lty = 'dashed',
      lwd = 3)

# Append plots for females.
lines((points_policy[, 'policy_F'])*100000,
      col = 'grey',
      lty = 'solid',
      lwd = 3)
lines((points_policy[, 'policy_F'] +
         1.96*points_policy[, 'std_err_F'])*100000,
      col = 'grey',
      lty = 'dashed',
      lwd = 3)
lines((points_policy[, 'policy_F'] -
         1.96*points_policy[, 'std_err_F'])*100000,
      col = 'grey',
      lty = 'dashed',
      lwd = 3)

# Plot line to denote zero effect.
abline(h = 0,
       col = 'black',
       lty = 'solid',
       lwd = 1)
dev.off()

#------------------------------------------------------------
# Model with age interactions
#------------------------------------------------------------


# y_lims <- c(min(points_policy_w_age[, 'policy_M'] -
#                   1.96*points_policy_w_age[, 'std_err_M'])*100000, 0)
y_lims <- c(-30, 5)
plot(points_policy_w_age[, 'policy_M']*100000,
     xlab = 'Demerit-Point Balance',
     ylab = 'Policy Effect',
     ylim = y_lims,
     type = 'l',
     col = 'black',
     lwd = 3)
# Append standard error bands.
lines((points_policy_w_age[, 'policy_M'] +
         1.96*points_policy_w_age[, 'std_err_M'])*100000,
      col = 'black',
      lty = 'dashed',
      lwd = 3)
lines((points_policy_w_age[, 'policy_M'] -
         1.96*points_policy_w_age[, 'std_err_M'])*100000,
      col = 'black',
      lty = 'dashed',
      lwd = 3)

# Append plots for females.
lines((points_policy_w_age[, 'policy_F'])*100000,
      col = 'grey',
      lty = 'solid',
      lwd = 3)
lines((points_policy_w_age[, 'policy_F'] +
         1.96*points_policy_w_age[, 'std_err_F'])*100000,
      col = 'grey',
      lty = 'dashed',
      lwd = 3)
lines((points_policy_w_age[, 'policy_F'] -
         1.96*points_policy_w_age[, 'std_err_F'])*100000,
      col = 'grey',
      lty = 'dashed',
      lwd = 3)

# Plot line to denote zero effect.
abline(h = 0,
       col = 'black',
       lty = 'solid',
       lwd = 1)



################################################################################
# Plot graphs together with grey scale
################################################################################


grey_scale <- grey.colors(n = 12, start = 0.0, end = 0.9,
                          gamma = 2.2, rev = FALSE)
col_no_age_M <- grey_scale[1]
col_w_age_M <- grey_scale[6]
col_no_age_F <- grey_scale[3]
col_w_age_F <- grey_scale[8]


#------------------------------------------------------------
# Model without age interactions
#------------------------------------------------------------

fig_file_name <- 'points_fig/points_fig_with_age_int.eps'
fig_file_path <- sprintf('%s/%s', md_dir, fig_file_name)


postscript(fig_file_path)
y_lims <- c(-30, 5)
plot(points_policy[, 'policy_M']*100000,
     xlab = 'Demerit-Point Balance',
     ylab = 'Policy Effect',
     ylim = y_lims,
     type = 'l',
     col = col_no_age_M,
     lwd = 3)
# Append standard error bands.
lines((points_policy[, 'policy_M'] +
         1.96*points_policy[, 'std_err_M'])*100000,
      col = col_no_age_M,
      lty = 'dashed',
      lwd = 3)
lines((points_policy[, 'policy_M'] -
         1.96*points_policy[, 'std_err_M'])*100000,
      col = col_no_age_M,
      lty = 'dashed',
      lwd = 3)

# Append plots for females.
lines((points_policy[, 'policy_F'])*100000,
      col = col_no_age_F,
      lty = 'solid',
      lwd = 3)
lines((points_policy[, 'policy_F'] +
         1.96*points_policy[, 'std_err_F'])*100000,
      col = col_no_age_F,
      lty = 'dashed',
      lwd = 3)
lines((points_policy[, 'policy_F'] -
         1.96*points_policy[, 'std_err_F'])*100000,
      col = col_no_age_F,
      lty = 'dashed',
      lwd = 3)

# Plot line to denote zero effect.
abline(h = 0,
       col = 'black',
       lty = 'solid',
       lwd = 1)


#------------------------------------------------------------
# Model with age interactions
#------------------------------------------------------------

lines(points_policy_w_age[, 'policy_M']*100000,
     col = col_w_age_M,
     lty = 'dotdash',
     lwd = 3)
# Append standard error bands.
lines((points_policy_w_age[, 'policy_M'] +
         1.96*points_policy_w_age[, 'std_err_M'])*100000,
      col = col_w_age_M,
      lty = 'dotted',
      lwd = 3)
lines((points_policy_w_age[, 'policy_M'] -
         1.96*points_policy_w_age[, 'std_err_M'])*100000,
      col = col_w_age_M,
      lty = 'dotted',
      lwd = 3)

# Append plots for females.
lines((points_policy_w_age[, 'policy_F'])*100000,
      col = 'grey',
      lty = 'dotdash',
      lwd = 3)
lines((points_policy_w_age[, 'policy_F'] +
         1.96*points_policy_w_age[, 'std_err_F'])*100000,
      col = 'grey',
      lty = 'dotted',
      lwd = 3)
lines((points_policy_w_age[, 'policy_F'] -
         1.96*points_policy_w_age[, 'std_err_F'])*100000,
      col = 'grey',
      lty = 'dotted',
      lwd = 3)
dev.off()


################################################################################
# End
################################################################################
