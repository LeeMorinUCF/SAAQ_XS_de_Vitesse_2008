################################################################################
#
# Investigation of SAAQ Excessive Speeding Laws
#
# Construction of a series of numbers of tickets awarded by the
# number of points per ticket.
# Datasets hold observations for sets of sequential id codes.
# Aggregate data by age and sex categories.
# Join with non-event data from total licensees on SAAQ webpage.
# Output an aggregate dataset suitable for regression analysis.
#
#
#
# Lee Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# June 19, 2021
#
################################################################################
#
################################################################################

################################################################################
# Clearing Workspace and Declaring Packages
################################################################################

# Clear workspace, if running interactively.
rm(list=ls(all=TRUE))

# Load data table package for quick selection on seq.
library(data.table)

# Load rpart package for partitioning with regression trees.
library(rpart)

# Load PRROC package for calculating area under the ROC curve.
library(PRROC)


################################################################################
# Set parameters for file IO
################################################################################

# Set working directory, if running interactively.
drive_path <- 'C:/Users/le279259/OneDrive - University of Central Florida/Documents'
git_path <- 'Research/SAAQ/SAAQspeeding/SAAQ_XS_de_Vitesse_2008'
wd_path <- sprintf('%s/%s',drive_path, git_path)
setwd(wd_path)

# The original data are stored in 'Data/'.
data_in_path <- 'Data'

# The data of counts of licensed drivers are also stored in 'Data/'.
data_out_path <- 'Data'

# Set name of output file for training, testing and estimation samples.
train_file_name <- 'saaq_train.csv'
test_file_name <- 'saaq_test.csv'


# Set name of output file for full dataset.
# out_file_name <- 'saaq_out.csv'


set.seed(42)


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


# saaq_data[date >= sample_beg & date <= sample_end,
#           sum(as.numeric(num)), by = points][order(points)]
saaq_data[, sum(as.numeric(num)), by = points][order(points)]


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

# Last, but not least, define the indicator for the policy change.
saaq_data[, policy := date >= april_fools_date]



summary(saaq_data[month == '06', date])
summary(saaq_data[policy == TRUE, date])
summary(saaq_data[policy == FALSE, date])


# saaq_data[date >= sample_beg & date <= sample_end,
#           sum(as.numeric(num)), by = points][order(points)]



################################################################################
# Fit a series of models
################################################################################

#-------------------------------------------------------------------------------
# Define list of models
#-------------------------------------------------------------------------------


# Predict for any ticket value.
pts_target <- 'all'
# All violations combined.
saaq_data[, events := points > 0]





#-------------------------------------------------------------------------------
# First version with all variables
#-------------------------------------------------------------------------------

# Fit a series of models.

# Predict for any ticket value.
# pts_target <- 'all'
# All violations combined.
saaq_data[, events := points > 0]


colnames(saaq_data)


full_var_list = c('curr_pts_grp', 'age_grp', 'sex', 'weekday', 'month', 'policy')
num_vars <- length(full_var_list)
model_list <- data.frame()
model_list[1, 'm_1'] <- c('curr_pts_grp')
model_list[1:2, 'm_2'] <- c('curr_pts_grp', 'age_grp')
model_list[1:3, 'm_3'] <- c('curr_pts_grp', 'age_grp', 'sex')
model_list[1:4, 'm_4'] <- c('curr_pts_grp', 'age_grp', 'sex', 'weekday')
model_list[1:5, 'm_5'] <- c('curr_pts_grp', 'age_grp', 'sex', 'weekday', 'month')
model_list[1:6, 'm_6'] <- c('curr_pts_grp', 'age_grp', 'sex', 'weekday', 'month', 'policy')

# Now add interactions.
num_models <- ncol(model_list)
for (i in 1:num_vars) {
  for (j in 1:(num_vars-1)) {
    
    var_name_i <- full_var_list[i]
    var_name_j <- full_var_list[j]
    int_var_name <- sprintf('%s*%s', var_name_i, var_name_j)
    
    num_models <- num_models + 1
    model_name <- sprintf('m_%d', num_models)
    
    model_list[1:(num_vars+1), model_name] <- c(full_var_list, int_var_name)
    
  }
}


# model_list[1:6, 'm7'] <- c('month', 'weekday', 'age_grp', 'curr_pts_grp',
#                            'policy', 'policy*age_grp')
# model_list[1:8, 'm8'] <- c('month', 'weekday', 'sex', 'age_grp', 'curr_pts_grp',
#                            'policy', 'policy*sex', 'policy*age_grp')





# Initialize matrix for AUC statistics. 
auc_mat <- data.frame(model_name = rep(NA, ncol(model_list)),
                      in_sample = rep(NA, ncol(model_list)), 
                      out_sample = rep(NA, ncol(model_list)), 
                      full_sample = rep(NA, ncol(model_list)))


model_num <- 1
for (model_num in 1:ncol(model_list)) {
  
  auc_mat[model_num, 'model_name'] <- sprintf('m_%d', model_num)
  
  
  var_list <- model_list[!is.na(model_list[, model_num]), model_num]
  
  # Summarize data before estimation.
  # summary(saaq_data[sample == 'train', c('events', var_list), with = FALSE])
  
  # Define candidate variables.
  fmla_str <- sprintf('events ~ %s',
                      paste(var_list, collapse = " + "))
  fmla <- as.formula(fmla_str)
  
  
  print(sprintf('Estimating model: %s', fmla_str))
  
  # # Fit regression model on training sample. 
  # lm_spec <- lm(formula = fmla, 
  #               data = saaq_data[sample == 'train', ],
  #               # data = saaq_data, # Full sample.
  #               # data = saaq_data[sex == 'M'], # Full sample of male drivers.
  #               # data = saaq_data[sex == 'F'], # Full sample of male drivers.
  #               weights = num, 
  #               model = FALSE, x = FALSE, y = FALSE, qr = FALSE)
  
  # summary(lm_spec)
  # print(summary(lm_spec))
  
  result <- tryCatch({
    
    # Fit regression model on training sample. 
    lm_spec <- lm(formula = fmla, 
                  data = saaq_data[sample == 'train', ],
                  # data = saaq_data, # Full sample.
                  # data = saaq_data[sex == 'M'], # Full sample of male drivers.
                  # data = saaq_data[sex == 'F'], # Full sample of male drivers.
                  weights = num) # , 
                  # model = FALSE, x = FALSE, y = FALSE, qr = FALSE)
    
    # If it reaches this message, there was no error.
    print("No error or warning from lm().")
    
  }, warning = function(w) {
    
    print(paste("WARNING from lm(): ", w))
    
  }, error = function(e) {
    
    print(paste("ERROR from lm(): ", e))
    
  }, finally = {
    # 
    # # Calculate predicted values to evaluate models.
    # saaq_data[, fit := predict(lm_spec, newdata = saaq_data)]
    # 
    # 
    # # Calculate AUROC on full sample.
    # roc <- roc.curve(scores.class0 = saaq_data[events == 0, -fit], 
    #                  scores.class1 = saaq_data[events == 1, -fit], 
    #                  weights.class0 = saaq_data[events == 0, num], 
    #                  weights.class1 = saaq_data[events == 1, num], 
    #                  curve = FALSE )
    # # print(roc)
    # # plot(roc)
    # auc_mat[model_num, 'full_sample'] <- roc$auc
    # 
    # # Calculate AUROC on training sample.
    # roc <- roc.curve(scores.class0 = saaq_data[sample == 'train' & events == 0, -fit], 
    #                  scores.class1 = saaq_data[sample == 'train' & events == 1, -fit], 
    #                  weights.class0 = saaq_data[sample == 'train' & events == 0, num], 
    #                  weights.class1 = saaq_data[sample == 'train' & events == 1, num], 
    #                  curve = FALSE )
    # 
    # auc_mat[model_num, 'in_sample'] <- roc$auc
    # 
    # # Calculate AUROC on testing sample.
    # roc <- roc.curve(scores.class0 = saaq_data[sample == 'test' & events == 0, -fit], 
    #                  scores.class1 = saaq_data[sample == 'test' & events == 1, -fit], 
    #                  weights.class0 = saaq_data[sample == 'test' & events == 0, num], 
    #                  weights.class1 = saaq_data[sample == 'test' & events == 1, num], 
    #                  curve = FALSE )
    # 
    # auc_mat[model_num, 'out_sample'] <- roc$auc
    
    # print("No error or warning from lm().")
  })
  
  
  # Calculate AUROC only if model fit was successful. 
  if (result == "No error or warning from lm().") {
    
    # Calculate predicted values to evaluate models.
    saaq_data[, fit := predict(lm_spec, newdata = saaq_data)]
    
    
    # Calculate AUROC on full sample.
    roc <- roc.curve(scores.class0 = saaq_data[events == 0, -fit],
                     scores.class1 = saaq_data[events == 1, -fit],
                     weights.class0 = saaq_data[events == 0, num],
                     weights.class1 = saaq_data[events == 1, num],
                     curve = FALSE )
    # print(roc)
    # plot(roc)
    auc_mat[model_num, 'full_sample'] <- roc$auc
    
    # Calculate AUROC on training sample.
    roc <- roc.curve(scores.class0 = saaq_data[sample == 'train' & events == 0, -fit],
                     scores.class1 = saaq_data[sample == 'train' & events == 1, -fit],
                     weights.class0 = saaq_data[sample == 'train' & events == 0, num],
                     weights.class1 = saaq_data[sample == 'train' & events == 1, num],
                     curve = FALSE )
    
    auc_mat[model_num, 'in_sample'] <- roc$auc
    
    # Calculate AUROC on testing sample.
    roc <- roc.curve(scores.class0 = saaq_data[sample == 'test' & events == 0, -fit],
                     scores.class1 = saaq_data[sample == 'test' & events == 1, -fit],
                     weights.class0 = saaq_data[sample == 'test' & events == 0, num],
                     weights.class1 = saaq_data[sample == 'test' & events == 1, num],
                     curve = FALSE )
    
    auc_mat[model_num, 'out_sample'] <- roc$auc
    
  }
  
  print(auc_mat)
}

print(auc_mat)


# Calculate AUROC in-sample and out-of-sample.





# Output table of results.



# summary(saaq_data)





#-------------------------------------------------------------------------------
# Calculate AUROC
#-------------------------------------------------------------------------------



#--------------------------------------------------------------------------------
# Fit regression tree on residuals
#--------------------------------------------------------------------------------

saaq_data[, resid := events - fit]

# 
# 
# 
# # Define candidate variables.
# fmla <- as.formula(sprintf('resid ~ %s',
#                            paste(var_list, collapse = " + ")))
# 
# # Set controls for regression tree. 
# rpart_ctrl <- rpart.control(minsplit = 20, 
#                             # minbucket = round(minsplit/3), 
#                             # cp = 0.01, 
#                             cp = 10^(-6), 
#                             # maxcompete = 4, 
#                             maxcompete = 0, 
#                             # maxsurrogate = 5, 
#                             maxsurrogate = 0, 
#                             usesurrogate = 2, 
#                             xval = 10,
#                             surrogatestyle = 0, 
#                             maxdepth = 30)
# 
# # Fit a regression tree.
# rpart_tree <- rpart(formula = fmla, 
#                     data = saaq_data[sample == 'train', ], weights = num, 
#                     method = 'anova', 
#                     control = rpart_ctrl)
# 
# summary(rpart_tree)
# 




#-------------------------------------------------------------------------------
# Separate trees for males and females
#-------------------------------------------------------------------------------









################################################################################
# Fit a series of models with higher-order interactions
################################################################################

# Fit a series of models.

# Calculate AUROC in-sample and out-of-sample.

# Output table of results.



################################################################################
# End
################################################################################

