
# Tests with calculations of the AUROC.

library(data.table)

# Calculations using rank.

# # Direct calculation:
# 
# auroc_dt <- saaq_data[, c('events', 'fit', 'num')]
# 
# summary(auroc_dt)
# 
# # Take the cumulative sum of ranked predictions, weighted by events.
# # n1 =  sum(lr$y) 
# # auroc = data.table(y=lr$y , p=lr$fitted.values)[, rp := frank(p)][y==1, (sum(rp)-.5*n1*(n1+1))/(n1*(n-n1))]  ## area under ROC curve = Mann-Whitney-Wilcoxon statistic
# 
# num_sum <- auroc_dt[, sum(num)]
# num_1 <- auroc_dt[events == 1, sum(num)]
# auroc_dt[, rank := frank(fit)]
# auroc_dt[events == 1, (sum(rank*num) - num_1*(num_1+1)/2)/(num_1*(num_sum - num_1))]



test_auroc <- data.table(fit = 10 - seq(10), 
                         events = c(1,1,0,1,1,1,0,0,1,0), 
                         num = rep(1, 10))
num_sum <- test_auroc[, sum(num)]
num_1 <- test_auroc[events == 1, sum(num)]
test_auroc[, rank := frank(fit)]
test_auroc[events == 1, (sum(rank*num) - num_1*(num_1+1)/2)/(num_1*(num_sum - num_1))]



# Calculations with a package.
library(PRROC)

# test_auroc[events == 1, (sum(rank*num) - num_1*(num_1+1)/2)/(num_1*(num_sum - num_1))]


# Examples from PRROC library:
# compute ROC curve and area under curve
x <- 10 - seq(10)
y <- c(1,1,0,1,1,1,0,0,1,0)
roc <- roc.curve(x, y, curve = TRUE )
# plot curve
plot(roc)

# Weighted example with repeated observations.


test_auroc <- data.table(x1 = c(10,10,9,9,9,
                               8,7,7,7,7,
                               6,5,5,5,4,
                               3,2,2,1,1), 
                         x2 = c(1,1,2,2,2,
                               3,4,4,4,4,
                               5,6,6,6,7,
                               8,9,9,10,10),
                         y = c(1,1,0,1,1,
                               0,1,0,1,1,
                               1,0,1,1,0,
                               1,0,0,1,0), 
                         num = rep(1,20))

test_auroc_agg_1 <- test_auroc[, .N, by = c('x1', 'y')]
test_auroc_agg_2 <- test_auroc[, .N, by = c('x2', 'y')]

x <- test_auroc[, x1]
y <- test_auroc[, y]

roc <- roc.curve(scores.class0 = x, weights.class0 = y, curve = TRUE )
roc
plot(roc)
# In reverse order:
# Area under curve:
#   0.6770833 
# In the correct order:
x <- test_auroc[, x2]
roc <- roc.curve(scores.class0 = x, weights.class0 = y, curve = TRUE )
roc
# Area under curve:
#   0.3229167 


# Now calculate with separate scores for class zero and one.
# Scores in order increasing with class 1.
x_0 <- test_auroc[y == 0, x1]
x_1 <- test_auroc[y == 1, x1]
roc <- roc.curve(scores.class0 = x_0, scores.class1 = x_1, curve = TRUE )
roc
plot(roc)

# Scores in order decreasing with class 1.
x_0 <- test_auroc[y == 0, x2]
x_1 <- test_auroc[y == 1, x2]
roc <- roc.curve(scores.class0 = x_0, scores.class1 = x_1, curve = TRUE )
roc
plot(roc)
# It seems to work but the order is reversed. 






# Now test with aggregated data. 
# Scores in order increasing with class 1.
x_0 <- test_auroc_agg_1[y == 0, x1]
x_1 <- test_auroc_agg_1[y == 1, x1]
w_0 <- test_auroc_agg_1[y == 0, N]
w_1 <- test_auroc_agg_1[y == 1, N]
roc <- roc.curve(scores.class0 = x_0, scores.class1 = x_1, 
                 weights.class0 = w_0, weights.class1 = w_1, curve = TRUE )
roc
plot(roc)

# Scores in order decreasing with class 1.
x_0 <- test_auroc_agg_2[y == 0, x2]
x_1 <- test_auroc_agg_2[y == 1, x2]
w_0 <- test_auroc_agg_2[y == 0, N]
w_1 <- test_auroc_agg_2[y == 1, N]
roc <- roc.curve(scores.class0 = x_0, scores.class1 = x_1, 
                 weights.class0 = w_0, weights.class1 = w_1, curve = TRUE )
roc
plot(roc)

# Use negative scores if order is wrong.
# Scores in order increasing with class 1.
x_0 <- test_auroc_agg_1[y == 0, -x1]
x_1 <- test_auroc_agg_1[y == 1, -x1]
w_0 <- test_auroc_agg_1[y == 0, N]
w_1 <- test_auroc_agg_1[y == 1, N]
roc <- roc.curve(scores.class0 = x_0, scores.class1 = x_1, 
                 weights.class0 = w_0, weights.class1 = w_1, curve = TRUE )
roc
plot(roc)




# Tests pass:
# ROC matches for aggregated and original data

