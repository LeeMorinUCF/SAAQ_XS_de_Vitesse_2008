
# Tests with calculations of the AUROC.


# Calculations using rank.

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





# create artificial weights
x.weights <- runif( 1000 );
y.weights <- runif( 1000 );
# compute PR curve and area under curve
pr <- pr.curve( x, y, x.weights, y.weights, curve = TRUE );
# plot curve
plot(pr)
# compute ROC curve and area under curve
roc <- roc.curve( x, y, x.weights, y.weights, curve = TRUE );
# plot curve
plot(roc)



