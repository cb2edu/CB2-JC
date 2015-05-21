# Install the package from the book
# install.packages (ISLR)

library(ISLR)
names (Hitters)
dim(Hitters)

# Idea is to find the which variables is most predictive of the salary

# First remove the NAs
Hitters <- na.omit(Hitters)
dim(Hitters)


# Feature selection has to be done only on training data

# Create a training set vector
train <- sample( c(TRUE, FALSE), nrow(Hitters), rep=TRUE)

# Create a test set vector
test <- (!train)

# We will use glmnet package for lasso. It requires a matrix of predictors 
# and the y variable
library(glmnet)
x <-model.matrix (Salary~., Hitters)[,-1]
y <- Hitters$Salary

# Create a set of lamdas
grid <- 10^seq(10, -2, length=100)

# Glmnet function requires alpha paramter. If it is 1 then it's a lasso
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda = grid)

# Cross validation
set.seed(1)
cv.out <- cv.glmnet( x[train,], y[train], alpha=1)
plot(cv.out)

# What is the best lamda
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlam, newx = x[test,])
# Mean sq error
mean((lasso.pred -y[test])^2)

# Feature selection
out <- glmnet(x,y, alpha=1, lambda=grid)
lasso.coef <- predict (out, type="coefficients",s=bestlam)[1:20,]
lasso.coef[lasso.coef > 0]
