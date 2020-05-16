
library(glmnet)

#Importing data
house_data <- read.csv("kc_house_subset.csv")

#Checking and omitting missing values
sum(is.na(house_data))
house_data = na.omit(house_data)
nrow(house_data)

#LASSO with cross validation 
n = nrow(house_data)
k = 20
RMSE_kcv_lasso = numeric(k)

#Create k equally size folds
folds <- cut(1:n,breaks=k,labels=FALSE)
folds

#Perform a k-fold cross validation
for(i in 1:k)
{
  # Find the indices for test data
  test_index = which(folds==i)
  
  # Obtain training/test data
  test_data = house_data[test_index, ]
  training_data = house_data[-test_index, ]
  
  #Training matrix
  X_tr = model.matrix(price~.,training_data)[, -1]
  y_tr = training_data$price
  
  #Test matrix
  X_ts = model.matrix(price~.,test_data)[, -1]
  y_ts = test_data$price
  
  fit_lasso_cv = cv.glmnet(X_tr, y_tr, alpha = 1)
  bestlam = fit_lasso_cv$lambda.min
  fit_lasso_best = glmnet(X_tr, y_tr, alpha = 1, lambda = bestlam)
  
  # Obtain RMSE on the 'test' data
  pred_lasso = predict(fit_lasso_best, s = bestlam, newx = X_ts)
  resid_lasso = y_ts - pred_lasso 
  RMSE_kcv_lasso[i] = sqrt(sum(resid_lasso^2)/length(y_ts)) 
}

#All the values for RMSE
RMSE_kcv_lasso

# Mean of RMSE 
mean(RMSE_kcv_lasso)

#Ridgewith cross validation
n = nrow(house_data)
k = 20
RMSE_kcv_ridge = numeric(k)

#Create k equally size folds
folds <- cut(1:n,breaks=k,labels=FALSE)
folds

#Perform a k-fold cross validation
for(i in 1:k)
{
  # Find the indices for test data
  test_index = which(folds==i)
  
  # Obtain training/test data
  test_data = house_data[test_index, ]
  training_data = house_data[-test_index, ]
  
  #Training matrix
  X_tr = model.matrix(price~.,training_data)[, -1]
  y_tr = training_data$price
  
  #Test matrix
  X_ts = model.matrix(price~.,test_data)[, -1]
  y_ts = test_data$price
  
  fit_ridge_cv = cv.glmnet(X_tr, y_tr, alpha = 0)
  bestlam = fit_ridge_cv$lambda.min
  fit_ridge_best = glmnet(X_tr, y_tr, alpha = 0, lambda = bestlam)
  
  
  # Obtain RMSE on the 'test' data
  pred_ridge = predict(fit_ridge_best, s = bestlam, newx = X_ts)
  resid_ridge = y_ts - pred_ridge 
  RMSE_kcv_ridge[i] = sqrt(sum(resid_ridge^2)/length(y_ts)) 
}

#All the values for RMSE
RMSE_kcv_ridge

# Mean of RMSE 
mean(RMSE_kcv_ridge)