
# Model Selection

sum(is.na(df_new))
df_new <- na.omit(df_new)

X =model.matrix(price~.,df_new)[,-1]
nrow(X)
y =df_new$price
length(y)
set.seed (10) 
n = nrow(df_new)
n
a<-sample(n ,round(0.6*n) , rep=FALSE) 
a
y_tr <- y[a]
X_tr <- X[a,]
y_ts <- y[-a]
X_ts <- X[-a,]

## Fit linear model
fit_model = lm(price~., data =df_new[a,] )
summary(fit_model)
pred_model = predict(fit_model, newdata=df_new[-a,]) # prediction for test data
mse_model = mean((pred_model-y_ts)^2) # mse for test data
sqrt(mse_model)


## OLS Regression with CV

## OLS Regression with cross validation
## Drop sqft_above
exclude_ <-c("sqft_basement")
df_new_ols<-df_new[ , !(names(df_new)%in%exclude_)]
n = nrow(df_new)
k = 20
RMSE_kcv_ols = numeric(k)

## Create k equally size folds
folds <- cut(1:n,breaks=k,labels=FALSE)
folds

## Perform a k-fold cross validation
for(i in 1:k)
{
  ## Find the indices for test data
  test_index = which(folds==i)
  
  ## Obtain training/test data
  test_data = df_new_ols[test_index, ]
  training_data = df_new_ols[-test_index, ]
  
  ## Training matrix
  X_tr = model.matrix(price~.,training_data)[, -1]
  y_tr = training_data$price
  
  ## Test matrix
  X_ts = model.matrix(price~.,test_data)[, -1]
  y_ts = test_data$price
  
  fit_ols = lm(price~., data =df_new[a,] )
  
  ## Obtain RMSE on the 'test' data
  pred_ols = predict(fit_ols,newx = X_ts)
  resid_ols = y_ts - pred_ols 
  RMSE_kcv_ols[i] = sqrt(sum(resid_ols^2)/length(y_ts)) 
}

## All the values for RMSE
RMSE_kcv_ols

## Mean of RMSE 
mean(RMSE_kcv_ols)

## Lasso Regression

library(glmnet)

## LASSO with cross validation 
n = nrow(df_new)
k = 20
RMSE_kcv_lasso = numeric(k)

## Create k equally size folds
folds <- cut(1:n,breaks=k,labels=FALSE)
folds

## Perform a k-fold cross validation
for(i in 1:k)
{
  ## Find the indices for test data
  test_index = which(folds==i)
  
  ## Obtain training/test data
  test_data = df_new[test_index, ]
  training_data = df_new[-test_index, ]
  
  ## Training matrix
  X_tr = model.matrix(price~.,training_data)[, -1]
  y_tr = training_data$price
  
  ## Test matrix
  X_ts = model.matrix(price~.,test_data)[, -1]
  y_ts = test_data$price
  
  fit_lasso_cv = cv.glmnet(X_tr, y_tr, alpha = 1)
  bestlam = fit_lasso_cv$lambda.min
  fit_lasso_best = glmnet(X_tr, y_tr, alpha = 1, lambda = bestlam)
  
  ## Obtain RMSE on the 'test' data
  pred_lasso = predict(fit_lasso_best, s = bestlam, newx = X_ts)
  resid_lasso = y_ts - pred_lasso 
  RMSE_kcv_lasso[i] = sqrt(sum(resid_lasso^2)/length(y_ts)) 
}

## All the values for RMSE
RMSE_kcv_lasso

## Mean of RMSE 
mean(RMSE_kcv_lasso)


## Ridge Regression

## Ridge regression with cross validation
n = nrow(df_new)
k = 20
RMSE_kcv_ridge = numeric(k)

## Create k equally size folds
folds <- cut(1:n,breaks=k,labels=FALSE)
folds

## Perform a k-fold cross validation
for(i in 1:k)
{
  ## Find the indices for test data
  test_index = which(folds==i)
  
  ## Obtain training/test data
  test_data = df_new[test_index, ]
  training_data = df_new[-test_index, ]
  
  ## Training matrix
  X_tr = model.matrix(price~.,training_data)[, -1]
  y_tr = training_data$price
  
  ## Test matrix
  X_ts = model.matrix(price~.,test_data)[, -1]
  y_ts = test_data$price
  
  fit_ridge_cv = cv.glmnet(X_tr, y_tr, alpha = 0)
  bestlam = fit_ridge_cv$lambda.min
  fit_ridge_best = glmnet(X_tr, y_tr, alpha = 0, lambda = bestlam)
  
  
  ## Obtain RMSE on the 'test' data
  pred_ridge = predict(fit_ridge_best, s = bestlam, newx = X_ts)
  resid_ridge = y_ts - pred_ridge 
  RMSE_kcv_ridge[i] = sqrt(sum(resid_ridge^2)/length(y_ts)) 
}

## All the values for RMSE
RMSE_kcv_ridge

## Mean of RMSE 
mean(RMSE_kcv_ridge)


coef(fit_ridge_best)
coef(fit_lasso_best)


## Fit Lasso Regression with orginal variables without any feature engineering
data <-data[data$bathrooms > 0,]
data <-data[data$bedrooms > 0,]
data_new<-data[-c(1:2)]#drop zipcode...
n = nrow(data_new)
k = 2
RMSE_kcv_lasso = numeric(k)

## Create k equally size folds
folds <- cut(1:n,breaks=k,labels=FALSE)
folds

## Perform a k-fold cross validation
for(i in 1:k)
{
  ## Find the indices for test data
  test_index = which(folds==i)
  
  ## Obtain training/test data
  test_data = data_new[test_index, ]
  training_data = data_new[-test_index, ]
  
  ## Training matrix
  X_tr = model.matrix(price~.,training_data)[, -1]
  y_tr = training_data$price
  
  ## Test matrix
  X_ts = model.matrix(price~.,test_data)[, -1]
  y_ts = test_data$price
  
  fit_lasso_cv = cv.glmnet(X_tr, y_tr, alpha = 1)
  bestlam = fit_lasso_cv$lambda.min
  fit_lasso_best = glmnet(X_tr, y_tr, alpha = 1, lambda = bestlam)
  
  ## Obtain RMSE on the 'test' data
  pred_lasso = predict(fit_lasso_best, s = bestlam, newx = X_ts)
  resid_lasso = y_ts - pred_lasso 
  RMSE_kcv_lasso[i] = sqrt(sum(resid_lasso^2)/length(y_ts)) 
}

## All the values for RMSE
RMSE_kcv_lasso

## Mean of RMSE 
mean(RMSE_kcv_lasso)


coef(fit_lasso_best)
