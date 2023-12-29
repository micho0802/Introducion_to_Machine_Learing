library(caTools)
library(ggplot2)
library(splines)
#Regression problem

#Upload the data - Heart disease UCI
data <- read.csv("C:\\Users\\hoang\\OneDrive\\Desktop\\winequality-red.csv")

#Check N/A value
print(any(is.na(data)))

#Check structure of the data
print(str(data))

#Train validation test split
set.seed(42)
spec <- c(train = .7, validate = .15, test = .15)

g <- sample(cut(
  seq(nrow(data)), 
  nrow(data)*cumsum(c(0,spec)),
  labels = names(spec)
))

res <- split(data, g)

cat('\n Shape of train: \n', dim(res$train), '\n Shape of validation: \n', dim(res$validate), '\n Shape of test: \n', dim(res$test))

# Multiple Linear Regression
#Fit the model
na_cub_spline.lm_model <- lm(quality ~ ns(chlorides, df = 4) + ns(sulphates, df = 4) + ns(alcohol, df = 4), data = res$train)
print(summary(na_cub_spline.lm_model))

#Plot residual check normality
#Grab residuals
residual <- residuals(na_cub_spline.lm_model)

#Convert to dataframe and plot it
residual <- as.data.frame(residual)
print('Head of the residual :')
print(head(residual))

#ggplot
ggplot(residual, aes(residual)) + geom_histogram(fill = 'blue', alpha = 0.5)

#Predict on the validation set
val.prediction <- predict(na_cub_spline.lm_model, res$validate)

#Result
val.results <- cbind(val.prediction, res$test$quality)
colnames(val.results) <- c('pred', 'valid')
val.results <- as.data.frame(val.results)

#Error
val_mse <- mean((val.results$valid - val.results$pred)^2)
cat('\nValidation MSE: ', val_mse, '\n')

#R^2 ajdusted
cat('\nR^2 adjusted ', summary(na_cub_spline.lm_model)$adj.r.squared, '\n')

#Predict on the test set
cat('\n******** Prediction the test set **********')
test_predictions <- predict(na_cub_spline.lm_model, res$test)

#Error
test_mse <- mean((test_predictions - res$test$quality)^2)
cat('\n Test MSE: ', test_mse, '\n')
