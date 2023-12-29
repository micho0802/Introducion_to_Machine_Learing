#Classification

#Upload data
data <- read.csv("C:\\Users\\hoang\\OneDrive\\Desktop\\data.csv")

#Drop N/A column
data <- subset(data, select = -c(X))

#Convert character data to numerical
data$diagnosis <- as.numeric(data$diagnosis == "M") # 'M' is mapped to 1 and 'B' is mapped to 0

#Train val test split
spec <- c(train = .7, validate = .15, test = .15)

g <- sample(cut(
  seq(nrow(data)), 
  nrow(data)*cumsum(c(0,spec)),
  labels = names(spec)
))

res <- split(data, g)

#I - Logisitcs Regression and LDA

#Logistics
#Fit the model
set.seed(42)

log.model <- glm(diagnosis ~ ., data = res$train, family = binomial(link = 'logit'))

print(summary(log.model))

#Predict on validation set
fitted.probabilities <- predict(log.model, newdata = res$validate, type = 'response' )
fitted.result <- ifelse(fitted.probabilities > 0.5, 1, 0)

#Validation error
misClassificError <- mean(fitted.result != res$validate$diagnosis)
cat('\nAccuracy of validation set: ', 1 - misClassificError, '\n')

#Missclassification table
table(res$validate$diagnosis, fitted.probabilities > 0.5)

#Predict on test set
fitted.probabilities <- predict(log.model, newdata = res$test, type = 'response' )
fitted.result <- ifelse(fitted.probabilities > 0.5, 1, 0)

#Validation error
misClassificError <- mean(fitted.result != res$test$diagnosis)
cat('\nAccuracy of test set: ', 1 - misClassificError, '\n')


#LDA
library(MASS)

set.seed(42)
#Fit the model
lda.model <- lda(diagnosis ~., data = res$train)
print(lda.model)

#Plot
plot(lda.model)

#Predict on validation set
lda.pred <- predict(lda.model, res$validate)
# Extract predicted classes
predicted_classes <- lda.pred$class

# Display the confusion matrix (if you have actual classes for comparison)
confusion_matrix <- table(res$validate$diagnosis, predicted_classes)
cat("Confusion Matrix:\n", confusion_matrix, "\n")

# Calculate classification accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Classification Accuracy: ", accuracy, "\n")


#Predict on test set
lda.pred <- predict(lda.model, res$test)
# Extract predicted classes
predicted_classes <- lda.pred$class

# Display the confusion matrix (if you have actual classes for comparison)
confusion_matrix <- table(res$test$diagnosis, predicted_classes)
cat("Confusion Matrix:\n", confusion_matrix, "\n")

# Calculate classification accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Classification Accuracy: ", accuracy, "\n")


#II - Classification Trees
library(rpart)
set.seed(42)

#Fit the model
tree.model <- rpart(diagnosis ~., method = 'class', data = res$train)
print(summary(tree.model))

#Plot
plot(tree.model, uniform = TRUE, main = 'Main Title')
text(tree.model, use.n = TRUE, all = TRUE)



#III - Support Vector Classifier
library(e1071)
set.seed(42)

#Fit the model
svm.model <- svm(diagnosis ~ ., data = data, kernel = 'linear', cost = 10, scale = TRUE)

#Plot
plot(svm.model, data)

#Hyper parameter tunning
tune.out <- tune(svm, diagnosis ~ ., data = data, kernel = "linear",
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

print(summary(tune.out))

bestmod <- tune.out$best.model
print(summary(bestmod))