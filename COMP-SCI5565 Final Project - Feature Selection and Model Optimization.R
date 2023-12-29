#Feature Selection/ Model Optimization

library(leaps)
#Load the data
data <- read.csv("C:\\Users\\hoang\\OneDrive\\Desktop\\data.csv")

#Check what column have N/A value
colSums(is.na(data)) #Print this to check

#Remove N/A
data <- subset(data, select = -c(X))

#Part I - Subsets selection
#Foward
regfit.fwd <- regsubsets(radius_mean ~ . , data = data, nvmax = 11, method = 'forward')
print(summary(regfit.fwd))

#Backward
regfit.bwd <- regsubsets(radius_mean ~ ., data = data, nvmax = 11, method = "backward")
summary(regfit.bwd)

#Part II
#Foward Features
reg.summaryfwd <- summary(regfit.fwd)
par(mfrow = c(1, 2))
plot(reg.summaryfwd $rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summaryfwd $adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

#Backward Features
reg.summarybwd <- summary(regfit.bwd)
par(mfrow = c(1, 2))
plot(reg.summarybwd $rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summarybwd $adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")


#Part III
#PCA and PLR

library(pls)
set.seed(42)

#Fit the model
pcr.fit <- pcr(radius_mean ~ ., data = data, scale = TRUE, validataion = 'CV')
print(summary(pcr.fit))

validationplot(pcr.fit, val.type = "MSEP")

#Train val test split
set.seed(42)
spec <- c(train = .7, validate = .15, test = .15)

g <- sample(cut(
  seq(nrow(data)), 
  nrow(data)*cumsum(c(0,spec)),
  labels = names(spec)
))

res <- split(data, g)

#Fit the model
pcr.fit <- pcr(radius_mean ~ ., data = res$train, scale = TRUE, validataion = 'CV')
validationplot(pcr.fit, val.type = "MSEP")

#Predict the model on validation set
pcr.pred <- predict(pcr.fit, res$validate, ncomp = 5)
val_mse <- mean((pcr.pred - res$validate$radius_mean)^2)
cat('\nValidation MSE: ', val_mse, '\n')


pcr.fit <- pcr(radius_mean ~ ., data = data, scale = TRUE, ncomp = 5)
summary(pcr.fit)

#PCA

data <- subset(data, select = -c(diagnosis))
#Standardize the variable
standardize_data <- scale(data)
#Correlation matrix
cor_matrix <- cor(standardize_data)
#PCA
pca_result <- princomp(cor_matrix)
#Print the summary
print(summary(pca_result))

#Scree plot
screeplot(pca_result, type = "lines", main = "Scree Plot", cex.axis = 1.2, cex.lab = 1.2)


loadings <- pca_result$loadings

# Display loadings for the selected components
selected_components <- 1:6  # Adjust as needed
loadings_selected <- loadings[, selected_components]

# Print loadings
print(loadings_selected)


