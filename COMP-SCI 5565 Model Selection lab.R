library('ISLR')
library('leaps')

###
# Subset Selection Methods
###

regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 12, method = 'forward')
print(summary(regfit.fwd))

regfit.bwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 12, method = 'backward')
print(summary(regfit.bwd))


###
# Ridge Regression and the Lasso
###

#x <- model.matrix(Salary ~ ., Hitters)[, -1]
x <- model.frame(Salary ~. , Hitters, na.action = NULL)[, -1]

y <- Hitters$Salary

# Ridge Regression
library('glmnet')
library('Matrix')
library('iterators')
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

print(dim(coef(ridge.mod)))
print(ridge.mod$lambda[50])


###
# Partial Least Squares
###

library('PLSR')

set.seed(7750)

train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

pls.fit <- plsr(Salary ~., data = Hitters, subset = train, scale = TRUE, validation = 'CV')
print(summary(pls.fit))


###
# Model Selection
###

# Non - Linear method

library('ISLR2')
attach(Wage)

fit <- lm(wage ~ poly(age, 4), data = Wage)
print(coef(summary(fit)))


fit2 <- lm(wage ~ poly(age, 4, raw = T), data = Wage)
print(coef(summary(fit2)))

fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
print(coef(fit2a))

fit2b <- lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage) #Same as fit2a

agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
#Plot degree 4 poly
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1 ,1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = 0.5, col = 'darkgrey')
title('Degree 4 Polynomial', outer = TRUE)
lines(age.grid, preds$fit, lwd = 2, col = 'blue')
matlines(age.grid, se.bands, lwd = 1, col = 'blue', lty = 3)


preds2 <- predict(fit2, newdata = list(age = age.grid), se = TRUE)
print(max(abs(preds$fit - preds2$fit)))

fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)
print(anova(fit.1, fit.2, fit.3, fit.4, fit.5))

print(coef(summary(fit.5)))
print((-11.983)^2)

fit.1 <- lm(wage ~ education + age, data = Wage)
fit.2 <- lm(wage ~ education + poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ education + poly(age, 3), data = Wage)
print(anova(fit.1, fit.2, fit.3))


fit <- glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)

preds <- predict(fit, newdata = list(age = age.grid), se = T)

pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit,
                        preds$fit - 2 * preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

preds <- predict(fit, newdata = list(age = age.grid),
                 type = "response", se = T)

plot(age, I(wage > 250), xlim = agelims, type = "n",
     ylim = c(0, .2))
points(jitter(age), I((wage > 250) / 5), cex = .5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)


print(table(cut(age, 4)))

fit <- lm(wage ~ cut(age, 4), data = Wage)
print(coef(summary(fit)))

library(splines)
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se = T)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")


print(dim(bs(age, knots = c(25, 40, 60))))
print(dim(bs(age, df = 6)))
print(attr(bs(age, df = 6), "knots"))



fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid),
                 se = T)
plot(age, wage, col = "gray")
lines(age.grid, pred2$fit, col = "red", lwd = 2)


plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age, wage, df = 22)
fit2 <- smooth.spline(age, wage, cv = TRUE)

print(fit2$df)

lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("9 DF", "6.8 DF"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)







