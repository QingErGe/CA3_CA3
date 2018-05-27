data <- read.csv("H:/LYIT COMPUTER SECURITY/大数据/data sinces/统计作业/death of male and female.csv")
data
plot(data$Male, type = "o", col = "red")
par(new = TRUE)
plot(data$Female, type = "o", col = "green")

plot(data$Male, data$Female, type = "o", col = "red", xlab = "Male", ylab = "Female")
title(main = "Famle and Male death number each year", col.main = 'orange')
#scatter plot
scatter.smooth(x = data$Male, y = data$Female, maintainer = "male and female")

#density plot
library(e1071)
par(mfrow = c(1, 2))
plot(density(data$Male), main = "Density Plot :Male",
ylab = "Frequency",
sub = paste("Skewness:", round(e1071::skewness(data$Male), 2)))
polygon(density(data$Male), col = "red")

plot(density(data$Female), main = "Density Plot :Female",
ylab = "Frequency",
sub = paste("Skewness:", round(e1071::skewness(data$Female), 2)))
polygon(density(data$Female), col = "green")

#Correlation Test
cor(data$Male, data$Female)
#linear Model
linearMod <- lm(Female ~ Male, data = data)
print(linearMod)
summary(linearMod)
#Polynomial Model
polynomialMod <- lm(Female ~ Male + I(Male ^ 2), data = data)
print(polynomialMod)
summary(polynomialMod)


#Sampling
#no_of_records <- sample(1:nrow(data), 0.8 * nrow(data))
#training_data <- data[no_of_records,]
#testing_data <- data[-no_of_records,]

#Training linear Model
lr_model <- lm(Female ~ Male, data = training_data)
lm_predicted <- predict(lr_model, testing_data)
lm_predicted
lm_actual_preds <- data.frame(cbind(actuals = testing_data$Female, predicted = lm_predicted))
lm_actual_preds

#Training Polynomial Model(Second Order)
pl_model <- lm(Female ~ Male + I(Male ^ 2), data = training_data)
pl_predicted <- predict(pl_model, testing_data)
pl_predicted
pl_actual_preds <- data.frame(cbind(actuals = testing_data$Female, predicted = pl_predicted))
pl_actual_preds



# Lets validate, Compare and Decide which model fits our data
# AIC
AIC(linearMod)
AIC(polynomialMod)
# BIC
BIC(linearMod)
BIC(polynomialMod)
# Correlation Accuracy
lm_correlation_accuracy <- cor(lm_actual_preds)
lm_correlation_accuracy

pl_correlation_accuracy <- cor(pl_actual_preds)
pl_correlation_accuracy

#min_max accuracy
lm_min_max_accuracy <- mean(apply(lm_actual_preds, 1, min) / apply(lm_actual_preds, 1, max))
lm_min_max_accuracy

pl_min_max_accuracy <- mean(apply(pl_actual_preds, 1, min) / apply(pl_actual_preds, 1, max))
pl_min_max_accuracy
# Mape
lm_mape <- mean(abs(lm_actual_preds$predicted - lm_actual_preds$actuals) / lm_actual_preds$actuals)
lm_mape
pl_mape <- mean(abs(pl_actual_preds$predicted - pl_actual_preds$actuals) / pl_actual_preds$actuals)
pl_mape
# Summary
summary(lr_model)
summary(pl_model)