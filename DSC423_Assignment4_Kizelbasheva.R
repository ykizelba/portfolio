# Yuliya Kizelbasheva
# Assignment 4
# DSC 423

library(ggplot2)
library(dplyr)

# Problem 1
#a
df = read.csv("churn_train.csv", header=TRUE)
head(df)

df$CHURN = as.numeric(df$CHURN)
df$AGE = as.numeric(df$AGE)
df$PCT_CHNG_BILL_AMT = as.numeric(df$PCT_CHNG_BILL_AMT)
ggplot(df, aes(x = CHURN, y = AGE, fill = CHURN, group=CHURN)) +
  geom_boxplot() +
  labs(title = "AGE by CHURN", x = "CHURN", y = "AGE")
ggplot(df, aes(x = CHURN, y =PCT_CHNG_BILL_AMT, fill = CHURN, group=CHURN)) +
  geom_boxplot() +
  labs(title = "PCT_CHNG_BILL_AMT by CHURN", x = "CHURN", y = "PCT_CHNG_BILL_AMT")

#b

df$EDUCATION = as.numeric(df$EDUCATION)
df$LAST_PRICE_PLAN_CHNG_DAY_CNT = as.numeric(df$LAST_PRICE_PLAN_CHNG_DAY_CNT)
df$TOT_ACTV_SRV_CNT = as.numeric(df$TOT_ACTV_SRV_CNT)
df$PCT_CHNG_IB_SMS_CNT = as.numeric(df$PCT_CHNG_IB_SMS_CNT)
df$COMPLAINT = as.numeric(df$COMPLAINT)
# df$GENDER <- ifelse(df$GENDER == 1, "M", "F")
# df$GENDER = as.numeric(df$GENDER)

head(df)
model <- lm(CHURN ~ ., data = df)
summary(model)

df = na.omit(df)

significant <- step(model, direction = "backward", alpha = 0.05)
significant

better_model <- lm(CHURN ~ LAST_PRICE_PLAN_CHNG_DAY_CNT + TOT_ACTV_SRV_CNT + 
            AGE + PCT_CHNG_IB_SMS_CNT + PCT_CHNG_BILL_AMT + COMPLAINT, 
          data = df)
summary(better_model)


#c

stud_resid <- rstudent(better_model)
pred_vals <- predict(better_model)
plot(pred_vals, stud_resid,
     xlab = "Predicted Values",
     ylab = "Studentized Residuals")

resid <- residuals(better_model)
qqnorm(resid)
qqline(resid)

odds <- exp(coef(significant))
conf <- confint(significant)

results <- data.frame(Odds_Ratio = odds, CI_Lower = conf[, 1], CI_Upper = conf[, 2])
results


#d

myydata <- data.frame(AGE = 43, LAST_PRICE_PLAN_CHNG_DAY_CNT=0, TOT_ACTV_SRV_CN=4, PCT_CHNG_IB_SMS_CNT= 1.04, PCT_CHNG_BILL_AMT= 1.19, COMPLAINT =1)  
new_churn <- predict(better_model, data = myydata, interval = "confidence", level = 0.95)

cat("Predicted CHURN is ", new_churn[1], "\n")
cat("95% CI is [", new_churn[3], "-", new_churn[2], "]\n")


#e

churn_test <- read.csv("churn_test.csv")  # Replace with the actual filename

churn_test$EDUCATION = as.numeric(churn_test$EDUCATION)
churn_test$LAST_PRICE_PLAN_CHNG_DAY_CNT = as.numeric(churn_test$LAST_PRICE_PLAN_CHNG_DAY_CNT)
churn_test$TOT_ACTV_SRV_CNT = as.numeric(churn_test$TOT_ACTV_SRV_CNT)
churn_test$PCT_CHNG_IB_SMS_CNT = as.numeric(churn_test$PCT_CHNG_IB_SMS_CNT)
churn_test$COMPLAINT = as.numeric(churn_test$COMPLAINT)

pred_prob_test <- predict(better_model, newdata = churn_test, type = "response")
pred_prob_test

thresholds <- seq(0.1, 1, by = 0.05)

best_threshold <- 0
best_metric <- 0

classify = function(plist, t){
  yclass=c()
  for (prob in plist)
    if (prob < t) yclass=c(yclass, 0)
  else yclass=c(yclass, 1)
  yclass}

compare=function(plist, yvar){
  i=1
  tp=0
  tn=0
  fp=0
  fn=0
  for (pred in plist) {
    if (pred==yvar[i])
      if (yvar[i]==1)
        tp=tp+1
    else
      tn=tn+1
    else
      if (yvar[i]==1)
        fn=fn+1
      else
        fp=fp+1
      i=i+1}
  matrix(c(tp, fp, fn, tn), nrow=2, ncol=2, dimnames=list(c("Actual 1", "Actual 0"),c("Predict 1", "Predict 0")))
}

sensitivity= function(m){
  (m[1,1])/(m[1,1]+m[2,1])
}

sens <- sensitivity(confusion_matrix_test)
sens
accuracy=function(m){ 
  (m[1,1]+m[2,2])/sum(m)} 
acc <- accuracy(confusion_matrix_test)
acc

specificity =function(m){ m[2,2]/(m[2,2]+m[1,2])}
spec <- specificity(confusion_matrix_test)
spec

recall = function(m){m[1,1]/(m[1,1]+m[2,1])}
rec <- recall(confusion_matrix_test)
rec

precision =function(m){ m[1,1]/(m[1,1]+m[1,2])}
precis = precision(confusion_matrix_test)
precis

pred_churn_test <- classify(pred_prob_test, best_threshold)
confusion_matrix_test <- compare(pred_churn_test, churn_test$CHURN)

cat("Optimal Threshold:", best_threshold, "\n")
cat("Classification Matrix: ")
print(confusion_matrix_test)

# Problem 2

df2 = read.table("energytemp.txt", header=TRUE)
head(df2)

#a

df2$energy = as.numeric(df2$energy)
df2$temp = as.numeric(df2$temp)
head(df2)
plot(df2$temp, df2$energy, main = "Scatterplot of ENERGY vs TEMPD", 
     xlab = "TEMPD", ylab = "ENERGY")
cor(df2)

#b

df2$temp2 <- df2$temp^2
df2$temp3 <- df2$temp^3

model <- lm(energy ~ temp + temp2 + temp3, data = df2)
summary(model)

#c

#Yes

#d

pred_values <- predict(model)
residuals <- residuals(model)

plot(pred_values, residuals, main = "Residuals vs Predicted Values", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 1, col = "black")

plot(df2$energy, residuals, main = "Residuals vs Energy", xlab = "Energy", ylab = "Residuals")
abline(h = 1, col = "black")

qqnorm(residuals)
qqline(residuals)


#e

model <- lm(energy ~ temp + temp2 + temp3, data = df2)

#f
new <- data.frame(tempd=c(10), tempd2=c(100), tempd3=c(1000))

new_balance <- predict(model, data = new, interval = "confidence", level = 0.95)

cat("Predicted Energy is ", new_balance[1], "\n")
cat("95% CI is [", new_balance[3], " - ", new_balance[2], "]\n")


