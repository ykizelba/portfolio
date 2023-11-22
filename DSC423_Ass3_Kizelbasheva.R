# Yuliya Kizelbasheva
# Assignment 3
# DSC 423

library(ggplot2)
library(dplyr)

# Problem 1
#a
mydata = read.csv("college.csv", header=TRUE)
head(mydata)
df <- mydata[, -c(1, 2)]
head(df)
df <- as.data.frame(lapply(df, as.numeric))
df <- cbind(df, mydata$school, mydata$Private)

colnames(df)[15:16] <- c("school", "Private")
head(df)

hist(df$Grad.Rate, main = "Grad.Rate Distribution", xlab = "Grad.Rate")

library(moments)   

skewness <- skewness(df$Grad.Rate)
skewness

#no skewness, it's symmetric, so the distribution is normal

#b
ggplot(data = df, aes(x = Accept.pct, y = Grad.Rate)) +
  geom_point() +
  ggtitle("Grad.Rate vs Accept.pct")
ggplot(data = df, aes(x = F.Undergrad, y = Grad.Rate)) +
  geom_point() +
  ggtitle("Grad.Rate vs F.Undergrad")
ggplot(data = df, aes(x = P.Undergrad, y = Grad.Rate)) +
  geom_point() +
  ggtitle("Grad.Rate vs P.Undergrad")
ggplot(data = df, aes(x = Outstate, y = Grad.Rate)) +
  geom_point() +
  ggtitle("Grad.Rate vs Outstate")
ggplot(data = df, aes(x = Room.Board, y = Grad.Rate)) +
  geom_point() +
  ggtitle("Grad.Rate vs Room.Board")
ggplot(data = df, aes(x = Books, y = Grad.Rate)) +
  geom_point() +
  ggtitle("Grad.Rate vs Books")
ggplot(data = df, aes(x = Personal, y = Grad.Rate)) +
  geom_point() +
  ggtitle("Grad.Rate vs Personal")
ggplot(data = df, aes(x = PhD, y = Grad.Rate)) +
  geom_point() +
  ggtitle("Grad.Rate vs PhD")
ggplot(data = df, aes(x = Terminal, y = Grad.Rate)) +
  geom_point() +
  ggtitle("Grad.Rate vs Terminal")
ggplot(data = df, aes(x = S.F.Ratio, y = Grad.Rate)) +
  geom_point() +
  ggtitle("Grad.Rate vs S.F.Ratio")
ggplot(data = df, aes(x = perc.alumni, y = Grad.Rate)) +
  geom_point() +
  ggtitle("Grad.Rate vs perc.alumni")
ggplot(data = df, aes(x = Expend, y = Grad.Rate)) +
  geom_point() +
  ggtitle("Grad.Rate vs Expend")

cor <- cor(df)
cor

#c
ggplot(df, aes(x = Private, y = Grad.Rate, fill = Private)) +
  geom_boxplot() +
  labs(title = "Graduation Rates by University Type", x = "University Type", y = "Graduation Rate")

df$Elite10 = as.numeric(df$Elite10)
df$Elite10 <- ifelse(df$Elite10 == 1, "Yes", "No")

ggplot(df, aes(x = Elite10, y = Grad.Rate, fill = Elite10)) +
  geom_boxplot() +
  labs(title = "Graduation Rates by Status", x = "Status", y = "Graduation Rate")

#d
df <- mydata[, -c(1,2)]
df <- as.data.frame(apply(df, 2, as.numeric))
head(df)
model <- lm(Grad.Rate ~ ., data = df)
summary(model)

#e
library(car)
vif_result <- car::vif(model)
vif_result

#f
#option 1
back_model <- step(model, direction = "backward")
summary(back_model)

#option 2
library(MASS)
step_model <- stepAIC(model, direction = "both", trace = FALSE)
summary(step_model)

#g
new_model <- lm(Grad.Rate ~ Accept.pct+Terminal + Elite10+F.Undergrad +P.Undergrad +Room.Board + Personal +Outstate+Expend+PhD+ perc.alumni, data = df)
summary(new_model)

#h
stud_resid <- rstudent(new_model)

pred_vals <- predict(new_model)
plot(pred_vals, stud_resid,
     xlab = "Predicted Values",
     ylab = "Studentized Residuals")

#i
resid <- residuals(new_model)
# normal probability plot
qqnorm(resid)
qqline(resid)

#j
cd <- cooks.distance(new_model)
influential_points <- which(cd > 4 / nobs(new_model))
influential_points

#k
summary_new_model <- summary(new_model)
rsquared_result<-summary_new_model$r.squared
rsquared_result

# Problem 2
#a
full = lm (Grad.Rate ~ (Elite10 + Accept.pct + Outstate + perc.alumni + Expend)^2, data=df)
summary(full)

#b
back_full <- step(full, direction = "backward")
summary(back_full)

step_full <- stepAIC(full, direction = "both", trace = FALSE)
summary(step_full)

new_full <- lm(Grad.Rate ~ Elite10 + Accept.pct + Outstate + perc.alumni + Expend + Elite10:Accept.pct + Elite10:Outstate + Elite10:Expend + Outstate:Expend, data = df)
summary(new_full)

#c
summary(new_full)

# optional

#a
select.df <- sample(1:nrow(df), 0.75*nrow(df))
train.df <- df[select.df,] #Selecting 75% of the data
test.df <- df[-select.df,] #Selecting 25% (remaining)
y_pred <- predict.glm(full, test.df)
y_obs<-test.df[,"Grad.Rate"]

# Compute mean absolute percentage error
mape_m1<-mean(abs((y_obs - y_pred)/y_obs))*100
mape_m1

#b
select.df <- sample(1:nrow(df), 0.75*nrow(df))
train.df <- df[select.df,] #Selecting 75% of the data
test.df <- df[-select.df,] #Selecting 25% (remaining)
y_pred <- predict.glm(new_full, test.df)
y_obs<-test.df[,"Grad.Rate"]

# Compute mean percentage absolute error
mape_m2<-mean(abs((y_obs - y_pred)/y_obs))*100
mape_m2
