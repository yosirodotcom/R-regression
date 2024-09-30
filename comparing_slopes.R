# Install and load necessary packages
install.packages(c("lmtest", "car", "emmeans"))
library(lmtest)
library(car)
library(emmeans)

# Simulate data
x1 <- c(0.2,0.25,0.25,0.3,0.4,0.5,0.5)
y1 <- c(30,26,40,35,54,56,65)
x2 <- c(0.2,0.25,0.3,0.4,0.4,0.5)
y2 <- c(23,24,42,49,55,70)

x1 <- c(3,4,5,6,8,9,10,11,12,14,15,16,17)
y1 <- c(1.4,1.5,2.2,2.4,3.1,3.2,3.2,3.9,4.1,4.7,4.5,5.2,5)
x2 <- c(3,4,5,7,7,8,9,10,12,14)
y2 <- c(2.1,2.5,3.1,3,3.8,3.2,4.3,3.9,4.4,4.8)

x1 <- c(10.4,10.8,11.1,10.2,10.6,11.3,11.6,11.4,10.7,10.9,11.3,11.4,11.5,11.7)
y1 <- c(7.4,7.6,7.9,7.2,8.1,8.5,8.7,8.3,7.5,7.4,7.9,8.9,8.8,9.7)
x2 <- c(10.7,10.5,10.9,11.7,11.2,11.6,11.9,12.1,12.4,12.5,11.4)
y2 <- c(7.9,8.1,8.5,8.9,8.8,9.1,9.4,9.8,9.9,10.1,10.5)
x3 <- c(11.2,11.7,10.5,10.9,10.3,11.5,11.8,11.4,12.1,12.7,12.5,12.3)
y3 <- c(8.1,9.2,9.3,9.8,8.9,9.5,9.7,9.3,9.9,10.3,10.2,10.5)


data <- data.frame(
  x = c(x1, x2, x3),
  y = c(y1, y2, y3),
  group = factor(c(rep("A", length(x1)), rep("B", length(x2)), rep("C", length(x3))))
)

# Fit the full model
model_full <- lm(y ~ x * group, data = data)

# for 2 linear models
# Compare slopes between groups
emm <- emtrends(model_full, ~ group, var = "x")

# View results
pairs(emm)

model_reduced <- lm(y ~ x + group, data = data)
lrtest(model_reduced, model_full)

# Bonus: Visualize the results
library(ggplot2)
ggplot(data, aes(x = x, y = y, color = group)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Comparison of Two Regression Lines")


# for n-liner models
# Test if all slopes are equal (i.e., test if all interaction terms are zero)
linearHypothesis(model_full, c( "x:groupB = 0", "x:groupC = 0"))

# Test if intercepts are the same across groups
linearHypothesis(model_full, c("groupB = 0", "groupC = 0"))

summary(model_full)
