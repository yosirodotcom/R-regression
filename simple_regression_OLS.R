pacman::p_load(ggplot2, insight, sjPlot, devtools)

# Modelling
## Assign X variable 
X <- c(1,1,2,3,4,4,5,6,6,7)

## Assign Y variable
y <- c(2.1,2.5,3.1,3,3.8,3.2,4.3,3.9,4.4,4.49)

## Create a data frame
df <- data.frame(X, y)

## Fit the linear regression model
model <- lm(y ~ X, data = df)

## Print the coefficients
tab_model(model)


# Predict Y with confidence interval

## Create a new data of X
data_X_new <- 4.5
new_data <- data.frame(X = data_X_new)

## Predict Y with confidence interval and prediction interval
y_mean_pred <- predict(model, new_data, interval = "confidence", level = 0.95, se.fit = TRUE)
y_individual_pred <- predict(model, new_data, interval = "prediction", level = 0.95, se.fit = TRUE)

## Combine the results into a single row
result_table <- data.frame(
  Pred_Y = y_mean_pred$fit[1],  # Predicted value for Y
  CI_Mean_Lower = y_mean_pred$fit[2],  # Lower bound of CI for mean response
  CI_Mean_Upper = y_mean_pred$fit[3],  # Upper bound of CI for mean response
  PI_Individual_Lower = y_individual_pred$fit[2],  # Lower bound of PI for individual response
  PI_Individual_Upper = y_individual_pred$fit[3]   # Upper bound of PI for individual response
)

## Print the table
knitr::kable(result_table, format = "markdown")


# Create plot with CI and PI
## Create new data for prediction
X_new <- data.frame(X = seq(0, max(X)+max(X)*0.1, length.out = 100))

## Confidence intervals for mean response
conf_interval_mean <- predict(model, newdata = X_new, interval = "confidence")

## Prediction intervals for individual response
conf_interval_individual <- predict(model, newdata = X_new, interval = "prediction")

## Combine the predicted values and intervals into a dataframe
X_new$Y_pred <- conf_interval_mean[, "fit"]
X_new$mean_lwr <- conf_interval_mean[, "lwr"]
X_new$mean_upr <- conf_interval_mean[, "upr"]
X_new$pred_lwr <- conf_interval_individual[, "lwr"]
X_new$pred_upr <- conf_interval_individual[, "upr"]

## Original data in a dataframe
data <- data.frame(X = X, Y = y)

## Plot using ggplot2
ggplot() +
  geom_point(data = data, aes(x = X, y = Y), color = "blue", size = 2) +  # Original data points
  geom_line(data = X_new, aes(x = X, y = Y_pred), color = "red", size = 1.2) +  # Regression line
  
  # Confidence interval for mean response (green)
  geom_ribbon(data = X_new, aes(x = X, ymin = mean_lwr, ymax = mean_upr, fill = "Mean Response CI"),
              alpha = 0.3) +
  
  # Prediction interval for individual response (orange)
  geom_ribbon(data = X_new, aes(x = X, ymin = pred_lwr, ymax = pred_upr, fill = "Individual Response PI"),
              alpha = 0.2) +
  
  # Add labels and customize the theme
  labs(title = "Confidence and Prediction Intervals in Simple Linear Regression",
       x = "X", y = "Y", fill = "Interval Type") +
  
  # Customize legend and colors
  scale_fill_manual(name = "Interval Type",
                    values = c("Mean Response CI" = "green", "Individual Response PI" = "orange"),
                    labels = c("95% CI for Mean Response", "95% PI for Individual Response")) +

  # Add the predicted point (X = 7.5) as a large red dot
  geom_point(aes(x = new_data$X, y = result_table$Pred_Y), color = "red", size = 4) +
  
  # Add error bars to represent the confidence interval for the predicted mean
  geom_errorbar(aes(x = new_data$X, ymin = result_table$CI_Mean_Lower, ymax = result_table$CI_Mean_Upper),
                width = 0.2, color = "red", linetype = "dashed", size = 1) +
  
  # Annotate the predicted point with an arrow and text
  annotate("curve", x = data_X_new+2, y = min(y)+max(y)*0.1, xend = data_X_new+max(X)*0.02, yend = result_table$Pred_Y, 
           curvature = 0.2, arrow = arrow(length = unit(0.3, "cm")), color = "darkred", size = 1, linetype = "dashed") +
  annotate("text", x = data_X_new+2, y = min(y)+max(y)*0.07, label = paste0("Predicted Y=", round(result_table$Pred_Y, 2), "\nat X=", data_X_new), color = "darkred", size = 4, hjust = 0) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "top")




