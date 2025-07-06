
library(quantreg)  # For LAD regression
library(ggplot2)

data_advertise <- read.csv("C:/Users/Aryan Deo/Downloads/advertising_cleaned.csv", header=TRUE, stringsAsFactors=FALSE)


biv_data <- data.frame(Sales = data_advertise$Sales, TV = data_advertise$TV)

# Least Squares Estimation (LSE)
lse_model <- lm(Sales ~ TV, data = biv_data)

# Least Absolute Deviation (LAD)
lad_model <- rq(Sales ~ TV, tau = 0.5, data = biv_data)

# Compute residuals
lse_resid <- resid(lse_model)
lad_resid <- resid(lad_model)

# Plot histograms
par(mfrow = c(1, 2))  # Arrange plots side by side
hist(lse_resid, main = "LSE Residuals", col = "blue", breaks = 20)
hist(lad_resid, main = "LAD Residuals", col = "red", breaks = 20)

#Plot comparison
par(mfrow = c(1, 1))  # Reset plot layout
ggplot(biv_data, aes(x = TV, y = Sales)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", aes(color = "LSE"), se = FALSE) +
  stat_quantile(quantiles = 0.5, aes(color = "LAD"), formula = y ~ x) +
  scale_color_manual(values = c("LSE" = "red", "LAD" = "green")) +
  labs(title = "Regression Comparison: LSE vs. LAD", 
       x = "TV Advertising", 
       y = "Sales",
       color = "Regression Type") +
  theme_minimal()



summary(lse_model)
summary(lad_model)

# Compute Mean Absolute Error (MAE)
lse_mae <- mean(abs(lse_resid))
lad_mae <- mean(abs(lad_resid))

# Compute Root Mean Squared Error (RMSE)
lse_rmse <- sqrt(mean(lse_resid^2))
lad_rmse <- sqrt(mean(lad_resid^2))

cat("LSE MAE:", lse_mae, "\nLAD MAE:", lad_mae, "\n")
cat("LSE RMSE:", lse_rmse, "\nLAD RMSE:", lad_rmse, "\n")
