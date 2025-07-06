library(copula)
library(ggplot2)

# Function to estimate the integral using Gaussian copula
compute_integral_gaussian <- function(n, rho, samples = 200) {
  if (n == 1) {
    u <- runif(samples)
    return(mean(u^101 / u))
  }
  
  gaussian_cop <- normalCopula(param = rho, dim = n)
  u_samples <- rCopula(samples, gaussian_cop)
  
  sum_x <- rowSums(u_samples)
  sum_x_power <- rowSums(u_samples^101)
  
  estimated_value <- mean(sum_x_power / sum_x)
  
  return(estimated_value)
}

# Define number of variables and correlation values
n_vals <- 1:200
correlation_values <- c(0.001, 0.0001, 0)

# Compute results for different correlations
results_list <- lapply(correlation_values, function(rho) {
  integral_results <- sapply(n_vals, function(n) compute_integral_gaussian(n, rho, samples = 200))
  data.frame(n = n_vals, integral = integral_results, correlation = factor(rho))
})

results_df <- do.call(rbind, results_list)

# Theoretical value of the integral
theoretical_value <- 1 / 51  # Derived from theoretical analysis

# Create separate plots for each correlation
for (rho in correlation_values) {
  plot_data <- subset(results_df, correlation == rho)
  p <- ggplot(plot_data, aes(x = n, y = integral)) +
    geom_line(color = "blue") +
    geom_point(size = 0.5) +
    geom_hline(yintercept = theoretical_value, color = "red", linetype = "dashed") +
    labs(title = paste("Estimation of the Integral for Correlation", rho),
         x = "Number of Variables (n)",
         y = "Estimated Integral Value") +
    theme_minimal()
  print(p)
}

