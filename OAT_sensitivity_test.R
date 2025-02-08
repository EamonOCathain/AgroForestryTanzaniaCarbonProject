# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Clear environment
rm(list = ls())

# Import parameters and function
source("~/Desktop/Coding/R_Studio/Tanzania_Project/Tanzania/Params.R")
source("~/Desktop/Coding Languages/R_Studio/Tanzania Project/Tanzania/New_growth_function.R")  # Fix path if needed

# Define the default parameter values
default_params <- list(
  baseline_C = baseline_C,
  woodlot_C = woodlot_C,
  agroforestry_C = agroforestry_C,
  biodiversity_C = biodiversity_C,
  woodlot_percent = woodlot_percent,
  biodiversity_percent = biodiversity_percent,
  mu = mu,
  sigma = sigma,
  target_total_area = target_total_area,
  replanting_interval = replanting_interval,
  replanting_intensity = replanting_intensity,
  k_agroforestry = k_agroforestry,
  k_woodlot = k_woodlot,
  p_woodlot = p_woodlot,
  p = p
)

# Define which parameters to use for sensitivity analysis
# To include/exclude parameters, just comment them in or out below:
sensitivity_parameters <- c(
  "baseline_C", 
   "woodlot_C",
   "agroforestry_C",
   "biodiversity_C",
   "woodlot_percent",
   "biodiversity_percent",
   "mu",
   "sigma",
   "target_total_area",
   "replanting_interval",
   "replanting_intensity",
   "k_agroforestry",
   "k_woodlot",
   "p_woodlot",
   "p"
)

# Filter parameters to only include uncommented ones
filtered_params <- default_params[names(default_params) %in% sensitivity_parameters]

# Function to perform sensitivity analysis
sensitivity_analysis <- function(params, param_name, slices = 20) {
  # Create a sequence of values for the parameter
  param_values <- seq(params[[param_name]] * 0.5, params[[param_name]] * 1.5, length.out = slices)
  
  # Initialize a data frame to store results
  results <- data.frame(param_value = numeric(), output = numeric())
  
  # Loop through each value of the parameter
  for (value in param_values) {
    # Update the parameter value
    params[[param_name]] <- value
    
    # Run the model with the updated parameter
    output <- tryCatch(
      {
        do.call(growth_model, params)
      },
      error = function(e) {
        print(paste("Error for", param_name, "value:", value, "-", e$message))
        return(NA)
      }
    )
    
    # Store the result only if it is not NA
    if (!is.na(output)) {
      results <- rbind(results, data.frame(param_value = value, output = output))
    }
  }
  
  return(results)
}

# Perform sensitivity analysis for each parameter
sensitivity_results <- list()

for (param_name in names(filtered_params)) {
  print(paste("Running sensitivity analysis for:", param_name))
  sensitivity_results[[param_name]] <- sensitivity_analysis(filtered_params, param_name)
}

# Compute the default model output (when all parameters are at default values)
default_output <- do.call(growth_model, default_params)

# Prepare data for plotting
sensitivity_plot_data <- data.frame(
  parameter = character(),
  min_value = numeric(),
  max_value = numeric()
)

for (param_name in names(sensitivity_results)) {
  if (!is.null(sensitivity_results[[param_name]]) && nrow(sensitivity_results[[param_name]]) > 0) {
    min_output <- min(sensitivity_results[[param_name]]$output, na.rm = TRUE)
    max_output <- max(sensitivity_results[[param_name]]$output, na.rm = TRUE)
    
    # Append to dataframe
    sensitivity_plot_data <- rbind(sensitivity_plot_data, data.frame(
      parameter = param_name,
      min_value = min_output,
      max_value = max_output
    ))
  }
}

# Ensure the parameter order is meaningful (sorted by min_value for better readability)
sensitivity_plot_data <- sensitivity_plot_data %>%
  arrange(min_value)

# Plot horizontal bar chart
p <- ggplot(sensitivity_plot_data, aes(y = reorder(parameter, min_value), xmin = min_value, xmax = max_value)) +
  geom_errorbarh(height = 0.4, color = "blue", size = 1.2) +  # Horizontal bars
  geom_point(aes(x = min_value), color = "blue", size = 3) +  # Min values
  geom_point(aes(x = max_value), color = "blue", size = 3) +  # Max values
  geom_vline(xintercept = default_output, linetype = "dotted", color = "red", size = 1.2) +  # Default value line
  labs(
    title = "Sensitivity Analysis: Range of Model Outcomes",
    x = "Model Output",
    y = "Parameter"
  ) +
  theme_minimal()

# Print the plot
print(p)