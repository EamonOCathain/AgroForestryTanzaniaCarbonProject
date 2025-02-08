growth_model <- function(
    # Total years to run for
  years_to_carbon_plateau = NULL,  
  years_to_crediting = NULL,            
  
  # Carbon Accumulation (Mg C/Ha)
  baseline_C = NULL,       
  woodlot_C = NULL,         
  agroforestry_C = NULL, 
  biodiversity_C = NULL, 
  
  # Ratio woodlot to agroforestry
  woodlot_percent = NULL,
  biodiversity_percent = NULL,
  
  # Planting Parameters
  mu = NULL,                 
  sigma = NULL,           
  target_total_area = NULL,  
  replanting_interval = NULL,  
  replanting_intensity = NULL, 
  
  # Chapman-Richards growth model parameters
  k_agroforestry = NULL,   
  k_woodlot = NULL,             
  p_woodlot = NULL,             
  p = NULL,                             
  rotation_period = NULL  
) {
  
  # List of parameter names
  param_names <- c(
    "years_to_carbon_plateau", "years_to_crediting", "baseline_C", "woodlot_C",
    "agroforestry_C", "biodiversity_C", "woodlot_percent", "biodiversity_percent",
    "mu", "sigma", "target_total_area", "replanting_interval", "replanting_intensity",
    "k_agroforestry", "k_woodlot", "p_woodlot", "p", "rotation_period"
  )
  
  # Assign values from global environment if NULL
  for (param in param_names) {
    if (is.null(get(param))) {
      assign(param, get(param, envir = .GlobalEnv), envir = environment())
    }
  }
  
  # The Model 
  
  # Load packages
  library(tidyverse)
  
  # Define the years until the maximum carbon value is reached for the agroforestry system after planting.
  year <- c(0:years_to_carbon_plateau) 
  
  # Define the years of the crediting period, i.e the time over which the carbon storage should be calculated.
  years_to_run <- c(1:years_to_crediting)
  
  
  ### Calculate the belowground biomass of biodiversity strips

  biodiversity_C <- biodiversity_C * 1.24 # Carins et al., 1994 (used in Henry et al.)
  woodlot_C <- woodlot_C * 1.24
  
  
  ### Subtract baseline

  # Subtract baseline
  woodlot_C <- woodlot_C - baseline_C
  biodiversity_C <- biodiversity_C - baseline_C
  agroforestry_C <- agroforestry_C - baseline_C
  
  
  ### Calculate Agroforestry Area
  
  agroforestry_percent <- 1 - woodlot_percent - biodiversity_percent
  
  
  
  ### Calculate Area of Each Land-Use to be Planted
  
  woodlot_area <- target_total_area*woodlot_percent
  agroforestry_area <- target_total_area*agroforestry_percent
  biodiversity_area <- target_total_area*biodiversity_percent
  
  
  ## Planting Rate Model

# Define the Gaussian Planting Model
planting_agroforestry <- function(t, a, mu, sigma) {
  a * exp(-((t - mu)^2) / (2 * sigma^2))
}

# Function to calculate required 'a' for target total area
calculate_a <- function(target_total, year_range, mu, sigma) {
  # Initial guess for 'a'
  a_guess <- 1
  
  # Define function to minimize (difference between target and calculated total area)
  f <- function(a) {
    year_area_agroforestry <- sapply(year_range, planting_agroforestry, a = a, mu = mu, sigma = sigma)
    return(abs(sum(year_area_agroforestry) - target_total))
  }
  
  # Optimize 'a'
  opt_result <- optimize(f, interval = c(0, 1e6))
  return(opt_result$minimum)
}

# Calculate 'a' dynamically
a <- calculate_a(agroforestry_area, year, mu, sigma)

# Compute area planted each year
year_area_agroforestry <- sapply(year, planting_agroforestry, a = a, mu = mu, sigma = sigma)

# Calculate total area planted
total_area <- sum(year_area_agroforestry)

### Replanting Agroforestry

replanting_function <- function(values, years, interval, intensity) {
  if (length(values) != length(years)) {
    stop("Values and years must have the same length")
  }
  
  # Assume the first year as the baseline
  base_year <- min(years)
  replanting_area <- rep(0, length(years))  # Initialize output vector with zeros
  
  for (i in seq_along(years)) {
    year <- years[i]
    
    # Skip years before the first interval
    if (year < base_year + interval) {
      next
    }
    
    # Look for original values only, applying decay at fixed intervals
    for (j in seq_along(years)) {
      original_year <- years[j]
      
      if (original_year + interval <= year && (year - original_year) %% interval == 0) {
        replanting_area[i] <- replanting_area[i] + (intensity * values[j])
      }
    }
  }
  
  return(replanting_area)
}

replanting_area <- replanting_function(year_area_agroforestry, year, replanting_interval, replanting_intensity)

# Add the replanting area to the final agroforestry area
year_area_agroforestry_with_replanting <- year_area_agroforestry + replanting_area


### Fixed Area Planting biodiversity

# Define the Gaussian Planting Model
planting_biodiversity <- function(t, a, mu, sigma) {
  a * exp(-((t - mu)^2) / (2 * sigma^2))
}

# Function to calculate required 'a' for target total area
calculate_a <- function(target_total, year_range, mu, sigma) {
  # Initial guess for 'a'
  a_guess <- 1
  
  # Define function to minimize (difference between target and calculated total area)
  f <- function(a) {
    year_area_biodiversity <- sapply(year_range, planting_biodiversity, a = a, mu = mu, sigma = sigma)
    return(abs(sum(year_area_biodiversity) - target_total))
  }
  
  # Optimize 'a'
  opt_result <- optimize(f, interval = c(0, 1e6))
  return(opt_result$minimum)
}

# Calculate 'a' dynamically
a <- calculate_a(biodiversity_area, year, mu, sigma)

# Compute area planted each year
year_area_biodiversity <- sapply(year, planting_biodiversity, a = a, mu = mu, sigma = sigma)

if (biodiversity_percent ==0){
  year_area_biodiversity <- 0*year_area_biodiversity
}

# Calculate total area planted
total_area <- sum(year_area_biodiversity)


### Fixed Area Planting woodlots

# Define the Gaussian Planting Model
planting_woodlot <- function(t, a, mu, sigma) {
  a * exp(-((t - mu)^2) / (2 * sigma^2))
}

# Function to calculate required 'a' for target total area
calculate_a <- function(target_total, year_range, mu, sigma) {
  # Initial guess for 'a'
  a_guess <- 1
  
  # Define function to minimize (difference between target and calculated total area)
  f <- function(a) {
    year_area_woodlot <- sapply(year_range, planting_woodlot, a = a, mu = mu, sigma = sigma)
    return(abs(sum(year_area_woodlot) - target_total))
  }
  
  # Optimize 'a'
  opt_result <- optimize(f, interval = c(0, 1e6))
  return(opt_result$minimum)
}

# Calculate 'a' dynamically
a <- calculate_a(woodlot_area, year, mu, sigma)

# Compute area planted each year
year_area_woodlot <- sapply(year, planting_woodlot, a = a, mu = mu, sigma = sigma)

# Calculate total area planted
total_area <- sum(year_area_woodlot)

#Calculate total area planted
total_area_each_year <- year_area_agroforestry_with_replanting + year_area_biodiversity + year_area_woodlot
cumulative_area_agro <- cumsum(year_area_agroforestry_with_replanting)
cumulative_area_biodiversity <- cumsum(year_area_biodiversity)
cumulative_area_woodlot <- cumsum(year_area_woodlot)
cumulative_area <- cumsum(total_area_each_year)

## Carbon Accumulation Model


# Define the function to solve for p
solve_p <- function(p, x_target, y_target, max, k) {
  return(max * (1 - exp(-k * x_target))^p - y_target)
}

# Find the correct p value
adjusted_p <- uniroot(solve_p, interval = c(0.1, 10), x_target = 6, y_target = 9.5424, 
                       max = agroforestry_C, k = k_agroforestry)$root

# Define Chapman-Richards function with auto-adjusted p
chapman_richards <- function(x, max, k, p) {
  return(max * (1 - exp(-k * x))^p)
}

# Calculate carbon accumulation using the optimized p value
year_C_agro <- sapply(year, function(i) chapman_richards(i, max = agroforestry_C, 
                                                          k = k_agroforestry, p = adjusted_p))

### Model of Carbon Accumulation in Woodlots 


# Calculate carbon accumulation
year_C_woodlot_5 <- c()
year_C_woodlot_10 <- c()

#Define function to find the max C at which the reference level is met after 5 years
chapman_richards_woodlot <- function(x, max = woodlot_C, k, p, rotation_period) {
  # Solve for max carbon storage (C_max)
  C_max <- woodlot_C / ((1 - exp(-k * rotation_period))^p)

  # Standard Chapman-Richards growth function
  base_growth <- C_max * (1 - exp(-k * x))^p

  return(base_growth)
}
 
# First rotation
for (i in 1:5) {
  year_C_woodlot_5 <- append(year_C_woodlot_5, chapman_richards_woodlot(i, max = woodlot_C, k = k_woodlot, p = p_woodlot, rotation_period))
}

# Second rotation
for (i in 1.5:6) {
  year_C_woodlot_10 <- append(year_C_woodlot_10, chapman_richards_woodlot(i, max = woodlot_C, k = k_woodlot, p = p_woodlot, rotation_period))
}

# Add the rotations together
year_C_woodlot_25 <- rep(year_C_woodlot_10, (length(year))/5)
year_C_woodlot <- append(year_C_woodlot_5, year_C_woodlot_25)
year_C_woodlot <- year_C_woodlot[1:31]

# This averages the standing biomass across all years
avg_woodlot_C <- mean(year_C_woodlot)
avg_woodlot_C_vect <- rep(avg_woodlot_C, length(years_to_run))

# This takes the total productivity (i.e sums the peaks)
max_value <- max(year_C_woodlot) # Find the maximum value
max_count <- sum(year_C_woodlot == max_value) # Count occurrences of the maximum value
# Sum the peaks of all the second rotations and then add the peak of the first rotatian and the final value at 30 years
total_woodlot_C <- woodlot_C*max_count + year_C_woodlot[31] + max(year_C_woodlot_5) 
total_woodlot_C_vect <- rep(total_woodlot_C, length(years_to_run))



### Model of C Accumulation in the Biodiversity

chapman_richards <- function(x, max, k, p) {
  return(max * (1 - exp(-k * x))^p)
}

# Calculate carbon accumulation
year_C_biodi <- c()

for (i in year) {
  year_C_biodi <- append(year_C_biodi, chapman_richards(i, max = biodiversity_C, k = k_agroforestry, p = p))
}





## Calculate the Total Carbon Storage for Each Land Use Type and Overall

# Matrix for the results
total_yearly_C_mat <- matrix(0, nrow = 0, ncol = years_to_crediting + 1)

# Function to calculate yearly carbon for each land use
yearly_C_function <- function(area, carbon) {
  counter <- 0  
  data_matrix <- matrix(0, nrow = years_to_crediting + 1, ncol = years_to_crediting + 1)
  
  # Go through the year_area_agroforestry_with_plantings and calculate yearly storage
  for (i in area[1:(years_to_crediting + 1)]) {
    # Multiply year_area_agroforestry_with_planting by growth curve, but only for the number of years left until the crediting period 
    C_vect <- i * carbon[1:(years_to_crediting + 1 - counter)]
    
    # Add the vector to the matrix at the appropriate point, with 0s before it.
    data_matrix[counter + 1, (counter + 1):(years_to_crediting + 1)] <- C_vect
    
    # Update counter
    counter <- counter + 1
  }
  return(data_matrix)
}

yearly_C_matrix_agro <- yearly_C_function(year_area_agroforestry_with_replanting, year_C_agro)
yearly_C_matrix_bio <- yearly_C_function(year_area_biodiversity, year_C_biodi)
yearly_C_matrix_woodlot <- yearly_C_function(year_area_woodlot, year_C_woodlot)

total_C_vect_agro <- colSums(yearly_C_matrix_agro)
total_C_vect_bio <- colSums(yearly_C_matrix_bio)
total_C_vect_woodlot <- colSums(yearly_C_matrix_woodlot)

final_matrix <- rbind(total_C_vect_agro, total_C_vect_bio, total_C_vect_woodlot)
final_matrix <- rbind(final_matrix, colSums(final_matrix))
final_df <- as.data.frame(final_matrix)
rownames(final_df) <- c("Total C Agroforestry", "Total C Biodiversity", "Total C Woodlot",  "Total C")

# Return the total carbon at the end of 30 years
return((final_df[4,31]/1000000)*3.67)  
}