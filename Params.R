# Total years to run for
years_to_carbon_plateau <-30 # Define years until final carbon
years_to_crediting <- 30 # Years until carbon calculation

### Carbon Accumulation
# Carbon after 30 years (Mg C/Ha)
baseline_C <- 11.1 # Muthuri et al., 2023 - Scattered Trees on Farms
woodlot_C <- 25.5 # Kimaro et al., 2011 - 5 year yields in Morogoro ranged between 11-25.5
                  # Alternative reference is 28.5 Mg C ha-1 Nyadzi et al. (2003)
agroforestry_C <- 31.9 # Muthuri et al., 2023 - Below and aboveground estimate of carbon synthesised from several studies 
biodiversity_C <- 35.6 # Mwakalukwa et al., 2024 - Combined estimate for AGB of trees and shrubs in Miombo woodland area.

# Ratio woodlot to agroforestry
woodlot_percent <- 0.2
biodiversity_percent <- 0

### Planting Parameters
# Area Under Curve Planting Scheme
mu <- 3  # Peak year
sigma <- 5  # Spread of planting
target_total_area <-16000  # Total area to be planted
replanting_interval <- 5 # How often each hectare gets replanted
replanting_intensity <- 0.1 # What percentage of it gets replanted

# Chapman-Richards
k_agroforestry <- 0.2 # Steepness factor
k_woodlot <- 1 # Steepness factor
p_woodlot <- 1
p <- 2 # Shape factor
rotation_period <- 5


