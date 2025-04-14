# Welcome
This repository was made as part of an MSc assignment at AgroParisTech as part of the 'Global Forestry' master's course, during which we acted as consultants for the ecological investment fund 'Livelihoods' and designed an agroforestry intervention in the Morogoro region of Tanzania. This repository models the carbon drawdown overtime with successive planting and using empirical growth functions from the literature. A one-at-a-time sensitivity analysis was used to provide information about the factors which contribute the most to the uncertainty in the carbon storage potential. The aim of quantifying the carbon storage was to provide estimates which could be sued to claim private carbon credits.

# Using the scripts
Open all scripts in an R-studio project. Set the working directory in Growth_Model.rmd and OAT_sensitivity_test.R. Run the Growth_model.rmd to check the model output for a single set of parameters.

Parameter values can be changed in Params.R. Run the OAT_sensitivity_test.R to see the effect of a 50% reduction/increase in each of the parameters on the final carbon storage. 

Enjoy!
