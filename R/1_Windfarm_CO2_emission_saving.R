## 1. Windfarm CO2 emission saving

#' Windfarm_CO2_emissions_saving
#' @return total Windfarm CO2 emissions saving
#' @export
Windfarm_CO2_emissions_saving <- function() {
  ## THIS FUNCTION...

  ## I don't like the parameter extraction, this should be much cleaner
  ## Find a better manipulation, the matix is efficient but the conversion to a data frame is ugly
  ## I need a standard naming procedure for functions/output

  ## Extract parms
  counterfactual_coal <- (core.dat$Counterfactual$Input.data %>% filter(Parameter=="Coal-fired plant emission factor (t CO2 MWh-1)")) %>% select(-Parameter)
  counterfactual_grid <- (core.dat$Counterfactual$Input.data %>% filter(Parameter=="Grid-mix emission factor (t CO2 MWh-1)")) %>% select(-Parameter)
  counterfactual_mix <- (core.dat$Counterfactual$Input.data %>% filter(Parameter=="Fossil fuel-mix emission factor (t CO2 MWh-1)")) %>% select(-Parameter)

  ## Calculate windfarm output
  energy_output <- Windfarm_energy_output()

  ## Calculate emissions savings
  energy_output_mat <- matrix(unlist(energy_output), nrow=3, ncol=3, byrow = T)
  counterfactual_mat <- matrix(unlist(c(counterfactual_coal,counterfactual_grid,counterfactual_mix)), nrow=3, ncol=3, byrow=T)
  savings <- as.data.frame(energy_output_mat * counterfactual_mat)
  colnames(savings) <- c("Exp", "Min", "Max")
  savings <- cbind(data.frame(Counterfactual=c("Coal", "Grid", "Mix")), savings)

  return(savings)
}

#' Windfarm_energy_output
#' @return total Windfarm energy output
#' @export
Windfarm_energy_output <- function() {
  ## THIS FUNCTION...

  # Extract parms
  power_rating_turbines <- (core.dat$Windfarm$Input.data %>% filter(Parameter=="Power rating of turbines (turbine capacity) (MW)")) %>% select(-Parameter)
  no_turbines <- (core.dat$Windfarm$Input.data %>% filter(Parameter=="No. of turbines")) %>% select(-Parameter)
  capacity_factor_method <- (core.dat$Windfarm$Input.data %>% filter(Parameter=="Capacity factor"))$Exp

  # Calculate/extract capacity factor
  if (capacity_factor_method == 1) {
    # input directly by user
    capacity_factor <- core.dat$Windfarm$Input.data %>% filter(Parameter=="Enter estimated capacity factor (percentage efficiency)") %>% select(-Parameter)
  } else if (capacity_factor_method == 2) {
    # computed based on forestry data
    # capacity_factor <- capacity_factor_forestry()
  }

  # Calculate annual energy output
  energy_output <- 365*24*power_rating_turbines*no_turbines*capacity_factor/100
  return(energy_output)
}
