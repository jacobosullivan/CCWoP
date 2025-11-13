## 7ii. Forestry CO2 loss - detail
#' Forestry_CO2_loss_detail
#' @param core.dat UI data
#' @param forestry.dat UI forestry data
#' @return Estimated lifetime loss of carbon sequestration
#' @export
Forestry_CO2_loss_detail <- function(core.dat,
                                     forestry.dat) {

  # THIS FUNCTION...
  CO2_C <- 3.667 # Molecular weight ratio C to CO2

  # Proportion of biomass used in biofuel and long, medium and short lived wood products
  # These may need to be area specific requiring different downstream implementation (i.e. using list_op)
  rho_felled <- list(biofuel = 1, wp_long = 0, wp_med = 0, wp_short = 0) # THIS WILL NEED TO BE A USER INPUT
  rho_replant <- list(biofuel = 1, wp_long = 0, wp_med = 0, wp_short = 0) # THIS WILL NEED TO BE A USER INPUT

  # exponential decay rate of long, medium and short lived wood products
  alpha_wp <- list(wp_long = c(Exp = 0.01, Min = 0.01, Max = 0.01),
                   wp_med = c(Exp = 0.1, Min = 0.1, Max = 0.1),
                   wp_short = c(Exp = 1, Min = 1, Max = 1)) # THIS COULD BE A USER INPUT OR COULD BE ESTIMATED FROM LITERATURE

  # average transportation distances of long, medium and short lived wood products
  D_wp <- list(wp_long = c(Exp = 100, Min = 100, Max = 100),
               wp_med = c(Exp = 100, Min = 100, Max = 100),
               wp_short = c(Exp = 100, Min = 100, Max = 100)) # THIS SHOULD BE A USER INPUT

  # average transportation emissions factors of long, medium and short lived wood products
  E_wp <- list(wp_long = c(Exp = 0.1, Min = 0.1, Max = 0.1),
               wp_med = c(Exp = 0.1, Min = 0.1, Max = 0.1),
               wp_short = c(Exp = 0.1, Min = 0.1, Max = 0.1)) # THIS COULD BE A USER INPUT OR COULD BE ESTIMATED FROM LITERATURE

  ## Loss of carbon sequestration due to felling of forestry for wind farm
  C_forestry <- C_sequest_in_trees(core.dat,
                                   forestry.dat)

  A_felled <- list_op(l1 = map(forestry.dat[grep("Area", names(forestry.dat))], "n_turb"),
                      l2 = map(forestry.dat[grep("Area", names(forestry.dat))], "A_harv_turb"),
                      func = "*")

  A_replant <- list_op(l1 = map(forestry.dat[grep("Area", names(forestry.dat))], "n_turb"),
                       l2 = map(forestry.dat[grep("Area", names(forestry.dat))], "A_replant_turb"),
                       func = "*")

  C_seq_loss_felled <- lapply(list_op(l1 = A_felled,
                                      l2 = C_forestry$seq_pot,
                                      func = "*"),
                              FUN = function (x) x * CO2_C)

  # SPREADSHEET ERROR
  # Replanted sequestration should consider a different per area total due to e.g. the age modifier
  C_seq_gain_replant <- lapply(list_op(l1 = A_replant,
                                       l2 = C_forestry$seq_pot_replant,
                                       func = "*"),
                               FUN = function (x) x * CO2_C)

  C_seq_loss_net <- list_op(l1 = C_seq_loss_felled,
                            l2 = C_seq_gain_replant,
                            func = "-")


  ## Cleared forest floor emissions

  # Extract and restructure data for easy access
  C_seq_soil_df <- C_seq_soil()
  soil_seq_rate <- list()
  t_wf_by_area <- list()
  emissions_from_felling <- list()
  emissions_from_transport <- list()
  fossil_fuel_emissions_factor <- list()
  dist_fuel_plant <- list()

  ii <- 1
  for (i in grep("Area", names(forestry.dat))) {
    soil_seq_rate[[ii]] <- C_seq_soil_df$seq_rate[which(C_seq_soil_df$Soil_type == forestry.dat[[i]]$soil_type[1])]
    t_wf_by_area[[ii]] <- core.dat$Windfarm$t_wf
    emissions_from_felling[[ii]] <- forestry.dat$Emissions$L_harv / 1e6 # unit conversion
    emissions_from_transport[[ii]] <- forestry.dat$Emissions$L_transport / 1e6 # unit conversion
    fossil_fuel_emissions_factor[[ii]] <- core.dat$Counterfactual$E_fossil_mix
    dist_fuel_plant[[ii]] <- forestry.dat$Windfarm$dist_biofuel_plant
    ii <- ii + 1
  }
  names(soil_seq_rate) <- grep("Area", names(forestry.dat), value = T)
  names(t_wf_by_area) <- grep("Area", names(forestry.dat), value = T)
  names(emissions_from_felling) <- grep("Area", names(forestry.dat), value = T)
  names(emissions_from_transport) <- grep("Area", names(forestry.dat), value = T)
  names(fossil_fuel_emissions_factor) <- grep("Area", names(forestry.dat), value = T)
  names(dist_fuel_plant) <- grep("Area", names(forestry.dat), value = T)

  L_floor_bfr_replant <- list_op(l1 = A_felled,
                                 l2 = map(forestry.dat[grep("Area", names(forestry.dat))], "t_replant"),
                                 l3 = soil_seq_rate,
                                 func = "*")

  L_floor_aft_replant <- list_op(l1 = list_op(l1 = A_felled,
                                              l2 = A_replant,
                                              func = "-"),
                                 l2 = list_op(l1 = t_wf_by_area,
                                              l2 = map(forestry.dat[grep("Area", names(forestry.dat))], "t_replant"),
                                              func = "-"),
                                 l3 = soil_seq_rate,
                                 func = "*")

  L_floor <- lapply(list_op(l1 = L_floor_bfr_replant,
                            l2 = L_floor_aft_replant,
                            func = "+"),
                    FUN = function(x) x * CO2_C)

  ## Emissions from harvesting operations (from growth yield table)
  vol_harv <- lapply(list_op(l1 = map(forestry.dat[grep("Area", names(forestry.dat))], "t_harv"),
                             l2 = map(forestry.dat[grep("Area", names(forestry.dat))], "soil_type"),
                             func = "c"),
                     FUN = function(x) growth_yield_tab(t = x[1:3], soil_type = x[4], species = 2)$volume)

  L_harv <- list_op(l1 = vol_harv,
                    l2 = A_felled,
                    l3 = emissions_from_felling,
                    func = "*")

  ## Savings from use of felled forestry as biofuel
  W_felled <- list_op(l1 = list_op(l1 = A_felled,
                                   l2 = C_forestry$C_tot,
                                   func = "*"),
                      l2 = lapply(map(forestry.dat[grep("Area", names(forestry.dat))], "r_CBiomass"),
                                  FUN = function(x) {
                                    x <- x[c(1,3,2)] # re-arrange Min, Max
                                    names(x) <- c("Exp", "Min", "Max")
                                    return(x)
                                  }),
                      func = "/")

  W_felled_biofuel <- lapply(W_felled,
                             FUN = function(x) x * rho_felled$biofuel)

  W_pow_val_felled <- list_op(l1 = W_felled_biofuel,
                              l2 = map(forestry.dat[grep("Area", names(forestry.dat))], "e_felled_biofuel"),
                              func = "*")

  # Get 1/0 for Yes/No converting felled forestry to biofuel
  # JDebug: this can be handled instead via rho_felled
  felled_biofuel <- lapply(map(forestry.dat[grep("Area", names(forestry.dat))], "felled_biofuel"),
                           FUN = function(x) {
                             if (x[1]==2) { # not used as biofuel
                               return(c(Exp = 0, Min = 0, Max = 0))
                             } else {
                               return(c(Exp = 1, Min = 1, Max = 1))
                             }
                           })

  S_biofuel_felled <- list_op(l1 = W_pow_val_felled,
                              l2 = fossil_fuel_emissions_factor,
                              l3 = felled_biofuel,
                              func = "*")

  L_transp_felled <- list_op(l1 = dist_fuel_plant,
                             l2 = emissions_from_transport,
                             l3 = W_felled_biofuel,
                             func = "*")

  S_biofuel_felled <- list_op(l1 = S_biofuel_felled,
                              l2 = L_transp_felled,
                              func = "-")

  ## Savings from use of replanted forestry as biofuel
  W_replant <- list_op(l1 = list_op(l1 = A_replant,
                                    l2 = C_forestry$seq_pot_replant,
                                    func = "*"),
                       l2 = lapply(map(forestry.dat[grep("Area", names(forestry.dat))], "r_CBiomass"),
                                   FUN = function(x) {
                                     x <- x[c(1,3,2)] # re-arrange Min, Max
                                     names(x) <- c("Exp", "Min", "Max")
                                     return(x)
                                   }),
                       func = "/")

  W_replant_biofuel <- lapply(W_replant,
                              FUN = function(x) x * rho_replant$biofuel)

  W_pow_val_replant <- list_op(l1 = W_replant_biofuel,
                               l2 = map(forestry.dat[grep("Area", names(forestry.dat))], "e_felled_biofuel"),
                               func = "*")

  # Assume that if harvested biomass converted to biofuel, so is replanted biomass...
  # This could instead be handled using rho_replant
  replant_biofuel <- felled_biofuel

  S_biofuel_replant <- list_op(l1 = W_pow_val_replant,
                               l2 = fossil_fuel_emissions_factor,
                               l3 = replant_biofuel,
                               func = "*")

  L_transp_replant <- list_op(l1 = dist_fuel_plant,
                              l2 = emissions_from_transport,
                              l3 = W_replant_biofuel,
                              func = "*")

  S_biofuel_replant <- list_op(l1 = S_biofuel_replant,
                               l2 = L_transp_replant,
                               func = "-")

  ## Wood product decay functions
  ### Felled forestry
  W_wp_long_felled <- lapply(W_felled,
                             FUN = function(x) x * rho_replant$wp_long)

  W_wp_med_felled <- lapply(W_felled,
                             FUN = function(x) x * rho_replant$wp_med)

  W_wp_short_felled <- lapply(W_felled,
                             FUN = function(x) x * rho_replant$wp_short)

  L_wp_long_felled <- lapply(W_wp_long_felled,
                             FUN = function(x) x * (1 - exp(-alpha_wp$wp_long * core.dat$Windfarm$t_wf) + D_wp$wp_long * E_wp$wp_long))

  L_wp_med_felled <- lapply(W_wp_med_felled,
                             FUN = function(x) x * (1 - exp(-alpha_wp$wp_med * core.dat$Windfarm$t_wf) + D_wp$wp_med * E_wp$wp_med))

  L_wp_short_felled <- lapply(W_wp_short_felled,
                             FUN = function(x) x * (1 - exp(-alpha_wp$wp_short * core.dat$Windfarm$t_wf) + D_wp$wp_short * E_wp$wp_short))

  L_wp_felled <- list_op(l1 = L_wp_long_felled,
                         l2 = L_wp_med_felled,
                         l3 = L_wp_short_felled,
                         func = "+")

  ### Replanted forestry
  W_wp_long_replant <- lapply(W_replant,
                              FUN = function(x) x * rho_replant$wp_long)

  W_wp_med_replant <- lapply(W_replant,
                             FUN = function(x) x * rho_replant$wp_med)

  W_wp_short_replant <- lapply(W_replant,
                               FUN = function(x) x * rho_replant$wp_short)

  L_wp_long_replant <- lapply(W_wp_long_replant,
                              FUN = function(x) x * (1 - exp(-alpha_wp$wp_long * core.dat$Windfarm$t_wf) + D_wp$wp_long * E_wp$wp_long))

  L_wp_med_replant <- lapply(W_wp_med_replant,
                             FUN = function(x) x * (1 - exp(-alpha_wp$wp_med * core.dat$Windfarm$t_wf) + D_wp$wp_med * E_wp$wp_med))

  L_wp_short_replant <- lapply(W_wp_short_replant,
                               FUN = function(x) x * (1 - exp(-alpha_wp$wp_short * core.dat$Windfarm$t_wf) + D_wp$wp_short * E_wp$wp_short))

  L_wp_replant <- list_op(l1 = L_wp_long_replant,
                          l2 = L_wp_med_replant,
                          l3 = L_wp_short_replant,
                          func = "+")
  ## Totals
  ### Emissions
  L_tot <- list_op(l1 = list_op(l1 = C_seq_loss_felled,
                                l2 = L_floor,
                                l3 = L_harv,
                                func = "+"),
                   l2 = list_op(l1 = L_wp_felled,
                                l2 = L_wp_replant,
                                func = "+"),
                   func = "+")

  ### Savings (biofuel)
  S_tot <- list_op(l1 = lapply(S_biofuel_felled,
                               FUN = function(x) {
                                 x <- x[c(1,3,2)] # re-arrange Min, Max
                                 names(x) <- c("Exp", "Min", "Max")
                                 return(x)
                               }),
                   l2 = lapply(S_biofuel_replant,
                               FUN = function(x) {
                                 x <- x[c(1,3,2)] # re-arrange Min, Max
                                 names(x) <- c("Exp", "Min", "Max")
                                 return(x)
                               }),
                   func = "+")

  L_forestry <- list_op(l1 = L_tot,
                        l2 = S_tot,
                        func = "-")

  L_forestry_tot <- colSums(bind_rows(L_forestry))

  return(L_forestry_tot)
}
