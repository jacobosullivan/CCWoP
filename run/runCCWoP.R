library(readxl) # read excel files (UI)
library(tidyverse) # dataframe manipulations
library(purrr) # nested list indexing
devtools::document()

################################################################################
############################# Load input data from UI ##########################
################################################################################

path_to_UI <- "Templates/Full carbon calculator for windfarms on peatlands - Version 2.14.1.xlsx" # select user input spreadsheet

dat <- getData(path_to_UI)

## I need tests to ensure all data has been passed. If not, return error messages
core.dat <- dat$core.dat
forestry.dat <- dat$forestry.dat
construct.dat <- dat$construct.dat
rm(dat)

## Counterfactuals in matrix form for fast multiplication
E_mat <- matrix(c(core.dat$Counterfactual$E_coal,
                  core.dat$Counterfactual$E_grid_mix,
                  core.dat$Counterfactual$E_fossil_mix),
                ncol=3,
                byrow = T,
                dimnames = list(c("coal", "grid_mix", "fossil_mix"),
                                names(core.dat$Counterfactual$E_coal)))

################################################################################
####################### Carbon loss due to turbine life ########################
################################################################################

L_life <- Lifetime_emissions(core.dat = core.dat,
                             construct.dat = construct.dat)

################################################################################
########################## Carbon loss due to back up ##########################
################################################################################

L_back <- Backup_emissions(core.dat = core.dat,
                           E_mat = E_mat)

################################################################################
############################ Volume of peat drained ############################
################################################################################

AV_indirect <- AV_peat_drained(core.dat = core.dat,
                               construct.dat = construct.dat)

################################################################################
############################ Volume of peat removed ############################
################################################################################

AV_direct <- AV_peat_removed(core.dat = core.dat,
                             construct.dat = construct.dat)

################################################################################
######################### Loss of CO2 fixing potential #########################
################################################################################

L_fix <- Loss_of_CO2_fix_pot(core.dat = core.dat,
                             AV_direct = AV_direct,
                             AV_indirect = AV_indirect)

################################################################################
########################## Emissions rates from soils ##########################
################################################################################

R_tot <- Emissions_rates_soils(core.dat = core.dat,
                               construct.dat = construct.dat,
                               AV_indirect = AV_indirect)

################################################################################
############################### Loss of Soil CO2 ###############################
################################################################################

L_indirect <- CO2_loss_drained(core.dat = core.dat,
                               AV_indirect = AV_indirect,
                               R_tot = R_tot)

L_direct <- CO2_loss_removed(core.dat = core.dat,
                             AV_direct = AV_direct,
                             L_indirect = L_indirect)

L_soil <- CO2_loss_from_soil(L_direct = L_direct,
                             L_indirect = L_indirect)

################################################################################
####################### CO2 gain due to site improvement #######################
################################################################################

L_improvement <- CO_2_gain_site_improve(core.dat = core.dat)

################################################################################
######################### CO2 loss by DOC and POC loss #########################
################################################################################

L_DPOC <- CO2_loss_DOC_POC(core.dat = core.dat,
                           L_indirect = L_indirect,
                           L_improvement = L_improvement)

################################################################################
##################### CO2 sequestration loss from Forestry #####################
################################################################################

if (core.dat$Forestry$for_detail[1] == 1) { # Simple version of forest modelling
  L_forest <- Forestry_CO2_loss_simple(core.dat = core.dat)
} else { # detailed version of forest modelling (3PG module)
  L_forest <- Forestry_CO2_loss_detail(core.dat = core.dat,
                                       forestry.dat = forestry.dat)
}

################################################################################
############################### Windspeed ratios ###############################
################################################################################

# Wrong with changes to inputs suggesting that conditionals may be failing
R_windspeed_all <- Wind_speed_ratios(core.dat = core.dat,
                                     forestry.dat = forestry.dat)

################################################################################
########################### Calculate capacity factor ##########################
################################################################################

if (core.dat$Windfarm$p_cap_in[1] == 2) { # capacity factor calculated from forestry module

  p_cap <- p_cap_forestry(core.dat = core.dat,
                          forestry.dat = forestry.dat,
                          R_windspeed_all = R_windspeed_all)
  n_turb <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "n_turb")

} else { # user input capacity factor
  p_cap <- core.dat$Windfarm$p_cap
  n_turb <- core.dat$Windfarm$n_turb
}


################################################################################
#################### Calculate potential emissions savings #####################
################################################################################

## Total Windfarm energy output
e_out <- Windfarm_output(p_cap = p_cap,
                         n_turb = n_turb,
                         c_turb = core.dat$Windfarm$c_turb)

S_fuel <- Windfarm_emissions_saving(e_out = e_out,
                                    E_mat = E_mat)

################################################################################
######################## Payback time and CO2 emissions ########################
################################################################################

e_out_wf <- e_out * core.dat$Windfarm$t_wf

L_tot <- L_life + L_back + L_fix + L_soil + L_DPOC + L_forest

L_improvement_tot <- colSums(bind_rows(L_improvement$L_improvement))

L_tot_net <- L_tot - L_improvement_tot[c(1,3,2)] # re-order Min/Max

B_payback_time <- matrix(rep(L_tot_net, each=3), 3, 3) / as.matrix(S_fuel[,-1])[,c(1,3,2)] # re-order Min/Max

B_payback_time <- cbind(S_fuel$Fuel, data.frame(B_payback_time))

r_CO2_to_pow <- (L_tot_net * 1000) / e_out_wf[c(1,3,2)] # re-order Min/Max

