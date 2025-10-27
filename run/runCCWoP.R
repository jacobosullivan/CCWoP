library(readxl) # read excel files (UI)
library(tidyverse) # dataframe manipulations
library(purrr) # nested list indexing
devtools::document()

################################################################################
############################# Load input data from UI ##########################
################################################################################

path_to_UI <- "Templates/Full carbon calculator for windfarms on peatlands - Version 2.14.1.xlsx" # select user input spreadsheet

dat <- getData(path_to_UI)

core.dat <- dat$core.dat
forestry.dat <- dat$forestry.dat
construct.dat <- dat$construct.dat
rm(dat)


################################################################################
########################### Calculate capacity factor ##########################
################################################################################

if (core.dat$Windfarm$p_cap_in[1] == 2) { # capacity factor calculated from forestry module
  p_cap <- 1 # placeholder for forestry module output
} else { # user input capacity factor
  p_cap <- core.dat$Windfarm$p_cap
}


################################################################################
#################### Calculate potential emissions savings #####################
################################################################################

## Total Windfarm energy output
e_out <- Windfarm_output(p_cap = p_cap,
                         n_turb = core.dat$Windfarm$n_turb,
                         c_turb = core.dat$Windfarm$c_turb)

## Total Windfarm CO2 emissions savings
E_mat <- matrix(c(core.dat$Counterfactual$E_coal,
                  core.dat$Counterfactual$E_grid_mix,
                  core.dat$Counterfactual$E_fossil_mix),
           ncol=3,
           byrow = T,
           dimnames = list(c("coal", "grid_mix", "fossil_mix"),
                           names(core.dat$Counterfactual$E_coal)))

S_fuel <- Windfarm_emissions_saving(e_out = e_out,
                                    E_mat = E_mat)


################################################################################
####################### Carbon loss due to turbine life ########################
################################################################################

if (core.dat$Windfarm$L_life_in[1] == 2) { # lifetime emissions calculated from linear regression coefficients

  # Regression coefficients of L_life_turb ~ c_turb
  coefs <- data.frame(m=c(517.62,
                          934.34),
                      c=c(-0.1788,
                          -467.55))

  # Total volume cement used
  V_concrete <- Reduce("+", map(construct.dat, "V_concrete")) # sum elements between list elements

  L_life <- Lifetime_emissions(c_turb = core.dat$Windfarm$c_turb,
                               n_turb = core.dat$Windfarm$n_turb,
                               coefs = coefs,
                               V_concrete = V_concrete,
                               c_turb_thresh=1,
                               E_concrete=0.316)

} else { # user input lifetime emissions
  L_life <- core.dat$Windfarm$L_life * core.dat$Windfarm$n_turb * core.dat$Windfarm$c_turb
}


################################################################################
########################## Carbon loss due to back up ##########################
################################################################################

L_back <- Backup_emissions(c_turb = core.dat$Windfarm$c_turb,
                           n_turb = core.dat$Windfarm$n_turb,
                           p_therm = core.dat$Windfarm$p_therm,
                           p_back = core.dat$Windfarm$p_back,
                           E_mat = E_mat,
                           t_wf = core.dat$Windfarm$t_wf)


################################################################################
############################ Volume of peat drained ############################
################################################################################

## Compute foundation and hardstanding dimensions
if (core.dat$Foundations$found_in[1] == 1) { # pool all foundation/hardstanding
  l_f <- core.dat$Foundations$l_found
  w_f <- core.dat$Foundations$w_found

  l_h <- core.dat$Foundations$l_hardstand
  w_h <- core.dat$Foundations$w_hardstand

  # Sum total length/width of foundations + hardstandings
  l_fh <- l_f + l_h
  w_fh <- w_f + w_h

  # Get max depths
  d_f <- core.dat$Foundations$d_peat_rem_found
  d_h <- core.dat$Foundations$d_peat_rem_hardstand

  d_fh <- apply(cbind(d_f,d_h), MAR=1, FUN=max)
} else { # each area considered individually (produces more drainage due to increased edge effects)
  l_f <- map(construct.dat, "l_found_bott") # 'bottom' width used in this case
  w_f <- map(construct.dat, "w_found_bott")

  l_h <- map(construct.dat, "l_hardstand_bott") # 'bottom' width used in this case
  w_h <- map(construct.dat, "w_hardstand_bott")

  n_turb <- map(construct.dat, "n_turb")

  # Sum total length/width of foundations + hardstandings by area
  l_fh <- list_op(l_f, l_h, func = "+")
  w_fh <- list_op(w_f, w_h, func = "+")

  d_f <- map(construct.dat, "d_peat_rem_found")
  d_h <- map(construct.dat, "d_peat_rem_hardstand")

  d_fh <- list_op(d_f, d_h, func="max")
}

AV_indirect <- Vol_peat_drained(drain_ext = core.dat$Peatland$drain_ext,
                                # borrow pit dimensions
                                pit_dims = list(n_pit = core.dat$Borrow.pits$n_pit,
                                                l_pit = core.dat$Borrow.pits$l_pit,
                                                w_pit = core.dat$Borrow.pits$w_pit,
                                                d_pit = core.dat$Borrow.pits$d_pit),
                                # foundation/hardstand dimensions
                                fh_dims = list(n = n_turb,
                                               l = l_fh,
                                               w = w_fh,
                                               d = d_fh),
                                # access track dimensions
                                at_dims = list(float = list(l = core.dat$Access.tracks$l_float,
                                                            w = core.dat$Access.tracks$w_float,
                                                            d = core.dat$Access.tracks$d_float_drain),
                                               track = list(l = core.dat$Access.tracks$l_track,
                                                            w = core.dat$Access.tracks$w_track,
                                                            d = core.dat$Access.tracks$d_track),
                                               rock = list(l = core.dat$Access.tracks$l_rock_drain,
                                                           w = core.dat$Access.tracks$w_rock,
                                                           d = core.dat$Access.tracks$d_rock_drain)),
                                # cable trench dimensions
                                ct_dims = list(l = core.dat$Cable.trenches$l_cab_trench,
                                               d = core.dat$Cable.trenches$d_cab_trench),
                                # additional excavation dimensions
                                add_dims = list(v = core.dat$Add.excavation$V_add,
                                                a = core.dat$Add.excavation$A_add))

################################################################################
############################ Volume of peat removed ############################
################################################################################

## Compute foundation and hardstanding dimensions
if (core.dat$Foundations$found_in[1] == 1) { # pool all foundation/hardstanding
  n_turb <- core.dat$Windfarm$n_turb
  l_fb <- l_fs <- core.dat$Foundations$l_found
  w_fb <- w_fs <- core.dat$Foundations$w_found
  d_f <- core.dat$Foundations$d_peat_rem_found

  l_hb <- l_hs <- core.dat$Foundations$l_hardstand
  w_hb <- w_hs <- core.dat$Foundations$w_hardstand
  d_h <- core.dat$Foundations$d_peat_rem_hardstand

} else { # each area considered individually (produces more drainage due to increased edge effects)
  n_turb <- map(construct.dat, "n_turb")

  d_f <- map(construct.dat, "d_peat_rem_found")
  d_h <- map(construct.dat, "d_peat_rem_hardstand")

  l_fb <- map(construct.dat, "l_found_bott")
  w_fb <- map(construct.dat, "w_found_bott")
  l_fs <- map(construct.dat, "l_found_surf")
  w_fs <- map(construct.dat, "w_found_surf")

  l_hb <- map(construct.dat, "l_hardstand_bott")
  w_hb <- map(construct.dat, "w_hardstand_bott")
  l_hs <- map(construct.dat, "l_hardstand_surf")
  w_hs <- map(construct.dat, "w_hardstand_surf")

}

AV_direct <- Vol_peat_removed(# borrow pit dimensions
                              pit_dims = list(n_pit = core.dat$Borrow.pits$n_pit,
                                             l_pit = core.dat$Borrow.pits$l_pit,
                                             w_pit = core.dat$Borrow.pits$w_pit,
                                             d_pit = core.dat$Borrow.pits$d_pit),
                              # foundation dimensions
                              f_dims = list(n = n_turb,
                                            l_b = l_fb,
                                            w_b = w_fb,
                                            l_s = l_fs,
                                            w_s = w_fs,
                                            d = d_f),
                              # hardstanding dimensions
                              h_dims = list(n = n_turb,
                                            l_b = l_hb,
                                            w_b = w_hb,
                                            l_s = l_hs,
                                            w_s = w_hs,
                                            d = d_h),
                              # access track dimensions
                              at_dims = list(float = list(l = core.dat$Access.tracks$l_float,
                                                          w = core.dat$Access.tracks$w_float,
                                                          d = core.dat$Access.tracks$d_float_drain),
                                             track = list(l = core.dat$Access.tracks$l_track,
                                                          w = core.dat$Access.tracks$w_track,
                                                          d = core.dat$Access.tracks$d_track),
                                             rock = list(l = core.dat$Access.tracks$l_rock_drain,
                                                         w = core.dat$Access.tracks$w_rock,
                                                         d = core.dat$Access.tracks$d_rock_drain)),
                              # additional excavation dimensions
                              add_dims = list(v = core.dat$Add.excavation$V_add,
                                              a = core.dat$Add.excavation$A_add))


################################################################################
######################### Loss of CO2 fixing potential #########################
################################################################################

L_fix <- Loss_of_CO2_fix_pot(A_direct = AV_direct$Total$a,
                             A_indirect = AV_indirect$Total$a,
                             G_bog = core.dat$Bog.plants$G_bog,
                             t_wf = core.dat$Windfarm$t_wf,
                             t_restore = core.dat$Bog.plants$t_restore,
                             CO2_C = 3.667)


################################################################################
########################## Emissions rates from soils ##########################
################################################################################

R_tot <- Emissions_rates_soils(em_factor_meth_in = core.dat$Em.factor.meth$em_factor_meth_in,
                               peat_type = core.dat$Peatland$peat_type,
                               A_indirect = AV_indirect$Total$a,
                               V_indirect = AV_indirect$Total$v,
                               T_air = core.dat$Peatland$T_air,
                               d_wt = core.dat$Peatland$d_wt,
                               CO2_C = 3.667,
                               CH4_CO2 = 30.67)

################################################################################
############################### Loss of Soil CO2 ###############################
################################################################################

if (core.dat$Peatland$peat_type[1] == 1) { # Acid bog
  D_f <- 178
} else { # Fen
  D_f <- 169
}

L_drainage <- CO2_loss_drained(pC_dry_peat = core.dat$Peatland$pC_dry_peat,
                               BD_dry_soil = core.dat$Peatland$BD_dry_soil,
                               R_tot = R_tot,
                               D_f = D_f,
                               restore_hydr_in = core.dat$Site.restoration$restore_hydr_in,
                               restore_hab_in = core.dat$Site.restoration$restore_hab_in,
                               A_indirect = AV_indirect$Total$a,
                               V_indirect = AV_indirect$Total$v,
                               t_wf = core.dat$Windfarm$t_wf,
                               t_restore = core.dat$Bog.plants$t_restore)

L_indirect <- L_drainage$L_drained - L_drainage$L_undrained

L_direct <- CO2_loss_removed(pC_dry_peat = core.dat$Peatland$pC_dry_peat,
                             BD_dry_soil = core.dat$Peatland$BD_dry_soil,
                             A_direct = AV_direct$Total$a,
                             V_direct = AV_direct$Total$v,
                             L_undrained_pa = L_drainage$L_undrained / AV_indirect$Total$a,
                             CO2_C = 3.667,
                             pCO2_lost = 100)
