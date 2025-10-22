library(readxl) # read excel files (UI)
library(tidyverse) # dataframe manipulations
library(purrr) # nested list indexing
devtools::document()

####################################################
############### Load input data from UI ############
####################################################

path_to_UI <- "Templates/Full carbon calculator for windfarms on peatlands - Version 2.14.1.xlsx" # select user input spreadsheet

dat <- getData(path_to_UI)

core.dat <- dat$core.dat
forestry.dat <- dat$forestry.dat
construct.dat <- dat$construct.dat
rm(dat)


####################################################
############# Calculate capacity factor ############
####################################################

if (core.dat$Windfarm$p_cap_in == 2) { # capacity factor calculated from forestry module
  p_cap <- 1 # placeholder for forestry module output
} else { # user input capacity factor
  p_cap <- core.dat$Windfarm$p_cap
}


####################################################
###### Calculate potential emissions savings #######
####################################################

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


####################################################
######### Carbon loss due to turbine life ##########
####################################################

if (core.dat$Windfarm$L_life_in == 2) { # lifetime emissions calculated from linear regression coefficients

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


####################################################
############ Carbon loss due to back up ############
####################################################

L_back <- Backup_emissions(c_turb = core.dat$Windfarm$c_turb,
                           n_turb = core.dat$Windfarm$n_turb,
                           p_therm = core.dat$Windfarm$p_therm,
                           p_back = core.dat$Windfarm$p_back,
                           E_mat = E_mat,
                           t_wf = core.dat$Windfarm$t_wf)
