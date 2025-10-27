## 5e. Emission rates from soils

#' Emissions_rates_soils
#' @param em_factor_meth_in Select IPCC default or ECOSSE model
#' @param peat_type Select acid bog or fen
#' @param A_indirect Area peat drained
#' @param V_indirect Volume peat drained
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return Emissions rates from drained and undrained peatland
#' @export
Emissions_rates_soils <- function(em_factor_meth_in,
                                  peat_type,
                                  A_indirect,
                                  V_indirect,
                                  T_air,
                                  d_wt,
                                  CO2_C = 3.667,
                                  CH4_CO2 = 30.67) {

  # THIS FUNCTION

  if (em_factor_meth_in[1] == 1) { # IPCC default calculation used

    if (peat_type[1] == 1) { # Acid bog selected
      R_CH4_undrained <- (11 / 1000000000) * 10000 * 365
    } else { # Fen selected
      R_CH4_undrained <- (60 / 1000000000) * 10000 * 365
    }

    R_CO2_drained <- 9.6 * (12 + 16 + 16) / 12 # Identical for both peat types
    R_CO2_undrained <- 0 # Assumption
    R_CH4_drained <- 0 # Assumption

  } else { # Site specific calculation using ECOSSE method

    d_wt_drained <- apply(cbind(V_indirect/A_indirect,d_wt), MAR=1, FUN=max)

    if (peat_type[1] == 1) { # Acid bog selected

      ECOSSR_CO2 <- function(CO2_C, d_wt, T_air) {
        return((CO2_C/1000) * ((6700 * exp(-0.26 * exp(-0.05153 * ((100*d_wt)-50)))) + ((72.54 * T_air) - 800)))
      }

      ECOSSR_CH4 <- function(CH4_CO2, d_wt, T_air) { # converts into CO2 eq units
        return((CH4_CO2/1000) * (500 * exp(-0.1234 * (100*d_wt)) + ((3.529*T_air) - 36.67)))
      }

      R_CO2_drained <- ECOSSR_CO2(CO2_C, d_wt_drained, T_air)
      R_CO2_undrained <- ECOSSR_CO2(CO2_C, d_wt, T_air)

      R_CH4_drained <- ECOSSR_CH4(CH4_CO2, d_wt_drained, T_air)
      R_CH4_undrained <- ECOSSR_CH4(CH4_CO2, d_wt, T_air)

    } else { # Fen selected

      ECOSSR_CO2 <- function(CO2_C, d_wt, T_air) {
        return((CO2_C/1000) * (16244 * exp(-0.17594 * exp(-0.07346 * ((d_wt*100)-50))) + (153.234*T_air)))
      }

      ECOSSR_CH4 <- function(CH4_CO2, d_wt, T_air) { # converts into CO2 eq units
        return((CH4_CO2/1000) * (-10 + 563.6253 * exp(-0.09702 * (100*d_wt)) + (0.662183*T_air)))
      }

      R_CO2_drained <- ECOSSR_CO2(CO2_C, d_wt_drained, T_air)
      R_CO2_undrained <- ECOSSR_CO2(CO2_C, d_wt, T_air)

      R_CH4_drained <- ECOSSR_CH4(CH4_CO2, d_wt_drained, T_air)
      R_CH4_undrained <- ECOSSR_CH4(CH4_CO2, d_wt, T_air)

    }

  }

  return(list(R_CO2_drained=R_CO2_drained,
              R_CO2_undrained=R_CO2_undrained,
              R_CH4_drained=R_CH4_drained,
              R_CH4_undrained=R_CH4_undrained))

}
