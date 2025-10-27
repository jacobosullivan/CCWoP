## 5d. CO2 loss from drained peat

#' CO2_loss_drained
#' @param pC_dry_peat Carbon content of dry peat
#' @param BD_dry_soil Dry soil bulk density
#' @param R_tot ECOSSE emissions factors
#' @param D_f Number flooded days undrained peatland (by peat type)
#' @param restore_hydr_in Select hydrology restoration
#' @param restore_hab_in Select habitat restoration
#' @param A_indirect Area peat drained
#' @param V_indirect Volume peat drained
#' @param t_wf Windfarm life time
#' @param t_restore Time to restoration after decomissioning
#' @param E_tot Total emissions of undrained per per unit area
#' @return Net CO2 loss from drained peat
#' @export
CO2_loss_drained <- function(pC_dry_peat,
                             BD_dry_soil,
                             R_tot,
                             D_f,
                             restore_hydr_in,
                             restore_hab_in,
                             A_indirect,
                             V_indirect,
                             t_wf,
                             t_restore) {

  # THIS FUNCTION...

  # If restored emissions from drained and counterfactual (undrained) calculated using ECOSSE factors
  A_t <- (A_indirect / 10000) * (t_wf + t_restore)  # convert area to ha
  pD_f <- D_f / 365
  L_drained_rest <- A_t * (R_tot$R_CO2_drained + R_tot$R_CH4_drained) # D_f = 0
  L_undrained_rest <- A_t * (((R_tot$R_CO2_undrained * pD_f) + (R_tot$R_CO2_drained * (1 - pD_f))) +
                             ((R_tot$R_CH4_undrained * pD_f) + (R_tot$R_CH4_drained * (1 - pD_f))))
  p_undrained <- L_undrained_rest / L_drained_rest # proportion used in case not restored

  # If not restored, all carbon assumed lost, counterfactual given by ratio computed above
  L_drained_no_rest <- (CO2_C / 100) * pC_dry_peat * BD_dry_soil * V_indirect
  L_undrained_no_rest <- L_drained_no_rest * p_undrained

  if (restore_hab_in[1] == 2 & restore_hydr_in[1] == 2) { # habitat AND hydrology restored
    return(list(L_drained = L_drained_rest,
                L_undrained = L_undrained_rest))
  } else {
    return(list(L_drained = L_drained_no_rest,
                L_undrained = L_undrained_no_rest))
  }

}
