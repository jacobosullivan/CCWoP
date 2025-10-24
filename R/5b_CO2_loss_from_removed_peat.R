## 5b. CO2 loss from removed peat

#' CO2_loss_removed
#' @param pC_dry_peat Carbon content of dry peat
#' @param BD_dry_soil Dry soil bulk density
#' @param A_direct Area peat removed
#' @param V_direct Volume peat removed
#' @param E_tot Total emissions of undrained per per unit area
#' @param CO2_C Molecular weight ratio C to CO2
#' @param pCO2_lost percent Carbon lost as CO2
#' @return Hypothetical carbon fixation of drained/removed peat
#' @export
CO2_loss_removed <- function(pC_dry_peat,
                             BD_dry_soil,
                             A_direct,
                             V_direct,
                             E_tot, # t_restore already accounted for
                             CO2_C = 3.667,
                             pCO2_lost = 100) {

  # THIS FUNCTION...

  # Total GHG emissions from removed land
  L_removed <- (pCO2_lost / 100) * (CO2_C / 100) * pC_dry_peat * BD_dry_soil * V_direct

  L_undrained <- E_tot * A_direct

  L_direct <- L_removed - L_drained

  return(L_direct)
}
