## 5d. CO2 loss from drained peat

#' CO2_loss_drained
#' @param pC_dry_peat Carbon content of dry peat
#' @param BD_dry_soil Dry soil bulk density
#' @param restore_hydr_in Select hydrology restoration
#' @param A_indirect Area peat drained
#' @param V_indirect Volume peat drained
#' @param t_wf Windfarm life time
#' @param t_restore Time to restoration after decomissioning
#' @param E_tot Total emissions of undrained per per unit area
#' @param CO2_C Molecular weight ratio C to CO2
#' @param pCO2_lost percent Carbon lost as CO2
#' @return Hypothetical carbon fixation of drained/removed peat
#' @export
CO2_loss_drained <- function(pC_dry_peat,
                             BD_dry_soil,
                             restore_hydr_in,
                             A_indirect,
                             V_indirect,
                             t_wf,
                             t_restore) {

  # THIS FUNCTION...

  # NEED EMISSION Rates FROM SOILS FIRST

  # Total GHG emissions from drained land
  if (restore_hydr_in==1) { # NOT restored following decomissioning
    L_drained <- (CO2_C / 100) * pC_dry_peat * BD_dry_soil * V_indirect
  } else {
    E_
  }



}
