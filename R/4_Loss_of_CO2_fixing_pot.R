## 4. Loss of CO2 fixing pot.

#' Loss_of_CO2_fix_pot
#' @param A_direct Area of removed peat
#' @param A_indirect Area of drained peat
#' @param G_bog Peatland carbon accumulation rate
#' @param t_wf Windfarm life time
#' @param t_restore Time to restoration after decomissioning
#' @param CO2_C Molecular weight ratio C to CO2
#' @return Hypothetical carbon fixation of drained/removed peat
#' @export
Loss_of_CO2_fix_pot <- function(A_direct,
                                A_indirect,
                                G_bog,
                                t_wf,
                                t_restore,
                                CO2_C = 3.667) {

  # THIS FUNCTION...
  C_accumultion_per_area <- G_bog * (t_wf + t_restore) * CO2_C # convert into units CO2
  L_fix <- C_accumultion_per_area * (A_direct + A_indirect)/10000 # normalisation by 10000 converts from m3 to ha

  return(L_fix)
}
