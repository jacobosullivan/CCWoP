## 4. Loss of CO2 fixing pot.

#' Loss_of_CO2_fix_pot
#' @param core.dat UI data
#' @param construct.dat UI construction data
#' @param AV_direct Area/Volume of removed peat
#' @return L_fix
#' @export
Loss_of_CO2_fix_pot <- function(core.dat, construct.dat, AV_direct, AV_indirect) {

  # Wrapper function for the Loss_of_CO2_fix_pot0() module
  # THIS FUNCTION...

  L_fix <- Loss_of_CO2_fix_pot0(A_direct = AV_direct$Total$a,
                                A_indirect = AV_indirect$Total$a,
                                G_bog = core.dat$Bog.plants$G_bog,
                                t_wf = core.dat$Windfarm$t_wf,
                                t_restore = core.dat$Bog.plants$t_restore,
                                CO2_C = 3.667)

  return(L_fix)
}

#' Loss_of_CO2_fix_pot0
#' @param A_direct Area of removed peat
#' @param A_indirect Area of drained peat
#' @param G_bog Peatland carbon accumulation rate
#' @param t_wf Windfarm life time
#' @param t_restore Time to restoration after decomissioning
#' @param CO2_C Molecular weight ratio C to CO2
#' @return L_fix
#' @export
Loss_of_CO2_fix_pot0 <- function(A_direct,
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
