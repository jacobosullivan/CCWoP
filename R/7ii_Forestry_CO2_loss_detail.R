## 7ii. Forestry CO2 loss - detail
#' Forestry_CO2_loss_detail
#' @param core.dat UI data
#' @param forestry.dat UI forestry data
#' @return Estimated lifetime loss of carbon sequestration
#' @export
Forestry_CO2_loss_detail <- function(core.dat,
                                     forestry.dat) {

  # THIS FUNCTION...
  C_forestry <- C_sequest_in_trees(core.dat,
                                   forestry.dat)

}
