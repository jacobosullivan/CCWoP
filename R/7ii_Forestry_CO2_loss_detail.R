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

  ## NExt I need to clarify the most appropriate way to compute 'Total emissions due to cleared land'

  # SEE: '7ii. Forestry CO2 loss - detail'!F17

  # The spreadsheet takes the C sequestration potential in units of ha-1 from 3PG and multiplies this by the difference between
  # the deforested and replanted land areas.

  # This assumes that the sequestration potential of the pre-development forest is the same as the replanted forest which is clearly not true
  # due to age modifiers etc.

  # It's also really weird to take this approach since both sequestration rates are explicitly calculated using 3PG

}
