## 1. Windfarm CO2 emission saving

#' Windfarm_output
#' @param p_cap capcity factor
#' @param n_turb number of turbines
#' @param c_turb maximum tubine power capacity
#' @return Total Windfarm energy output
#' @export
Windfarm_output <- function(p_cap,
                            n_turb,
                            c_turb) {
  ## THIS FUNCTION...

  # Calculate annual energy output
  e_out <- (365*24) * (p_cap/100) * n_turb * c_turb

  return(e_out)
}

#' Windfarm_emissions_saving
#' @param energy_output output of Windfarm_output
#' @param E_mat counterfactuals in matrix form with IDs as rownames
#' @return Total Windfarm CO2 emissions savings
#' @export
Windfarm_emissions_saving <- function(e_out,
                                      E_mat) {
  ## THIS FUNCTION...

  ## Calculate emissions savings
  e_out_mat <- matrix(e_out,
                      nrow = nrow(E_mat),
                      ncol = length(e_out),
                      byrow = T,
                      dimnames = dimnames(E_mat))
  S_fuel <- data.frame(Fuel=rownames(e_out_mat), e_out_mat * E_mat, row.names = NULL)

  return(S_fuel)
}
