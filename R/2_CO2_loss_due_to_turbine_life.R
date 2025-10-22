## 2. CO2 loss due to turbine life

#' Lifetime_emissions
#' @param c_turb maximum tubine power capacity
#' @param n_turb number of turbines
#' @param coefs regression coefficeints
#' @param V_concrete total volume of cement used
#' @param c_turb_thresh threshold turbine power capacity for regression models
#' @param E_cement emissions per unit volume cement
#' @return Windfarm lifetime emissions
#' @export
Lifetime_emissions <- function(c_turb,
                               n_turb,
                               coefs,
                               V_concrete,
                               c_turb_thresh=1,
                               E_concrete=0.316) {

  # THIS FUNCTION...

  # sapply call required in case c_turb includes values both above and below the threshold
  L_life_turb <- sapply(c_turb, FUN = function(x) {
    if (x <= c_turb_thresh) {
      return(coefs$m[1]*x+coefs$c[1])
    } else {
      return(coefs$m[2]*x+coefs$c[2])
    }
  })

  L_life_emissions <- n_turb * L_life_turb

  # Note apparent mis-match in naming as input variable called 'Volume of concrete used (m3)' used in assessment of cement emissions
  # #'concrete' used in variable naming
  L_concrete <- E_concrete * V_concrete

  L_life <- L_life_emissions + L_concrete

  return(L_life)
}
