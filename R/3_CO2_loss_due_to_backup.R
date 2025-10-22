## 3. CO2 loss due to backup

#' Backup_emissions
#' @param c_turb maximum tubine power capacity
#' @param n_turb number of turbines
#' @param p_therm percent thermal efficiency of backup source
#' @param p_back percent extra capacity needed for backup
#' @param E_mat counterfactuals in matrix form with IDs as rownames
#' @param t_wf windfarm lifetime
#' @return Backup lifetime emissions
#' @export
Backup_emissions <- function(c_turb,
                             n_turb,
                             p_therm,
                             p_back,
                             E_mat,
                             t_wf) {

  # THIS FUNCTION...

  L_back_tot <- matrix((p_therm/100) * (24*365) * n_turb * c_turb * (p_back/100) * t_wf,
                       nrow = nrow(E_mat),
                       ncol = length(p_therm),
                       byrow = T,
                       dimnames = dimnames(E_mat))

  L_back <- L_back_tot * E_mat

  return(L_back)
}
