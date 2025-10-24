## 5a. Volume of peat removed

#' removal_bp
#' @param pit_dims number, length, width and depth of borrow pits (list)
#' @return Area/Volume of peat removed due to borrow pits
#' @export
removal_bp <- function(pit_dims) {

  # THIS FUNCTION...
  area_removed_bp <- pit_dims$n_pit * pit_dims$l_pit * pit_dims$w_pit
  vol_removed_bp <- area_removed_bp * pit_dims$d_pit

  return(list(a = area_removed_bp,
              v = vol_removed_bp))
}

#' removal_f
#' @param f_dims foundation dimensions (list)
#' @return Area/Volume of peat removed due to foundations
#' @export
removal_f <- function(f_dims) {

  # THIS FUNCTION...

  removal_for_turb <- function(l_s, w_s, l_b, w_b, d) {
    return(((d/3) * (l_s*w_s + l_b*w_b + sqrt(l_s*w_s*l_b*l_s)))/d)
  }

  if (class(l_f)=="list") { # dimensions passed as list: handle each area individually

    # removal
    removal_per_turb <- lapply(Map(list, f_dims$l_s, f_dims$w_s, f_dims$l_b, f_dims$w_b, f_dims$d),
                               FUN=function(x) removal_for_turb(l_s = unlist(x[[1]]),
                                                                w_s = unlist(x[[2]]),
                                                                l_b = unlist(x[[3]]),
                                                                w_b = unlist(x[[4]]),
                                                                d = unlist(x[[5]])))

    # multiply by n_turb
    removal_f <- list_op(removal_per_turb, f_dims$n, func = "*")

    # sum across areas
    area_removed_f <- Reduce("+", removal_f)

    # compute volume (unclear why factor 2) and sum
    vol_removed_f <- Reduce("+", list_op(removal_f, f_dims$d, func = "*"))

  } else { # dimensions passed as vector: single area/areas pooled
    area_removed_f <- removal_for_turb(f_dims$l_s, f_dims$w_s, f_dims$l_b, f_dims$w_b, f_dims$d) * f_dims$n

    vol_removed_f <- area_removed_f * f_dims$d
  }

  return(list(a = area_removed_f,
              v = vol_removed_f))
}

#' removal_h
#' @param h_dims hardstand dimensions (list)
#' @return Area/Volume of peat removed due to hardstanding
#' @export
removal_h <- function(h_dims) {

  # THIS FUNCTION...

  removal_for_turb <- function(l_s, w_s, l_b, w_b, d) {
    return((l_b * w_b) + (w_s * (l_s - l_b) * 0.5) + (l_s * (w_s - w_b) * 0.5) + 0.5 * ((l_s - l_b) * (w_s - w_b)))
  }

  if (class(l_h)=="list") { # dimensions passed as list: handle each area individually

    # removal
    removal_per_turb <- lapply(Map(list, h_dims$l_s, h_dims$w_s, h_dims$l_b, h_dims$w_b, h_dims$d),
                               FUN=function(x) removal_for_turb(l_s = unlist(x[[1]]),
                                                                w_s = unlist(x[[2]]),
                                                                l_b = unlist(x[[3]]),
                                                                w_b = unlist(x[[4]]),
                                                                d = unlist(x[[5]])))

    # multiply by n_turb
    removal_h <- list_op(removal_per_turb, h_dims$n, func = "*")

    # sum across areas
    area_removed_h <- Reduce("+", removal_h)

    # compute volume (unclear why factor 2) and sum
    vol_removed_h <- Reduce("+", list_op(removal_h, h_dims$d, func = "*"))

  } else { # dimensions passed as vector: single area/areas pooled
    area_removed_h <- removal_for_turb(h_dims$l_s, h_dims$w_s, h_dims$l_b, h_dims$w_b, h_dims$d) * h_dims$n

    vol_removed_h <- area_removed_h * h_dims$d
  }

  return(list(a = area_removed_h,
              v = vol_removed_h))
}

#' removal_at
#' @param at_dims access track dimensions (list)
#' @return Area/Volume of peat removed due to access tracks
#' @export
removal_at <- function(at_dims) {

  # THIS FUNCTION...

  # Floating roads
  area_removed_float <- at_dims$float$l * at_dims$float$w
  vol_removed_float <- area_removed_float* at_dims$float$d

  # Excavated tracks
  area_removed_track <- at_dims$track$l * at_dims$track$w
  vol_removed_track <- area_removed_track * at_dims$track$d

  # Rock-filled roads
  area_removed_rock <- at_dims$rock$l * at_dims$rock$w
  vol_removed_rock <- area_removed_rock * at_dims$rock$d

  # Total drainage due to access tracks
  area_removed_at <- area_removed_float + area_removed_track + area_removed_rock

  vol_removed_at <- vol_removed_float + vol_removed_track + vol_removed_rock

  return(list(a = area_removed_at,
              v = vol_removed_at))
}

#' Vol_peat_removed
#' @param pit_dims borrow pit dimensions (list)
#' @param f_dims foundation dimensions (list)
#' @param h_dims hardstand dimensions (list)
#' @param at_dims access track dimensions (list)
#' @param add_dims additional excavation dimensions (list)
#' @return Area/Volume of peat removed
#' @export
Vol_peat_removed <- function(pit_dims,
                             f_dims,
                             h_dims,
                             at_dims,
                             add_dims) {

  # THIS FUNCTION...

  ## Removal due to borrow pits
  removed_bp <- removal_bp(pit_dims = pit_dims)

  area_removed_bp <- removed_bp$a
  vol_removed_bp <- removed_bp$v

  ## Removal due to foundations
  removed_f <- removal_f(f_dims = f_dims)

  area_removed_f <- removed_f$a
  vol_removed_f <- removed_f$v

  ## Removal due to hardstanding
  removed_h <- removal_h(h_dims = h_dims)

  area_removed_h <- removed_h$a
  vol_removed_h <- removed_h$v

  ## Removal due to access tracks
  removed_at <- removal_at(at_dims = at_dims)

  area_removed_at <- removed_at$a
  vol_removed_at <- removed_at$v

  ## Removal due to additional excavation
  area_removed_add <- add_dims$a
  vol_removed_add <- add_dims$v

  ## Total Removal
  area_removed <- area_removed_bp + area_removed_f + area_removed_h + area_removed_at + area_removed_add
  vol_removed <- vol_removed_bp + vol_removed_f + vol_removed_h + vol_removed_at + vol_removed_add

  return(list(Borrow.pits = list(a = area_removed_bp,
                                 v = vol_removed_bp),
              Foundations = list(a = area_removed_f,
                                 v = vol_removed_f),
              Hard.standing = list(a = area_removed_h,
                                 v = vol_removed_h),
              Access.tracks = list(a = area_removed_at,
                                   v = vol_removed_at),
              Add.excavation = list(a = area_removed_add,
                                    v = vol_removed_add),
              Total = list(a = area_removed,
                           v = vol_removed)))
}
