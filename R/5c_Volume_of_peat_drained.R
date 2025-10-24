## 5c. Volume of peat drained

#' area_drained
#' @param drain_ext average extent of drainage around drainage features
#' @param l length of drainage feature
#' @param w width of drainage feature
#' @return Area of drained peat (assuming rectangular drainage profile)
#' @export
area_drained <- function(drain_ext, l, w) {
  return((2 * drain_ext + l) * (2 * drain_ext + w) - (l * w))
}

#' drainage_bp
#' @param drain_ext average extent of drainage around drainage features
#' @param pit_dims number, length, width and depth of borrow pits (list)
#' @return Area/Volume of peat drained due to borrow pits
#' @export
drainage_bp <- function(drain_ext,
                        pit_dims) {

  # THIS FUNCTION...
  area_drained_bp <- pit_dims$n_pit * area_drained(drain_ext, pit_dims$l_pit, pit_dims$w_pit)
  vol_drained_bp <- pit_dims$d_pit * area_drained_bp * 0.5 # not clear why the factor 2 here

  return(list(a = area_drained_bp,
              v = vol_drained_bp))
}

#' drainage_fh
#' @param drain_ext average extent of drainage around drainage features
#' @param fh_dims foundations + hardstanding dimensions
#' @return Area/Volume of peat drained due to foundations + hardstanding
#' @export
drainage_fh <- function(drain_ext,
                        fh_dims) {

  # THIS FUNCTION...

  if (class(fh_dims$l)=="list") { # dimensions passed as list: handle each area individually

    # drainage
    drainage_per_turb <- lapply(Map(list, fh_dims$l, fh_dims$w), FUN=function(x) area_drained(drain_ext, unlist(x[[1]]), unlist(x[[2]])))

    # multiply by n_turb
    drainage_fh <- list_op(drainage_per_turb, fh_dims$n, func = "*")

    # sum across areas
    area_drained_fh <- Reduce("+", drainage_fh)

    # compute volume (unclear why factor 2) and sum
    vol_drained_fh <- Reduce("+", list_op(drainage_fh, fh_dims$d, func = "*0.5"))

  } else { # dimensions passed as vector: single area/areas pooled
    area_drained_fh <- area_drained(drain_ext, fh_dims$l, fh_dims$w) * fh_dims$n

    vol_drained_fh <- 0.5 * area_drained_fh * fh_dims$d
  }

  return(list(a = area_drained_fh,
              v = vol_drained_fh))
}

#' drainage_at
#' @param drain_ext average extent of drainage around drainage features
#' @param at_dims access track dimensions (list)
#' @return Area/Volume of peat drained due to access tracks
#' @export
drainage_at <- function(drain_ext,
                        at_dims) {

  # THIS FUNCTION...

  # Floating roads
  area_drained_float <- at_dims$float$l * (2*drain_ext + at_dims$float$w)
  vol_drained_float <- 0.5 * area_drained_float * at_dims$float$d

  # Excavated tracks
  area_drained_track <- at_dims$track$l * (2*drain_ext)
  vol_drained_track <- 0.5 * area_drained_track * at_dims$track$d

  # Rock-filled roads
  area_drained_rock <- at_dims$rock$l * (2*drain_ext)
  vol_drained_rock <- 0.5 * area_drained_track * at_dims$rock$d

  # Total drainage due to access tracks
  area_drained_at <- area_drained_float + area_drained_track + area_drained_rock

  vol_drained_at <- vol_drained_float + vol_drained_track + vol_drained_rock

  return(list(a = area_drained_at,
              v = vol_drained_at))
}

#' drainage_ct
#' @param drain_ext average extent of drainage around drainage features
#' @param ct_dims cable trench dimensions (list)
#' @return Area/Volume of peat drained due to cable trenches
#' @export
drainage_ct <- function(drain_ext,
                        ct_dims) {

  #THIS FUNCTION...

  area_drained_ct <- ct_dims$l * (2*drain_ext)
  vol_drained_ct <- 0.5 * area_drained_ct * ct_dims$d

  return(list(a = area_drained_ct,
              v = vol_drained_ct))
}

#' drainage_add
#' @param drain_ext average extent of drainage around drainage features
#' @param add_dims additional excavation dimensions (list)
#' @return Area/Volume of peat drained due to additional excavations
#' @export
drainage_add <- function(drain_ext,
                         add_dims) {

  #THIS FUNCTION...
  if (any(add_dims$v > 0)) {
    d_add <- add_dims$a / add_dims$a
    r_add <- sqrt(add_dims$a/pi)
    r_add_drained <- r_add + drain_ext
    area_drained_add <- pi * r_add_drained * r_add_drained - add_dims$a
    vol_drained_add <- area_drained_add * d_add
  } else {
    area_drained_add <- c(Exp=0, Min=0, Max=0)
    vol_drained_add <- c(Exp=0, Min=0, Max=0)
  }

  return(list(a = area_drained_add,
              v = vol_drained_add))
}

#' drainage
#' @param drain_ext average extent of drainage around drainage features
#' @param pit_dims borrow pit dimensions (list)
#' @param fh_dims foundations + hardstanding dimensions
#' @param at_dims access track dimensions (list)
#' @param ct_dims cable trench  dimensions (list)
#' @param add_dims additional excavation dimensions (list)
#' @return Area/Volume of peat drained
#' @export
drainage <- function(drain_ext,
                     pit_dims,
                     fh_dims,
                     at_dims,
                     ct_dims,
                     add_dims) {

  # THIS FUNCTION...

  ## Drainage due to borrow pits
  drained_bp <- drainage_bp(drain_ext = drain_ext,
                            pit_dims = pit_dims)

  area_drained_bp <- drained_bp$a
  vol_drained_bp <- drained_bp$v

  ## Drainage due to foundations + hardstanding
  drained_fh <- drainage_fh(drain_ext = drain_ext,
                            fh_dims = fh_dims)

  area_drained_fh <- drained_fh$a
  vol_drained_fh <- drained_fh$v

  ## Drainage due to access tracks
  drained_at <- drainage_at(drain_ext = drain_ext,
                            at_dims = at_dims)

  area_drained_at <- drained_at$a
  vol_drained_at <- drained_at$v

  ## Drainage due to cable trenches
  drained_ct <- drainage_ct(drain_ext = drain_ext,
                            ct_dims = ct_dims)

  area_drained_ct <- drained_ct$a
  vol_drained_ct <- drained_ct$v

  ## Drainage due to additional excavation
  drained_add <- drainage_add(drain_ext = drain_ext,
                              add_dims = add_dims)

  area_drained_add <- drained_add$a
  vol_drained_add <- drained_add$v

  ## Total drainage
  area_drained <- area_drained_bp + area_drained_fh + area_drained_at + area_drained_ct + area_drained_add
  vol_drained <- vol_drained_bp + vol_drained_fh + vol_drained_at + vol_drained_ct + vol_drained_add

  return(list(Borrow.pits = list(a = area_drained_bp,
                                 v = vol_drained_bp),
              Foundations = list(a = area_drained_fh,
                                 v = vol_drained_fh),
              Access.tracks = list(a = area_drained_at,
                                   v = vol_drained_at),
              Cable.trenches = list(a = area_drained_ct,
                                    v = vol_drained_ct),
              Add.excavation = list(a = area_drained_add,
                                    v = vol_drained_add),
              Total = list(a = area_drained,
                           v = vol_drained)))
}
