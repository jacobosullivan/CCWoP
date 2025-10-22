## 7a. C sequest. in trees (3PG)

#' get3PGparms
#' @return 3PGparms
#' @export
get3PGparms <- function(forestry.dat) {
  parmname <- c("3PG module",
                "Average annual photosynthetically active radiation",
                "Light extinction coefficient",
                "Specific leaf area",
                "Leaf longevity",
                "Fine root longevity",
                "Maximum light-use efficiency",
                "Total reduction factor for misc environmental effects",
                "Ratio of NPP to GPP",
                "Allocation to foliage",
                "Coefficient for allocation response to nitrogen",
                "Age for 50% reduction in light-use efficiency",
                "Initial foliage biomass",
                "Understorey module",
                "Understorey LAI")

  df0 <- data.frame(Parameter = parmname,
                    Value = NA,
                    Scots.Pine = NA,
                    Sitka.Spruce = NA)

  df <- data.frame(Parameter = parmname,
                   Area.1 = NA,
                   Area.2 = NA,
                   Area.3 = NA,
                   Area.4 = NA,
                   Area.5 = NA)

  # now fill in df taking values/conditionals from forestry.dat
}
