## Helper functions

#' list_op
#' @param l1 list object 1
#' @param l2 list_object 2
#' @param func string describing elementwise operation
#' @return outcome of elementwise operation
#' @export
list_op <- function(l1, l2, l3 = NULL, func) {

  # THIS FUNCTION...

  if (func == "+") {
    res <- lapply(seq_along(l1), FUN=function(x) unlist(unname(l1[x])) + unlist(unname(l2[x])))
  } else if (func == "-") {
    res <- lapply(seq_along(l1), FUN=function(x) unlist(unname(l1[x])) - unlist(unname(l2[x])))
  } else if (func == "*") {
    res <- lapply(seq_along(l1), FUN=function(x) unlist(unname(l1[x])) * unlist(unname(l2[x])))
  } else if (func == "*0.5") {
    res <- lapply(seq_along(l1), FUN=function(x) 0.5 * unlist(unname(l1[x])) * unlist(unname(l2[x])))
  } else if (func == "max") {
    res <- lapply(seq_along(l1), FUN=function(x) apply(cbind(unlist(unname(l1[x])), unlist(unname(l2[x]))), MAR=1, FUN=max))
  }

  if (!is.null(l3)) {
    if (func == "+") {
      res <- lapply(seq_along(res), FUN=function(x) unlist(unname(res[x])) + unlist(unname(l3[x])))
    } else if (func == "-") {
      res <- lapply(seq_along(res), FUN=function(x) unlist(unname(res[x])) - unlist(unname(l3[x])))
    } else if (func == "*") {
      res <- lapply(seq_along(res), FUN=function(x) unlist(unname(res[x])) * unlist(unname(l3[x])))
    } else if (func == "*0.5") {
      res <- lapply(seq_along(res), FUN=function(x) unlist(unname(res[x])) * unlist(unname(l3[x])))
    } else if (func == "max") {
      res <- lapply(seq_along(res), FUN=function(x) apply(cbind(unlist(unname(res[x])), unlist(unname(l3[x]))), MAR=1, FUN=max))
    }
  }

  names(res) <- names(l1)

  return(res)
}
