## Helper functions

#' list_op
#' @param l1 list object 1
#' @param l2 list_object 2
#' @param func string describing elementwise operation
#' @return outcome of elementwise operation
#' @export
list_op <- function(l1, l2, func) {

  # THIS FUNCTION...

  if (func == "+") {
    res <- lapply(seq_along(l1), FUN=function(x) unlist(unname(l1[x])) + unlist(unname(l2[x])))
  } else if (func == "*") {
    res <- lapply(seq_along(l1), FUN=function(x) unlist(unname(l1[x])) * unlist(unname(l2[x])))
  } else if (func == "*0.5") {
    res <- lapply(seq_along(l1), FUN=function(x) 0.5 * unlist(unname(l1[x])) * unlist(unname(l2[x])))
  } else if (func == "max") {
    res <- lapply(seq_along(l1), FUN=function(x) apply(cbind(unlist(unname(l1[x])), unlist(unname(l2[x]))), MAR=1, FUN=max))
  }

  names(res) <- names(l1)

  return(res)
}
