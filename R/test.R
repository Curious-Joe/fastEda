#' Test Function
#'
#' @param x A text string
#'
#' @return
#' The output from \code{\link{print}}
#'
#' @examples
#' hello("John")
#'
#'
#' @export
#'
hello <- function(x){
  print(paste0("Hello, ", x))
}
