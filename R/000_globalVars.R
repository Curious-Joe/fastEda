globalVariables(names = c(
  "where"
))

wrap_by <- function(...) {
  ggplot2::facet_wrap(vars(...), labeller = label_both)
}
