# CUSTOMISED GGPLOT ----

#' A util function to plot a customized ggplot
#' @noRd
violinPlot <- function(data, x, y, order = NULL, colors = NULL){
  data %>%
    ggplot2::ggplot(ggplot2::aes_string(x, y, fill = x)) +
    ggplot2::geom_violin(show.legend = FALSE) +
    ggplot2::geom_boxplot(width = 0.1, color = 'white') +
    ggplot2::scale_x_discrete(limits = order) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(title = paste0("Distribution of ", y, " in Different ", x, " Categories")) +
    ggplot2::theme_minimal()
}


# VIOLIN PLOTS FOR NUMERIC FEATURES SPLIT IN TARGET FEAT CATEGORIS ----

#' A function to display or save violin plots created on all numeric features.
#'
#' @description Creates (or saves as png if asked) violin plots for all numeric features. Split the violin plots based on target feature categories.
#'
#' @param dataset Name of the data frame object
#' @param classVar Name of the target feature
#' @param order A vector listing the target feature labels in the desired order. To use default order leave this parameter. Default value NULL.
#' @param colors A vector listing which color to use to represent which target feature label. To have the function pick color leave this parameter. Deafult value NULL.
#' @param loc A string with the directory where you want to save the plots. If no location is provided the plots will be created and displayed but not stored as image files.
#' @param facet Name of the features along which to separate the plots as facets
#'
#' @return
#'
#' Returns three outputs:
#' 1. If loc = NULL, returns plots and displays in the RStudio Plots window,
#' 2. If loc has location provided, creates and saves the plots. Doesn't display in RStudio.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' biv_violin_plot(dataset = iris,
#'     classVar = Species,
#'              order = c("virginica", "versicolor", "setosa"),
#'                         colors = c("virginica" = "#32a897", "versicolor" = "#328bab", "setosa" = "#8031ad"))
#'
#'
#' @export

biv_violin_plot <- function(dataset, classVar, order = NULL, colors = NULL, facet = NULL, loc = NULL) {

  x <- rlang::enquo(classVar) %>% rlang::get_expr()
  nLevels <- dplyr::select(dataset, {{classVar}}) %>% unique() %>% nrow()
  # producing random color n case no color is provided
  if(is.null(colors)){
    colors = randomcoloR::randomColor(nLevels, luminosity = "bright")
  }

  if(is.null(loc)){
    for(i in names(dplyr::select(dataset, where(is.numeric)))) {
      print(dataset %>%
              violinPlot(x = x, y = i, order = order, colors = colors) +
              wrap_by({{facet}})
      )
    }
  } else{
    for(i in names(dplyr::select(dataset, where(is.numeric)))) {
      png(paste0(loc, "/boxplot_", i, ".PNG"), width = 627, height = 453)
      plot <- dataset %>%
        violinPlot(x = x, y = i, order = order, colors = colors) +
        wrap_by({{facet}})
      print(plot)
      dev.off()
    }
  }
}
