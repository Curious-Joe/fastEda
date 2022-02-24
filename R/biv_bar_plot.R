# CUSTOMISED GGPLOT ----

#' A util function to plot a customized ggplot
#' @noRd
barPlot <- function(data, x, y, fill, position, colors){
  data %>%
    ggplot2::ggplot(aes_string(x=x, y=y, fill=fill)) +
    ggplot2::geom_bar(position=position, stat="identity") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(title = paste0(x, " in Different Categories of ", fill)) +
    ggplot2::theme_minimal()
}

# BAR PLOTS FOR CATEGORICAL FEATURES SPLIT IN TARGET FEAT CATEGORIS ----

#' A function to display or save density plots created on all categorical features.
#'
#' @description Creates (or saves as png if asked) bar plots for all density features. Organize the bar plots based on target feature categories and desired barplot types of: dodge, stack, fill.
#'
#' @param dataset Name of the data frame object
#' @param classVar Name of the target feature
#' @param order A vector listing the target feature labels in the desired order. To use default order leave this parameter. Default value NULL.
#' @param colors A vector listing which color to use to represent which target feature label. To have the function pick color leave this parameter. Default value NULL.
#' @param barType A string value indicating bar plot type i.e. stack, dodge, fill.
#' @param loc A string with the directory where you want to save the plots. If no location is provided the plots will be created and displayed but not stored as image files.
#' @param facet Name of the features along which to separate the plots as facets
#'
#' @return
#'
#' Returns three outputs:
#' 1. If loc = NULL, returns plots and displays in the RStudio Plots window,
#' 2. If loc has location provided, creates and saves the plots. Doesn't display in RStudio.
#'
#' @importFrom rlang enquo get_expr
#' @import dplyr ggplot2
#' @importFrom forcats fct_relevel
#' @importFrom randomcoloR randomColor
#' @importFrom grDevices dev.off png
#' @importFrom data.table :=
#'
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' biv_bar_plot(dataset = iris %>%
#' mutate(sepal_width_cat = ifelse(Sepal.Width < mean(iris$Sepal.Width), 'Low', 'High')),
#' classVar = Species)
#'
#' @export
#'

biv_bar_plot <- function(dataset, classVar, order = NULL,
                    colors = NULL, barType = "dodge", facet = NULL, loc = NULL) {

  x <- rlang::enquo(classVar) %>% rlang::get_expr()
  nLevels <- dplyr::select(dataset, {{classVar}}) %>% unique() %>% nrow()

  # reordering levels in factor
  if(!is.null(order)){
    dataset <- dataset %>%
      dplyr::mutate({{classVar}} := forcats::fct_relevel({{classVar}}, order))
  }

  # producing random color n case no color is provided
  if(is.null(colors)){
    colors = randomcoloR::randomColor(nLevels, luminosity = "bright")
  }

  # fetching categorical feature names
  cols <- names(dplyr::select(dataset %>%
                                dplyr::select(-all_of(x)), !where(is.numeric)))

  {
    # fetching bar plot type
    if(!barType %in% c("fill", "stack", "dodge")){
      barType = readline(prompt = "Please enter one of these options fill, stack, dodge: ")
    }

    # print("plotting...")
    if(is.null(loc)){
      for(i in cols) {
        print(dataset %>%
                dplyr::select(x, all_of(i)) %>%
                table() %>%
                data.frame() %>%
                barPlot(x = i, y = "Freq", fill = x, position = barType, colors) +
                wrap_by({{facet}})
        )
      }
    } else{

      for(i in cols) {

        png(paste0(loc, "/barplot_", i, ".PNG"), width = 627, height = 453)
        plot <- dataset %>%
          dplyr::select(x, all_of(i)) %>%
          table() %>%
          data.frame() %>%
          barPlot(x = i, y = "Freq", fill = x, position = barType, colors) +
          wrap_by({{facet}})
        print(plot)
        dev.off()
      }
    }
  }
}
