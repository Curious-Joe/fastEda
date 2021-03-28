#' A color customizable theme
#'
#' @description This is a customizable ggplot theme. Where a user can change the default background color
#' of ggplot with the color of their choice by providing the hexa-code of the color inside the function.
#'
#'@param color A hexacode starting with `#`.
#'
#' @return
#' A customised theme to be used in ggplots
#'
#' @export

theme_fasteda <- function(color = "#2F4F4F"){
  font <- "sans"   #assign font family up front

  ggplot2::theme_minimal() %+replace%    #replace elements we want to change

    ggplot2::theme(

      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),
      panel.grid.minor.y = element_line(colour = "grey30"),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks

      #text elements
      text = element_text(colour = "#ffffff",
                          margin = margin(t = 1.5, r = 1, b = 1, l = 1.5, unit = "pt")),

      plot.title = element_text(             #title
        family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2,                #raise slightly
        margin = margin(t = 10, unit = "pt")),

      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 14,                #font size
        hjust = 0,                #left align
        vjust = 2,                #raise slightly
        margin = margin(t = 10, unit = "pt")),

      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align

      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10,
        vjust = 2),               #font size

      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 9),                #font size

      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),

      # adding background color
      panel.background = element_rect(fill = color),
      plot.background = element_rect(fill = color),
      legend.background = element_rect(fill = color),
      legend.key = element_rect(fill = color),
      legend.position = "right",
      legend.direction = "vertical"
    )
}
