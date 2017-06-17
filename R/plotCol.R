#' Plots a vecktor from a dataframe
#'
#' Plots a column of a data frame
#' @param x a vector from a data.frame to plot
#' @param defaultPalette use default color palette or a slightly prettier one,
#' but limited to 12 colours.
#' @param Legend logical with default FALSE. If set to TRUE, a legend will be added to the plot
#' @return ggplot object
#' #' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 facet_grid
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 theme
#' @importFrom plotly ggplotly
#' @examples
#' plotCol(mydf$grp_tv1)
#' @export
plotCol <- function(x, defaultPalette = FALSE, legend = FALSE, usePlotly = TRUE){

  z <- as.character(substitute(x))
  tempdf <- eval(parse(text=z[2]))
  if (!("date" %in% colnames(tempdf))) {
    tempdf$RowIdx <- seq(nrow(tempdf))
    xColName <- "RowIdx"
    z <- paste0('tempdf','[,c(',"'",'RowIdx',"','",z[length(z)],"'",')]')
    # print(z)
    return(plotDfCols2(eval(parse(text=z)), defaultPalette = defaultPalette, legend = legend, xColName = xColName, usePlotly = usePlotly))
   
  }else{
    z <- paste0(z[2],'[,c(',"'",'date',"','",z[length(z)],"'",')]')
    return(plotDfCols2(eval(parse(text=z)), defaultPalette = defaultPalette, legend = legend, usePlotly = usePlotly))
  }
  
}


