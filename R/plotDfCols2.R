#' A wraperfunction of plotDfCols 
#'
#' This this a wraperfunction of plotDfCols that exposes the optin to turn of the legend, plotDfCols - plots a data.frame against a specified coulmn
#' @param df data.frame to plot
#' @param xColName name of column to plot against. Default is 'date', but if
#' 'date' is not present, it defaults to row indexes
#' @param defaultPalette use default color palette or a slightly prettier one,
#' but limited to 12 colours.
#' @param facet logical with default FALSE. If set to TRUE, the timeseries are plotted in
#' seperate plots instead of all in one plot
#' @param legend logical with default FALSE. If set to TRUE, the timeseries are plotted with a legend
#' @param ... passed to ggplot2
#' @return ggplot object
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 facet_grid
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom plotly ggplotly
#' @export
plotDfCols2 <-
  function (df, xColName = "date", defaultPalette = FALSE, facet = FALSE, legend = FALSE, usePlotly = TRUE,
            ...) 
  {
    if (xColName == "date" & !("date" %in% colnames(df))) {
      df$RowIdx <- seq(nrow(df))
      xColName <- "RowIdx"
    }
    if(legend){
      ggout <- ggplot(melt(df, id.vars = c(xColName)), aes_string(x = xColName,
                                                                  y = "value", colour = "variable"), ...) + geom_line(size = I(1)) 
    }else{
      ggout <- ggplot(melt(df, id.vars = c(xColName)), aes_string(x = xColName,
                                                                  y = "value", colour = "variable"), ...) + geom_line(size = I(1))+ theme(legend.position="none")
    }
    if (facet) {
      ggout <- ggout + facet_grid(variable ~ ., scales = "free_y")
    }
    if (ncol(df) > 12) {
      # return(ggout)
      ggout = ggout
    }
    else if (ncol(df) > 3 && !defaultPalette) {
      ggout <- ggout + scale_colour_manual(values = brewer.pal(ncol(df) - 
                                                                 1, "Paired"))
      # return(ggout)
    }
    # else return(ggout)
    
    if(usePlotly){
      return(ggplotly(ggout))
    }else{
      return(ggout)
    }
  }