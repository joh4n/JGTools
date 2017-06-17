#' A function retuns a data.frame with the correlations in the BModelList for the selected kpi
#'
#' This function sorts the lower triangular part of the correlation matrix for the fitted varibels in the BModelList
#' @author Johan Gudmundsson, \email{jgu@blackwoodseven.com}
#' @param kpi kpi which should be investigated
#' @param BModelList the fitted BModelList
#' @return A data frame with the correlation coefficents in descending order
#' @examples
#' correlationsInBmodelList(kpi = 'transactions', BModelList = bModelList)
#' @export
correlationsInBmodelList <- function(kpi, BModelList){
  return(topCor(extractVariblesFromBModelList(kpi, BModelList)))
}