#' A function to extract fitted varibles from a BModelList
#'
#' This function exctracts all the fitted varibles from the BModelList
#' @author Johan Gudmundsson, \email{jgu@blackwoodseven.com}
#' @param kpi The kpi which the varibles should be extracted for
#' @param BModelList The BModelList
#' @return A data frame with extracted varibels
#' @examples
#' extractVariblesFromBModelList(kpi, BModelList)
#' @importFrom dautility rampR
#' @importFrom dautility adstockerR
#' 
#' @export
extractVariblesFromBModelList <- function(kpi, BModelList){
  if(is.null(BModelList$stanfitdf)){
    stop('The BModelList does not seem to be fitted')
  }
  ex <- list()
  for ( nn in BModelList$models[[kpi]]$variables){
    n <- nn$variableName
    y <- NULL
    try(y <- BModelList$models[[kpi]]$variables[[n]]$getVariableSplit(responseVar = kpi, stanfitdf = BModelList$stanfitdf,myDataframe = BModelList$myDataframe), silent = F)
    if (!is.null(y)){
      ex[[length(ex)+1]] <- y
    }else{
      x <- BModelList$models[[kpi]]$getVariable(responseVar = kpi, stanfitdf = BModelList$stanfitdf, myDataframe =  BModelList$myDataframe, obj = BModelList$models[[kpi]]$variables[[n]])
      ex[[length(ex)+1]] <- x
    }
  }
  df <- as.data.frame(ex)
  return(df)
}