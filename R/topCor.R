#' A function to rank correlation coefficients
#'
#' This function sorts the lower triangular part of the correlation matrix in descending order according to the absolute value of the correlation coefficient
#' @author Johan Gudmundsson, \email{jgu@blackwoodseven.com}
#' @param df Data frame which should be annalyzed
#' @param pairwiseCompleteObs logical, turn on the option use="pairwise.complete.obs" in the calculation of the correlation coefficents
#' @param removeDateCol logical, if true, a column named 'date' will be removed if it excists
#' @param method character vector, the method the cor function should use, ex.  c("pearson", "kendall", "spearman")
#' @param allCorrelationMethods logical, uses method = c("pearson", "kendall", "spearman") independet of waht argument is specified in the method argument
#' @return A data frame with the correlation coefficents in descending order
#' @examples
#' df = data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
#' head(topCor(df))
#' @importFrom dplyr rename
#' @importFrom dplyr %>%
#' @export
topCor <-  function(df, pairwiseCompleteObs=F, removeDateCol=T, allCorrelationMethods = F, method = c("pearson")){
  if(allCorrelationMethods){
    method = c("pearson", "kendall", "spearman")
  }
  result <- list()
  if(removeDateCol && class(df)=="data.frame"){
    df$date <- NULL
  }
  for(nn in method){
    
    if(pairwiseCompleteObs){
      correlationM <- cor(df, use="pairwise.complete.obs", method =nn) 
    }else{
      correlationM <- cor(df, method =nn) 
    }
    z <- correlationM
    z[lower.tri(z,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
    z=as.data.frame(as.table(z))  #Turn into a 3-column table
    z=na.omit(z)  #Get rid of the junk
    z=z[order(-abs(z$Freq)),]
    rownames(z)<- NULL
    if(length(method)==1){
      # z <- z %>% rename(correlation = Freq)
      names(z) = c("Var1", "Var2", 'correlation')
    }else{
      names(z) = c("Var1", "Var2", nn)
    }
    result[[nn]] <- z
  }
  result = Reduce(function(a, b) full_join(a, b, by=c("Var1", "Var2")), result)
  return(result)
}