#' A function to remove correlated covariets
#'
#' This function takes column names and looks for correlations between these columns and drops one of them if the correlation is above the tolerance
#' @param names The columnames to be annalyzed
#' @param tolerance The highest allowed correlation
#' @param myDataFrame Data frame which should be annalyzed
#' @return A data frame with the correlation coefficents in descending order
#' @examples
#' names = c('one', 'kpi', 'sun')
#' removeCorrelatedCovariates(names = names, tolerance = .9, myDataFrame = mydf)
#' @export
removeCorrelatedCovariates <- function(names, tolerance, myDataFrame){
  # topCor <- function(df){
  #   df$date <- NULL
  #   correlationM <- cor(df)
  #   z <- correlationM
  #   z[lower.tri(z,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
  #   z=as.data.frame(as.table(z))  #Turn into a 3-column table
  #   z=na.omit(z)  #Get rid of the junk
  #   z=z[order(-abs(z$Freq)),]
  #   
  #   rownames(z)<- NULL
  #   z <- z %>% rename(correlation = Freq)
  #   #return(head(z, N))
  #   return(z)
  # }
  
  tempNames <- names
  temp <- topCor(mydf[,tempNames ])
  temp <- temp[abs(temp$correlation) > tolerance,]
  while(length(temp$correlation)>0){
    v1 <- temp2[1,"Var1"]
    v2 <- temp2[1,"Var2"]
    s <- c(sum(mydf[[v1]]), sum(mydf[[v2]]))
    ii <- which.min(s)
    
    tempNames <- tempNames[!tempNames==temp[1,ii]]
    if(length(tempNames)>1){
      temp <- topCor(mydf[,tempNames ])
      temp <- temp[abs(temp$correlation) >tolerance,]
    }else{
      break
    }
}

return(tempNames)
}


