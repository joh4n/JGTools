#' Removes the intercept bahavior of varibles in a decomposition
#'
#' This function removes the intercept bahavior of all the varibles in a decomposition and returns the new decompsition
#' @author Johan Gudmundsson, \email{jgu@blackwoodseven.com}
#' @param decomp The decomposition which should be fixed
#' @param intercept The intercept column in the decomp as a string
#' @return A decomposition
#' @examples
#' removeInterceptBehavior(removeInterceptBehavior, intercept='allOnes')
#' 
#' @export
removeInterceptBehavior <- function(decomp, intercept){
  decompNames <- names(decomp)[names(decomp) != intercept]
  
  for(nn in decompNames){
    if(max(decomp[[nn]]) > 0 && min(decomp[[nn]])){
      print('oscilating')
    }
    if(max(decomp[[nn]])<0){
      print('below zero')
      decomp <- setMaxToBase(decomp = decomp,baseName =  intercept, nn)
    }
    if(min(decomp[[nn]])>0){
      print('above zero')
      decomp <- setMinToBase(decomp = decomp,baseName =  intercept, nn)
    }
  }  
  class(decomp) <- c("decomposition" ,"data.frame" )
  return(decomp)
}


#' Moves the min value of a varible in a decomposition to the intercept
#'
#' This function Sets the min value of a varible in a decomposition to the intercept and removes it from the varible
#' @author Johan Gudmundsson, \email{jgu@blackwoodseven.com}
#' @param decomp The decomposition which should be fixed
#' @param baseName The intercept column in the decomp as a string
#' @param varsToBeShifted The colmuns to be shifted 
#' @param removeZeros Logic, If true the function will ignore zeros for the columns that are shifted
#' @return A fixed decomposition
#' @examples
#' setMinToBase(decomp, baseName = 'allOnes', varsToBeShifted = c('trend', 'econmic'))
#' 
#' @export
setMinToBase <- function (decomp, baseName, varsToBeShifted, removeZeros = FALSE) {
  minNonNa <- function(x){
    min(x, na.rm = T)
  }
  if(removeZeros){
    decomp[varsToBeShifted] <- apply(decomp[varsToBeShifted], 2, zerosToNa)
  }
  newDecomp <- decomp
  min <- apply(decomp[varsToBeShifted],2, minNonNa)
  newDecomp[baseName] = newDecomp[baseName] + sum(min)
  for (n in names(min)) {
    newDecomp[n] <- sapply(newDecomp[n], FUN = function(x) x - 
                             min[n])
  }
  
  if(removeZeros){
    newDecomp = newDecomp %>% naToZero()
  }
  
  return(newDecomp)
}


#' Moves the max value of a varible in a decomposition to the intercept
#'
#' This function Sets the max value of a varible in a decomposition to the intercept and removes it from the varible
#' @author Johan Gudmundsson, \email{jgu@blackwoodseven.com}
#' @param decomp The decomposition which should be fixed
#' @param baseName The intercept column in the decomp as a string
#' @param varsToBeShifted The colmuns to be shifted 
#' @param removeZeros Logic, If TRUE the function will ignore zeros for the columns that are shifted
#' @return A fixed decomposition
#' @examples
#' setMaxToBase(decomp, baseName = 'allOnes', varsToBeShifted = c('trend', 'econmic'))
#' 
#' @export
setMaxToBase <- function (decomp, baseName, varsToBeShifted, removeZeros = FALSE) {
  maxNonNa <- function(x){
    max(x, na.rm = T)
  }
  if(removeZeros){
    decomp[varsToBeShifted] <- apply(decomp[varsToBeShifted], 2, zerosToNa)
  }
  newDecomp <- decomp
  max <- apply(decomp[varsToBeShifted],2,maxNonNa)
  newDecomp[baseName] = newDecomp[baseName]+sum(max)
  for (n in names(max)) {
    newDecomp[n] <- sapply(newDecomp[n], FUN = function(x) x - 
                             max[n])
  }
  if(removeZeros){
    newDecomp = newDecomp %>% naToZero()
  }
  return(newDecomp)
}