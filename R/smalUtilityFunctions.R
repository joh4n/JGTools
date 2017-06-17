#' Sets zeros to Na's in vectors and other stuff and returns
#'
#' This function removes the intercept bahavior of all the varibles in a decomposition and returns the new decompsition
#' @author Johan Gudmundsson, \email{jgu@blackwoodseven.com}
#' @param x 	vector or matrix, hopfully  
#' @return A vector or matrix
#' @export
zerosToNa <- function(x){
  x[x==0] <- NA
  return(x)
}

#' Checks if variance excits for selected columns in a data frame, and retuns the column names where variance excist
#'
#' This function checks if variance excits for selected columns in a data frame, and retuns the column names where variance excist
#' @author Johan Gudmundsson, \email{jgu@blackwoodseven.com}
#' @param names 	names of columns to be checked 
#' @param df a data frame with a date column 
#' @param startDate the starting date of where the evaluation should start
#' @return a character vector
#' @export
checkIfVarinaceExcist <- function(names, df, startDate){
  okNames <- c()
  for( nn in names){
    if(sd(df[which(df$date>=startDate),nn]) >0){
      okNames <- c(okNames, nn)
    }
  }
  return(okNames)
}

#' Checks if the names of a character vector or colnames of a data frames are valid C varibles
#'
#' Checks if the names of a character vector or colnames of a data frames are valid C varibles
#' @author Johan Gudmundsson, \email{jgu@blackwoodseven.com}
#' @param x 	data frame or caracter vectror
#' @return a character vector with names who are not valid C varibles, if it returns
#' character(0) all names are fine
#' @importFrom dautility stringsToCVariable
#' @export
checkCVarNames <- function(x){
  if(class(x) == "data.frame" ){
    # n <- names(x)[names(x)!=stringsToCVariable(names(x))]
    x <- names(x)
  }
  n <- x[x!=stringsToCVariable(x)]
  return(n)
}

#' Subtract the min value of a vector from each elemnt in the vector
#'
#' Checks if the names of a character vector or colnames of a data frames are valid C varibles
#' @author Johan Gudmundsson, \email{jgu@blackwoodseven.com}
#' @param x 	vectror
#' @return vector vector with same structure as input vector
#' @export
subtractMin <- function(x) {
  x<- x-min(x, na.rm = T)
}

#' Shifts columns so that the min value for each column in a data frame becomes zero 
#'
#' Shifts columns so that the min value for each column in a data frame becomes zero, 
#' if a column is named 'date' will not be affected by the transformation
#' @author Johan Gudmundsson, \email{jgu@blackwoodseven.com}
#' @param df 	data frame 
#' @return data frame with same structure as the input data frame
#' @export
shiftMinToZero<- function(df){ 
  temp<- as.data.frame(lapply(df[, names(df) != 'date'],subtractMin))
  if(!is.null(df$date)){
    temp <- cbind(as.Date(df$date), temp)
    names(temp)[1]='date'
  }
  return(temp)
}