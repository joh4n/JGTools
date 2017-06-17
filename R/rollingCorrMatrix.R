
#' Calculates the roolingCorrelation matrix and returns the uniqe correlations ranked
#'
#' alculates the roolingCorrelation matrix and returns the uniqe correlations ranked
#' @author Johan Gudmundsson, \email{jgu@blackwoodseven.com}
#' @param df data frame
#' @param removeConstantColumns removes columns in df which are constant
#' @param tolerance the lowest accepted correlation
#' @param window the window of which the correlation should be calculated
#' @return a data frame
#' @importFrom zoo rollapply
#' @importFrom dplyr group_by
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @export
rollingCorMatrix <- function(df, removeConstantColumns = T, tolerance=.6, window = 100){

mycor <- function(x) {
  rval <- cor(x)
  rval[lower.tri(rval)]
}

if(removeConstantColumns){
  tempDf <- df[,!apply(df, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
}else{
  tempDf <- df
}
xx <- cor(tempDf[1:5,])
yy <- rollapply(tempDf, width = window, FUN = mycor, by.column = FALSE, align='right')


result <- data.frame(Var1 = character(1), Var2 = character(1), correlation = numeric(1), windowLocation = numeric(1))
for(ii in 1:dim(yy)[1]){
  # for(ii in 1:5){
  ind <- which( lower.tri(xx,diag=F) , arr.ind = TRUE )
  z <- data.frame( Var1 = dimnames(xx)[[2]][ind[,2]] ,
                   Var2 = dimnames(xx)[[1]][ind[,1]] ,
                   correlation = yy[ii, ],
                   #correlation = xx[ii, ind ]
                   windowLocation = ii)
  z=na.omit(z)  #Get rid of the junk
  z=z[order(-abs(z$correlation)),]
  rownames(z)<- NULL
  
  
  temp <- z[z$correlation>tolerance,]
  if(nrow(temp)>0){
    result <- rbind(result, temp)
  }
}



# result2 <- result
# result2=result2[order(-abs(result2$correlation)),]
# rownames(result2)<- NULL


# result3 <- result %>%
#   na.omit() %>%
#   group_by(Var1, Var2)%>% 
#   summarise(correlation = max(abs(correlation)))
#   
# result3=result3[order(-abs(result3$correlation)),]
# rownames(result3)<- NULL


result4 <- result %>%
  na.omit() %>%
  group_by(Var1, Var2)%>% 
  filter( correlation ==max(abs(correlation)))
result4=result4[order(-abs(result4$correlation)),]
rownames(result4)<- NULL
return(result4)
}

# uniqeCorr <- unique(result4$correlation)
# 
# pNames <- c()
# for(ii in 1:length(uniqeCorr)){
# temp <- result4[result4$correlation==uniqeCorr[ii],]
# problemNames <- c(as.character(temp$Var1), as.character(temp$Var2))
# 
# occurance <- as.data.frame(table(problemNames))
# occurance2 <- occurance[occurance$Freq>1,]
# print(as.character(occurance2$problemNames))
# pNames <- c(pNames, as.character(occurance2$problemNames))
# }
# plotDfCols2(df[, pNames[1]])
# plotCol(df[, pNames[1]])
