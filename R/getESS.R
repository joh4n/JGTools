#' A function to calculate the ESS of a STAN_fit
#'
#' This function returns the ESS of a STAN_fit ascending order
#' @param min_ess the lowest ESS to not be returned
#' @param STAN_fit a stan fit df
#' @return A data frame with the ESS below the min_ess limit in ascending order
#' @examples
#' getESS( min_ess = 900, STAN_fit = sfitdf)
#' @export
getESS <- function(min_ess = 400, STAN_fit = NULL ){
  if(is.null(STAN_fit)){
    ess_minimums <- sort(ess_warn(STAN_fit = sfitdf, min_ess = min_ess))
  }else{
    ess_minimums <- sort(ess_warn(STAN_fit = STAN_fit, min_ess = min_ess))
  }
  ess_minimums <- data.frame(Parameter = names(ess_minimums), ESS = ess_minimums)
  rownames(ess_minimums) <- NULL
  return(ess_minimums)
}

