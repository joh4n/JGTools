#' A function to calculate the RHat of a STAN_fit
#'
#' This function returns the RHat of a stan fit
#' @param STAN_fit a stan fit df
#' @return A data frame with the RHat decending order
#' @examples
#' getRHat(STAN_fit = sfitdf)
#' @export
getRHat <- function(STAN_fit){
  rhat_maximums <- sort(rhat_warn(STAN_fit = STAN_fit, rhat_dev = 0.01), decreasing = TRUE)
  rhat_maximums <- data.frame(Parameter = names(rhat_maximums), rhat = rhat_maximums)
  rownames(rhat_maximums) <- NULL
  # rhat_maximums <- head(rhat_maximums, n = 10)
  return(rhat_maximums)
}
