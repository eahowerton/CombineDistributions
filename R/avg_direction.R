#' Implement quantile averaging
#'
#' Given a set of cumulative distribution functions  (assume for now: defined on same values),
#'   combine using quantile averaging (also called Vincent average).
#'   This method calculates the (weighted) average of values at a given quantile

#' @param x A vector that denotes each x value over which cdf is defined
#' @param y A matrix with length(x) rows, and n columns where n is the number of distrbutions to combine
#' @return vin A data.frame with x and y arguments, where x is the values and y is the combined probabilities

calculate_aggregate_vin <- function(x, y){



  vin <- team_cdfs %>%
    dplyr::group_by(quantile) %>%
    dplyr::summarise(value= mean(value))
  return(vin)
}




