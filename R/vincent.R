#' Implement quantile averaging
#'
#' Given a set of cumulative distribution functions  (assume for now: defined on same values),
#'   combine using quantile averaging (also called Vincent average).
#'   This method calculates the (weighted) average of values at a given quantile
#'
#' @inheritParams calculate_aggregate_LOP
#' @return A data.frame with \code{quantile} and \code{value} of quantile averaged aggregate.
#'
#' @examples
#' dat <- expand.grid(id = c("A", "B"),
#'                    quantile = seq(0,1,0.01))
#' dat$value <- ifelse(dat$id == "A", qnorm(dat$quantile), qnorm(dat$quantile, 0,2))
#' calculate_aggregate_vin(dat, seq(0,1,0.05))
#'
#' @export
vincent <- function(quantile, value, id, ret_quantiles, weight_fn = equal_weights, ...){
  data = data.table::data.table(id = id, quantile = quantile, value = value)
  df_weighted <- weight_fn(data, ...)
  df_weighted <- remove_zero_weights(df_weighted)
  df_agg <- calculate_aggregate_vin(df_weighted, ret_quantiles)
  return(df_agg$value)
}

calculate_aggregate_vin <- function(data, ret_quantiles){ #model_weights
  #dat <- left_join(dat, model_weights)
  # calculate vincent average
  vinc <- data %>%
    dplyr::group_by(quantile) %>%
    dplyr::summarise(value= weighted.mean(value, weight))
  vinc_agg <- return_specified_quantiles(vinc, ret_quantiles)
  return(vinc_agg)
}


