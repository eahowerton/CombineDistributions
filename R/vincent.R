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
calculate_aggregate_vin <- function(dat, ret_quantiles){ #model_weights
  #dat <- left_join(dat, model_weights)
  # calcualte vincent average
  vinc <- dat %>%
    dplyr::group_by(quantile) %>%
    dplyr::summarise(value= mean(value))
  # return specified quantiles
  if(all(round(ret_quantiles,4) %in% round(unique(vinc$quantile),4))){ # rounding probably not the best way to handle
    return(vinc %>% dplyr::filter(round(quantile,4) %in% round(ret_quantiles,4)))
  }
  else{
    warning("more quantiles to return than provided, interpolating missing quantiles")
    vinc_interp <- approx(vinc$quantile, vinc$value, xout = ret_quantiles)
    vinc <- tibble::tibble(quantile = vinc_interp$x,
                   value = vinc_interp$y)
    return(vinc)
  }
}

