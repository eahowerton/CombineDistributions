#' Implement quantile averaging
#'
#' Given a set of cumulative distribution functions  (assume for now: defined on same values),
#'   combine using quantile averaging (also called Vincent average).
#'   This method calculates the (weighted) average of values at a given quantile
#'
#' @inheritParams LOP
#' @return vector of values for corresponding \code{ret_quantiles} of aggregate distribution
#'
#' @examples
#' dat <- expand.grid(id = c("A", "B"),
#'                    quantile = seq(0,1,0.01))
#' dat$value <- ifelse(dat$id == "A", qnorm(dat$quantile), qnorm(dat$quantile, 0,2))
#' vincent(dat$quantile, dat$value, dat$id, seq(0,1,0.05))
#'
#' @export
vincent <- function(quantile, value, id, ret_quantiles, ret_values,
                    weight_fn = equal_weights, ...){
  data = data.table::data.table(id = id, quantile = quantile, value = value)
  # interpolate individual cdfs to same quantiles
  quants <- unique(quantile)
  if(length(quants)>1000){
    limits = c(min(quants),max(quants))
    if(anyNA(limits)){return(NA)
    }
  quants <- seq(limits[1], limits[2], length.out = 1000)}
  df_long <- evaluate_qf(quantile, value, id, quants)
  df_weighted <- weight_fn(df_long, ...)
  df_weighted <- remove_zero_weights(df_weighted)
  df_agg <- calculate_aggregate_vin(df_weighted, ret_quantiles, ret_values)
  return(df_agg)
}

calculate_aggregate_vin <- function(data, ret_quantiles, ret_values){ #model_weights
  #dat <- left_join(dat, model_weights)
  # calculate vincent average
  vinc <- data %>%
    dplyr::group_by(quantile) %>%
    dplyr::summarise(value= weighted.mean(value, weight))
  vinc_agg <- return_specified_quantiles(vinc, ret_quantiles, ret_values)
  return(vinc_agg)
}

#' function to interpolate to consistent quantiles
evaluate_qf <- function(quantile, value, id, ret_quants){
  # create df to store interpolations
  interp_functions <- data.frame(quantile = double(),
                                 id = character(),
                                 value = double())
  # for each id, subset df and create interpolation
  for(i in unique(id)){
    quant_sub <- quantile[which(id == i)]
    val_sub <- value[which(id == i)]
    if(length(unique(val_sub)) == 1){
      interp_functions = interp_functions %>%
        dplyr::bind_rows(data.frame(quantile = quant_vals,
                                    value = val_sub,
                                    id = i))
    }
    else{
      interp <- approx(x = quant_sub,
                       y = val_sub,
                       xout = ret_quants,
                       method = "linear",
                       rule = 2,
                       ties = list("ordered", max))
      interp_functions = interp_functions %>% dplyr::bind_rows(data.frame(quantile = interp$x, id = i, value = interp$y))
    }
  }
  return(interp_functions)
}

