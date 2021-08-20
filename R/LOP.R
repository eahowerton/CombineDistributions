#' Implement probability averaging
#'
#' Given a set of cumulative distribution functions  (assume for now: defined on same values),
#'   combine using probability averaging (also called linear opinion pool). #TODO CITE
#'   This method calculates the (weighted) average of quantiles at a given value
#'
#' @param quantile vector containing quantiles for all cdfs to be aggregated
#' @param value vector containing values for all cdfs to be aggregated
#' @param id vector containing unqiue ids to distinguish cdf to be aggregated
#' @param ret_quantiles vector of quantiles to return specifying the aggregate distribution
#' @weight_fn function? specifying how to weight each model *FIX
#'
#' @return vector of values for corresponding \code{ret_quantiles} of aggregate distribution
#'
#' @examples
#' dat <- expand.grid(id = c("A", "B"),
#'                    quantile = seq(0,1,0.01))
#' dat$value <- ifelse(dat$id == "A", qnorm(dat$quantile), qnorm(dat$quantile, 0,2))
#' LOP(dat$quantile, dat$value, dat$id, seq(0,1,0.05))
#'
#' @export
LOP <- function(quantile, value, id, ret_quantiles, weight_fn = equal_weights, ...){
  # find min/max values across all teams (used as min/max values for agg cdf)
  vals <- unique(value)
  if(length(vals)>1000){
    limits = c(min(value),max(value))
    if(anyNA(limits)){return(NA)
    }
    vals <- seq(limits[1], limits[2], length.out = 1000)}
  # interpolate individual cdfs to same values
  df_long <- evaluate_cdf(quantile, value, id, vals)
  df_weighted <- weight_fn(df_long, ...)
  df_weighted <- remove_zero_weights(df_weighted)
  df_agg <- calculate_aggregate_LOP(df_weighted, ret_quantiles)
  return(df_agg$value)
}

#### HELPERS ####
#' @export
calculate_aggregate_LOP <- function(data, ret_quantiles){ #model_weights
  df_cdfs <- avg_probs(data)
  agg <- return_specified_quantiles(df_cdfs, ret_quantiles)
  return(agg)
}

evaluate_cdf <- function(quantile, value, id, ret_vals){
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
        dplyr::bind_rows(data.frame(quantile = ifelse(ret_vals < unique(val_sub), 0, 1), id = i, value = ret_vals))
    }
    else{
      interp <- approx(x = val_sub,
                       y = quant_sub,
                       xout = ret_vals,
                       method = "linear",
                       yleft = 0, yright = 1, rule = 2, ties = list("ordered", max))
      interp_functions = interp_functions %>% dplyr::bind_rows(data.frame(quantile = interp$y, id = i, value = interp$x))
    }
  }
  return(interp_functions)
}

avg_probs <- function(df_cdfs){
  gdf_cdf <- df_cdfs %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(quantile = weighted.mean(quantile, weight))
  return(gdf_cdf)
}

