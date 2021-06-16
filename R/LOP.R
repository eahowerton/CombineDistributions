#' Implement probability averaging
#'
#' Given a set of cumulative distribution functions  (assume for now: defined on same values),
#'   combine using probability averaging (also called linear opinion pool). #TODO CITE
#'   This method calculates the (weighted) average of quantiles at a given value
#'
#' @param data data.frame containing \code{quantile}, \code{value} columns
#' @param ret_quantiles vector of quantiles to return specifying the aggregate distribution
#' @weight_fn function? specifying how to weight each model *FIX
#'
#' @export
LOP <- function(data, ret_quantiles, weight_fn = equal_weights, ...){
  # find min/max values across all teams (used as min/max values for agg cdf)
  vals <- unique(data$value)
  if(length(vals)>1000){
    limits = c(min(data$value),max(data$value))
    if(anyNA(limits)){return(NA)
    }
    vals <- seq(limits[1], limits[2], length.out = 1000)}
  # interpolate individual cdfs to same values
  df_long <- evaluate_cdf(data, vals)
  df_weighted <- weight_fn(df_long, ...)
  df_weighted <- remove_zero_weights(df_weighted)
  df_agg <- calculate_aggregate_LOP(df_weighted, ret_quantiles)
  return(df_agg)
}

calculate_aggregate_LOP <- function(data, ret_quantiles){ #model_weights
  df_cdfs <- avg_probs(data)
  agg <- return_specified_quantiles(df_cdfs, ret_quantiles)
  return(agg)
}

#### PREP DISTRIBUTION FOR LOP ####

evaluate_cdf <- function(dat, vals){
  # create df to store interpolations
  interp_functions <- data.frame(quantile = double(),
                                 id = character(),
                                 value = double())
  # for each id, subset df and create interpolation
  for(i in unique(dat$id)){
    df_sub <- subset(dat, id == i)
    if(length(unique(df_sub$value)) == 1){
      interp_functions = rbind(interp_functions, data.frame(quantile = rep(unique(df_sub$value), length(vals)),
                                                            id = i, value = vals))
    }
    else{
      interp <- approx(x = df_sub$value,
                       y = df_sub$quantile,
                       xout = vals,
                       method = "linear",
                       yleft = 0, yright = 1, rule = 2, ties = list("ordered", mean))
      interp_functions = rbind(interp_functions, data.frame(quantile = interp$y, id = i, value = interp$x))
    }
  }
  return(interp_functions)
}

#### CALCULATE LOP ####

avg_probs <- function(df_cdfs){
  gdf_cdf <- df_cdfs %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(quantile = weighted.mean(quantile, weight))
  return(gdf_cdf)
}

