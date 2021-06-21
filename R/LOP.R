
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
#' @import data.table
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
  browser()
  df_long <- evaluate_cdf(data, vals)
  df_weighted <- weight_fn(df_long, ...)
  df_weighted <- remove_zero_weights(df_weighted)
  df_agg <- calculate_aggregate_LOP(df_weighted, ret_quantiles)
  return(df_agg$value)
}

calculate_aggregate_LOP <- function(data, ret_quantiles){ #model_weights
  df_cdfs <- avg_probs(data)
  agg <- return_specified_quantiles(df_cdfs, ret_quantiles)
  return(agg)
}

#### PREP DISTRIBUTION FOR LOP ####

evaluate_cdf <- function(data, ret_vals){
  # create df to store interpolations
  interp_functions <- list()
  # for each id, subset df and create interpolation
  for(i in unique(data$id)){
    data_sub <- data[which(id == i),]
    if(data.table::uniqueN(data_sub$value) == 1){
      interp_functions[[i]] <- list(quantile = ifelse(ret_vals < unique(data_sub$value), 0, 1), id = i, value = ret_vals)
    }
    else{
      interp <- approx(x = data_sub$value, y = data_sub$quantile,
                       xout = ret_vals, method = "linear",
                       yleft = 0, yright = 1, rule = 2, ties = list("ordered", max))
      interp_functions[[i]] <- list(quantile = interp$y, id = i, value = interp$x)
    }
  }
  return(rbindlist(interp_functions))
}


#### CALCULATE LOP ####

avg_probs <- function(df_cdfs){
  gdf_cdf <- df_cdfs[, .(quantile = weighted.mean(quantile, weight)), by = "value"]
  return(gdf_cdf)
}

