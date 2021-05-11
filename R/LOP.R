#' Implement probability averaging
#'
#' Given a set of cumulative distribution functions  (assume for now: defined on same values),
#'   combine using probability averaging (also called linear opinion pool). #TODO CITE
#'   This method calculates the (weighted) average of quantiles at a given value
#'
#' @param data
#'
#' @export
LOP <- function(data, ret_quantiles, weight_fn = equal_weights, ...){
  # interpolate team cdfs
  interp_functions <- create_interp_fns(data)
  if(length(interp_functions) == 0){return(NA)}
  # find min/max values across all teams (used as min/max values for agg cdf)
  limits = c(min(data$value),max(data$value))
  if(anyNA(limits)){return(NA)}
  vals <- seq(limits[1], limits[2], length.out = 1000)
  df_long <- evaluate_cdf(vals, interp_functions)
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

create_interp_fns <- function(dat){
  # create list to store interpolations
  interp_functions <- list()
  # for each id, subset df and create interpolation
  for(i in unique(dat$id)){
    df_sub <- subset(dat, id == i)
    interp_functions[[i]] <- approxfun(x = df_sub$value,
                                       y = df_sub$quantile,
                                       method = "linear",
                                       yleft = 0, yright = 1, rule = 2, ties = list("ordered", mean))
  }
  return(interp_functions)
}

evaluate_cdf <- function(vals, interp_functions){
  # for each id, estimate quantile for each value in vals
  cdf_out <- lapply(interp_functions, function(fn){fn(vals)}) #if(!is(fn,"function")){exit()};
  # Find long dataset (easier for plotting in ggplot)
  df_long <- reshape2::melt(cdf_out)
  names(df_long) <- c("quantile", "id")
  df_long$value <- rep(vals, length(cdf_out))
  df_long <- df_long %>% dplyr::filter(!is.na(quantile))
  return(df_long)
}

#### CALCULATE LOP ####

avg_probs <- function(df_cdfs){
  gdf_cdf <- df_cdfs %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(quantile = weighted.mean(quantile, weight))
  return(gdf_cdf)
}

