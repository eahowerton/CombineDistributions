#' Identify trimmed ids
#'
#' Given empirical cdfs, identify which ids to trim based on mean of distribution.
#'
#' @inheritParams calculate_aggregate_LOP
#' @param keep_vals vector specifying which probabilities to keep at a given value. See details.
#'
#' @details \code{keep_vals} should contain integer values between 1 and \code{n_id},
#'   the number of unique ids. Then, at each value, \code{trim_cdf()} will order probabilities and
#'   keep only those that defined by \code{keep_vals}. For example, if \code{n_id = 5} and
#'   \code{keep_vals = 2:4}, \code{trim_cdf()} will return the second, third and fourth ranked probability,
#'   excluding the smallest and larges probabilities from the average.
mean_trim_ids <- function(dat, keep_vals){
  means <- sapply(unique(dat$id),
                  function(i){m <- cdf_mean(dat %>% filter(id == i));
                  return(list(data.frame(id = i, m)))})
  means <- do.call(rbind, means)
  trim_ids <- means %>% dplyr::arrange(m)
  keep_ids <- trim_ids$id[keep_vals]
  return(keep_ids)
}

#' Approximate mean of ecdf
#'
#' Given an empirical cdf, use triangle rule to estimate mean of distribution.
#'
#' @param cdf data.frame containing \code{quantile} and \code{value} columns
cdf_mean <- function(cdf){
  mn <- 0
  for(i in 1:(length(cdf$quantile)-1)){
    s <- ((cdf$value[i]+cdf$value[i+1])/2)*((cdf$quantile[i+1]-cdf$quantile[i])/100)
    mn <-mn+s
  }
  return(mn)
}

#' Implement mean trimming
#'
#' Trim entire distribution based on mean value ADD REF.
#'
#' @inheritParams calculate_aggregate_LOP
#' @param n_trim integer, number of ids to trim
#' @param trim_type "interior" for omitting central distributions or
#'   "exterior" for omitting outermost distributions
#' @param avg_dir "LOP" for probability averaging or "vincent" for quantile averaging
#' @return data.frame containing \code{quantile} and \code{value} of aggregate distribution,
#'   plus columns specifying \code{direction}, \code{method}, \code{trim_type}, and \code{n_trim}.
#'   TO DO: MAKE THIS NAMING CONSISTENT
mean_trim <- function(dat, n_trim, trim_type, avg_dir, ret_quantiles){
  n_models <- length(unique(dat$id))
  keep_vals <- keep_vals(trim_type, n_trim, n_models)
  if(length(keep_vals$keep) == 0){next}
  keep_ids <- mean_trim_ids(dat, keep_vals$keep)
  d <- dat %>% dplyr::filter(id %in% keep_ids)
  if(avg_dir == "vincent"){
    agg <- calculate_aggregate_vin(d, ret_quantiles)
  }
  else if(avg_dir == "LOP"){
    agg <- calculate_aggregate_LOP(d, 1:length(unique(d$id)),ret_quantiles) #unique(d$id), 1:length(unique(d$id))
  }
  agg$direction <- avg_dir
  agg$method <- "mean_trim"
  agg$type <- trim_type
  agg$n_trim <- keep_vals$n_trim
  return(agg)
}

#' Implement cdf trimming
#'
#' For each unique value, trim probabilities ADD REF.
#'
#' @inheritParams mean_trim
#' @return data.frame containing \code{quantile} and \code{value} of aggregate distribution,
#'   plus columns specifying \code{direction}, \code{method}, \code{trim_type}, and \code{n_trim}.
#'   TO DO: MAKE THIS CONSISTENT
cdf_trim <- function(dat, n_trim, trim_type, avg_dir, ret_quantiles){
  n_models <- length(unique(dat$id))
  keep_vals <- keep_vals(trim_type, n_trim, n_models)
  if(length(keep_vals$keep) == 0){next}
  if(avg_dir == "vincent"){
    d <- sapply(unique(dat$quantile),
                function(i){x <- dat %>% dplyr::filter(quantile == i) %>% dplyr::arrange(value);
                x <- x[keep_vals$keep,];return(list(x))})
    d <- do.call(rbind, d)
    agg <- calculate_aggregate_vin(d, ret_quantiles)
  }
  else if(avg_dir == "LOP"){
    agg <- calculate_aggregate_LOP(dat, keep_vals$keep, ret_quantiles)
  }
  agg$direction <- avg_dir
  agg$method <- "cdf_trim"
  agg$type <- trim_type
  agg$n_trim <- keep_vals$n_trim
  return(agg)
}

