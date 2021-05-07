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
  means <- sapply(unique(dat$id), function(i){m <- cdf_mean(dat %>% filter(id == i));
  return(list(data.frame(id = i, m)))})
  means <- do.call(rbind, means)
  trim_ids <- means %>% dplyr::arrange(m)
  keep_ids <- trim_ids$id[keep_vals]
  return(keep_ids)
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
#'   TO DO: MAKE THIS CONSISTENT
mean_trim <- function(dat, n_trim, trim_type, avg_dir, ret_quantiles){
  n_models <- length(unique(dat$id))
  keep_vals <- keep_vals(trim_type, n_trim, n_models, avg_dir, "mean")
  if(length(keep_vals$keep) == 0){next}
  keep_ids <- mean_trim_ids(dat, keep_vals$keep)
  #print(keep_ids)
  # keep_ids <- data.frame(model = unique(dat$model),
  #                        weight = ifelse(unique(dat$model) %in% keep_ids,1/length(keep_ids),0))
  # d <- left_join(dat, keep_ids)
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
  keep_vals <- keep_vals(trim_type, n_trim, n_models, avg_dir, "cdf")
  if(length(keep_vals$keep) == 0){next}
  #keep_ids <- mean_trim_ids(dat, keep_vals$keep)
  if(avg_dir == "vincent"){
    #browser()
    d <- sapply(unique(dat$quantile),
                function(i){x <- dat %>% dplyr::filter(quantile == i) %>% dplyr::arrange(value);
                x <- x[keep_vals$keep,];return(list(x))})
    d <- do.call(rbind, d)
    agg <- calculate_aggregate_vin(d, ret_quantiles)
  }
  else if(avg_dir == "LOP"){
    d <- dat
    agg <- calculate_aggregate_LOP(d, keep_vals$keep, ret_quantiles) #unique(d$model),
  }
  agg$direction <- avg_dir
  agg$method <- "cdf_trim"
  agg$type <- trim_type
  agg$n_trim <- keep_vals$n_trim
  return(agg)
}


#' Identify values to trim
#'
#' Based on trimming direction, method, and number, return values to trim. TO DO: Lower/upper
#'
#' @inheritParams mean_trim
#' @param n_ids integer, number of unique ids being aggregated
#' @param method "cdf" or "mean" for the trimming method REF
#' @return list containing two elements: (1) vector of values to keep;
#'   (2) number of ids trimmed
#'
#' @details In some cases,the number of models, the number of ids to trim
#'   and the trimming type are not compatible. In these cases, \code{keep_vals()}
#'   modifies n_trim. The following rules must be met for compatibility:
#'   1. For exterior trimming, \code{n_trim \leq floor(n_models/2)}.
#'   2. For interior trimming, \code{n_trim} and \code{n_models} must have same
#'   parity (i.e., both are even or both are odd).
#'   TODO: ADD CHECKS/WARNINGS HERE, EXAMPLES
keep_vals <- function(trim_type, n_trim, n_ids, avg_dir, method){
  if(trim_type == "exterior"){
    keep <- (n_trim+1):(n_ids-n_trim)
    n_trim <- n_trim * 2
  }
  else if(trim_type == "interior"){
    n_models_par <- n_ids %% 2
    n_trim_par <- n_trim %% 2
    if(n_models_par != n_trim_par){n_trim<-n_trim+1}
    rm <- (n_ids - n_trim)/2
    rm <-(rm + 1):(n_ids - rm)
    keep <- 1:n_ids
    keep <- keep[-rm]
  }
  else if(avg_dir == "LOP" & method == "cdf"){
    # in this special case, we are trimming probabilities not values
    # so the "lower" case corresponds to trimming the uppermost probabilities for any given value
    # and vice versa
    if(trim_type == "lower"){keep <-1:(n_ids-n_trim)}
    else if(trim_type == "upper"){keep <- (n_trim+1):n_ids}
  }
  else if(trim_type == "lower"){
    keep <- (n_trim+1):n_ids
  }
  else if(trim_type == "upper"){
    keep <- 1:(n_ids-n_trim)
  }
  return(list(keep = keep, n_trim = n_trim))
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

