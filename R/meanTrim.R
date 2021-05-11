#' Implement mean trimming - TODO: UPDATE
#'
#' Trim entire distribution based on mean value ADD REF.
#'
#' @inheritParams calculate_aggregate_LOP
#' @param n_trim integer, number of ids to trim
#' @param trim_type "interior" for omitting central distributions or
#'   "exterior" for omitting outermost distributions
#' @param avg_dir "LOP" for probability averaging or "vincent" for quantile averaging
#'
#' @return data.frame containing \code{quantile} and \code{value} of aggregate distribution,
#'   plus columns specifying \code{direction}, \code{method}, \code{trim_type}, and \code{n_trim}.
#' @export
mean_trim <- function(data, trim_type, n_trim){
  n_models <- length(unique(data$id))
  keep_vals <- keep_vals(trim_type, n_trim, n_models)$keep
  keep_ids <- find_mean_trim_ids(data, keep_vals)
  weighted_data <- implement_mean_trim(data, keep_ids)
  return(weighted_data)
}


#### HELPERS ####

approx_cdf_mean <- function(df_cdf){
  mn <- 0
  for(i in 1:(length(df_cdf$quantile)-1)){
    s <- ((df_cdf$value[i]+df_cdf$value[i+1])/2)*((df_cdf$quantile[i+1]-df_cdf$quantile[i])/100)
    mn <-mn+s
  }
  return(mn)
}

find_mean_trim_ids <- function(dat, keep_vals){
  means <- sapply(unique(dat$id),
                  function(i){m <- approx_cdf_mean(dat %>% filter(id == i));
                  return(list(data.frame(id = i, m)))})
  means <- do.call(rbind, means)
  trim_ids <- means %>% dplyr::arrange(m)
  keep_ids <- trim_ids$id[keep_vals]
  return(keep_ids)
}

implement_mean_trim <- function(data, keep_ids){
  data <- data %>%
    dplyr::mutate(weight = ifelse(id %in% keep_ids,1,0))
  return(data)
}




