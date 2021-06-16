#' Implement cdf trimming - TO UPDATE
#'
#' Cdf trimming removes a set number of innermost/outermost probabilities.
#'   at each value before averaging **ADD REF**.
#'
#' @param data data.frame containing ids, quantiles, and values
#' @param trim_type string specifying "interior" or "exterior"
#' @param n_trim integer specifying the number of values to trim
#' @param avg_dir string indicating the direction of the averaging/trimming; use "LOP" or "vin".
#'
#' @return data.frame with columns \code{id}, \code{quantile}, and \code{value} containing trimmed ecdfs
#'
#' @details \code{keep_vals} should contain integer values between 1 and \code{n_id},
#'   the number of unique ids. Then, at each value, \code{trim_cdf()} will order probabilities and
#'   keep only those that defined by \code{keep_vals}. For example, if \code{n_id = 5} and
#'   \code{keep_vals = 2:4}, \code{trim_cdf()} will return the second, third and fourth ranked probability,
#'   excluding the smallest and larges probabilities from the average.
#'
#'
#' @inheritParams mean_trim
#' @return data.frame containing \code{quantile} and \code{value} of aggregate distribution,
#'   plus columns specifying \code{direction}, \code{method}, \code{trim_type}, and \code{n_trim}.
#'   TO DO: MAKE THIS CONSISTENT
#'
cdf_trim <- function(data, trim_type, n_trim, avg_dir){
  n_models <- length(unique(data$id))
  keep_vals <- keep_vals(trim_type, n_trim, n_models)$keep
  weighted_data <- implement_trim_cdf(data, keep_vals, avg_dir)
  return(weighted_data)
}

#### HELPERS ####

implement_trim_cdf <- function(data, keep_vals, avg_dir){
  if(avg_dir == "LOP"){df_cdfs <- lop_trim_cdf(data, keep_vals)}
  else if(avg_dir == "vincent"){df_cdfs <- vin_trim_cdf(data, keep_vals)}
  return(df_cdfs)
}

lop_trim_cdf <- function(data, keep_vals){
  df_cdfs <- data %>%
    dplyr::group_by(value) %>%
    dplyr::mutate(rank = rank(quantile, ties.method = "first"),
                  weight = ifelse(rank %in% keep_vals, 1, 0)) %>%
    dplyr::select(-rank)
  return(df_cdfs)
}

vin_trim_cdf <- function(data, keep_vals){
  df_cdfs <- data %>%
    dplyr::group_by(quantile) %>%
    dplyr::mutate(rank = rank(value, ties.method = "first"),
                  weight = ifelse(rank %in% keep_vals, 1, 0)) %>%
    dplyr::select(-rank)
  return(df_cdfs)
}
