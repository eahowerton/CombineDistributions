#### Find values for trimming ####
#' Identify values to trim
#'
#' Based on trimming direction, method, and number, return values to trim. TO DO: Lower/upper
#'
#' @inheritParams mean_trim
#' @param n_ids integer, number of unique ids being aggregated
#'
#' @return list containing two elements: (1) vector of values to keep;
#'   (2) number of ids trimmed
keep_vals <- function(trim_type, n_trim, n_ids){
  n_trim_adj <- ntrim_checks(trim_type, n_trim, n_ids)
  # implement trimming
  if(trim_type == "exterior"){
    return(keep_vals_exterior(n_trim_adj, n_ids))
  }
  if(trim_type == "interior"){
    return(keep_vals_interior(n_trim_adj, n_ids))
  }
}

#### helpers --------------------

ntrim_checks <- function(trim_type, n_trim, n_ids){
  if(trim_type == "exterior"){n_trim <- check_update_ntrim_even(n_trim)}
  if(trim_type == "interior"){n_trim <- check_update_ntrim_sameparity(n_trim, n_ids)}
  n_trim <- check_ntrim_vs_nid(n_trim, n_ids)
  return(n_trim)
}

check_update_ntrim_even <- function(n_trim){
  if(n_trim %% 2 == 1){
    warning("Cannot trim an odd number of ids with external trimming.
            Rounding n_trim up.")
    n_trim <- n_trim + 1
  }
  return(n_trim)
}

check_update_ntrim_sameparity <- function(n_trim, n_ids){
  n_ids_par <- n_ids %% 2
  n_trim_par <- n_trim %% 2
  if(n_ids_par != n_trim_par){
    warning(paste0("Cannot trim ", n_trim, " values, with ", n_ids, " distributions.
                   Rounding n_trim up to ", n_trim + 1, "."))
    n_trim<-n_trim+1}
  return(n_trim)
}

check_ntrim_vs_nid <- function(n_trim, n_ids){
  if(n_trim <= 0){
    stop("Number to trim <= 0.")
  }
  if(n_trim >= n_ids){
    n_trim <- 0
    warning("Trying to trim more values than available. Aggregating without trimming.") # THINK ABOUT THIS...
  }
  return(n_trim)
}

#' Setup exterior trimming
#'
#' For exterior trimming, remove \code{n_trim/2} lowest and highest values.
#'
#' @inheritParams keep_vals
#' @return list containing two elements: (1) vector of values to keep;
#'   (2) number of ids trimmed
#'
#' @examples
#' keep_vals_exterior(2, 4)
#' keep_vals_exterior(4, 5)
keep_vals_exterior <- function(n_trim, n_ids){
  n_trim_per_side <- n_trim/2
  keep <- (n_trim_per_side+1):(n_ids-n_trim_per_side)
  return(list(keep = keep, n_trim = n_trim))
}

#' Setup interior trimming
#'
#' For interior trimming, remove innermost \code{n_trim} values.
#'
#' @inheritParams keep_vals
#' @return list containing two elements: (1) vector of values to keep;
#'   (2) number of ids trimmed
#'
#' @details Interior trimming requires (1) \code{n_trim < n_ids} and
#' (2) \code{n_trim} and \code{n_ids} have the same parity
#' (i.e., both are even or both are odd). If not,
#'  \code{n_trim} is increased to \code{n_trim + 1}.
#'
#' @examples
#' keep_vals_interior(2, 4)
#' keep_vals_interior(1, 4)
keep_vals_interior <- function(n_trim, n_ids){
  num_keep <- (n_ids-n_trim)/2
  keep <- c(1:num_keep, (n_ids-num_keep+1):n_ids)
  return(list(keep = keep, n_trim = n_trim))
}

