## to implement aggregation we have two steps
## (1) single implementation
## (2) apply


#' aggregate a set of cdfs
#'
#' Given a data.frame containing cdfs , return a single aggregate
#'   cdf using specified method.
#'
#' @param data data.frame that contains multiple cdfs, grouped by \code{by} column.
#'   Specify cdf with \code{quantile} and \code{value} columns,
#' @param by string containing the name of the column that groups unique cdfs
#' @param method string containing the method for aggregation. See details for methods.
#' @param trim
#' @param n_trim
#'
#' @return TBD
#' @export
#'
#' @details Methods include
#'   1. "LOP" - simple probability averaging, also called Linear Opinion Pool.
#'   2. "Vinc" - simple quantile averaging, also called Vincent average.
#'   Trim inputs should be one of the following: "cdf_interior", "cdf_exterior"
#'   "mean_interior", "mean_exterior", or "none" following REF.
aggregate_data(data, by, method, ret_quantiles, trim = "none", n_trim = NA){
  data <- prep_input_data(data, by)
  if(trim != "none"){
    parse <- parse_trim_input(trim)
    trim_meancdf <- parse[1]
    trim_intext <- parse[2]
    keep_vals <- keep_vals(trim_intext, n_trim, length(unqiue(dat$id)))
    if(trim_meancdf == "mean"){
      data <- mean_trim(data, keep_vals$keep)
    }
  }
  agg$direction <- avg_dir
  agg$method <- "mean_trim"
  agg$type <- trim_type
  agg$n_trim <- keep_vals$n_trim
}


#### PREP INPUT DATA ####
prep_input_data <- function(data, by){
  data <- update_by_col(data, by)
  data <- filter_input_data(data)
}

update_by_col <- function(data, by){
  group_index <- which(colnames(data) == by)
  colnames(data)[group_index] = "id"
}

filter_input_data <- function(data, by){
  data <- data %>% filter(!(by %in% check_na_vals(data, by)))
  data <- data %>% filter(!(by %in% check_num_unq_vals(data, by)))
  data <- data %>% filter(!(by %in% check_monotonic(data, by)))
  return(data)
}

# cannot have NA values
check_na_vals <- function(data){
  n_na_vals <- data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(l = length(is.na(value)))
  rm_ids <- n_na_vals %>%
    dplyr::filter(l <= 1) %>%
    dplyr::pull(id)
  if(length(rm_ids) != 0){
    warning(paste0('excluding id(s) ',rm_ids,': NA value'))
  }
  return(rm_ids)
}

# cdfs must be non decreasing
check_monotonic <- function(data){
  diff_bn_vals <- data %>%
    dplyr::group_by(id) %>%
    dplyr::arrange(quantile) %>%
    dplyr::mutate(diff = c(diff(value),0))
  rm_ids <- diff_bn_vals %>%
    dplyr::filter(diff <= 0) %>%
    dplyr::pull(id )
  if(length(rm_ids) != 0){
    warning(paste0('excluding id(s) ',rm_ids,': not a cdf'))
  }
  return(rm_ids)
}

# cannot have a single value for all quantiles
check_num_unq_vals <- function(data){
  n_unique_vals <- data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(l = length(unique(value)))
 rm_ids <- n_unique_vals %>%
   dplyr::filter(l <= 1) %>%
   dplyr::pull(id)
 if(length(rm_ids) != 0){
   warning(paste0('excluding id(s) ',rm_ids,': single value for all quantiles'))
 }
 return(rm_ids)
}

#### HELPERS ####
parse_trim_input <- function(trim){
  split <- strsplit(trim, "_")[[1]]
  return(split)
}

