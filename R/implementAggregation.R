#' aggregate a set of cdfs
#'
#' Given a data.frame containing cdfs , return a single aggregate
#'   cdf using specified method.
#'
#' @param data data.frame that contains multiple cdfs, grouped by \code{by} column.
#'   Specify cdf with \code{quantile} and \code{value} columns,
#' @param id_var string containing the name of the column that identifies unique cdfs
#' @param group_by vector containing the names of the columns to create unique aggregates for
#' @param method function name of the method for aggregation. See details for methods.
#' @param trim string to indicate trimming method (see Details)
#' @param n_trim integer denoting the number of models to trim
#'
#' @return TBD
#' @export
#'
#' @details Methods include
#'   1. LOP - simple probability averaging, also called Linear Opinion Pool.
#'   2. vincent - simple quantile averaging, also called Vincent average.
#'   Trim inputs should be one of the following: "cdf_interior", "cdf_exterior",
#'   "mean_interior", "mean_exterior", or "none" following REF.
aggregate_cdfs <- function(data, id_var, group_by, method, ret_quantiles, trim = "none", n_trim = NA){
  unq_groups <- data %>%
    dplyr::select(group_by) %>%
    dplyr::distinct()
  aggs <- apply(unq_groups, 1, apply_aggregation,data = data, id_var = id_var, method = method, ret_quantiles = ret_quantiles, trim = trim, n_trim = n_trim)
  aggs <- do.call(rbind, aggs)
  aggs <- do.call(rbind, aggs)
  return(aggs)
}

apply_aggregation <- function(data, groups, id_var, method, ret_quantiles, trim = "none", n_trim = NA){
  for(i in 1:length(groups)){ # TO DO: more efficient way to do this?
    data = data %>% dplyr::filter(.[[names(groups)[i]]] == groups[i])
  }
  agg <- calculate_single_aggregate(data, id_var, method, ret_quantiles, trim, n_trim)
  if(all(is.na(agg))){agg <- data.frame(quantile = ret_quantiles, value = NA)}
  grps_df <- as.data.frame(matrix(rep(groups, times = nrow(agg)), nrow = nrow(agg), byrow = TRUE))
  colnames(grps_df) <- names(groups)
  agg <- agg %>% dplyr::bind_cols(grps_df)
  return(list(agg))
}

#' export
calculate_single_aggregate <- function(data, id_var, method, ret_quantiles, trim = "none", n_trim = NA){
  data <- prep_input_data(data, id_var)
  if(nrow(data) == 0){return(NA)}
  if(trim == "none"){
    agg <- method(data, ret_quantiles)
  }
  else{
    parse <- parse_trim_input(trim)
    trim_fn <- ifelse(parse[1] == "mean", mean_trim, cdf_trim)
    agg <- method(data, ret_quantiles, weight_fn = trim_fn, trim_type = parse[2], n_trim)
  }
  return(agg)
}


#### PREP INPUT DATA ####
prep_input_data <- function(data, id_var){
  data <- update_id_var_col(data, id_var)
  data <- filter_input_data(data)
}

update_id_var_col <- function(data, id_var){
  group_index <- which(colnames(data) == id_var)
  colnames(data)[group_index] = "id"
  return(data)
}

filter_input_data <- function(data){
  data <- data %>% dplyr::filter(!(id %in% check_na_vals(data)))
  #data <- data %>% dplyr::filter(!(id %in% check_num_unq_vals(data)))
  data <- data %>% dplyr::filter(!(id %in% check_monotonic(data)))
  return(data)
}

# cannot have NA values
check_na_vals <- function(data){
  n_na_vals <- data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(l = length(which(is.na(value))))
  rm_ids <- n_na_vals %>%
    dplyr::filter(l >= 1) %>%
    dplyr::pull(id)
  if(length(rm_ids) != 0){
    warning(paste0('excluding id(s) ',rm_ids,': NA value'))
  }
  return(as.character(rm_ids))
}

# cdfs must be non decreasing
check_monotonic <- function(data){
  ids <- unique(data$id)
  n_nonpos_diff <- sapply(ids, function(i){
    newd <- data %>%
      dplyr::filter(id == i) %>%
      dplyr::arrange(quantile)
    diff_vals <- diff(newd$value)
    return(length(which(diff_vals<0)))
    })
  rm_ids <- ids[which(n_nonpos_diff > 0)]
  if(length(rm_ids) != 0){
    warning(paste0('excluding id(s) ',rm_ids,': not a cdf'))
  }
  return(as.character(rm_ids))
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
 return(as.character(rm_ids))
}

#### HELPERS ####
parse_trim_input <- function(trim){
  split <- strsplit(trim, "_")[[1]]
  return(split)
}

