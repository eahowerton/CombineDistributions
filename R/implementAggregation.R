#' aggregate a set of cdfs
#'
#' Given a data.frame containing cdfs , return a single aggregate
#'   cdf using specified method.
#'
#' @param data data.frame that contains multiple cdfs, grouped by \code{by} column.
#'   Specify cdf with \code{quantile} and \code{value} columns,
#' @param id_var string containing the name of the column that identifies unique cdfs
#' @param group_by vector containing the names of the columns to create unique aggregates for
#' @param method character name of the method for aggregation. See details for methods.
#' @param trim string to indicate trimming method (see Details)
#' @param n_trim integer denoting the number of models to trim
#'
#' @return TBD
#' @export
#'
#' @details Methods include
#'   1. "LOP" - simple probability averaging, also called Linear Opinion Pool.
#'   2. "vincent" - simple quantile averaging, also called Vincent average.
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
  data = data %>% dplyr::filter(eval(parse(text = create_filter_string(groups))))
  agg <- calculate_single_aggregate(data, id_var, method, ret_quantiles, trim, n_trim)
  if(all(is.na(agg))){agg <- data.frame(quantile = ret_quantiles, value = NA)}
  grps_df <- as.data.frame(matrix(rep(groups, times = nrow(agg)), nrow = nrow(agg), byrow = TRUE))
  colnames(grps_df) <- names(groups)
  agg <- agg %>% dplyr::bind_cols(grps_df)
  return(list(agg))
}

#' export
calculate_single_aggregate <- function(quant, val, id, method, ret_quantiles, trim = "none", n_trim = NA){
  data <- prep_input_data(quant, val, id)
  method_fn <- ifelse(method == "LOP", LOP, vincent)
  if(nrow(data) == 0){return(NA)}
  if(trim == "none"){
    agg <- method_fn(data, ret_quantiles)
  }
  else{
    parse <- parse_trim_input(trim)
    if(parse[1] == "cdf"){
      agg <- method_fn(data, ret_quantiles, weight_fn = cdf_trim, trim_type = parse[2], n_trim, avg_dir = method)
    }
    else{
      agg <- method_fn(data, ret_quantiles, weight_fn = mean_trim, trim_type = parse[2], n_trim)
    }
  }
  return(data.frame(quantile = ret_quantiles, value = agg))
}


#### PREP INPUT DATA ####
prep_input_data <- function(quant, val, id){
  data <- data.table::data.table(id = id, quantile = quant, value = val)
  data <- data[!(id %in% check_na_vals(data)),]
  data <- data[!(id %in% check_monotonic(data)),]
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
  eps <- -0.001
  ids <- unique(data$id)
  n_nonpos_diff <- sapply(ids, function(i){
    newd <- data %>%
      dplyr::filter(id == i) %>%
      dplyr::arrange(quantile)
    diff_vals <- diff(newd$value)
    return(length(which(diff_vals<eps)))
  })
  rm_ids <- ids[which(n_nonpos_diff > 0)]
  if(length(rm_ids) != 0){
    warning(paste0('excluding id(s) ',rm_ids,': not a cdf'))
  }
  return(as.character(rm_ids))
}


#### HELPERS ####
parse_trim_input <- function(trim){
  split <- strsplit(trim, "_")[[1]]
  return(split)
}

create_filter_string <- function(groups){
  filter_string = rep(NA, length(groups))
  for(i in 1:length(groups)){
    filter_string[i] = paste0(names(groups[i]), "== '", groups[i],"'")
  }
  filter_string <- paste0(filter_string, collapse = ' & ')
  return(filter_string)
}

