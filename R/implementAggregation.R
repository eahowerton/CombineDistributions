#' aggregate a set of cdfs
#'
#' Given a data.frame containing cdfs , return a single aggregate
#'   cdf using specified method.
#'
#' @import data.table
#'
#' @param data data.frame that contains multiple cdfs, grouped by \code{by} column.
#'   Specify cdf with \code{quantile} and \code{value} columns,
#' @param id_var string containing the name of the column that identifies unique cdfs
#' @param group_by vector containing the names of the columns to create unique aggregates for
#' @param method character name of the method for aggregation (see details).
#' @param weighting_scheme string to indicate how to weight in the aggregate (see details).
#' @param reorder_quantiles TRUE to ensure that quantiles are ordered;
#'   set to FALSE if quantiles in data are already ordered (increasing)
#' @param n_trim integer denoting the number of models to trim
#'
#' @return TBD
#' @export
#'
#' @details Methods include:
#'
#'   1. "LOP" - simple probability averaging, also called Linear Opinion Pool.
#'   2. "vincent" - simple quantile averaging, also called Vincent average.
#'
#' Weighting schemes include:
#'   1. "equal" - equal weighting of all models and values
#'   2. "user_defined" - user supplies weights applied to each model
#'   (additional input `weights` that is a data.frame containing `id` and `weight` columns)
#'   3. trimming - "cdf_interior", "cdf_exterior",
#'   "mean_interior", "mean_exterior", following REF (additional inputs ...)
aggregate_cdfs <- function(data, id_var, group_by, method, ret_quantiles, ret_values = NA,
                           weighting_scheme = "equal", reorder_quantiles = TRUE, ...){
  data <- update_id_var_col(data, id_var)
  data.table::setDT(data)
  aggs <- data[,calculate_single_aggregate(quant = quantile,
                                           val = value,
                                           id = id,
                                           method = method,
                                           ret_quantiles = ret_quantiles,
                                           ret_values = ret_values,
                                           weighting_scheme = weighting_scheme,
                                           reorder_quantiles = reorder_quantiles,
                                           ...),
               by = group_by]
  return(aggs)
}

#' export
calculate_single_aggregate <- function(quant, val, id, method, ret_quantiles, ret_values,
                                       weighting_scheme = "equal", reorder_quantiles, ...){
  with(list(...),{
    data <- prep_input_data(quant, val, id, reorder_quantiles)
    method_fn <- ifelse(method == "LOP", LOP, vincent)
    if(nrow(data) == 0){return(NA)}
    if(weighting_scheme == "equal"){
      agg <- method_fn(data$quantile, data$value, data$id, ret_quantiles, ret_values)
    }
    else if (weighting_scheme == "user_defined"){
      agg <- method_fn(data$quantile, data$value, data$id, ret_quantiles, ret_values, weight_fn = user_specified_weights, ...)
    }
    else{
      parse <- parse_trim_input(weighting_scheme)
      if(parse[1] == "cdf"){
        agg <- method_fn(data$quantile, data$value, data$id, ret_quantiles, ret_values, weight_fn = cdf_trim, trim_type = parse[2], n_trim, avg_dir = method)
      }
      else{
        agg <- method_fn(data$quantile, data$value, data$id, ret_quantiles, ret_values, weight_fn = mean_trim, trim_type = parse[2], n_trim)
      }
    }
    return(agg)
  })
}

#### PREP INPUT DATA ####
prep_input_data <- function(quant_col, val_col, id_col, reorder_quantiles){
  data <- data.table::data.table(id = id_col, quantile = quant_col, value = val_col)
  if(reorder_quantiles){
    data <- data[order(id, quantile)]
  }
  rm_na <- check_na_vals(data)
  data <- data[!(id %in% rm_na$id)]
  rm_nonmono <- check_monotonic(data)
  data <- data[!(id %in% rm_nonmono$id)]
  return(data)
}


# cannot have NA values
check_na_vals <- function(data){
  na_vals <- data[is.na(value), .(id)]
  rm_ids <- unique(na_vals)
  if(nrow(rm_ids) != 0){
    warning(paste0('excluding id(s) ',as.character(rm_ids),': NA value'))
  }
  return(rm_ids)
}

# cdfs must be non decreasing
check_monotonic <- function(data){
  eps <- -0.001
  diff_data <- data[order(quantile),.(dif = diff(value)), by = "id"]
  rm_ids <- diff_data[dif < eps, .(id)]
  rm_ids <- unique(rm_ids)
  if(nrow(rm_ids) != 0){
    warning(paste0('excluding id(s) ',as.character(rm_ids$id),': not a cdf'))
  }
  return(rm_ids)
}

#### HELPERS ####
parse_trim_input <- function(trim){
  split <- strsplit(trim, "_")[[1]]
  return(split)
}

update_id_var_col <- function(data, id_var){
  group_index <- which(colnames(data) == id_var)
  colnames(data)[group_index] = "id"
  return(data)
}

