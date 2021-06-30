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
  data <- update_id_var_col(data, id_var)
  data.table::setDT(data)
  aggs <- data[,calculate_single_aggregate(quant = quantile, val = value, id = id,
                                              method = method, ret_quantiles = ret_quantiles,
                                           trim = trim, n_trim = n_trim),
               by = group_by]
  return(aggs)
}

#' export
calculate_single_aggregate <- function(quant, val, id, method, ret_quantiles, trim = "none", n_trim = NA){
  data <- prep_input_data(quant, val, id)
  method_fn <- ifelse(method == "LOP", LOP, vincent)
  if(nrow(data) == 0){return(NA)}
  if(trim == "none"){
    agg <- method_fn(data$quantile, data$value, data$id, ret_quantiles)
  }
  else{
    parse <- parse_trim_input(trim)
    if(parse[1] == "cdf"){
      agg <- method_fn(data$quantile, data$value, data$id, ret_quantiles, weight_fn = cdf_trim, trim_type = parse[2], n_trim, avg_dir = method)
    }
    else{
      agg <- method_fn(data$quantile, data$value, data$id, ret_quantiles, weight_fn = mean_trim, trim_type = parse[2], n_trim)
    }
  }
  return(data.frame(quantile = ret_quantiles, value = agg))
}


#### PREP INPUT DATA ####
prep_input_data <- function(quant_col, val_col, id_col){
  data <- data.table::data.table(id = id_col, quantile = quant_col, value = val_col)
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

