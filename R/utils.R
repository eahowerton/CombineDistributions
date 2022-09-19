remove_zero_weights <- function(data){
  data <- data %>%
    dplyr::filter(weight!=0)
  return(data)
}

equal_weights <- function(data){
  n_ids <- length(unique(data$id))
  data <- data %>%
    dplyr::mutate(weight = 1/n_ids)
  return(data)
}


user_specified_weights <- function(data, weights){
  weights <- check_weights_df(weights)
  data <- data %>% dplyr::left_join(weights, by = "id")
  return(data)
}

check_weights_df <- function(weights){
  if(ncol(weights) != 2){
    stop("weights not properly specified (ncol != 2)")}
  if(!("weight" %in% colnames(weights))){
    stop("weights not properly specified (weight col not included)")}
  if(!("id" %in% colnames(weights))){
    colnames(weights)[which(colnames(weights)!="weight")] = "id"
  }
  return(weights)
}

## modified function with the ret_values
return_specified_quantiles <- function(data, ret_quantiles, ret_values){
  data <- rm_duplicates(data)
  if(anyNA(ret_values)){
    if(length(unique(data$value)) == 1){
      data_return <- tibble::tibble(quantile = ret_quantiles,
                                    value = unique(data$value))
    }
    else{
      data_interp <- approx(data$quantile, data$value, xout = ret_quantiles, ties = "mean", rule = 2)
      data_return <- tibble::tibble(quantile = data_interp$x,
                                    value = data_interp$y)
    }
    return(data_return)
  }
  
  else{
    if(length(unique(data$value)) == 1){
      data_return <- tibble::tibble(quantile = unique(data$quantile),
                                    value = ret_values)
    }
    else{
      data_interp <- approx(data$value, data$quantile, xout = ret_values, ties = "mean", rule = 2)
      data_return <- tibble::tibble(quantile = data_interp$x,
                                    value = data_interp$y)
    }
    return(data_return)      
    
  }
}

rm_duplicates <- function(data){
  if(length(which(data$quantile == 0))>1){
    data <- data %>%
      dplyr::filter(quantile != 0 | value == max(value[quantile == 0]))
  }
  if(length(which(data$quantile == 1))>1){
    data <- data %>%
      dplyr::filter(quantile != 1 | value == min(value[quantile == 1]))
  }
  return(data)
}

