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



return_specified_quantiles <- function(data, ret_quantiles){
  data <- rm_duplicates(data)
  data_interp <- approx(data$quantile, data$value, xout = ret_quantiles, ties = "mean", rule = 2:1)
  data_return <- tibble::tibble(quantile = data_interp$x,
                         value = data_interp$y)
  return(data_return)
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

