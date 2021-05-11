remove_zero_weights <- function(data){
  data <- data %>%
    dplyr::filter(weight!=0)
  return(data)
}

