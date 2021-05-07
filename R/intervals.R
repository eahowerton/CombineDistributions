###################
#### INTERVALS ####
###################


## vinc aggregate with trimming - intervals
## methods following Taylor and Taylor 2020

# symmetric trimming: remove n_trim lower and n_trim upper for each quantile
calculate_aggregate_trim_sym <- function(team_cdfs, n_trim){
  n_models <- length(unique(team_cdfs$id))
  trim <- team_cdfs %>% 
    group_by(quantile) %>% 
    summarise(value = mean(value[value>(sort(value)[n_trim]) & value<=(sort(value)[n_models-n_trim])]))
  trim$id = paste0("vinc_trim_sym_",2*n_trim)
  return(trim)
}

# interior/exterior trimming defined in Taylor and Taylor 2020 and Gaba et al 2017 for intervals 

# exterior trimming: for lower endpoints (i.e., q < 50) remove n_trim lowest endpoints before mean
#                    for upper endpoints (i.e., q > 50) remove n_trim highest endpoints before mean
# only return 50% and 90% intervals for now
# for median, average outcome using both methods?
# think a bit more about this... sometimes leads to lower bound >upper bound (including in MMODS data)
calculate_aggregate_trim_ext <- function(team_cdfs, n_trim){
  n_models <- length(unique(team_cdfs$id))
  trim_ext <- team_cdfs %>% 
    group_by(quantile) %>%
    filter(quantile %in% c(5, 25, 50)) %>%
    summarise(value= mean(sort(value)[(n_trim+1):n_models]))
  trim_ext <- rbind(trim_ext, 
                    team_cdfs %>% 
                      group_by(quantile) %>%
                      filter(quantile%in% c(50,75, 95)) %>%
                      summarise(value= mean(sort(value)[1:(n_models-n_trim)])))
  trim_ext <- trim_ext %>% 
    group_by(quantile) %>% 
    summarise(value = mean(value))
  trim_ext$id <- paste0("vinc_trim_ext_",n_trim*2)
  return(trim_ext)
}




# interior trimming: for lower endpoints (i.e., q < 50) remove n_trim highest endpoints before mean
#                    for upper endpoints (i.e., q >50) remove n_trim lowest endpoints before mean
# for median, average outcome using both methods?
calculate_aggregate_trim_int <- function(team_cdfs, n_trim){
  n_models <- length(unique(team_cdfs$id))
  trim_int <- team_cdfs %>% 
    group_by(quantile) %>%
    filter(quantile <= 50) %>%
    summarise(value= mean(sort(value)[1:(n_models-n_trim)]))
  trim_int <- rbind(trim_int, 
                    team_cdfs %>% 
                      group_by(quantile) %>%
                      filter(quantile >= 50) %>%
                      summarise(value= mean(sort(value)[(n_trim+1):n_models])))
  trim_int <- trim_int %>% 
    group_by(quantile) %>% 
    summarise(value = mean(value))
  trim_int$id = paste0("vinc_trim_int_",n_trim*2)
  return(trim_int)
}

