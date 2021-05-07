######################
# Preamble
# --------------------
# require(tidyverse)
# 
# 
# ######################
# # aggregation functions
# # --------------------
# 
# # read in LOP and Vincent methods
# source("src/aggregation_methods/LOP.R")
# source("src/aggregation_methods/Vincent.R")
# source("src/aggregation_methods/Trimming.R")
# 
# 
# #######################
# #### DISTRIBUTIONS ####
# #######################
# 
# 
# implement_aggregation <- function(){
#   
# } 
#   
# 
# 
# 
# 
# 
# 
# 
# # change_vars: vector containing n_trim, trim_type, avg_dir, trim_meth
# apply_across_methods <- function(change_vars, team_cdfs){
#   with(as.list(change_vars),{
#     n_trim <- as.double(n_trim)
#     if(trim_meth == "mean"){
#       agg <- mean_trim(team_cdfs, n_trim, trim_type, avg_dir)
#     }
#     if(trim_meth == "cdf"){
#       agg <- cdf_trim(team_cdfs, n_trim, trim_type, avg_dir)
#     }
#     return(list(agg))
#   })
# }
# 
# 
# 
# # apply_agg_calculation
# # function: implement aggregation across multiple l-t-s combinations
# #           intended to be passed into apply() function for speed
# # input:    dat            data.frame formated following scenario hub with data for all l-t-s scenarios to aggregate
# #                             columns ---
# #                               scenario_name - character, scenario name
# #                               location - character, location code
# #                               target - character, target name
# #                               model - character with model name
# #                               quantile - double containing quantiles (represented as values between 0 and 1)
# #                               value - integer? containing team cdf for each quantile
# #           model_weights  data.frame to define weights for each model when averaging
# #                             columns ---
# #                                model - character, one row for each unique model
# #                                weight - double, weight given to this model in average
# #           agg_combo      vector containing specific l-t-s combination to aggregate 
# #                          elements ---
# #                                loc - one specific location
# #                                targ - one specific target
# #                                scen - one specific scenario 
# # output:   data.frame containing l-t-s combination and aggregate cdf
# #           if no teams or only one team has submitted for this l-t-s combination, return NA quantile and value
# #           columns ---
# #              scenario_name - character, name of scenario
# #              location - character?, location code
# #              target - character, target
# #              quantile - double, value between 0 and 1 representing quantile (in 0.05 increments)
# #              value - double, aggregate cdf values
# apply_agg_calculation <- function(dat, agg_combo){ #model_weights,
#   with(as.list(agg_combo),{ # in order to call scen, loc, targ directly
#     # filter data to specific l-t-s
#     dat_sub <- dat %>% filter(scenario_name == scen, location == loc, target == targ)
#     # calculate aggregate
#     ret <- calculate_agg(dat_sub, model_weights)
#     # check for NAs, and return l-t-s with aggregate
#     ifelse(any(is.na(ret)), ret<-data.frame(quantile = NA, value = NA), ret<-ret)
#     return(list(data.frame(scenario_name = scen, location =  loc, target = targ, model = "aggregate", ret)))
#   })
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
