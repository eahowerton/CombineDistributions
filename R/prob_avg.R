#' Implement probability averaging
#'
#' Given a set of cumulative distribution functions  (assume for now: defined on same values),
#'   combine using probability averaging (also called Linear Opinion Pool).
#'   This method calculates the (weighted) average of probabilities at a given value.

calculate_aggregate_LOP <- function(dat,ids, keep_vals){
  # generate interpolated functions for each int-obj-id combo
  interp_functions = create_interp_fns(dat, ids)
  # calculate max and min  values across all teams for
  # each intervention/objective pair
  #browser()
  lims <- dat %>%
    summarise(min = min(value, na.rm = TRUE),
              max = max(value, na.rm = TRUE))
  lims <- data.frame(lims)
  aggregate_all = calc_aggregate_cdf(dat, lims, interp_functions, ids, keep_vals)
  return(aggregate_all)
}

create_interp_fns <- function(df, ids){
  interp_functions <- cdfs <- list()
  for(i in ids){
    # Create lists for storing the CDF, PPF, and dataframe for each team
    interp_functions[[i]] <- list()
    # Subset for the intervention of interest
    df_sub <- subset(df, id == i)
    # if all quantiles equal to single value, do not call approxfun()
    # (some groups only entereted value in quantile 50 - thus NA check too)
    if(length(unique(df_sub$value))==1 |
       (all(df_sub[df_sub$quantile != 50,"value"]==0) & df_sub[df_sub$quantile == 50,"value"]!=0) |
       any(is.na(df_sub$value))){
      interp_functions[[i]] <- NA
      next
    }
    interp_functions[[i]] <- approxfun(x = df_sub$value, y = df_sub$quantile/100,
                                       method = "linear", yleft = 0, yright = 1, rule = 2, ties = mean)
  }
  return(interp_functions)
}

calc_aggregate_cdf <- function(dat, lims, interp_functions, ids, keep_vals){
  # Create a list to store output
  cdf_out <- list()
  # Create a vector of values of the objective (of length 1000) over which to interpolate
  # take min/max across all teams for the current obj/int
  x <- seq(lims[,"min"], lims[,"max"], length.out = 1000)
  for(i in ids){
    # for point estimates, assume cdf is step fn. from 0 to 1 at point.est value
    # interp_functions is usually a function, so this checks it is not a function (it'll be an NA)
    if(!is(interp_functions[[i]], "function")){
      tmp <- rep(0, 1000)
      point.est <- subset(dat, id == i & quantile == 50, "value")
      switch.x <- min(which(x >= as.numeric(point.est)))
      tmp[switch.x:1000] = 1
      cdf_out[[i]] <- tmp
      next
    }
    cdf_out[[i]] <- interp_functions[[i]](x)
  }
  df_cdfs <- as.data.frame(cdf_out)
  # if trimmed - for each value, rm interior/exterior values
  df_cdfs <- apply(df_cdfs, 1, function(r){r<-sort(r); r<-r[keep_vals];return(list(r))})
  df_cdfs <- do.call(rbind, df_cdfs)
  df_cdfs <- do.call(rbind, df_cdfs)
  gdf_cdf<-apply(df_cdfs, 1, mean)
  # Convert back to a CDF function and interpolate for the quantiles we've asked for
  if(length(gdf_cdf) == 1){
    df_agg <- data.frame("quantile" = 0:100, "value" = gdf_cdf$value)
  }
  else{
    cdf_agg <- approxfun(x = gdf_cdf, y = x, rule = 2, ties = mean)
    df_agg <- data.frame("quantile" = 0:100, "value" = cdf_agg((0:100)/100))
  }
  return(df_agg)
}
