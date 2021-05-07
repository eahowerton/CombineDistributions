#' Implement probability averaging
#'
#' Given a set of cumulative distribution functions  (assume for now: defined on same values),
#'   combine using probability averaging (also called linear opinion pool). #TODO CITE
#'   This method calculates the (weighted) average of quantiles at a given value
#'
#' @param dat data.frame containing cdfs to be combined. \code{dat} should contain three columns, named \code{id}, \code{quantile}, and \code{value}.
#'   \code{id} contains identifiers to distinguish between different individual cdfs.
#'   \code{quantile} contains quantiles (between 0 and 1) and \code{value} contains the cdf value for that id and quantile.
#' @param keep_vals ADD
#' @param ret_quantiles A matrix with length(x) rows, and n columns where n is the number of distributions to combine
#' @return A data.frame with \code{quantile} and \code{value} of probability averaged aggregate.
#'
#' @examples
#' dat <- expand.grid(id = c("A", "B"),
#'                    quantile = seq(0,1,0.01))
#' dat$value <- ifelse(dat$id == "A", qnorm(dat$quantile), qnorm(dat$quantile, 0,2))
#' calculate_aggregate_vin(dat, seq(0,1,0.05))
#'
#' @export
calculate_aggregate_LOP <- function(dat, keep_vals, ret_quantiles){ #model_weights
  # interpolate team cdfs
  interp_functions <- create_interp_fns(dat)
  if(length(interp_functions) == 0){return(NA)}
  # find min/max values across all teams (used as min/max values for agg cdf)
  limits = c(min(dat$value),max(dat$value))
  if(anyNA(limits)){return(NA)}
  vals <- seq(limits[1], limits[2], length.out = 1000)
  df_long <- evaluate_cdf(vals, interp_functions)
  df_trim <- trim_cdf(df_long, keep_vals)
  df_cdfs <- avg_probs(df_trim)
  agg <- LOP_distribution(df_cdfs, vals, ret_quantiles)
  # average across teams to calculate aggregate
  #agg <- LOP_cdf(limits, interp_functions, keep_vals) #model_weights
  return(agg)
}

#' Interpolate empirical cdfs
#'
#' Perform linear interpolation on set of empirical cdfs. Step 1 in Linear Opinion Pool calculation.
#'
#' @inheritParams calculate_aggregate_LOP
#' @return a list of functions defining linearly interpolated cdfs (input: value, output: quantile),
#'   where list elements are named by \code{id}.
#'
#' @details This function performs completeness checks for inputted cdfs. To be included,
#'   input cdfs are excluded if they meet one of the following conditions:
#'   1. all quantiles are equal to a single value
#'   2. any value contains NA
#'   3. decreasing values for increasing quantiles
create_interp_fns <- function(dat){
  # create list to store interpolations
  interp_functions <- list()
  # for each id, subset df and create interpolation
  for(i in unique(dat$id)){
    df_sub <- subset(dat, id == i)
    # checks, skip id if any of the following conditions are met
    #   (1) if all quantiles equal to single value
    #   (2) any quantile contains NA
    #   (3) non-monotonic values
    if(length(unique(df_sub$value)) == 1){
      # to treat as point estimates (single step function) use the commented code
      # val <- df_sub %>% filter(quantile == 0.5) %>% pull(value)
      # interp_functions[[i]] <- function(x){ifelse(x < val, 0, ifelse(x == val, 0.5, 1))}
      warning(paste0('excluding id ',i,': single value for all quantiles'))
      next
    }
    if(any(is.na(df_sub$value))){
      warning(paste0('excluding id ',i,': NA value'))
      next
    }
    if(any(diff(df_sub %>% dplyr::arrange(quantile) %>% dplyr::pull(value))<0)){
      warning(paste0('excluding id ',i,': not a cdf'))
      next
    }
    interp_functions[[i]] <- approxfun(x = df_sub$value,
                                       y = df_sub$quantile,
                                       method = "linear",
                                       yleft = 0, yright = 1, rule = 2, ties = list("ordered", mean))
  }
  return(interp_functions)
}

#' Evaluate unique cdfs for standard values
#'
#' Calculate probability for standard values across all unique cdfs. Step 2 in LOP calculation
#'
#' @param vals standard values over which to calculate probability for each unique cdf
#' @param interp_functions linearly interpolated cdf functions (output from \code{create_interp_fns()})
#' @return data.frame with columns \code{id}, \code{quantile}, and \code{value}
evaluate_cdf <- function(vals, interp_functions){
  # for each id, estimate quantile for each value in vals
  cdf_out <- lapply(interp_functions, function(fn){fn(vals)}) #if(!is(fn,"function")){exit()};
  # Find long dataset (easier for plotting in ggplot)
  df_long <- reshape2::melt(cdf_out)
  names(df_long) <- c("quantile", "id")
  df_long$value <- rep(vals, length(cdf_out))
  df_long <- df_long %>% dplyr::filter(!is.na(quantile))
  return(df_long)
}

#' Implement cdf trimming
#'
#' Cdf trimming removes a set number of innermost/outermost probabilities
#'   at each value before averaging **ADD REF**. Step 3 in LOP calculation.
#'
#' @param df_long data.frame containing probabilities across standardized values for each unique cdf
#'   (output from \code{evaluate_cdf})
#' @param keep_vals vector specifying which probabilities to keep at a given value. See details.
#' @return data.frame with columns \code{id}, \code{quantile}, and \code{value} containing trimmed ecdfs
#'
#' @details \code{keep_vals} should contain integer values between 1 and \code{n_id},
#'   the number of unique ids. Then, at each value, \code{trim_cdf()} will order probabilities and
#'   keep only those that defined by \code{keep_vals}. For example, if \code{n_id = 5} and
#'   \code{keep_vals = 2:4}, \code{trim_cdf()} will return the second, third and fourth ranked probability,
#'   excluding the smallest and larges probabilities from the average.
trim_cdf <- function(df_long, keep_vals){
  df_cdfs <- df_long %>%
    dplyr::group_by(value) %>%
    dplyr::mutate(rank = rank(quantile, ties.method = "first")) %>%
    dplyr::filter(rank %in% keep_vals) %>%
    dplyr::select(-rank)
  return(df_cdfs)
}

#' Average probabilities
#'
#' Calculate average of trimmed probabilities for each value. Step 4 in LOP calculation.
#'
#' @param df_cdfs data.frame containing trimmed ecdf values (output from \code{trim_cdf()})
#' @return vector of probabilities for each value (specified in \code{evaluate_cdf()} argument \code{vals})
avg_probs <- function(df_cdfs){
  gdf_cdf <- df_cdfs %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(value = mean(quantile))
  return(gdf_cdf$value)
}

#' Back to quantile function UPDATE
#'
#' Return LOP aggregate distribution for specified quantiles. Step 5 in LOP calculation.
#'
#' @param gdf_cdf vector of probabilities for each value (output from \code{avg_probs()})
#' @param vals vector of unique values (specified in \code{evaluate_cdf()} argument \code{vals})
#' @param ret_q vector of quantiles to return values from aggregate LOP distribution
LOP_distribution <- function(gdf_cdf, vals, ret_q){
  # if(nrow(gdf_cdf) == 1){ # check for point estimates - can remove?
  #   df_agg <- data.frame("quantile" = seq(0,1,0.05), "value" = gdf_cdf$value)
  # }
  # else{
  # in some cases (e.g., trimmed), vals is wider than ids included in LOP avg
  # leads to excess 0/1 on head/tail of gdf_cdf -- remove in this step
  min_val <- 1
  max_val <- length(vals)
  if(length(which(gdf_cdf == 0))>1){min_val <- max(which(gdf_cdf == 0))}
  if(length(which(gdf_cdf == 1))>1){max_val <- min(which(gdf_cdf == 1))}
  cdf_agg <- approxfun(x = gdf_cdf[min_val:max_val],
                       y = vals[min_val:max_val], method = "linear",
                       ties = list("ordered", mean))
  df_agg <- data.frame("quantile" = ret_q, "value" = cdf_agg(ret_q))
  # }
  return(df_agg)
}

