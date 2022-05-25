#' Projections from Multiple Models for Outbreak Decision Support (MMODS) study
#'
#' This data includes projections from 17 distinct models elicited during the first
#' round of the MMODS study. These projections are of cumulative deaths from
#' May-November 2020 in a US county of approximately 100,000 individuals that
#' implements a strict stay-at-home order for the duration of the projection period.
#' Details on the elicitation and the data can be found in Shea et al. 2020.
#'
#' @format A data frame with 1700 rows and 3 variables:
#' \describe{
#'   \item{id}{id to differentiate projections from distinct models}
#'   \item{quantile}{projection quantile from 0 to 1}
#'   \item{value}{projected cumulative number of deaths for that quantile}
#' }
#'
#' @source \url{https://github.com/MMODS-org/Elicitation-1}
"MMODS"


#' Observations from Multiple Models for Outbreak Decision Support (MMODS) study
#'
#' This data includes observations from 84 US counties with population between
#' 90,000 and 110,000 individuals that implemneted a strict stay-at-home orders
#' between May and November 2020. These observations represent cumulative deaths
#' in these counties.
#' Details on the elicitation and the data can be found in Shea et al. 2020.
#'
#' @format A data frame with 84 rows and 5 variables:
#' \describe{
#'   \item{FIPS}{FIPS code for each county}
#'   \item{County}{name of county}
#'   \item{State}{state in which county is}
#'   \item{Population}{popualtion size of county}
#'   \item{cumu_deaths}{number of COVID-19 deaths recorded in county through November 15, 2020}
#' }
#'
#' @source \url{https://github.com/MMODS-org/Elicitation-1}
"MMODS_obs"
