## code to prepare `MMODS_obs` dataset
library(dplyr)
library(readxl)
library(httr)


countryattr_path <- 'https://raw.githubusercontent.com/MMODS-org/Elicitation-1/main/data/processed/county/county_attributes.csv'
countydeaths_path <-'https://raw.githubusercontent.com/MMODS-org/Elicitation-1/main/data/processed/county/time_series_covid19_deaths_US.csv'

# get data
df_county_attr <- read.csv(countryattr_path)
df_county_deaths <- read.csv(countydeaths_path)


MMODS_obs <- df_county_deaths %>%
  reshape2::melt(c("UID",
         "iso2",
         "iso3",
         "code3",
         "FIPS",
         "Admin2",
         "Province_State",
         "Country_Region",
         "Lat",
         "Long_",
         "Combined_Key",
         "Population" ), value.name = "cumu_deaths", variable.name = c("date")) %>%
  # reformat date column
  dplyr::mutate(date = as.Date(gsub("X", "",date), format = "%m.%d.%y")) %>%
  # filter to only dates within range
  dplyr::filter(date >= as.Date("2020-05-15") & date <= as.Date("2020-11-15")) %>%
  # add county attributes
  dplyr::left_join(df_county_attr, by = c("FIPS", "Population")) %>%
  # only similar to Adams
  dplyr::filter(SimilarToAdams == 1) %>%
  # select final day
  dplyr::filter(date == "2020-11-15") %>%
  # keep necessary columns
  dplyr::select(FIPS, County, State, Population, cumu_deaths)
usethis::use_data(MMODS_obs, overwrite = TRUE)

