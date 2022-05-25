## code to prepare `MMODS` dataset
library(dplyr)

MMODS_path <- 'https://raw.githubusercontent.com/MMODS-org/Elicitation-1/main/data/processed/round2/team_submissions_only_round2.csv'

MMODS <-read.csv(MMODS_path) %>%
  # only include cumulative deaths for closed intervention
  dplyr::filter(intervention == "closed",
         objective == "cumu_deaths") %>%
  # reformat quantile
  dplyr::mutate(quantile = quantile/100) %>%
  # keep necessary columns
  dplyr::select(id, quantile, value)
usethis::use_data(MMODS, overwrite = TRUE)

