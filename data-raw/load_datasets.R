
library(readr)
library(dplyr)
library(tibble)

qc_dataset <- read_csv("data-raw/quality_control.csv") %>% as_tibble
raw_dataset <- read_csv("data-raw/complete_dataset_kept_duplicates.csv") %>% as_tibble
deduplicated_dataset <- read_csv("data-raw/complete_dataset_merged_duplicates_by_mean.csv") %>% as_tibble

usethis::use_data(qc_dataset, overwrite = TRUE)
usethis::use_data(raw_dataset, overwrite = TRUE)
usethis::use_data(deduplicated_dataset, overwrite = TRUE)
