## code to prepare `DATASET` dataset goes here

rename_vars <- readxl::read_xlsx("data/rename_vars.xlsx")

usethis::use_data(rename_vars, overwrite = TRUE)
