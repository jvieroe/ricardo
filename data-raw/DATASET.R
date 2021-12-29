## code to prepare `DATASET` dataset goes here

rename_vars <- readxl::read_xlsx("/Users/jeppeviero/github/rename_vars.xlsx")

usethis::use_data(rename_vars, overwrite = TRUE)
