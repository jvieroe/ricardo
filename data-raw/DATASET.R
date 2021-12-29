## code to prepare `DATASET` dataset goes here

#rename_vars <- readxl::read_xlsx("/Users/jeppeviero/github/rename_vars.xlsx")

rename_vars <- c("HT" = "HomeTeam",
                 "AT" = "AwayTeam")

usethis::use_data(rename_vars, overwrite = TRUE)
