#' @noRd
custom_fun_not_all_na <- function(x) any(!is.na(x))


#' @noRd
custom_fun_not_any_na <- function(x) all(!is.na(x))


#' @noRd
check_input <- function(seasons,
                        leagues) {

  if(is.null(seasons)){
    stop("No seasons defined")
  }

  if(is.null(leagues)){
    stop("No leagues defined")
  }


}

#' @noRd
check_url <- function(get_data) {

  if(any(isFALSE(get_data$url))){
    warning("Invalid league-season combinations provided, one or more URLs don't exist. Proceeding with filter")
  }

  if(!any(isTRUE(get_data$url))){
    stop("No valid league-season combinations provided, no URLs exist")
  }

  # if(!RCurl::url.exists(url)){
  #   stop("Invalid league(s)/season(s), URL doesn't exist")
  # }

}


#' @noRd
get_fd_fun <- function(data,
                       output) {

  tmp_league <- data$league[1]
  tmp_season <- data$league[1]

  url <- data$url

  output <- utils::read.csv(url) %>%
    tibble::tibble() %>%
    dplyr::mutate(league = tmp_league) %>%
    dplyr::mutate(season = tmp_season)

}


