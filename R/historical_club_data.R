#' name
#'
#' This function...
#'
#' @param x ..
#' @return ...
#' @author Jeppe Vierø
#' @import RCurl tibble dplyr purrr
#' @export

historical_club_data <- function(leagues = NULL,
                                 seasons = NULL) {

  check_input(seasons = seasons,
              leagues = leagues)

  get_data <- base::expand.grid(leagues,
                                seasons) %>%
    tibble::tibble() %>%
    dplyr::rename(league = .data$Var1,
                  season = .data$Var2) %>%
    dplyr::mutate(league_season = base::paste0(.data$league,
                                               .data$season)) %>%
    dplyr::mutate(url = base::paste0("https://www.football-data.co.uk/mmz4281/",
                                     .data$season,
                                     "/",
                                     .data$league,
                                     ".csv")) %>%
    dplyr::mutate(url_exists = RCurl::url.exists(.data$url))

  if(any(get_data$url_exists == FALSE)){
    warning("Invalid league-season combinations provided, one or more URLs don't exist. Proceeding with valid combinations")
  }

  if(!any(get_data$url_exists == TRUE)){
    stop("No valid league-season combinations provided, no URLs exist")
  }

  get_data <- get_data %>%
    dplyr::filter(.data$url_exists == TRUE)

  split_data <- base::split(get_data,
                            f = get_data$league_season)

  fd_data <- purrr::map(.x = split_data,
                        .f = get_fun)

  fd_data <- dplyr::bind_rows(fd_data) %>%
    dplyr::filter(!is.na(.data$HomeTeam) & !is.na(.data$AwayTeam))

  return(fd_data)

}
