#' Download historical football match data
#'
#' This function extracts data on historical football matches from \url{https://www.football-data.co.uk/}
#'
#' @param leagues a vector of league codes
#' @param seasons a vector of season codes
#' @return a `tibble` with data on historical football matches
#' @author Jeppe Vierø
#' @import RCurl tibble dplyr purrr janitor
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

  if(!any(get_data$url_exists == TRUE)){
    stop("No valid league-season combinations provided, no URLs exist")
  }

  if(any(get_data$url_exists == FALSE)){
    warning("Invalid league-season combinations provided, one or more URLs don't exist. Proceeding with valid combinations")
  }

  get_data <- get_data %>%
    dplyr::filter(.data$url_exists == TRUE)

  split_data <- base::split(get_data,
                            f = get_data$league_season)

  fd_data <- purrr::map(.x = split_data,
                        .f = get_fd_fun)

  lookup_vars <- c("HT" = "HomeTeam",
                   "AT" = "AwayTeam")

  fd_data <- dplyr::bind_rows(fd_data)

  fd_data <- fd_data %>%
    dplyr::rename_with(.fn = ~lookup_vars[.x],
                       .cols = dplyr::intersect(names(.),
                                                names(lookup_vars))
                       )

  fd_data <- fd_data %>%
    dplyr::mutate(dplyr::across(names(.),
                                ~ ifelse(.x == "", NA, .x)))

  fd_data <- fd_data %>%
    dplyr::filter(!is.na(.data$HomeTeam) & !is.na(.data$AwayTeam)) %>%
    dplyr::select(where(custom_fun_not_all_na))

  fd_data <- fd_data %>%
    janitor::clean_names()

  return(fd_data)

}
