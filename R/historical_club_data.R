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
                   "AT" = "AwayTeam",
                   "FTHG" = "full_time_home_goals",
                   "FTAG" = "full_time_away_goals",
                   "FTR" = "full_time_result",
                   "HTHG" = "half_time_home_goals",
                   "HTAG" = "half_time_away_goals",
                   "HTR" = "half_time_result",
                   "HS" = "shots_home",
                   "AS" = "shots_away",
                   "HST" = "shots_on_target_home",
                   "AST" = "shots_on_target_away",
                   "HHW" = "shots_on_woodwork_home",
                   "AHW" = "shots_on_woodwork_away",
                   "HC" = "corners_home",
                   "AC" = "corners_away",
                   "HF" = "fouls_home",
                   "AF" = "fouls_away",
                   "HFKC" = "free_kicks_conceded_home",
                   "AFKC" = "free_kicks_conceded_way",
                   "HO" = "offsides_home",
                   "AO" = "offsides_away",
                   "HY" = "yellow_cards_home",
                   "AY" = "yellow_cards_away",
                   "HR" = "red_cards_home",
                   "AR" = "red_cards_away",
                   "HBP" = "booking_points_home",
                   "ABP" = "booking_points_away",
                   # -----------------------------
                   "B365H" = "Bet365_home_win_odds",
                   "B365D" = 'Bet365_draw_odds',
                   "B365A" = 'Bet365_away_win_odds',
                   "BSH" = 'BlueSquare_home_win_odds',
                   "BSD" = 'BlueSquare_draw_odds',
                   "BSA" = 'BlueSquare_away_win_odds',
                   "BWH" = 'BetandWin_home_win_odds',
                   "BWD" = 'BetandWin_draw_odds',
                   "BWA" = 'BetandWin_away_win_odds',
                   "GBH" = 'Gamebookers_home_win_odds',
                   "GBD" = 'Gamebookers_draw_odds',
                   "GBA" = 'Gamebookers_away_win_odds',
                   "IWH" = 'Interwetten_home_win_odds',
                   "IWD" = 'Interwetten_draw_odds',
                   "IWA" = 'Interwetten_away_win_odds',
                   "LBH" = 'Ladbrokes_home_win_odds',
                   "LBD" = 'Ladbrokes_draw_odds',
                   "LBA" = 'Ladbrokes_away_win_odds',
                   "PSH" = 'Pinnacle_home_win_odds',
                   "PH" = 'Pinnacle_home_win_odds',
                   "PSD" = 'Pinnacle_draw_odds',
                   "PD" = 'Pinnacle_draw_odds',
                   "PSA" = 'Pinnacle_away_win_odds',
                   "PA" = 'Pinnacle_away_win_odds',
                   "SOH" = 'SportingOdds_home_win_odds',
                   "SOD" = 'SportingOdds_draw_odds',
                   "SOA" = 'SportingOdds_away_win_odds',
                   "SBH" = 'Sportingbet_home_win_odds',
                   "SBD" = 'Sportingbet_draw_odds',
                   "SBA" = 'Sportingbet_away_win_odds',
                   "SJH" = 'StanJames_home_win_odds',
                   "SJD" = 'StanJames_draw_odds',
                   "SJA" = 'StanJames_away_win_odds',
                   "SYH" = 'Stanleybet_home_win_odds',
                   "SYD" = 'Stanleybet_draw_odds',
                   "SYA" = 'Stanleybet_away_win_odds',
                   "VCH" = 'VCBet_home_win_odds',
                   "VCD" = 'VCBet_draw_odds',
                   "VCA" = 'VCBet_away_win_odds',
                   "WHH" = 'WilliamHill_home_win_odds',
                   "WHD" = 'WilliamHill_draw_odds',
                   "WHA" = 'WilliamHill_away_win_odds',
                   #
                   "Bb1X2" = 'Number_of_BetBrain_bookmakers_used_to_calculate_match_odds_averages_and_maximums',
                   "BbMxH" = 'Betbrain_maximum_home_win_odds',
                   "BbAvH" = 'Betbrain_average_home_win_odds',
                   "BbMxD" = 'Betbrain_maximum_draw_odds',
                   "BbAvD" = 'Betbrain_average_draw_win_odds',
                   "BbMxA" = 'Betbrain_maximum_away_win_odds',
                   "BbAvA" = 'Betbrain_average_away_win_odds',
                   "MaxH" = 'market_maximum_home_win_odds',
                   "MaxD" = 'market_maximum_draw_win_odds',
                   "MaxA" = 'market_maximum_away_win_odds',
                   "AvgH" = 'market_average_home_win_odds',
                   "AvgD" = 'market_average_draw_win_odds',
                   "AvgA" = 'Market_average_away_win_odds',
                   #
                   "BbOU" = 'Number_of_BetBrain_bookmakers_used_to_calculate_over_under_2.5_goals_total_goals_averages_and_maximums',
                   "BbMx.2.5" = 'Betbrain_maximum_over_2.5_goals',
                   "BbAv.2.5.1" = 'Betbrain_average_over_2.5_goals',
                   "BbMx.2.5" = 'Betbrain_maximum_under_2.5_goals',
                   "BbAv.2.5.1" = 'Betbrain_average_under_2.5_goals',
                   #
                   "GB.2.5" = 'Gamebookers_over_2.5_goals',
                   "GB.2.5.1" = 'Gamebookers_under_2.5_goals',
                   "B365.2.5" = 'Bet365_over_2.5_goals',
                   "B365.2.5.1" = 'Bet365_under_2.5_goals',
                   "P.2.5" = 'Pinnacle_over_2.5_goals',
                   "P.2.5.1" = 'Pinnacle_under_2.5_goals',
                   "Max.2.5" = 'Market_maximum_over_2.5_goals',
                   "Max.2.5.1" = 'Market_maximum_under_2.5_goals',
                   "Avg.2.5" = 'Market_average_over_2.5_goals',
                   "Avg.2.5.1" = 'Market_average_under_2.5_goals',
                   #
                   "BbAH" = 'Number_of_BetBrain_bookmakers_used_to_Asian_handicap_averages_and_maximums',
                   "BbAHh" = 'Betbrain_size_of_handicap_home_team',
                   "AHh" = 'Market_size_of_handicap_home_team_since_2019_2020',
                   "BbMxAHH" = 'Betbrain_maximum_Asian_handicap_home_team_odds',
                   "BbAvAHH" = 'Betbrain_average_Asian_handicap_home_team_odds',
                   "BbMxAHA" = 'Betbrain_maximum_Asian_handicap_away_team_odds',
                   "BbAvAHA" = 'Betbrain_average_Asian_handicap_away_team_odds',
                   #
                   "GBAHH" = 'Gamebookers_Asian_handicap_home_team_odds',
                   "GBAHA" = 'Gamebookers_Asian_handicap_away_team_odds',
                   "GBAH" = 'Gamebookers_size_of_handicap_home_team',
                   "LBAHH" = 'Ladbrokes_Asian_handicap_home_team_odds',
                   "LBAHA" = 'Ladbrokes_Asian_handicap_away_team_odds',
                   "LBAH" = 'Ladbrokes_size_of_handicap_home_team',
                   "B365AHH" = 'Bet365_Asian_handicap_home_team_odds',
                   "B365AHA" = 'Bet365_Asian_handicap_away_team_odds',
                   "B365AH" = 'Bet365_size_of_handicap_home_team',
                   "PAHH" = 'Pinnacle_Asian_handicap_home_team_odds',
                   "PAHA" = 'Pinnacle_Asian_handicap_away_team_odds',
                   "MaxAHH" = 'Market_maximum_Asian_handicap_home_team_odds',
                   "MaxAHA" = 'Market_maximum_Asian_handicap_away_team_odds',
                   "AvgAHH" = 'Market_average_Asian_handicap_home_team_odds',
                   "AvgAHA" = 'Market_average_Asian_handicap_away_team_odds'

  )

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
    janitor::clean_names() %>%
    dplyr::relocate(c(league, season), .before = date)


  return(fd_data)

}
