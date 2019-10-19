
find_rank <- function(country, date) {
        
        i <- sum((date - rank_dates) >= 0)
        df <- filter(rank, rank_date == rank_dates[i] & country_full == country)
        
        if (nrow(df) == 1) {
                return(df$rank)
        } else {
                return(-1)
        }
}



find_n_teams <- function(date) {
        i <- sum((date - rank_dates) >= 0)
        n <- nrow(filter(rank, rank_date == rank_dates[i]))
        return(n)
}



match_weight <- function(tournament) {
        
        if (tournament %in% c("FIFA World Cup",
                              "Confederations Cup",
                              "AFC Asian Cup",
                              "African Cup of Nations",
                              "Copa América",
                              "Gold Cup",
                              "Oceania Nations Cup",
                              "UEFA Euro")) {
                return(2)
        } else if (tournament %in% c("FIFA World Cup qualification",
                                     "AFC Asian Cup qualification",
                                     "African Cup of Nations qualification",
                                     "Copa América qualification",
                                     "Gold Cup qualification",
                                     "Oceania Nations Cup qualification",
                                     "UEFA Euro qualification")) {
                return(1)
        } else {
                return(0)
        }
}