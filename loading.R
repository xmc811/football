
library(tidyverse)
library(magrittr)

res <- read_csv("results.csv")
rank <- read_csv("fifa_ranking.csv")

res <- filter(res, date >= "1993-08-08" & date <= "2018-07-07")
rank <- select(rank, c(1:3,15,16))

rank_dates <- unique(rank$rank_date)


# View

rank_country <- sort(unique(rank$country_full))
res_country <- sort(unique(c(res$home_team, res$away_team)))

res_country[!res_country %in% rank_country]
rank_country[!rank_country %in% res_country]



# clean country names

rank %<>%
        mutate(country_full = replace(country_full, country_full == "Brunei Darussalam", "Brunei"),
               country_full = replace(country_full, country_full == "Cape Verde Islands", "Cape Verde"),
               country_full = replace(country_full, country_full == "IR Iran", "Iran"),
               country_full = replace(country_full, country_full == "Côte d'Ivoire", "Ivory Coast"),
               country_full = replace(country_full, country_full == "Kyrgyz Republic", "Kyrgyzstan"),
               country_full = replace(country_full, country_full == "Korea DPR", "North Korea"),
               country_full = replace(country_full, country_full == "Korea Republic", "South Korea"),
               country_full = replace(country_full, country_full == "Netherlands Antilles", "Curaçao"),
               country_full = replace(country_full, country_full == "RCS", "Czech Republic"),
               country_full = replace(country_full, country_full == "Zaire", "Congo DR"),
               country_full = replace(country_full, country_full == "Yugoslavia", "Serbia"),
               country_full = replace(country_full, country_full == "Serbia and Montenegro", "Serbia")
               )

res %<>%
        mutate(home_team = replace(home_team, home_team == "Burma", "Myanmar"),
               home_team = replace(home_team, home_team == "DR Congo", "Congo DR"),
               home_team = replace(home_team, home_team == "East Timor", "Timor-Leste"),
               home_team = replace(home_team, home_team == "Saint Kitts and Nevis", "St Kitts and Nevis"),
               home_team = replace(home_team, home_team == "Saint Vincent and the Grenadines", "St Vincent and the Grenadines"),
               home_team = replace(home_team, home_team == "Saint Lucia", "St Lucia"),
               home_team = replace(home_team, home_team == "São Tomé and Príncipe", "São Tomé e Príncipe"),
               home_team = replace(home_team, home_team == "North Macedonia", "FYR Macedonia"),
               home_team = replace(home_team, home_team == "U.S. Virgin Islands", "US Virgin Islands"),
               home_team = replace(home_team, home_team == "Eswatini", "Swaziland"),
               home_team = replace(home_team, home_team == "United States", "USA"),
               away_team = replace(away_team, away_team == "Burma", "Myanmar"),
               away_team = replace(away_team, away_team == "DR Congo", "Congo DR"),
               away_team = replace(away_team, away_team == "East Timor", "Timor-Leste"),
               away_team = replace(away_team, away_team == "Saint Kitts and Nevis", "St Kitts and Nevis"),
               away_team = replace(away_team, away_team == "Saint Vincent and the Grenadines", "St Vincent and the Grenadines"),
               away_team = replace(away_team, away_team == "Saint Lucia", "St Lucia"),
               away_team = replace(away_team, away_team == "São Tomé and Príncipe", "São Tomé e Príncipe"),
               away_team = replace(away_team, away_team == "North Macedonia", "FYR Macedonia"),
               away_team = replace(away_team, away_team == "U.S. Virgin Islands", "US Virgin Islands"),
               away_team = replace(away_team, away_team == "Eswatini", "Swaziland"),
               away_team = replace(away_team, away_team == "United States", "USA")
        )

rank_country <- sort(unique(rank$country_full))
res_country <- sort(unique(c(res$home_team, res$away_team)))

res %<>%
        filter(!home_team %in% res_country[!res_country %in% rank_country] & !away_team %in% res_country[!res_country %in% rank_country])

res <- mutate(res, 
               home_rank = map2_dbl(.x = res$home_team, .y = res$date, .f = find_rank),
               away_rank = map2_dbl(.x = res$away_team, .y = res$date, .f = find_rank))

res %<>%
        filter(home_rank != -1 & away_rank != -1)

res %<>%
        mutate(home_diff = away_rank - home_rank,
               away_diff = home_rank - away_rank)

res %<>% 
        mutate(home_1 = -neutral + 1,
               home_2 = neutral - 1)

res %<>% 
        mutate(weight = map_dbl(.x = res$tournament, .f = match_weight))

res %<>%
        mutate(n_teams = map_dbl(.x = res$date, .f = find_n_teams))

res %<>%
        mutate(home_diff = home_diff / n_teams * 200,
               away_diff = away_diff / n_teams * 200)


df <- as_tibble(mapply(c, res %>% select(c(10,12,14,16,4)), res %>% select(c(11,13,15,16,5))))
colnames(df) <- c("rank", "diff", "home", "weight", "score")


# latest ranking

rank_2018 <- filter(rank, rank_date == as.Date(rank_dates[length(rank_dates)]))


