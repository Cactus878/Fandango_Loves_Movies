# Created: 14/05/2025 
# Creator: Yanni Karlaftis
# Purpose: Clean fandango_score_comparison.csv 

library(readr)
library(dplyr)
library(writexl)

#setwd("C:/Users/yanni/Documents/GitHub/Fandango_Loves_Movies")

fandango_score_comparison <- read_csv("fandango_score_comparison.csv")

# Fix deceptive data issue
fandango_score_comparison <- fandango_score_comparison %>%
  filter(Metacritic_user_vote_count > 30, IMDB_user_vote_count > 30)

# Select only specific columns for visualization
movie_ratings <- fandango_score_comparison %>%
  select(FILM, Fandango_Stars, IMDB_norm_round, RT_norm_round, Metacritic_norm_round, Metacritic_user_norm_round, RT_user_norm_round) %>%
  rename(
    Film = FILM,
    Fandango_Rating = Fandango_Stars,
    IMDB_Rating = IMDB_norm_round,
    RT_Rating = RT_norm_round,
    Metacritic_Rating = Metacritic_norm_round,
    Metacritic_User_Rating = Metacritic_user_norm_round,
    RT_User_Rating = RT_user_norm_round
  )

# Remove the year from the movie names
movie_ratings <- movie_ratings %>% mutate(Film = gsub('\\(2015)', '', Film), 
                                          Film = gsub('\\(2014)', '', Film) )

# Calculate distributions
fandango_distribution <- movie_ratings %>%
  group_by(Fandango_Rating) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count) * 100) %>% mutate(Source = "Fandango")

IMDB_distribution <- movie_ratings %>%
  group_by(IMDB_Rating) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count) * 100) %>% mutate(Source = "IMDB")

RT_distribution <- movie_ratings %>%
  group_by(RT_Rating) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count) * 100) %>% mutate(Source = "Rotten Tomatoes")

Metacritic_distribution <- movie_ratings %>%
  group_by(Metacritic_Rating) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count) * 100) %>% mutate(Source = "Metacritic")

Metacritic_user_distribution <- movie_ratings %>%
  group_by(Metacritic_User_Rating) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count) * 100) %>% mutate(Source = "Metacritic User")

RT_user_distribution <- movie_ratings %>%
  group_by(RT_User_Rating) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count) * 100) %>% mutate(Source = "Rotten Tomatoes User")

# Rename rating columns to the same name so we can bind rows
fandango_distribution <- fandango_distribution %>% rename(Rating = Fandango_Rating)
IMDB_distribution <- IMDB_distribution %>% rename(Rating = IMDB_Rating)
RT_distribution <- RT_distribution %>% rename(Rating = RT_Rating)
Metacritic_distribution <- Metacritic_distribution %>% rename(Rating = Metacritic_Rating)
RT_user_distribution <- RT_user_distribution %>% rename(Rating = RT_User_Rating)
Metacritic_user_distribution <- Metacritic_user_distribution %>% rename(Rating = Metacritic_User_Rating)

# Combine into one dataframe
final_data <- bind_rows(fandango_distribution, IMDB_distribution, RT_distribution, Metacritic_distribution, Metacritic_user_distribution, RT_user_distribution)
final_data$Source <- factor(final_data$Source)

write_xlsx(movie_ratings, "movie_ratings.xlsx")

write_xlsx(final_data, "final_data.xlsx")
