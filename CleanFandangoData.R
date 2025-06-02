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

# Remove the year from the movie names
fandango_score_comparison <- fandango_score_comparison %>% mutate(FILM = gsub('\\(2015)', '', FILM), 
                                                                  FILM = gsub('\\(2014)', '', FILM) )

# Select only specific columns for visualization (normalized)
movie_ratings_normalized <- fandango_score_comparison %>%
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

# Select only specific columns for visualization (un-normalized)
movie_ratings_unnormalized <- fandango_score_comparison %>%
  select(FILM, Fandango_Stars, IMDB, RottenTomatoes, Metacritic, Metacritic_User, RottenTomatoes_User) %>%
  rename(
    Film = FILM,
    Fandango_Rating = Fandango_Stars,
    IMDB_Rating = IMDB,
    RT_Rating = RottenTomatoes,
    Metacritic_Rating = Metacritic,
    Metacritic_User_Rating = Metacritic_User,
    RT_User_Rating = RottenTomatoes_User
  )

# Calculate distributions
fandango_distribution_normalized <- movie_ratings_normalized %>%
  group_by(Fandango_Rating) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count) * 100) %>% mutate(Source = "Fandango")

IMDB_distribution_normalized <- movie_ratings_normalized %>%
  group_by(IMDB_Rating) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count) * 100) %>% mutate(Source = "IMDB")

RT_distribution_normalized <- movie_ratings_normalized %>%
  group_by(RT_Rating) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count) * 100) %>% mutate(Source = "Rotten Tomatoes")

Metacritic_distribution_normalized <- movie_ratings_normalized %>%
  group_by(Metacritic_Rating) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count) * 100) %>% mutate(Source = "Metacritic")

Metacritic_user_distribution_normalized <- movie_ratings_normalized %>%
  group_by(Metacritic_User_Rating) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count) * 100) %>% mutate(Source = "Metacritic User")

RT_user_distribution_normalized <- movie_ratings_normalized %>%
  group_by(RT_User_Rating) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count) * 100) %>% mutate(Source = "Rotten Tomatoes User")

# Rename rating columns to the same name so we can bind rows
fandango_distribution_normalized <- fandango_distribution_normalized %>% rename(Rating = Fandango_Rating)
IMDB_distribution_normalized <- IMDB_distribution_normalized %>% rename(Rating = IMDB_Rating)
RT_distribution_normalized <- RT_distribution_normalized %>% rename(Rating = RT_Rating)
Metacritic_distribution_normalized <- Metacritic_distribution_normalized %>% rename(Rating = Metacritic_Rating)
RT_user_distribution_normalized <- RT_user_distribution_normalized %>% rename(Rating = RT_User_Rating)
Metacritic_user_distribution_normalized <- Metacritic_user_distribution_normalized %>% rename(Rating = Metacritic_User_Rating)

# Combine into one dataframe
final_data_normalized <- bind_rows(fandango_distribution_normalized, IMDB_distribution_normalized, RT_distribution_normalized, Metacritic_distribution_normalized, Metacritic_user_distribution_normalized, RT_user_distribution_normalized)
final_data_normalized$Source <- factor(final_data_normalized$Source)

# Create new files for app
write_xlsx(movie_ratings_normalized, "movie_ratings_normalized.xlsx")

write_xlsx(movie_ratings_unnormalized, "movie_ratings_unnormalized.xlsx")

write_xlsx(final_data_normalized, "final_data_normalized.xlsx")
