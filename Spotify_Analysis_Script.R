# ------------------------------------------------------------------------#
# Author: Manuel Mena
# Date: 26/05/2021
# Project: Spotify Analysis
# Scope: Feature analysis
# ------------------------------------------------------------------------#

# Step 0. Load libraries and functions ------------------------------------
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggdendro)

source("Spotify_Analysis_Connection.R")

# Step 1. Load data and make basic exploration ----------------------------
pearl_jam_songs <- get_artist_audio_features("pearl jam")
dim(pearl_jam_songs)
glimpse(pearl_jam_songs)

# Are there any NA's in the dataset?
colSums(!is.na(pearl_jam_songs))

# Let's count the albums and the songs per album
pearl_jam_songs %>% 
  group_by(album_id) %>%
  summarise(name = first(album_name),
            count = n()) %>%
  arrange(name) %>%
  as.data.frame() %>%
  head(25)

# We can see what songs are in Vitalogy and Vs.
pearl_jam_songs %>% 
  filter(album_name %in% c("Vitalogy","Vs.")) %>%
  select(album_name, track_name)

# Step 2. Data cleaning and wrangling -------------------------------------
pearl_jam_songs_clean <- pearl_jam_songs %>%
  select(album_id, album_name,album_release_year,
         track_name, duration_ms, explicit,
         danceability, energy, loudness,
         speechiness, acousticness, instrumentalness,
         liveness, valence, tempo) %>%
  filter(album_id %in% c("5B4PYA7wNN4WdEXdIJu58a",
                         "3BSOiAas8BpJOii3kCPyjV",
                         "5pd9B3KQWKshHw4lnsSLNy",
                         "3FKhxgSZdtJBIjdHsjbxI0",
                         "5zsDtoSrXK4usJ4MB1tCh2",
                         "1RuYprt6Qlqu286h1f4dzZ",
                         "7AOWw68DEPnDmTpquZw8bG")) %>% 
  mutate(track_name = gsub("\\(Remastered\\)","",track_name),
         track_name = gsub("- Remastered","",track_name),
         track_name = trimws(track_name)) %>%
  filter(!track_name %in% c("Hold On - bonus track",
                            "Cready Stomp - bonus track",
                            "Crazy Mary",
                            "Better Man - Guitar / Organ Only",
                            "Corduroy - Alternate Take",
                            "Nothingman - Demo"))

pearl_jam_songs_clean %>%
  select(album_name, track_name)

# Step 3. Exploratory data analysis ---------------------------------------
# Let's see the summary of the data
summary(pearl_jam_songs_clean)

# Now we can create an histogram for each measure
pearl_jam_songs_clean %>%
  select(album_name, duration_ms, explicit,
         danceability, energy, loudness,
         speechiness, acousticness, instrumentalness,
         liveness, valence, tempo) %>%
  pivot_longer(-album_name, names_to = "Measure", 
               values_to = "Value") %>%
  ggplot(aes(Value)) +
  geom_histogram(fill = "midnightblue", alpha = .9) +
  facet_wrap(~Measure, scales = "free")

# Let's see the density plot of energy by album
pearl_jam_songs_clean %>%
  select(album_name, energy) %>%
  pivot_longer(-album_name, names_to = "Measure", 
               values_to = "Value") %>%
  ggplot(aes(Value, fill = Measure)) +
  geom_density(fill = "seagreen", alpha = .7) +
  facet_wrap(~album_name, scales = "free")

# And now the density plot of loudness by album
pearl_jam_songs_clean %>%
  select(album_name, loudness) %>%
  pivot_longer(-album_name, names_to = "Measure", 
               values_to = "Value") %>%
  ggplot(aes(Value, fill = Measure)) +
  geom_density(fill = "midnightblue", color = "midnightblue", alpha = .7) +
  facet_wrap(~album_name, scales = "free")

# Step 4. Cluster analysis ------------------------------------------------


 
