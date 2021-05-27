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
str(pearl_jam_songs)
glimpse(pearl_jam_songs)

pearl_jam_songs %>% 
  group_by(album_id) %>%
  summarise(name = first(album_name)) %>%
  arrange(name) %>%
  as.data.frame() %>%
  head(25)

# Step 2.  Data cleaning and wrangling ------------------------------------
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
                         "7AOWw68DEPnDmTpquZw8bG"))

pearl_jam_songs_clean %>% 
  group_by(album_id) %>% 
  summarise(name = first(album_name),
            count = n()) %>%
  arrange(name) %>%
  as.data.frame() %>%
  head(25)

pearl_jam_songs_clean %>% 
  filter(album_name == "Vitalogy") %>%
  select(track_name)

