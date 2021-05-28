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
library(dendextend)

source("Spotify_Analysis_Connection.R")
source("Spotify_Analysis_Plot_Config.R")

# PART A - Pearl Jam 

# Step 1. Load data and make basic exploration ----------------------------
pearl_jam_songs <- retrieve_audio_features("pearl jam")
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
                         "1RuYprt6Qlqu286h1f4dzZ")) %>% 
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
  facet_wrap(~Measure, scales = "free") +
  labs(
    title = "Album feature histogram",
    subtitle = "Artist: Pearl Jam",
    caption = "Source: Spotify API",
    x = "value",
    y = "count"
  )

# Let's see the density plot of energy by album
pearl_jam_songs_clean %>%
  select(album_name, energy) %>%
  pivot_longer(-album_name, names_to = "Measure", 
               values_to = "Value") %>%
  ggplot(aes(Value, fill = Measure)) +
  geom_density(fill = "seagreen", color = "seagreen", alpha = .7) +
  facet_wrap(~album_name, scales = "free") +
  labs(
    title = "Energy density plot by album",
    subtitle = "Artist: Pearl Jam",
    caption = "Source: Spotify API",
    x = "value",
    y = "density"
  )

# And now the density plot of loudness by album
pearl_jam_songs_clean %>%
  select(album_name, loudness) %>%
  pivot_longer(-album_name, names_to = "Measure", 
               values_to = "Value") %>%
  ggplot(aes(Value, fill = Measure)) +
  geom_density(fill = "midnightblue", color = "midnightblue", alpha = .7) +
  facet_wrap(~album_name, scales = "free") +
  labs(
    title = "Loudness density plot by album",
    subtitle = "Artist: Pearl Jam",
    caption = "Source: Spotify API",
    x = "value",
    y = "count"
  )

# Step 4. Cluster analysis ------------------------------------------------
songs_distance <- pearl_jam_songs_clean %>%
  select(-c("album_id","album_name","track_name",
            "album_release_year","explicit")) %>%
  as.matrix() %>%
  scale(center = TRUE, scale = TRUE) %>% 
  dist()

songs_hclust <- hclust(songs_distance, method = "average")
dend <- songs_hclust %>% as.dendrogram()
plot(dend)

album_colors <- pearl_jam_songs_clean$album_name %>%
  recode("Ten" = "red", "Vs." = "grey", "Vitalogy" = "cyan1",
         "No Code" = "green", "Yield" = "yellow", "Binaural" = "orange")

album_colors <- album_colors[order.dendrogram(dend)]
labels(dend) <- pearl_jam_songs_clean$track_name[order.dendrogram(dend)]

par(mar = c(2,2,2,12))
par(family = 'Avenir')
dend %>% 
  set("branches_col", "gray30") %>% 
  set("labels_col", "gray30") %>%
  set("labels_cex", 0.6) %>%
  set("leaves_pch", 15) %>%
  set("leaves_col", album_colors) %>%
  set("nodes_cex", 0.85) %>% 
  plot(horiz = TRUE, main = list("Pearl Jam Song Similarity", cex = 1.5))
legend("topleft", cex = 0.5, title = 'Album',
       legend = c('Ten', 'Vs.', 'Vitalogy', 
                  'No Code','Yield','Binaural'), 
       fill = c('red', 'grey', 'cyan1', 'green', 'yellow', 
                'orange'))
 
songs_distance <- pearl_jam_songs_clean %>% 
  mutate(track_name = substring(track_name,1,12)) %>%
  tibble::remove_rownames() %>%
  tibble::column_to_rownames(var = "track_name") %>%
  select(-c("album_id","album_name",
            "album_release_year","explicit")) %>%
  scale(center = TRUE, scale = TRUE)

hc <- hclust(dist(songs_distance), method = "average")
hcdata <- dendro_data(hc, type = "rectangle")
ggplot() +
  geom_segment(data = segment(hcdata),
               aes(x = x, y = y, xend = xend, yend = yend),
               size = .5) +
  geom_text(data = label(hcdata),
            aes(x = x, y = y, label = label, hjust = 0),
            size = 2) +
  coord_flip() +
  scale_y_reverse(expand = c(0.2,0))
labs <- label(hcdata)

labs$group <- sort_label$sort_label

sort_label <- hcdata$labels$label
sort_label <- as.data.frame(sort_label) %>%
  rename(link = sort_label)
nesuno <- pearl_jam_songs_clean %>%
  select(album_name, track_name) %>%
  mutate(track_name = substring(track_name,1,12))

sort_label <- sort_label %>%
  left_join(nesuno, by = c("link"="track_name"))

labs$group <-sort_label$album_name

# Create ggdendogram
ggplot() +
  geom_segment(data = segment(hcdata),
               aes(x = x, y = y, xend = xend, yend = yend),
               size = .5) +
  geom_text(data = label(hcdata),
            aes(x = x, y = y, label = label, hjust = 0, 
                colour = labs$group),
            size = 3) +
  coord_flip() +
  scale_y_reverse(expand = c(0.2,0))

# PART B - Pedro Suarez-Vertiz

# Step 1. Load data and make basic exploration ----------------------------
pedro_songs <- retrieve_audio_features("pedro suarez vertiz")

dim(pedro_songs)
glimpse(pedro_songs)

# Are there any NA's in the dataset?
colSums(!is.na(pedro_songs))

# Let's count the albums and the songs per album
pedro_songs %>% 
  group_by(album_id) %>%
  summarise(name = first(album_name),
            year = first(album_release_year),
            count = n()) %>%
  arrange(name) %>%
  as.data.frame() %>%
  head(25)

# We can see what songs are in Pontelo en la Lengua and Talk Show albums
pedro_songs %>% 
  filter(album_id %in% c("0MJmowf2LOm4U6TJmXHnW1",
                         "01nnzNP8wRSGdwmXne0a8N",
                         "6jTyptkI9CVq0qAgQsoxUd",
                         "7huVX9xQKmsWtRQrS7LCbB")) %>%
  select(album_name, track_name)

# Step 2. Data cleaning and wrangling -------------------------------------
pedro_songs_clean <- pedro_songs %>%
  select(album_id, album_name,album_release_year,
         track_name, duration_ms, explicit,
         danceability, energy, loudness,
         speechiness, acousticness, instrumentalness,
         liveness, valence, tempo) %>%
  filter(!album_id %in% c("5Wb51Re0iEMPBKCT1uFDRa",
                         "7BjsgXmi5ZPl2rT30HLFs8",
                         "5Ys57kjTWys8pkMMM3guJk",
                         "6jTyptkI9CVq0qAgQsoxUd")) %>% 
  mutate(track_name = substring(track_name,1,20)) %>%
  filter(!track_name %in% c("Bailar (Remix)",
                            "Rara Soledad (Incidental)"))

pedro_songs_clean %>%
  select(album_name, track_name)

# Step 3. Exploratory data analysis ---------------------------------------
# Let's see the summary of the data
summary(pedro_songs_clean)

# Now we can create an histogram for each measure
pedro_songs_clean %>%
  select(album_name, duration_ms, explicit,
         danceability, energy, loudness,
         speechiness, acousticness, instrumentalness,
         liveness, valence, tempo) %>%
  pivot_longer(-album_name, names_to = "Measure", 
               values_to = "Value") %>%
  ggplot(aes(Value)) +
  geom_histogram(fill = "midnightblue", alpha = .9) +
  facet_wrap(~Measure, scales = "free") +
  labs(
    title = "Album feature histogram",
    subtitle = "Artist: Pedro Suarez-Vertiz",
    caption = "Source: Spotify API",
    x = "value",
    y = "count"
  )

# Let's see the density plot of energy by album
pedro_songs_clean %>%
  select(album_name, energy) %>%
  pivot_longer(-album_name, names_to = "Measure", 
               values_to = "Value") %>%
  ggplot(aes(Value, fill = Measure)) +
  geom_density(fill = "seagreen", color = "seagreen", alpha = .7) +
  facet_wrap(~album_name, scales = "free") +
  labs(
    title = "Energy density plot by album",
    subtitle = "Artist: Pedro Suarez-Vertiz",
    caption = "Source: Spotify API",
    x = "value",
    y = "density"
  )

# And now the density plot of loudness by album
pedro_songs_clean %>%
  select(album_name, loudness) %>%
  pivot_longer(-album_name, names_to = "Measure", 
               values_to = "Value") %>%
  ggplot(aes(Value, fill = Measure)) +
  geom_density(fill = "midnightblue", color = "midnightblue", alpha = .7) +
  facet_wrap(~album_name, scales = "free") +
  labs(
    title = "Loudness density plot by album",
    subtitle = "Artist: Pedro Suarez-Vertiz",
    caption = "Source: Spotify API",
    x = "value",
    y = "count"
  )
