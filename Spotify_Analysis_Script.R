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


