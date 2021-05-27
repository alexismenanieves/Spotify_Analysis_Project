# Step 0.  Load libraries and functions -----------------------------------
library(httr)
library(tidyr)
library(dplyr)
library(purrr)
library(jsonlite)
library(glue)
library(lubridate)

# This function retrieves the access token from Spotify via its API
get_spotify_access <- function(
  # Environment variables are saved in .Renviron
  client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
  client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")) {
  # Now we call the API via httr, using POST method
  post <- RETRY('POST','https://accounts.spotify.com/api/token',
                accept_json(), authenticate(client_id, client_secret),
                body = list(grant_type = "client_credentials"),
                encode = "form", 
                httr::config(http_version = 2)) %>%
    content
  # In case there is no result due to bad authentication
  if(!is.null(post$error)){
    stop(glue('Could not authenticate with given credentials:\n',
              '{post$error_description}'))
  }
  # The access token is returned
  access_token <- post$access_token
}

# This is the main function that retrieves artist, albums and tracks
get_artist_audio_features <- function(
  artist = NULL, include_groups = 'album', 
  return_closest_artist = TRUE, dedupe_albums = TRUE,
  authorization = get_spotify_access()) {
  
  if(is_uri(artist)){
    artist_info = get_artist(
      artist, authorization = get_spotify_access)
    artist_id <- artist_info$id
    artist_name <- artist_id$name
  } else {
    artist_ids <- search_spotify(
      artist, "artist", authorization = authorization)
    
    if(return_closest_artist){
      artist_id <- artist_ids$id[1]
      artist_name <- artist_ids$name[1]
    } else {
      choices <- map_char(1:length(artist_ids$name), function(x) {
        glue("[{x}] {artist_ids$name[x]}")
      }) %>% paste0(collapse = '\n\t')
      
      cat(glue("Your artist {artist} could be in this list",
               "\nSelect the number"))
      selection <- as.numeric(readline())
      artist_id <- artist_ids$id[selection]
      artist_name <- artist_ids$name[selection]
    }
  }
  
  artist_albums <- get_artist_albums(
    artist_id, include_groups = include_groups,
    include_meta_info = TRUE, authorization = authorization)

  num_loops_albums <- ceiling(artist_albums$total / 20)
  if(num_loops_albums > 1) {
    artist_albums <- map_df(1:num_loops_albums, function(x){
      get_artist_albums(
        artist_id, include_groups = include_groups,
        offset = (x-1) * 20, authorization = authorization)
    })
  } else {
    artist_albums <- artist_albums$items
  }

  artist_albums <- artist_albums %>%
    rename(album_id = id,
           album_name = name) %>%
    mutate(album_release_year = case_when(
      release_date_precision == "year" ~
        suppressWarnings(as.numeric(release_date)),
      release_date_precision == "day" ~
        year(as.Date(release_date,"%Y-%m-%d",origin = "1970-01-01")),
      TRUE ~ as.numeric(NA)
    ))
  
  album_tracks <- map_df(artist_albums$album_id, function(x) {
    print(x)
    album_tracks <- get_album_tracks(x, include_meta_info = TRUE,
                                     authorization = authorization)
    num_loops_tracks <- ceiling(album_tracks$total / 20)
    if(num_loops_tracks > 1) {
      album_tracks <- map_df(1:num_loops_tracks, function(y){
        get_album_tracks(x, offset = (y -1)*20,
                         authorization = authorization)
      })
    } else {
      album_tracks <- album_tracks$items
    }

    album_tracks <- album_tracks %>%
      mutate(album_id = x,
             album_name =
               artist_albums$album_name[artist_albums$album_id == x]) %>%
      rename(track_name = name,
             track_uri = uri,
             track_preview_url = preview_url,
             track_href = href,
             track_id = id)
  })

  total_loop_tracks <- ceiling(nrow(album_tracks)/100)
  track_audio_features <- map_df(
    1:total_loop_tracks, function(x){
      track_ids <- album_tracks %>%
        slice(((x*100)-99):(x*100)) %>%
        pull(track_id)
      get_track_audio_features(track_ids, authorization = authorization)
    }
  ) %>%
    select(-c("duration_ms","type","uri","track_href")) %>%
    rename(track_id = id) %>%
    left_join(album_tracks, by = "track_id")

  artist_albums %>%
    mutate(artist_name = artist_name,
           artist_id = artist_id) %>%
    select(artist_name, artist_id, album_id,
           album_type, album_images = images,
           album_release_date = release_date,
           release_year,
           album_release_date_precision = release_date_precision) %>%
    left_join(track_audio_features, by = "album_id") %>%
    mutate(key_name = pitch_class_lookup[key + 1],
           mode_name = case_when(
             mode == 1 ~ "major",
             mode == 0 ~"minor",
             TRUE ~ as.character(NA)
           ))
}

get_artist <- function(id, authorization = get_spotify_access()) {
  base_url <- "https://api.spotify.com/v1/artists"
  params <- list(access_token = authorization)
  url <- glue("{base_url}/{id}")
  res <- GET(url, query = params, encode = "json")
  stop_for_status(res)
  res <- fromJSON(content(res, as = "text", encoding = "utf-8"), 
                  flatten = TRUE)
  return(res)
} 

get_artist_albums <- function(
  id, include_groups = c("album","single","appears_on","compilation"),
  market = NULL, limit = 20, offset = 0, 
  authorization = get_spotify_access(),
  include_meta_info = FALSE){
  
  base_url <- 'https://api.spotify.com/v1/artists'
  
  if(!is.null(market)){
    if(!grepl('^[[:alpha:]]{2}$',market)){
      stop("Argument market must be ISO 3166-1 alpha-2 country code")
    }
  }
  
  params <- list(
    include_groups = paste(include_groups, collapse = ","),
    market = market,
    limit = limit,
    offset = offset,
    access_token = authorization
  )
  
  url <- glue("{base_url}/{id}/albums")
  
  res <- GET(url, query = params, encode = "json")
  
  stop_for_status(res)
  
  res <- fromJSON(content(res, as = "text", encoding = "utf-8"), 
                  flatten = TRUE)
  
  if(!include_meta_info){
    res <- res$items
  }
  
  return(res)
}

get_album_tracks <- function(
  id, limit = 20, offset = 0, market = NULL,
  authorization = get_spotify_access(),
  include_meta_info = FALSE) {
  
  base_url <- 'https://api.spotify.com/v1/albums'
  
  if(!is.null(market)){
    if(!grepl('^[[:alpha:]]{2}$',market)){
      stop("Argument market must be ISO 3166-1 alpha-2 country code")
    }
  }
  
  params <- list(
    market = market,
    offset = offset,
    limit = limit,
    access_token = authorization
  )
  
  url <- glue("{base_url}/{id}/tracks")
  res <- RETRY("GET", url, query = params, encode = "json")
  stop_for_status(res)
  
  res <- fromJSON(content(res, as = "text", encoding = "utf-8"),
                  flatten = TRUE)
  
  if(!include_meta_info){
    res <- res$items
  }
  
  return(res)
}

get_track_audio_features <- function(
  ids, authorization = get_spotify_access()) {
  
  stopifnot(length(ids)<=100)
  base_url <- "https://api.spotify.com/v1/audio-features"
  params <- list(
    access_token = authorization,
    ids = paste0(ids, collapse = ",")
  )
  
  res <- RETRY("GET", base_url, query = params, encode = "json")
  stop_for_status(res)
  res <- fromJSON(content(res, as = "text", encoding = "utf-8"),
                  flatten = TRUE) %>%
    .$audio_features %>%
    as_tibble()
  
  return(res)
}

search_spotify <- function(
  q, type = c("album","artist","playlist","track"), market = NULL,
  limit = 20, offset = 0, include_external = NULL,
  authorization = get_spotify_access(), include_meta_info = FALSE
){
  
  base_url <- "https://api.spotify.com/v1/search"
  
  if(!is.character(q)){
    stop("Argument q must be a string")
  }
  
  if(!is.null(market)){
    if(!grepl('^[[:alpha:]]{2}$',market)){
      stop("Argument market must be ISO 3166-1 alpha-2 country code")
    }
  }
  
  if(limit < 1 | limit > 50 | !is.numeric(limit)){
    stop("Argument limit must be a number between 1 and 50")
  }
  
  if(offset < 0 | offset > 10000 | !is.numeric(offset)){
    stop("Argument offset must be a number between 0 and 10000")
  }
  
  if(!is.null(include_external)){
    if(include_external != "audio"){
      stop("Argument include external must be 'audio' or null")
    }
  }
  
  params <- list(
    q = q,
    type = paste(type,collapse = ","),
    market = market,
    limit = limit,
    offset = offset,
    include_external = include_external,
    access_token = authorization
  )
  
  res <- RETRY('GET', base_url, query = params, encode = "json")
  stop_for_status(res)
  
  res <- fromJSON(content(res, as = "text", encoding = "utf-8"),
                  flatten = TRUE)
  
  if(!include_meta_info && length(type) == 1){
    res <- res[[glue("{type}s")]]$items %>%
    as_tibble
  }
  
  return(res)
}

is_uri <- function(x) {
  nchar(x) == 22 &
    !grepl(" ",x) &
    grepl("[[:digit:]]",x) &
    grepl("[[:lower:]]",x) &
    grepl("[[:upper:]]",x)
}