library(httr)
library(tidyr)
library(dplyr)
library(purrr)
library(jsonlite)
library(glue)

get_spotify_access <- function(
  client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
  client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")) {
  post <- RETRY('POST','https://accounts.spotify.com/api/token',
                accept_json(), authenticate(client_id, client_secret),
                body = list(grant_type = "client_credentials"),
                encode = "form", 
                httr::config(http_version = 2)) %>%
    content
  
  if(!is.null(post$error)){
    stop(glue('Could not authenticate with given credentials:\n',
              '{post$error_description}'))
  }
  
  access_token <- post$access_token
}

get_artist_audio_features <- function(
  artist = NULL, include_groups = 'albums', 
  return_closest_artist = TRUE, dedupe_albums = TRUE,
  authorization = get_spotify_access()) {
  if(is_uri(artist)){
    artist_info = get_artist(
      artist, authorization = get_spotify_access)
    artist_id <- artist_info$id
    artist_name <- artist_id$name
  } else {
    artist_ids <- search_spotify(
      artist, "artist", autorizathion = authorization)
    if(return_closest_artist){
      artist_id <- artists_ids$id[1]
      artist_name <- artists_ids$name[1]
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
        include_meta_info = TRUE, autorization = autorization,
        offset = (x-1) * 20)
    })
  } else {
    artist_albums <- artist_albums$items
  }
  
  artist_albums <- artist_albums %>%
    rename(album_id = id,
           album_name = name) %>%
    mutate(album_release_year = case_when(
      release_date_precision = "year" ~ 
        suppressWarnings(as.numeric(release_date)),
      release_date_precision = "day" ~
        year(as.Date(release_date,"%Y-%m-%d",origin = "1970-01-01")),
      TRUE ~ as.numeric(NA)
    ))
  
  album_tracks <- map_df(artist_albums$album_id, function(x) {
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
      tracks_id <- album_tracks %>%
        slice(((x*100)-99):(x*100)) %>%
        pull(track_id)
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