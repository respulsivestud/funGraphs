# spotifyAnalysis.R
# About - This script spotifyr library to get the favourite artists and their favourite songs. Then we plot the Valence vs Energy score for each song to see which songs we actually listen to
# Author - @repulsivestud
# Date - 31/05/2020

#devtools::install_github('charlie86/spotifyr')
library(tidyverse)
library(spotifyr)
library(data.table)

rm(list = ls())

Sys.setenv(SPOTIFY_CLIENT_ID = 'YOUR-SPOTIFY-CLIENT-ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOUR-SPOTIFY-CLIENT-SECRET')

access_token <- get_spotify_access_token()

favTracks <- ceiling(get_my_saved_tracks(include_meta_info = TRUE)[['total']]/50) %>% 
  seq() %>% 
  map(function(x){
    get_my_saved_tracks(limit = 50, offset = (x - 1) * 50)
  }) %>% 
  reduce(rbind)

favArtists <-
  favTracks %>% 
  select(track.album.artists) %>% 
  reduce(rbind) %>% 
  reduce(rbind) %>% 
  select(id, name)

favArtists <-
  favArtists %>%
  count(name, sort = TRUE) %>%
  unique() %>%
  top_n(20, n)

favArtists <- favArtists[favArtists$name != "Ritviz",]
favArtists <- favArtists[favArtists$name != "Various Artists",]
audioFeatures <- get_artist_audio_features("Tool")
audioFeatures <- audioFeatures[audioFeatures$artist_name != "TOOL",]

for (name in favArtists$name) {
  audioFeatures <- rbind(audioFeatures, get_artist_audio_features(name))
}

favSongsValence <- inner_join(audioFeatures, favTracks, 
by = c("track_id" = "track.id"))

ggplot(data = favSongsValence, aes(x = valence, y = energy, color = artist_name)) +
  geom_jitter() +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.25 / 2, 0.95, label = "Turbulent/Angry", fontface =
             "bold") +
  annotate('text', 1.75 / 2, 0.95, label = "Happy/Joyful", fontface = "bold") +
  annotate('text', 1.75 / 2, 0.05, label = "Chill/Peaceful", fontface =
             "bold") +
  annotate('text', 0.25 / 2, 0.05, label = "Sad/Depressing", fontface =
             "bold")
