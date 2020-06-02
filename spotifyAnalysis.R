# spotifyAnalysis.R
# About - This script spotifyr library to get the favourite artists and their favourite songs. Then we plot the Valence vs Energy score for each song to see which songs we actually listen to
# Author - @repulsivestud
# Date - 31/05/2020

#devtools::install_github('charlie86/spotifyr')
library(tidyverse)
library(spotifyr)
library(ggthemes)
library(data.table)

#I like to clear out all the environment variables. 
rm(list = ls())

#Get a Spotify for Developer's account and set your client ID and secret here
Sys.setenv(SPOTIFY_CLIENT_ID = 'YOUR-CLIENT-ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOUR-CLIENT-SECRET')

#This authenticates you and gets an access token to connect to Spotify API
access_token <- get_spotify_access_token()

#Gets favourite tracks. Spotify has a limit of 50 results at a time, so we have to call the same function multiple times with an offset to get all the tracks
favTracks <- ceiling(get_my_saved_tracks(include_meta_info = TRUE)[['total']]/50) %>% 
  seq() %>% 
  map(function(x){
    get_my_saved_tracks(limit = 50, offset = (x - 1) * 50)
  }) %>% 
  reduce(rbind)

#Extracts favourite artists from the favourite songs
favArtists <-
  favTracks %>% 
  select(track.album.artists) %>% 
  reduce(rbind) %>% 
  reduce(rbind) %>% 
  select(id, name)

#Select top 20 artists
favArtists <-
  favArtists %>%
  count(name, sort = TRUE) %>%
  unique() %>%
  top_n(20, n)

#Some minor data jugglery here. This needs to be cleaned up.
favArtists <- favArtists[favArtists$name != "Ritviz",]
favArtists <- favArtists[favArtists$name != "Various Artists",]
audioFeatures <- get_artist_audio_features("Tool")
audioFeatures <- audioFeatures[audioFeatures$artist_name != "TOOL",]

#Get all the audiofeatures of all the songs for the artists
for (name in favArtists$name) {
  audioFeatures <- rbind(audioFeatures, get_artist_audio_features(name))
}

#Get all the audiofeatures of all the songs that I like from the artists
favSongsValence <- inner_join(audioFeatures, favTracks, 
by = c("track_id" = "track.id"))

#Plot Valence vs Enegery of the songs by Artists
ggplot(data = favSongsValence, aes(x = valence, y = energy, color = artist_name, size = 1)) +
  geom_jitter() +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.25 / 2, 0.95, label = "Turbulent/Angry", fontface =
             "bold", size = 6) +
  annotate('text', 1.75 / 2, 0.95, label = "Happy/Joyful", fontface = "bold", size = 6) +
  annotate('text', 1.75 / 2, 0.05, label = "Chill/Peaceful", fontface =
             "bold", size = 6) +
  annotate('text', 0.25 / 2, 0.05, label = "Sad/Depressing", fontface =
             "bold", size = 6) +
  scale_colour_viridis_d() + scale_size(guide="none") +
labs(title = 'Mood Analysis of my top Spotify Artists', subtitle = "For my favourite songs from these artists", x = 'Valence', y = 'Enegergy', caption="© Omkar Shukla (@shukla_omkar)\n Data source: Spotify API", color = "Artists")

#Save it and enjoy!
ggsave("SpotifyAnalysis.png", height = 8, width = 12, units = "in", dpi = 300)