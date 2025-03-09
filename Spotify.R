# Load the libraries
library(tidyverse) 
library(dplyr)
library(ggplot2)
library(scales)
library(colorspace)  
install.packages("ggthemes")
library(ggthemes)

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Load contributed packages with pacman
pacman::p_load(pacman, party, rio, tidyverse)

# Import CSV files with readr::read_csv() from tidyverse
df <- read_csv("/Users/ctvyte/Downloads/Cleaned_Spotify_Dataset.csv")

# Inspect the data
head(df)
summary(df)

# Rename the columns 
df <- df %>%
  rename(
    artists = 'artist(s)_name',
    danceability = 'danceability_%',
    valence = 'valence_%',
    energy = 'energy_%',
    acousticness = 'acousticness_%',
    instrumentalness = 'instrumentalness_%',
    liveness = 'liveness_%',
    speechiness = 'speechiness_%'
  )

# Convert the data type in streams column to numerical
df$streams <- as.numeric(df$streams)

# VISUALISATION
# Visualisation 1: Influence of Playlist Inclusions on Streams
# Analyze how the number of Spotify, Apple, and Deezer playlists a track is included 
# in correlates with its streaming numbers.
# Reshape the data to a long format
df_long <- df %>%
  pivot_longer(cols = starts_with("in_"), 
               names_to = "playlist_type", 
               values_to = "playlist_count") %>%
  filter(playlist_type %in% c("in_spotify_playlists", "in_apple_playlists", "in_deezer_playlists"))

# Rename playlist types for better readability in the plot
df_long <- df_long %>%
  mutate(playlist_type = recode(playlist_type,
                                "in_spotify_playlists" = "Spotify Playlists",
                                "in_apple_playlists" = "Apple Playlists",
                                "in_deezer_playlists" = "Deezer Playlists"))

# Define custom colors for each playlist type
custom_colors <- c("Spotify Playlists" = "deeppink3", "Apple Playlists" = "purple", "Deezer Playlists" = "blue")

# Create the plot
ggplot(df_long, aes(x = playlist_count, y = streams, color = playlist_type)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Influence of Playlist Inclusions on Streams",
       x = "Number of Playlists",
       y = "Streams",
       color = "Playlist Type") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(legend.position = "top")

# Visualisation 2: Streams by Released Month
# Plot the streams by month
ggplot(df, aes(x = factor(released_month), y = streams, fill = released_month)) +
  geom_bar(stat = "identity") +
  labs(title = "Monthly Stream Trends", x = "Month", y = "Streams") +
  scale_y_continuous(labels = scales::comma) +
  theme_economist()

# Visualisation 3: Streams by Artist Count
# Generate a color palette with the number of unique artist counts
num_colors <- length(unique(df$artist_count))
palette <- rainbow_hcl(num_colors)
# Plot the data with the correct color mapping
ggplot(df, aes(x = factor(artist_count), y = streams, fill = factor(artist_count))) +
  geom_bar(stat = "identity") +
  labs(title = "Streams by Artist Count", x = "Number of Artists", y = "Streams") +
  scale_y_continuous(labels = scales::comma) +
  theme_clean() +
  scale_fill_manual(values = palette, guide = guide_legend(title = "Artist(s) per Song"))

# Visualisation 4: Trends in Audio Features Over Time
# Aggregate the data by year to get the average values
df_yearly <- df %>%
  group_by(released_year) %>%
  summarize(
    avg_danceability = mean(danceability, na.rm = TRUE),
    avg_valence = mean(valence, na.rm = TRUE),
    avg_energy = mean(energy, na.rm = TRUE),
    avg_acousticness = mean(acousticness, na.rm = TRUE),
    avg_instrumentalness = mean(instrumentalness, na.rm = TRUE),
    avg_liveness = mean(liveness, na.rm = TRUE),
    avg_speechiness = mean(speechiness, na.rm = TRUE)
  )

# Transform data to long format for easier plotting
df_long <- df_yearly %>%
  pivot_longer(cols = starts_with("avg_"), 
               names_to = "feature", 
               values_to = "value") %>%
  mutate(feature = recode(feature,
                          avg_danceability = "Danceability",
                          avg_valence = "Valence",
                          avg_energy = "Energy",
                          avg_acousticness = "Acousticness",
                          avg_instrumentalness = "Instrumentalness",
                          avg_liveness = "Liveness",
                          avg_speechiness = "Speechiness"))

# Plot the area chart
ggplot(df_long, aes(x = released_year, y = value, fill = feature)) +
  geom_area(alpha = 0.6) +
  labs(
    title = "Trends in Audio Features Over Time",
    x = "Year",
    y = "Average Value"
  ) +
  scale_fill_manual(
    name = "Feature",
    values = c(
      "Valence" = "#fd9e00",
      "Speechiness" = "#fb8500",
      "Liveness" = "#ffb703",
      "Instrumentalness" = "#023047",
      "Energy" = "#126782",
      "Danceability" = '#219ebc',
      "Acousticness" = "#8ecae6"
    )
  ) +
  theme_linedraw() +
  theme(legend.position = "bottom")

# Visualisation 5: Streams vs Danceability
ggplot(df, aes(x = danceability, y = streams)) +
  geom_point(color = "blue", size = 3, alpha = 0.5) +
  labs(title = "Streams vs. Danceability", x = "Danceability", y = "Streams") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  theme_pander()