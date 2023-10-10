## Source: https://ebird.github.io/ebird-best-practices/ebird.html


###### Chapter 2: eBird Data ######

setwd("C:/Research_Projects/Bird/eBird")


# 2.3 Importing eBird data into R -----------------------------------------

library(auk)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(readr)
library(sf)

# Read the checklist data.
f_sed <- file.path("Data",
                   "ebd_US-GA_woothr_smp_relAug-2023",
                   "ebd_US-GA_woothr_smp_relAug-2023_sampling.txt")
checklists <- read_sampling(f_sed)
glimpse(checklists)

# Import the observation data.
f_ebd <- file.path("Data",
                   "ebd_US-GA_woothr_smp_relAug-2023",
                   "ebd_US-GA_woothr_smp_relAug-2023.txt")
observations <- read_ebd(f_ebd)
glimpse(observations)


## 2.3.1 Filtering

# filter the checklist data
checklists <- checklists %>% 
  filter(all_species_reported, # Keep complete checklists.
         protocol_type %in% c("Stationary", "Traveling"),
         year(observation_date) >= 2014, year(observation_date) <= 2023, 
         month(observation_date) == 6)

# filter the observation data
observations <- observations %>% 
  filter(all_species_reported, # Keep complete checklists.
         protocol_type %in% c("Stationary", "Traveling"),
         year(observation_date) >= 2014, year(observation_date) <= 2023, 
         month(observation_date) == 6)


# 2.4 Zero-filling --------------------------------------------------------

# Zero-filling:
# if there is a record in the SED but no record for a species in the EBD, 
# then a count of zero individuals of that species can be inferred.
zf <- auk_zerofill(observations, checklists, collapse = TRUE)

# function to convert time observation to hours since midnight
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

# clean up variables
zf <- zf %>% 
  mutate(
    # convert X to NA
    observation_count = if_else(observation_count == "X", 
                                NA_character_, observation_count),
    observation_count = as.integer(observation_count),
    # effort_distance_km to 0 for non-travelling counts
    effort_distance_km = if_else(protocol_type != "Traveling", 
                                 0, effort_distance_km),
    # convert duration to hours
    effort_hours = duration_minutes / 60,
    # speed km/h
    effort_speed_kmph = effort_distance_km / effort_hours,
    # convert time to decimal hours since midnight
    hours_of_day = time_to_decimal(time_observations_started),
    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date)
  )


# 2.5 Accounting for variation in detectability ---------------------------

# To reduce the variation in detectability between checklists:
# Restricting checklists to those less than 6 hours in duration 
# and 10km in length, at speeds below 100km/h, 
# and with 10 or fewer observers.
# additional filtering
zf_filtered <- zf %>% 
  filter(effort_hours <= 6,
         effort_distance_km <= 10,
         effort_speed_kmph <= 100,
         number_observers <= 10)


# 2.6 Test-train split ----------------------------------------------------

# Randomly split the data into 80% of checklists for training 
# and 20% for testing.
zf_split <- zf_filtered %>% 
  mutate(type = if_else(runif(nrow(.)) <= 0.8, 
                        "train", "test"))

# confirm the proportion in each set is correct
table(zf_split$type) / nrow(zf_split)

# Remove redundant variables.
checklists <- zf_split %>% 
  select(checklist_id, observer_id, type,
         observation_count, species_observed, 
         state_code, locality_id, latitude, longitude,
         protocol_type, all_species_reported,
         observation_date, year, day_of_year,
         hours_of_day, 
         effort_hours, effort_distance_km, effort_speed_kmph,
         number_observers)

# write_csv(checklists, "data/checklists-zf_woothr_june_us-ga.csv", na = "")


# 2.7 Exploratory analysis and visualization ------------------------------

# load and project gis data to albers equal area conic projection
map_proj <- st_crs("ESRI:102003")

ne_land <- read_sf("data/gis-data.gpkg", "ne_land") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()

ne_country_lines <- read_sf("data/gis-data.gpkg", "ne_country_lines") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()

ne_state_lines <- read_sf("data/gis-data.gpkg", "ne_state_lines") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()

ga_boundary <- read_sf("data/gis-data.gpkg", "ne_states") %>% 
  filter(state_code == "US-GA") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()

# prepare ebird data for mapping
checklists_sf <- checklists %>% 
  # convert to spatial points
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = map_proj) %>% 
  select(species_observed)

# map
par(mar = c(0.25, 0.25, 0.25, 0.25))
# set up plot area
plot(st_geometry(checklists_sf), col = NA)
# contextual gis data
plot(ne_land, col = "#dddddd", border = "#888888", lwd = 0.5, add = TRUE)
plot(ga_boundary, col = "#cccccc", border = NA, add = TRUE)
plot(ne_state_lines, col = "#ffffff", lwd = 0.75, add = TRUE)
plot(ne_country_lines, col = "#ffffff", lwd = 1.5, add = TRUE)
# ebird observations
# not observed
plot(filter(checklists_sf, !species_observed),
     pch = 19, cex = 0.1, col = alpha("#555555", 0.25),
     add = TRUE)
# observed
plot(filter(checklists_sf, species_observed),
     pch = 19, cex = 0.3, col = alpha("#4daf4a", 1),
     add = TRUE)
# legend
legend("bottomright", bty = "n",
       col = c("#555555", "#4daf4a"),
       legend = c("eBird checklists", "Wood Thrush sightings"),
       pch = 19)

box()
par(new = TRUE, mar = c(0, 0, 3, 0))
title("Wood Thrush eBird Observations\nJune 2014-2023")


## 2.7.1 Time of day

# summarize data by hourly bins
breaks <- seq(0, 24)

labels <- breaks[-length(breaks)] + diff(breaks) / 2

checklists_time <- checklists %>% 
  mutate(hour_bins = cut(hours_of_day, 
                         breaks = breaks, 
                         labels = labels,
                         include.lowest = TRUE),
         hour_bins = as.numeric(as.character(hour_bins))) %>% 
  group_by(hour_bins) %>% 
  summarise(n_checklists = n(),
            n_detected = sum(species_observed),
            det_freq = mean(species_observed))

# histogram
g_tod_hist <- ggplot(checklists_time) +
  aes(x = hour_bins, y = n_checklists) +
  geom_segment(aes(xend = hour_bins, y = 0, yend = n_checklists),
               color = "grey50") +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 24, by = 3), limits = c(0, 24)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Hours since midnight",
       y = "# checklists",
       title = "Distribution of observation start times")

# frequency of detection
g_tod_freq <- ggplot(checklists_time %>% filter(n_checklists > 100)) +
  aes(x = hour_bins, y = det_freq) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 24, by = 3), limits = c(0, 24)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Hours since midnight",
       y = "% checklists with detections",
       title = "Detection frequency")

# combine
grid.arrange(g_tod_hist, g_tod_freq)


## 2.7.2 Checklist duration

# summarize data by hour long bins
breaks <- seq(0, 6)

labels <- breaks[-length(breaks)] + diff(breaks) / 2

checklists_duration <- checklists %>% 
  mutate(duration_bins = cut(effort_hours, 
                             breaks = breaks, 
                             labels = labels,
                             include.lowest = TRUE),
         duration_bins = as.numeric(as.character(duration_bins))) %>% 
  group_by(duration_bins) %>% 
  summarise(n_checklists = n(),
            n_detected = sum(species_observed),
            det_freq = mean(species_observed))

# histogram
g_duration_hist <- ggplot(checklists_duration) +
  aes(x = duration_bins, y = n_checklists) +
  geom_segment(aes(xend = duration_bins, y = 0, yend = n_checklists),
               color = "grey50") +
  geom_point() +
  scale_x_continuous(breaks = breaks) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Checklist duration (hours)",
       y = "# checklists",
       title = "Distribution of checklist durations")

# frequency of detection
g_duration_freq <- ggplot(checklists_duration %>% filter(n_checklists > 100)) +
  aes(x = duration_bins, y = det_freq) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = breaks) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Checklist duration (hours)",
       y = "% checklists with detections",
       title = "Detection frequency")

# combine
grid.arrange(g_duration_hist, g_duration_freq)


## 2.7.3 Distance traveled

# summarize data by 1 km bins
breaks <- seq(0, 10)

labels <- breaks[-length(breaks)] + diff(breaks) / 2

checklists_dist <- checklists %>% 
  mutate(dist_bins = cut(effort_distance_km, 
                         breaks = breaks, 
                         labels = labels,
                         include.lowest = TRUE),
         dist_bins = as.numeric(as.character(dist_bins))) %>% 
  group_by(dist_bins) %>% 
  summarise(n_checklists = n(),
            n_detected = sum(species_observed),
            det_freq = mean(species_observed))

# histogram
g_dist_hist <- ggplot(checklists_dist) +
  aes(x = dist_bins, y = n_checklists) +
  geom_segment(aes(xend = dist_bins, y = 0, yend = n_checklists),
               color = "grey50") +
  geom_point() +
  scale_x_continuous(breaks = breaks) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Distance travelled (km)",
       y = "# checklists",
       title = "Distribution of distance travelled")

# frequency of detection
g_dist_freq <- ggplot(checklists_dist %>% filter(n_checklists > 100)) +
  aes(x = dist_bins, y = det_freq) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = breaks) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Distance travelled (km)",
       y = "% checklists with detections",
       title = "Detection frequency")

# combine
grid.arrange(g_dist_hist, g_dist_freq)


## 2.7.4 Number of observers

# summarize data
breaks <- seq(0, 10)

labels <- seq(1, 10)

checklists_obs <- checklists %>% 
  mutate(obs_bins = cut(number_observers, 
                        breaks = breaks, 
                        label = labels,
                        include.lowest = TRUE),
         obs_bins = as.numeric(as.character(obs_bins))) %>% 
  group_by(obs_bins) %>% 
  summarise(n_checklists = n(),
            n_detected = sum(species_observed),
            det_freq = mean(species_observed))

# histogram
g_obs_hist <- ggplot(checklists_obs) +
  aes(x = obs_bins, y = n_checklists) +
  geom_segment(aes(xend = obs_bins, y = 0, yend = n_checklists),
               color = "grey50") +
  geom_point() +
  scale_x_continuous(breaks = breaks) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "# observers",
       y = "# checklists",
       title = "Distribution of the number of observers")

# frequency of detection
g_obs_freq <- ggplot(checklists_obs %>% filter(n_checklists > 100)) +
  aes(x = obs_bins, y = det_freq) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = breaks) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "# observers",
       y = "% checklists with detections",
       title = "Detection frequency")

# combine
grid.arrange(g_obs_hist, g_obs_freq)

