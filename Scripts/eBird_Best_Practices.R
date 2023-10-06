# Source: https://ebird.github.io/ebird-best-practices/


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

