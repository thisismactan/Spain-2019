#### LIBRARIES ####
if(!require(tidyverse)) {
  install.packages(c("httr", "xml2", "data.table", "Hmisc", "lubridate", "reshape2", "tidyverse", "lme4", "mvnfast", "Rfast", "leaflet",
                     "rgdal", "rmapshaper", "sf", "sp"))
}

## Scraping
library(httr)
library(xml2)

## Data manipulation and cleaning
library(data.table)
library(Hmisc)
library(lubridate)
library(reshape2)
library(tidyverse)

## Simulation
library(lme4)
library(mvnfast)
library(Rfast)

## Mapping
library(leaflet)
library(rgdal)
library(rmapshaper)
library(sf)
library(sp)

#### PARTY COLORS ####
party_labels <- c("pp" = "People's Party (PP)",
                  "psoe" = "Socialist Workers' Party (PSOE)",
                  "up" = "Unidos Podemos",
                  "ciudadanos" = "Ciudadanos",
                  "catalan_republican" = "Rep. Left of Catalonia (ERC)",
                  "catalan_european_democrat" = "Catalan Euro. Dem. Party (PDeCAT)",
                  "basque_nationalist" = "Basque National Party (EAJ)",
                  "animalist" = "Animalist",
                  "eh_bildu" = "EH Bildu",
                  "canarian_coalition" = "Canarian Coalition",
                  "compromis" = "Coalició Compromís",
                  "vox" = "Vox")

Encoding(party_labels) <- "UTF-8"

party_colors <- c("pp" = "#008CD7",
                  "psoe" = "red",
                  "up" = "#683064",
                  "ciudadanos" = "#FA5000",
                  "catalan_republican" = "gold",
                  "catalan_european_democrat" = "darkturquoise",
                  "basque_nationalist" = "forestgreen",
                  "animalist" = "darkolivegreen",
                  "eh_bildu" = "deeppink",
                  "canarian_coalition" = "yellow",
                  "compromis" = "#E78955",
                  "vox" = "#5AC035")

major_party_labels <- party_labels[c(1:4, 12)]
major_party_colors <- party_colors[c(1:4, 12)]

#### CUSTOM FUNCTIONS ####
fread_to_tbl <- function(file) {
  data <- data.table::fread(file) %>%
    as.data.frame() %>%
    as.tbl()
  return(data)
}

logit <- function(x) {
  logit_x <- log(x/(1-x))
  return(logit_x)
}

invlogit <- function(x) {
  invlogit_x <- exp(x)/(1+exp(x))
  return(invlogit_x)
}

process_spain_data <- function(year) {
  require(dplyr)
  results <- read_csv(paste0("Data/results_", year, ".csv")) %>%
    na.omit()
  Encoding(results$party) <- "UTF-8"
  return(results)
}
