# Load libraries ----
# 
#install.packages(c('shinyjqui', 'plotly'))
install.packages(c("gifski", "png", "grDevices"))
install.packages('DT')
install.packages('later')
install.packages("basemaps")
library(shiny)
library(leaflet)
library(tidyverse)
library(terra)
library(ncdf4)
library(sp)
library(raster)
library(sf)
library(bslib)
library(shinyWidgets)
library(base64enc)
library(leafem)
library(stars)
library(leaflet.extras)
library(rlang)
library(ggplot2)
library(shinyjqui)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(gifski)
library(png)
library(grDevices)
library(DT)
library(magick)
library(scico)
library(basemaps)

# Load processed data objects ----
data_objects <- readRDS("data/processed_data.Rds")
var_labels <- data_objects$var_labels
display_labels <- data_objects$display_labels
var_names <- data_objects$var_names
var_range_list <- data_objects$var_range_list


rast_list <- list()

for (var in var_names) {
  path <- file.path("data/processed_rasters", paste0(var, ".tif"))
  cat("Loading:", var, "\n")
  rast_list[[var]] <- rast(path)
}



# Load variable groups & palettes ----
source('R/var_groups_and_palettes.R')

# Load function for palette selection ----
source('R/get_palette_for_var.R')

# Set time sequence and labels ----
time_seq <- seq(as.Date("2014-01-01"), as.Date("2022-12-31"), by = "month")
time_labels <- format(time_seq, "%Y-%m")

# Helper infix for default values
`%||%` <- function(a, b) if (!is.null(a)) a else b