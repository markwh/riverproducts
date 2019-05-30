# Globals for shiny app

# library(shinyFiles)
library(shinythemes)
library(shinydashboard)
library(rivertile)
# devtools::load_all("~/Documents/rivertile")
library(ncdf4)
library(fs)
library(ggplot2)
library(leaflet)
library(purrr)
library(dplyr)
library(tidyr)
library(plotly)
library(crosstalk)
library(DT)
library(gdata)
library(RColorBrewer)


theme_set(theme_bw())

# Pass data
load("rdata/sacpasses_sf.RData")
passpal <- colorFactor("Set1", sacpasses_sf$pass)

# Pixel formatting
classes <- c(1, 2, 3, 4, 22, 23, 24)
classlabs <- c("gdem water", "land near water", "water near land", 
               "open water", "land near dark water", 
               "dark water edge", "dark water")
classpal <- colorFactor(palette = "Set1", domain = classes)
classcolvec <- setNames(classpal(classes), classes)

fmt_pixc_data <- function(node_index, height, pixel_area, water_frac, cross_track) {
  annotvars <- c("node_index", "height", "pixel_area", "water_frac",
                 "cross_track")
  paste(annotvars,
        sep = ": ", collapse = "<br/>")
}

# SLC simulation info
load("rdata/rundf.RData")

# Prior nodes, reaches, and centerlines
load("rdata/clsf1.RData")
load("rdata/clsf2.RData")
load("rdata/nodesf1.RData")
load("rdata/nodesf2.RData")
clsf <- clsf2
nodesf <- nodesf2

ncols <- length(unique(clsf$reach_id))
palcols <- rep(brewer.pal(8, "Dark2"), length.out = ncols)
reachpal <- colorFactor(palcols, clsf$reach_id)
sacbbox <- sf::st_bbox(nodesf) %>% as.list()

load("rdata/xtkdf.RData")
load("rdata/xtk_gg.RData")


# File info
rodirs <- sprintf("./data/sac/%s", 51:54)

splitPiece <- function (strvec, split, piece, ...) {
  spl <- strsplit(strvec, split = split, ...)
  out <- vapply(spl, `[`, character(1), piece)
  out
}
get_files_info <- function(dir) {
  # browser()
  ncfiles <- list.files(dir, full.names = TRUE, include.dirs = FALSE)
  ncfiles_short <- list.files(dir, full.names = FALSE, include.dirs = FALSE)
  ncfiles_info <- file.info(ncfiles) %>% 
    mutate(filename = ncfiles_short) %>% 
    dplyr::filter(filename != "shapefiles") %>% 
    transmute(filename, size = gdata::humanReadable(size, standard = "SI"),
              format = "NetCDF")
    
  sfiles <- list.files(fs::path(dir, "shapefiles"), full.names = TRUE)
  sfiles_noext <- list.files(fs::path(dir, "shapefiles"), full.names = FALSE) %>% 
    splitPiece("\\.", piece = 1)
  sfiles_info <- file.info(sfiles)
  sfiles_info$filename <- sfiles_noext
  sfiles_smry <- sfiles_info %>% 
    group_by(filename) %>% 
    summarize(size = gdata::humanReadable(sum(size), standard = "SI")) %>% 
    mutate(format = "Shapefile")
  
  out <- rbind(ncfiles_info, sfiles_smry)
  out
}

get_file_summary <- function(file) {
  if (grepl("\\.nc$", file)) {
    # file is netcdf
    nc <- nc_open(file)
    on.exit(nc_close(nc))
    out <- capture.output(print(nc))
  } else {
    # file is shapefile
    out <- "See attribute table"
  }
  out
}



# Map layers, defaults ----------------------------------------------------

map_layer_names <- c("Passes", "Tiles", "PriorDB Centerlines", 
                     "PriorDB Nodes", "Pixels", "Nodes", "Reaches")
map_layers <- c("passes", "tiles", "prior_ctl", 
                "prior_node", "pixels", "rt_node", "rt_reach")

tabnames <- c("Prior DB", "Passes/Tiles", "Files", "Pixels", "Nodes", "Reaches")
tablyrs <- list(
  map_layers[c(3, 4)],
  map_layers[c(1, 2, 3)],
  map_layers[c(3, 4)],
  map_layers[c(5)],
  map_layers[c(6)],
  map_layers[c(7)]
) %>% setNames(tabnames)
