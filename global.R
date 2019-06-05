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
library(geosphere)


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

fmt_atts <- function(attdf) {
  if (!is.data.frame(attdf) || !nrow(attdf) == 1) return("")
  attnames <- names(attdf)
  attvals <- unname(attdf[1, ])
  
  naatts <- is.na(attvals)
  attvals <- attvals[!naatts]
  attnames <- attnames[!naatts]
  
  sprstr <- "<b>%s:</b><br/><i>%s</i>"
  out <- paste(sprintf(sprstr, attnames, attvals), collapse = "<br/>")
  out
}

# SLC simulation info
load("rdata/rundf.RData")

# Prior nodes, reaches, and centerlines
load("rdata/clsf1.RData")
load("rdata/clsf2.RData")
load("rdata/nodesf1.RData")
load("rdata/nodesf2.RData")
load("rdata/add_swot_tile.RData")
load("rdata/getTileCorners.RData")
load("rdata/tilelist.RData")
clsf <- clsf2
nodesf <- nodesf2

ncols <- length(unique(clsf$reach_id))
palcols <- rep(brewer.pal(8, "Dark2"), length.out = ncols)
reachpal <- colorFactor(palcols, clsf$reach_id)
sacbbox <- sf::st_bbox(nodesf) %>% as.list()

load("rdata/xtkdf.RData")
load("rdata/xtk_gg.RData")



# File info ---------------------------------------------------------------

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



# Plot axis, etc. defaults ------------------------------------------------

# Nodes
node_xaxis_vals <-  c("node_id", "xtrk_dist", 
                      "width", "height", "area_total")
node_xaxis_names <- c("Node ID", "Cross-track Dist",
                     "Width", "Height", "Area")
node_xaxis_units <- c("", " (m)", " (m)", " (m)", " (m^2)")


node_yaxis_vals <-  c("width", "height", "area_total", "custom")
node_yaxis_names <- c("Width", "Height", "Area", "Custom (from table)")
node_yaxis_units <- c(" (m)", " (m)", " (m^2)", "")
node_unc_vars <- c("width_u", "height_u", "area_tot_u", "")

# Reaches
reach_xaxis_vals <-  c("reach_id", "xtrk_dist", 
                      "width", "height", "area_total")
reach_xaxis_names <- c("Reach ID", "Cross-track Dist",
                      "Width", "Height", "Area")
reach_xaxis_units <- c("", " (m)", " (m)", " (m)", " (m^2)")


reach_yaxis_vals <-  c("slope", "width", "height", "area_total", "custom")
reach_yaxis_names <- c("Slope", "Width", "Height", "Area", "Custom (from table)")
reach_yaxis_units <- c(" ( mm/km)", " (m)", " (m)", " (m^2)", "")
reach_unc_vars <- c("slope_u", "width_u", "height_u", "area_tot_u", "")

