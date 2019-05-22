
# A stripped-down app for showing only how error propagates along with uncertainty 
# across scales: pixel to node, node to reach. 
#

# default_data_url <- "https://osu.box.com/shared/static/9ng2ys6kubcbkqu8riar0l89uzk101zr.rdata"
# run_manifest <- read.csv("./roruns.csv", stringsAsFactors = FALSE) %>% 
#   dplyr::filter(!is.na(rtviz_url),
#                 nchar(rtviz_url) > 0) 

####------------------------------------
#### START OF SERVER FUNCTION ----------
####------------------------------------
function(input, output, session) {
  output$map <- renderLeaflet({
    basemap <- leaflet() %>% 
      addTiles()
  })
}


