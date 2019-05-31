
function(input, output, session) {
  
  # Leaflet map and proxies -------------------------------------------------
  
  # Base map
  output$map <- renderLeaflet({
    basemap <- leaflet() %>% 
      addTiles() %>% 
      # addPolygons(data = sf::as_Spatial(tilesf$geometry)) %>% 
      leaflet::fitBounds(lng1 = sacbbox$xmax, lat1 = sacbbox$ymax, 
                         lng2 = sacbbox$xmin, lat2 = sacbbox$ymin)
      
  })
  
  # Observer for prior database nodes
  observeEvent("prior_node" %in% input$maplayers, {
    proxy <- leafletProxy("map")
    if ("prior_node" %in% input$maplayers) {
      proxy <- proxy %>% 
        addCircles(fillColor = ~reachpal(reach_id), radius = 100, 
                   stroke = FALSE, fillOpacity = 0.4, data = nodesf, 
                   group = "priornodes")
    } else {
      proxy <- proxy %>% 
        clearGroup("priornodes")
    }
    return(proxy)
  })
  
  # Observer for prior database centerlines
  observeEvent("prior_ctl" %in% input$maplayers, {
    proxy <- leafletProxy("map")
    if ("prior_ctl" %in% input$maplayers) {
      proxy <- proxy %>% 
        addPolylines(color = ~reachpal(reach_id), weight = 3, data = clsf, 
                     opacity = 0.85, group = "priorcl")
    } else {
      proxy <- proxy %>% 
        clearGroup("priorcl")
    }
    
    return(proxy)
  })
  
  # Observer for orbit tracks
  observeEvent("passes" %in% input$maplayers, {
    proxy <- leafletProxy("map")
    if ("passes" %in% input$maplayers) {
      proxy <- proxy %>% 
        addPolylines(data = sacpasses_sf, color = ~passpal(pass), 
                     popup = ~paste("Passs ", pass), group = "passes")
    } else {
      proxy <- proxy %>% 
        clearGroup("passes")
    }

    proxy
  })
  
  # Observer for tiles
  observeEvent("tiles" %in% input$maplayers, {
    proxy <- leafletProxy("map")
    # browser()
    if ("tiles" %in% input$maplayers) {
      proxy <- proxy %>% 
        add_swot_tile(tilelist$`51`$nadir1, tilelist$`51`$nadir2, 
                      tilelist$`51`$heading, tilelist$`51`$half,
                      group = "tiles", stroke = FALSE, 
                      fillColor = passpal("249")) %>% 
        add_swot_tile(tilelist$`52`$nadir1, tilelist$`52`$nadir2, 
                      tilelist$`52`$heading, tilelist$`52`$half,
                      group = "tiles", stroke = FALSE, 
                      fillColor = passpal("264")) %>% 
        add_swot_tile(tilelist$`53`$nadir1, tilelist$`53`$nadir2, 
                      tilelist$`53`$heading, tilelist$`53`$half,
                      group = "tiles", stroke = FALSE, 
                      fillColor = passpal("264")) %>% 
        add_swot_tile(tilelist$`54`$nadir1, tilelist$`54`$nadir2, 
                      tilelist$`54`$heading, tilelist$`54`$half,
                      group = "tiles", stroke = FALSE, 
                      fillColor = passpal("527"))
    } else {
      proxy <- proxy %>% 
        clearGroup("tiles")
    }
    
    proxy
  })
  
  # Observer for nodes in tile on map
  sprstring_node <- paste(c("node_index", "reach_index", "height", "width",
                            "area_total", "xtrk_dist", ""), 
                          collapse = ": %s<br/>")
  observeEvent("rt_node" %in% input$maplayers, {
    proxy <- leafletProxy("map")
    if ("rt_node" %in% input$maplayers) {
      proxy <- proxy %>% 
        addCircles(~longitude, ~latitude, fillColor = ~reachpal(reach_id), 
                   radius = ~sqrt(area_total / pi), 
                   stroke = FALSE, fillOpacity = 0.9, data = node_df(), 
                   popup = ~sprintf(sprstring_node, node_id, reach_id, height,
                                    width, area_total, xtrk_dist),
                   group = "nodedata")
    } else {
      proxy <- proxy %>% 
        clearGroup("nodedata")
    }

    return(proxy)
  })
  
  # Observer for pixc, pixcvec
  sprstring_pix <- paste(c("node_index", "height", "pixel_area", "water_frac",
                       "cross_track", ""), collapse = ": %s<br/>")
  pixlyr <- FALSE
  pixreach <- 1
  observe({
    newlyr <- "pixels" %in% input$maplayers
    newreach <- input$pixc_reach
    
    proxy <- leafletProxy("map")
    if (pixlyr == newlyr && newreach == pixreach) return(proxy)
    pixlyr <<- newlyr
    pixreach <<- newreach
    
    proxy <- proxy %>% 
      clearGroup("pixcdata")
    if (!length(file_dir()) || !("pixels" %in% input$maplayers)) return(proxy)
    proxy <- proxy %>% 
      addCircles(~longitude, ~latitude, fillColor = ~classpal(classification), 
                 popup = ~sprintf(sprstring_pix, node_index, height, 
                                  pixel_area, water_frac, cross_track), 
                 radius = ~sqrt(pixel_area / pi), 
                 stroke = FALSE, fillOpacity = 0.9, 
                 data = filter(pixc_df(), 
                               # reach_index == reach_index[1]),
                               reach_index == cur_reaches()[input$pixc_reach]),
                 group = "pixcdata")
    proxy
  })
  
  # Legend
  leglyr <- FALSE
  observeEvent(input$maplayers, {
    newlyr <- "pixels" %in% input$maplayers
    proxy <- leafletProxy("map")
    
    if (leglyr == newlyr) return(proxy)
    leglyr <<- newlyr
    
    if ("pixels" %in% input$maplayers) {
      proxy <- proxy %>% 
        addLegend(position = "topright",
                  colors = classpal(classes),
                  labels = classlabs)
    } else {
      proxy <- proxy %>% 
        clearControls()
    }
    proxy
  })
  
  # Observer for Reach product centerlines
  sprstring_reach <- paste(c("reach_id", "height", "width", "slope",
                            "area", ""), 
                          collapse = ": %s<br/>")
  observeEvent("rt_reach" %in% input$maplayers, {
    proxy <- leafletProxy("map")

    if ("rt_reach" %in% input$maplayers) {
      reachdata <- clsf %>% 
        right_join(reach_df(), by = "reach_id")
      # browser()
      proxy <- proxy %>% 
        addPolylines(color = ~reachpal(reach_id), weight = 3, data = reachdata, 
                     opacity = 0.85, group = "reaches", 
                     popup = ~sprintf(sprstring_reach, reach_id, height, width,
                                      slope, area_total))
    } else {
      proxy <- proxy %>% 
        clearGroup("reaches")
    }
    
    return(proxy)
  })

  # UI defaults per tab -----------------------------------------------------
  observeEvent(input$tabpan1, {
    # browser()
    pan <- input$tabpan1
    updateCheckboxGroupInput(session, "maplayers", selected = tablyrs[[pan]])
  })

  # Data objects for selected tile ----------------------------------------
  
  # Files in folder for selected tile
  file_dir <- reactive({
    dir <- rodirs[input$tile_table_rows_selected]
    dir
  })
  file_table <- reactive({
    filedf <- get_files_info(file_dir())
    # rownames(filedf) <- NULL
    filedf
  })
  
  file_summaries <- reactive({
    files <- fs::path(file_dir(), file_table()$filename)
    out <- lapply(files, get_file_summary)
    out
  })
  
  output$file_table <- renderDT({
    file_table()
  }, selection = "single", rownames = FALSE)
  
  output$file_structure <- renderText({
    file_summaries()[[input$file_table_rows_selected]]
  })
  
  # Node product(netcdf)
  node_df <- reactive({
    rt_read(fs::path(file_dir(), "rivertile.nc"))
  })
  
  # Reach product(netcdf)
  reach_df <- reactive({
    req(length(file_dir()))
    out <- rt_read(fs::path(file_dir(), "rivertile.nc"), group = "reaches")
    updateSliderInput(session, "pixc_reach", max = length(unique(out$reach_id)),
                      value = 1)
    out
  })
  
  # Pixel product
  pixc_df <- reactive({
    join_pixc(fs::path(file_dir()), pcvname = "pixcvec.nc", 
              pixcname = "pixel_cloud.nc")
  })
  cur_reaches <- reactive({
    unique(reach_df()$reach_id)
  })
  
  
  # Plots -----------------------------------------------------------------
  
  # x-track distance range by pass, tile
  output$xtk_gg <- renderPlot({
    xtk_gg + scale_color_manual(values = passpal(c(249, 264, 527)))
  })
  
  output$node_scatter1 <- renderPlot({
    node_df() %>% 
      mutate(colorcol = reachpal(reach_id)) %>% 
      ggplot(aes(x = node_id, y = height)) + 
      geom_point(aes(color = colorcol)) + 
      scale_color_identity()
  })
  
  output$node_scatter2 <- renderPlot({
    node_df() %>% 
      mutate(colorcol = reachpal(reach_id)) %>% 
      ggplot(aes(x = node_id, y = width)) + 
      geom_point(aes(color = colorcol)) + 
      scale_color_identity()
  })
  
  output$reach_scatter1 <- renderPlot({
    reach_df() %>% 
      mutate(colorcol = reachpal(reach_id)) %>% 
      ggplot(aes(x = reach_id, y = slope)) + 
      geom_point(aes(color = colorcol)) + 
      scale_color_identity()
  })
  
  output$reach_scatter2 <- renderPlot({
    reach_df() %>% 
      mutate(colorcol = reachpal(reach_id)) %>% 
      ggplot(aes(x = reach_id, y = slope)) + 
      geom_point(aes(color = colorcol)) + 
      scale_color_identity()
  })
  
  

  # Tables ------------------------------------------------------------------
  
  # Tile info table
  output$tile_table <- renderDT({
    dplyr::transmute(rundf, 
                     pass, tile, date)
  }, selection = "single", rownames= FALSE)
  
  # Node data
  output$node_dt <- renderDT({
    req(input$tabpan1 == "Nodes")
    if (!length(node_df())) {
      mtcars
    } else {
      node_df()
    }
  }, options = list(scrollX = TRUE, scrollY = TRUE), rownames = FALSE)

  # Reach data
  output$reach_dt <- renderDT({
    req(input$tabpan1 == "Reaches")
    if (!length(reach_df())) {
      mtcars
    } else {
      reach_df()
    }
  }, options = list(scrollX = TRUE, scrollY = TRUE),
     selection = list(mode = "single", target = "column"), 
  rownames = FALSE)
  
  # Pixel data
  output$pixc_dt <- renderDT({
    req(input$tabpan1 == "Pixels")
    if (!length(pixc_df())) {
      mtcars
    } else {
      pixc_df()
    }
  }, options = list(scrollX = TRUE, scrollY = TRUE),
  selection = list(mode = "single", target = "column"),
  rownames = FALSE)
  
  
}


