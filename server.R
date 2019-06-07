
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
    proxy <- leafletProxy("map") %>% 
      clearGroup("priornodes")
    if ("prior_node" %in% input$maplayers) {
      proxy <- proxy %>% 
        addCircles(fillColor = ~reachpal(reach_id), radius = 100, 
                   stroke = FALSE, fillOpacity = 0.4, data = nodesf, 
                   popup = ~paste0("Node: ", node_id, "<br/>", 
                                   "Reach: ", reach_id),
                   group = "priornodes")
    } 
    return(proxy)
  })
  
  # # Observer for prior database nodes in selected tile
  # observeEvent(input$tile_table_rows_selected, {
  #   req(node_df())
  #   proxy <- leafletProxy("map") %>% 
  #     clearGroup("priornodes_highlight")
  #   if (length(input$tile_table_rows_selected)) {
  #     proxy <- proxy %>% 
  #       addCircles(fillColor = ~reachpal(reach_id), radius = 100, 
  #                  stroke = TRUE, fillOpacity = 0.4, 
  #                  data = dplyr::filter(nodesf, node_id %in% unique(node_df()$node_id)), 
  #                  popup = ~paste0("Node: ", node_id, "<br/>", 
  #                                  "Reach: ", reach_id),
  #                  group = "priornodes_highlight")
  #   } 
  #   return(proxy)
  # })
  
  # Observer for prior database centerlines
  observeEvent("prior_ctl" %in% input$maplayers, {
    proxy <- leafletProxy("map") %>% 
      clearGroup("priorcl")
    if ("prior_ctl" %in% input$maplayers) {
      proxy <- proxy %>% 
        addPolylines(color = ~reachpal(reach_id), weight = 3, data = clsf, 
                     opacity = 0.85, group = "priorcl",
                     popup = ~paste0("Reach: ", reach_id))
    } 
    
    return(proxy)
  })
  
  # Observer for orbit tracks
  observeEvent("passes" %in% input$maplayers, {
    proxy <- leafletProxy("map") %>% 
      clearGroup("passes")
    if ("passes" %in% input$maplayers) {
      proxy <- proxy %>% 
        addPolylines(data = sacpasses_sf, color = ~passpal(pass), 
                     popup = ~paste("Passs ", pass), group = "passes")
    } 
    proxy
  })
  
  # Observer for tiles
  observe({
    # req("tiles" %in% input$maplayers)
    proxy <- leafletProxy("map") %>% 
      clearGroup("tiles")
    if ("tiles" %in% input$maplayers) {
      proxy <- proxy %>% 
        add_swot_tile(tilelist$`51`$nadir1, tilelist$`51`$nadir2, 
                      tilelist$`51`$heading, tilelist$`51`$half,
                      group = "tiles", stroke = FALSE, 
                      fillColor = passpal("249"),
                      popup = "Pass 249, Tile 001L") %>% 
        add_swot_tile(tilelist$`52`$nadir1, tilelist$`52`$nadir2, 
                      tilelist$`52`$heading, tilelist$`52`$half,
                      group = "tiles", stroke = FALSE, 
                      fillColor = passpal("264"),
                      popup = "Pass 264, Tile 001L") %>% 
        add_swot_tile(tilelist$`53`$nadir1, tilelist$`53`$nadir2, 
                      tilelist$`53`$heading, tilelist$`53`$half,
                      group = "tiles", stroke = FALSE, 
                      fillColor = passpal("264"), 
                      popup = "Pass 264, Tile 001R") %>% 
        add_swot_tile(tilelist$`54`$nadir1, tilelist$`54`$nadir2, 
                      tilelist$`54`$heading, tilelist$`54`$half,
                      group = "tiles", stroke = FALSE, 
                      fillColor = passpal("527"),
                      popup = "Pass 527, Tile 001R")
    }
    
    proxy
  })
  
  # Observer for selected tile
  observe({
    # req("tiles" %in% input$maplayers)
    proxy <- leafletProxy("map") %>% 
      clearGroup("tiles_selected")
    selrow <- input$tile_table_rows_selected
    if ("tiles" %in% input$maplayers && length(selrow)) {
      tilenum <- as.character((51:54)[selrow])
      # browser()
      proxy <- proxy %>% 
        add_swot_tile(tilelist[[tilenum]]$nadir1, tilelist[[tilenum]]$nadir2, 
                      tilelist[[tilenum]]$heading, tilelist[[tilenum]]$half,
                      group = "tiles_selected", stroke = TRUE, fill = FALSE,
                      color = "#888888")
    }
    
    proxy
  })
  
  
  # Observer for nodes in tile on map
  sprstring_node <- paste(c("node_index", "reach_index", "height", "width",
                            "area_total", "xtrk_dist", ""), 
                          collapse = ": %s<br/>")
  observeEvent("rt_node" %in% input$maplayers, {
    proxy <- leafletProxy("map") %>% 
      clearGroup("nodedata")
    
    if ("rt_node" %in% input$maplayers) {
      proxy <- proxy %>% 
        addCircles(~longitude, ~latitude, fillColor = ~reachpal(reach_id), 
                   radius = ~sqrt(area_total / pi), 
                   stroke = FALSE, fillOpacity = 0.9, data = node_df(), 
                   popup = ~sprintf(sprstring_node, node_id, reach_id, height,
                                    width, area_total, xtrk_dist),
                   group = "nodedata")
    }

    return(proxy)
  })
  
  # Observer for pixc, pixcvec
  sprstring_pix <- paste(c("node_index", "classification", "height", 
                           "pixel_area", "water_frac",
                           "cross_track", "range_index", "azimuth_index", ""), 
                         collapse = ": %s<br/>")
  pixlyr <- FALSE
  pixnode <- 1
  geoloc <- "med"
  observe({
    newlyr <- "pixels" %in% input$maplayers
    newnode <- input$pixc_nodes
    # input$improve_geoloc
    # browser()
    proxy <- leafletProxy("map")
    if (pixlyr == newlyr && newnode == pixnode && 
        input$improve_geoloc == geoloc) return(proxy)
    pixlyr <<- newlyr
    pixnode <<- newnode
    geoloc <<- input$improve_geoloc
    
    proxy <- proxy %>% 
      clearGroup("pixcdata")
    if (!length(file_dir()) || !("pixels" %in% input$maplayers)) return(proxy)
    # browser()
    lonvar <- ifelse(input$improve_geoloc == "wd", 
                     "longitude_vectorproc", "longitude")
    latvar <- ifelse(input$improve_geoloc == "wd", 
                     "latitude_vectorproc", "latitude")
    plotdat <- pixc_df_filtered()
    if (input$improve_geoloc == "wd") {
      plotdat$latitude <- plotdat$latitude_vectorproc
      plotdat$longitude <- plotdat$longitude_vectorproc
    }
    
    proxy <- proxy %>% 
      addCircles(~longitude, ~latitude, 
                 fillColor = ~classpal(classification), 
                 popup = ~sprintf(sprstring_pix, node_index, classification, height, 
                                  pixel_area, water_frac, cross_track,
                                  range_index, azimuth_index), 
                 radius = ~sqrt(pixel_area / pi), 
                 stroke = FALSE, fillOpacity = 0.9, 
                 data = plotdat,
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
                  colors = classpal(classes[-1]),
                  labels = classlabs[-1])
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
    proxy <- leafletProxy("map") %>% 
      clearGroup("reaches")

    if ("rt_reach" %in% input$maplayers) {
      reachdata <- clsf %>% 
        right_join(reach_df(), by = "reach_id")
      # browser()
      proxy <- proxy %>% 
        addPolylines(color = ~reachpal(reach_id), weight = 3, data = reachdata, 
                     opacity = 0.85, group = "reaches", 
                     popup = ~sprintf(sprstring_reach, reach_id, height, width,
                                      slope, area_total))
    }
    
    return(proxy)
  })

  # UI elements per tab -----------------------------------------------------
  # defaults
  observeEvent(input$tabpan1, {
    # browser()
    pan <- input$tabpan1
    updateCheckboxGroupInput(session, "maplayers", selected = tablyrs[[pan]])
  })
  observe({
    req(length(pixc_df()))
    pixc_df()
    updateSliderInput(session, "pixc_nodes", 
                      min = min(pixc_df()$node_index, na.rm = TRUE),
                      max = max(pixc_df()$node_index, na.rm =TRUE),
                      value = 1)
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
    req(file_dir())
    rt_read(fs::path(file_dir(), "rivertile.nc"))
  })
  
  # Reach product(netcdf)
  reach_df <- reactive({
    req(file_dir())
    input$tabpan1
    out <- rt_read(fs::path(file_dir(), "rivertile.nc"), group = "reaches")
    out
  })
  atts_df <- reactive({
    # req()
    seltab <- input$tabpan1
    if (seltab == "Pixels") {
      outdf <- attr(pixc_df(), "atts")
    } else if (seltab == "Nodes") {
      outdf <-  attr(node_df(), "atts")
    } else if (seltab == "Reaches") {
      outdf <-  attr(reach_df(), "atts")
    } else {
      outdf <- NULL
    }
    outdf
  })
  expert_atts <- reactive({
    out <- atts_df()$name[atts_df()$tag_basic_expert == "Expert"]
    out <- intersect(out, names(data_df()))
  })
  
  # Pixel product
  pixcvec_df <- reactive({
    req(file_dir())
    pixcvec_read(fs::path(file_dir(), "pixcvec.nc"))
  })
  pixc_df <- reactive({
    req(file_dir())
    req(pixcvec_df())
    pixc_full <- pixc_read(fs::path(file_dir(), "pixel_cloud.nc")) %>% 
      dplyr::mutate(classification = as.factor(classification))
    
    outdf <- left_join(pixc_full, pixcvec_df(), 
                       by = c("azimuth_index", "range_index"))
    outattdf <- rivertile:::bind_rows2(
      list(attr(pixcvec_df(), "atts"), attr(pixc_full, "atts")),
      addMissing = TRUE
    )
    
    out <- structure(outdf, atts = outattdf)
    out
  })
  pixc_df_filtered <- reactive({
    req(pixc_df())
    req(input$pixc_nodes)
    pixc_df() %>% 
      filter(node_index %in% (input$pixc_nodes + 0:15))
  })
  
  # pixcvec_cols <- reactive({
  #   req(pixcvec_df())
  #   names(pixcvec_df())
  # })
  # 
  # # Entire pixel cloud
  # pixcdf_full <- reactive({
  #   req(file_dir())
  #   pixc_read(fs::path(file_dir(), "pixel_cloud.nc"), 
  #             group = "pixel_cloud") %>% 
  #     mutate(classification = as.factor(classification))
  # })
  # pixcdf_sset <- reactive({
  #   req(pixcdf_full())
  #   req(pixc_df_filtered())
  #   pixcdf_full() %>% 
  #     filter(range_index >= min(pixc_df_filtered()$range_index),
  #            range_index <= max(pixc_df_filtered()$range_index),
  #            azimuth_index >= min(pixc_df_filtered()$azimuth_index),
  #            azimuth_index <= max(pixc_df_filtered()$azimuth_index))
  # })
  # 
  pixc_only_cols <- reactive({
    req(pixcvec_df())
    req(pixc_df())
    c("azimuth_index", "range_index",
      setdiff(names(pixc_df()), names(pixcvec_df())))
  })
  pixc_columns <- reactive({
    req(input$tabpan1 == "Pixels")
    namelist <- list(pixc = pixc_only_cols(), 
                     pixcvec = names(pixcvec_df()))
    keepnames <- unname(namelist[input$pixc_columns_select])
    if (length(keepnames) == 1) return(keepnames[[1]])
    out <- do.call(union, args = keepnames)
    out
  })
  
  cur_nodes <- reactive({
    unique(pixc_df()$node_index)
  })
  
  
  # Plots -----------------------------------------------------------------
  
  # x-track distance range by pass, tile
  output$xtk_gg <- renderPlot({
    xtk_gg + scale_color_manual(values = passpal(c(249, 264, 527)))
  })
  
  # node scatter based on selected column
  output$node_scatter1 <- renderPlotly({
    req(node_df())
    xvar <- input$node_xvar
    xind <- which(node_xaxis_vals == xvar)
    xlab <- paste0(node_xaxis_names[xind], node_xaxis_units[xind])
    
    yvar <- input$node_yvar
    yind <- which(node_yaxis_vals == yvar)
    ylab <- paste0(node_yaxis_names[yind], node_yaxis_units[yind])
    yvar0 <- yvar # because I overwrite yvar
    if (yvar == "custom") {
      if (!length(input$data_dt_columns_selected)) {
        showModal(modalDialog("Please select a column from the table below."))
        yvar <- "node_id"
      } else {
        yvar <- names(node_df())[input$data_dt_columns_selected + 1]        
      }
      ylab <- yvar
    }
    yuncvar <- node_unc_vars[yind]

    if (!length(yvar)) yvar <- "width"
    
    ggdata <- node_df() %>% 
      mutate(colorcol = reachpal(reach_id),
             text = sprintf("Reach: %s\nNode: %s", 
                            reach_id, node_id))

    gg <-  ggdata %>% 
      ggplot(aes(x = !!sym(xvar), y = !!sym(yvar),
                 color = colorcol, text = text)) +
      scale_color_identity() + 
      xlab(xlab) + ylab(ylab)
    
    if (yvar0 == "custom") {
      # browser()
      gg <- gg + 
        geom_point()
    } else {
        gg <- gg + 
          geom_pointrange(aes(ymin = !!sym(yvar) - !!sym(yuncvar),
                              ymax = !!sym(yvar) + !!sym(yuncvar)))
    }
    out <- ggplotly(gg, tooltip = "text") %>% 
      hide_guides()
    out
  })
  
  output$reach_scatter1 <- renderPlotly({
    xvar <- input$reach_xvar
    xind <- which(reach_xaxis_vals == xvar)
    xlab <- paste0(reach_xaxis_names[xind], reach_xaxis_units[xind])
    
    yvar <- input$reach_yvar
    yind <- which(reach_yaxis_vals == yvar)
    ylab <- paste0(reach_yaxis_names[yind], reach_yaxis_units[yind])
    yuncvar <- reach_unc_vars[yind]
    yvar0 <- yvar
    
    if (yvar == "custom") {
      if (!length(input$data_dt_columns_selected)) {
        showModal(modalDialog("Please select a column from the table below."))
        yvar <- "reach_id"
      } else {
        yvar <- names(reach_df())[input$data_dt_columns_selected + 1]        
      }
      ylab <- yvar
    }
    
    ggdata <- reach_df() %>% 
      mutate(colorcol = reachpal(reach_id),
             text = sprintf("Reach: %s", reach_id))
    
    gg <- ggdata %>% 
      ggplot(aes(x = !!sym(xvar), y = !!sym(yvar),
                 color = colorcol, text = text)) +
      scale_color_identity() +
      ylab(ylab) + xlab(xlab)
    
    if (yvar0 == "custom") {
      gg <- gg + 
        geom_point()
    } else {
      gg <- gg + 
        geom_pointrange(aes(ymin = !!sym(yvar) - !!sym(yuncvar),
                            ymax = !!sym(yvar) + !!sym(yuncvar)))
    }
    out <- ggplotly(gg, tooltip = "text") %>% 
      hide_guides()
    out
  })
  
  # Range-azimuth raster
  
  output$slantplane <- renderPlot({
    req(pixc_df())
    pixc_color <- input$pixc_color
    
    
    if (pixc_color != "classification") {
      if (!length(input$data_dt_columns_selected)) {
        showModal(modalDialog("Please select a column from the table below."))
        pixc_color <- "classification"
      } else {
        pixc_color <- names(data_df())[input$data_dt_columns_selected + 1]        
      }
    }
    
    pixc_df() %>% 
      dplyr::filter(range_index >= min(pixc_df_filtered()$range_index - 10),
                    range_index <= max(pixc_df_filtered()$range_index + 10),
                    azimuth_index >= min(pixc_df_filtered()$azimuth_index - 10),
                    azimuth_index <= max(pixc_df_filtered()$azimuth_index + 10)) %>% 
      dplyr::mutate(alpha = ifelse(node_index %in% (input$pixc_nodes + 0:15), 1, 0.65)) %>% 
      ggplot(aes(x = range_index, y = azimuth_index, 
                 fill = !!sym(pixc_color), alpha = alpha)) +
      geom_raster() +
      scale_alpha_identity() +
      coord_equal()
  })
  

  # Tables ------------------------------------------------------------------
  
  # Tile info table
  output$tile_table <- renderDT({
    dplyr::transmute(rundf, 
                     pass, tile, date)
  }, autoHideNavigation = TRUE, selection = "single", rownames= FALSE)
  
  # Data table
  data_df <- reactive({
    req(input$tabpan1 %in% c("Pixels", "Nodes", "Reaches"))
    if (!(length(input$tile_table_rows_selected))) {
      # browser()
      updateTabsetPanel(session, inputId = "tabpan1", selected = "Passes/Tiles")
      showModal(modalDialog("First select a tile by clicking a row in the table."))
      return(NULL)
    }
    seltab <- input$tabpan1
    # browser()
    if (seltab == "Pixels") {
      # browser()
      outdf <- pixc_df() %>% # TODO: filter this using a proxy (don't filter outright!)
        select(pixc_columns())
    } else if (seltab == "Nodes") {
      outdf <- node_df()
    } else if (seltab == "Reaches") {
      outdf <- reach_df()
    } else {
      outdf <- NULL
    }
    outdf
  })
  output$data_dt <- renderDT({
    # browser()
    datatable(data_df(), options = list(scrollX = TRUE, scrollY = TRUE),
              selection = list(mode = "single", target = "column"),
              rownames = FALSE) %>% 
      formatStyle(expert_atts(), color = "red")
  })
  
  # Attributes of selected variale
  output$atts_info <- renderText({
    selvar <- names(data_df())[input$data_dt_columns_selected + 1]
    if (!length(selvar)) return("")
    # if (input$tabpan1 == "Reaches") browser()
    tofmtdf <- dplyr::filter(atts_df(), 
                             name == selvar)
    fmt_atts(tofmtdf)
  })

  
  
  
}


