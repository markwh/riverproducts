
header <- dashboardHeader(title = "SWOT River Products")


sidebar <- dashboardSidebar(
  disable = FALSE,
  collapsed = TRUE,
  
  checkboxGroupInput("maplayers", "Map Layers", choiceNames = map_layer_names, 
                     choiceValues = map_layers, selected = tablyrs[[1]])
  )

body <- dashboardBody(
  fluidRow(
    column(width = 6, title = "Map",
           leafletOutput("map", height = 700)),
    column(width = 6, title = "tabset1", 
           tabsetPanel(id = "tabpan1",
                       tabPanel("Prior DB"),
                       tabPanel("Passes/Tiles",
                                # checkboxInput("show_passes", "Show Passes"),
                                plotOutput("xtk_gg"),
                                DT::DTOutput("tile_table"),
                                actionButton("load_tile", "Load Selected Tile")),
                       tabPanel("Files",
                                DT::DTOutput("file_table"),
                                textOutput("file_structure")),
                       tabPanel("Pixels",
                                # uiOutput("pixc_reach_slider")
                                sliderInput("pixc_reach", "Reach to Show",
                                            min = 1, max = 1, 
                                            value = 1, step = 1)
                                ),
                       tabPanel("Nodes",
                                fluidRow(
                                  plotOutput("node_scatter1")
                                )
                                # fluidRow(
                                #   box(
                                #     # plotOutput("node_scatter2"))#,
                                #     DTOutput("node_dt", width = 6)
                                #   )
                                # )
                       ),
                       tabPanel("Reaches",
                                fluidRow(
                                  plotOutput("reach_scatter1"),
                                  plotOutput("reach_scatter2")
                                ))
           ))
  ),
  fluidRow(
    DTOutput("node_dt")
  ),
  fluidRow(
    DTOutput("reach_dt")
  ),
  fluidRow(
    DTOutput("pixc_dt")
  )

)

ui <- dashboardPage(
  header,
  sidebar,
  body
)
