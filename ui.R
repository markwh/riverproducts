
header <- dashboardHeader(title = "SWOT River Products")


sidebar <- dashboardSidebar(
  disable = FALSE,
  collapsed = TRUE,
  
  checkboxGroupInput("maplayers", "Map Layers", choiceNames = map_layer_names, 
                     choiceValues = map_layers, selected = tablyrs[[1]])
  )

body <- dashboardBody(
  fluidRow(
    column(width = 5, title = "Map",
           leafletOutput("map", height = 700)),
    column(width = 7, title = "tabset1", 
           tabsetPanel(id = "tabpan1",
                       tabPanel("Prior DB",
                                includeMarkdown("mdfiles/priordb.md")),
                       tabPanel("Passes/Tiles",
                                DT::DTOutput("tile_table"),
                                includeMarkdown("mdfiles/passes-tiles.md")),
                       # tabPanel("Files",
                       #          DT::DTOutput("file_table"),
                       #          textOutput("file_structure")),
                       tabPanel("Pixels",
                                includeMarkdown("mdfiles/pixels.md"),
                                sliderInput("pixc_nodes", "Nodes to Show",
                                            min = 1, max = 1, 
                                            value = 1, step = 1),
                                checkboxGroupInput("pixc_columns_select", 
                                                   "Table Columns To Display",
                                                   choiceNames = c("PIXC", "PIXCVec"),
                                                   choiceValues = c("pixc", "pixcvec"),
                                                   selected = c("pixc", "pixcvec"))
                                ),
                       tabPanel("Nodes",
                                includeMarkdown("mdfiles/nodes.md"),
                                fluidRow(
                                  plotlyOutput("node_scatter1")
                                ),
                                radioButtons("node_yvar", "Y-axis variable",
                                             choiceValues = node_yaxis_vals,
                                             choiceNames = node_yaxis_names,
                                             inline = TRUE),
                                radioButtons("node_xvar", "X-axis variable",
                                             choiceValues = node_xaxis_vals,
                                             choiceNames = node_xaxis_names,
                                             inline = TRUE)
                       ),
                       tabPanel("Reaches",
                                includeMarkdown("mdfiles/reaches.md"),
                                fluidRow(
                                  plotlyOutput("reach_scatter1")
                                ),
                                radioButtons("reach_yvar", "Y-axis variable",
                                             choiceValues = reach_yaxis_vals,
                                             choiceNames = reach_yaxis_names,
                                             inline = TRUE),
                                radioButtons("reach_xvar", "X-axis variable",
                                             choiceValues = reach_xaxis_vals,
                                             choiceNames = reach_xaxis_names,
                                             inline = TRUE))
           ))
  ),
  fluidRow(
    column(width = 5,
           htmlOutput("atts_info")),
    column(width = 7,
           fluidRow(DTOutput("data_dt"))
    )
  )

)

ui <- dashboardPage(
  header,
  sidebar,
  body
)
