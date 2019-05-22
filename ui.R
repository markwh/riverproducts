
header <- dashboardHeader(title = "SWOT River Products")


sidebar <- dashboardSidebar(
  disable = TRUE,
  radioButtons("runselect", "Flow Condition", 
               choices = c("High", "Low"))
  )

body <- dashboardBody(
  column(width = 6, title = "Map",
         leafletOutput("map")),
  column(width = 6, title = "tabset1", 
         tabsetPanel(id = "tabpan1",
           tabPanel("Passes"),
           tabPanel("Files"),
           tabPanel("Pixels"),
           tabPanel("Nodes"),
           tabPanel("Reaches")
         ))
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)
