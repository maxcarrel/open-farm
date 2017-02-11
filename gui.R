ui <- fluidPage(
  
  titlePanel("OpenFarm"),
  
  
  leafletOutput("producerMap"),
  
  hr(),
  
  fluidRow(
    column(3,
           selectInput("productSelection",
                       "Choose Product:",
                       choices = productList(datasetStatic))
           ),
    
    column(3, offset=6,
           div(style="display:inline-block",
               actionButton("Details","Details"),
               actionButton("Edit","Edit"),
               actionButton("Add","Add")
               )
           )
    ),
  
  DT::dataTableOutput('ProducerTable')
)

