server <- shinyServer(function(input, output,session) {
  
  
  ##########################
  ## Filter
  ##########################
  
  filteredDataset <- reactive({
    if(input$productSelection == "All") {
      return(values$dataset)
    } else {
      return(values$dataset[grepl(input$productSelection,values$dataset$Products),])
    }
  })
  
  
  ##########################
  ## Update Map
  ##########################
  
  output$producerMap <- renderLeaflet({
    map <-leaflet() %>%
      addTiles()
    
    selectedProducer = input$ProducerTable_rows_selected
    
    
    
    if(length(selectedProducer)) {
      filteredSelectedDataset <- filteredDataset()[selectedProducer,]
    } else {
      filteredSelectedDataset <- filteredDataset()
    }
    
    map <- addMarkers(map,data = filteredSelectedDataset[,c("Longitude","Latitude")],
                      popup = filteredSelectedDataset$Name)
    map
  })
  
  
  
  ##########################
  ## List
  ##########################
  
  output$ProducerTable <- DT::renderDataTable(
    filteredDataset()[,c("Name","Street","City","Products")]
    ,options = list(lengthChange = FALSE,
                    autoWidth = TRUE,
                    sDom  = '<"top">lrt<"bottom">ip')
  )
  
  ##########################
  # Details
  ##########################
  
  ## Show details
  observeEvent(input$Details, {
    if(length(input$ProducerTable_rows_selected)) {
      showModal(modalDialog(
        title = "Detail View",
        textOutput('name'),
        easyClose = TRUE
      )
      )
    }
  })
  
  # Detailed View
  output$name <- renderText({
    
    selectedProducer = input$ProducerTable_rows_selected
    
    return(paste0("Name : ",filteredDataset()[tail(selectedProducer,n=1),"Name"]))
  })
  
  ##########################
  # Edit
  ##########################
  
  # Show Edit
  observeEvent(input$Edit, {
    if(length(input$ProducerTable_rows_selected)) {
      selectedProducer <- filteredDataset()[tail(
        input$ProducerTable_rows_selected, n=1),]
      
      showModal(modalDialog(
        title = "Edit View",
        fluidPage(
          fluidRow(
            column(6,
                   textInput('editName',
                             label='Name',
                             value=selectedProducer$Name[1])
            ),
            column(6,
                   textInput('editProduct',
                             label='Products',
                             value=selectedProducer$Products[1])
            )
          ),
          fluidRow(
            column(6,
                   textInput('editStreet',
                             label='Address',
                             value=selectedProducer$Street[1])
            ),
            column(6,
                   textInput('editCity',
                             label='City',
                             value=selectedProducer$City[1])
            )
          )
        ),
        footer = list(actionButton("SaveEdit","Save"),
                      actionButton("Cancel","Cancel")),
        easyClose = TRUE
      )
      )
    }
  })
  
  # Save Edit
  
  observeEvent(input$SaveEdit, {
    
    if(length(input$ProducerTable_rows_selected)) {
      editIndex <- tail(input$ProducerTable_rows_selected, n=1)
      
      editIndex <- as.numeric(rownames(filteredDataset()[editIndex,]))
      
      values$dataset[editIndex,"Name"] <- input$editName
     
      
      values$dataset[editIndex,"Products"] <- input$editProduct
      
      
      values$dataset[editIndex,"Street"] <- input$editStreet
      
      
      values$dataset[editIndex,"City"] <- input$editCity
      
      values$products <- productList(values$dataset)
      updateSelectInput(session, "productSelection", choices = values$products)
    }
    
    removeModal()
    
    saveDataSet()
  })
  
  # Cancel Edit
  observeEvent(input$Cancel, {
    removeModal()
  })
})