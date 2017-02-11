server <- shinyServer(function(input, output,session) {
  
  
  ##########################
  ## Filter
  ##########################
  
  filteredDataset <- reactive({
    if(input$productSelection == "All") {
      return(values$dataset[[1]])
    } else {
      return(values$dataset[[1]][grepl(input$productSelection,values$dataset[[1]]$Products),])
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
      
      values$dataset[[1]][editIndex,"Name"] <- input$editName
      
      
      values$dataset[[1]][editIndex,"Products"] <- input$editProduct
      
      
      values$dataset[[1]][editIndex,"Street"] <- input$editStreet
      
      
      values$dataset[[1]][editIndex,"City"] <- input$editCity
      
      values$products[[1]] <- productList(values$dataset[[1]])
      updateSelectInput(session, "productSelection", choices = values$products[[1]])
    }
    
    removeModal()
    
    saveDataSet()
  })
  
  # Cancel Edit
  observeEvent(input$Cancel, {
    removeModal()
  })
  
  ##########################
  ## Add
  ##########################
  
  observeEvent(input$Add, {
    
    showModal(modalDialog(
      title = "Edit View",
      fluidPage(
        fluidRow(
          column(6,
                 textInput('addName',
                           label='Name')
          ),
          column(6,
                 textInput('addProduct',
                           label='Products')
          )
        ),
        fluidRow(
          column(6,
                 textInput('addStreet',
                           label='Address')
          ),
          column(6,
                 textInput('addCity',
                           label='City')
          )
        )
      ),
      footer = list(actionButton("SaveAdd","Save"),
                    actionButton("CancelAdd","Cancel")),
      easyClose = TRUE
    )
    )
  })
  
  # Save Add
  
  observeEvent(input$SaveAdd, {
    
    newRow <- data.frame(
      Name = input$addName,
      Products = input$addProduct,
      Street = input$addStreet,
      City = input$addCity,
      Longitude = 6.332217,
      Latitude = 46.532801)
    
    
    values$dataset[[1]] <- rbind(values$dataset[[1]],
                            newRow)
    
    values$products[[1]] <- productList(values$dataset[[1]])
    updateSelectInput(session, "productSelection", choices = values$products[[1]])
    
    removeModal()
    
    saveDataSet()
  })
  
  # Cancel Add
  observeEvent(input$CancelAdd, {
    removeModal()
  })
  
})