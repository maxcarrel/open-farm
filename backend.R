server <- shinyServer(function(input, output,session) {
  
  
  ##########################
  ## Filter
  ##########################
  
  filteredDataset <- reactive({
    if(input$productSelection == "All") {
      return(values$dataset[[1]][,])#c("Name","Street","City","Label","Products","Longitude","Latitude","ProductsTruncated")])
    } else {
      return(values$dataset[[1]][grepl(input$productSelection,values$dataset[[1]]$Products),c("Name","Street","City","Label","Products","ProductsTruncated","Longitude","Latitude")])
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
  
  tableDataset <- reactive({
    filteredDataset()[,c("Name","City","ProductsTruncated","Label")]
  })
  
  output$ProducerTable <- DT::renderDataTable( {
    tableDataset()
  },    options = list(lengthChange = FALSE,
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
        fluidPage(
          h2(textOutput("DetailsName")),
          fluidRow(
            column(6,textOutput("DetailsStreet"),
                   textOutput("DetailsCity")),
            column(6,textOutput("DetailsTelephone"),
                   textOutput("DetailsMail"),
                   textOutput("DetailsWeb"))
          ),
          hr(),
          textOutput("DetailsLabel"),
          textOutput("DetailsProduct")
          
        ),
        easyClose = TRUE
      )
      )
    }
  })
  
  selectedProducer <- reactive({
    tail(input$ProducerTable_rows_selected,n=1)
  })
  
  # Detailed View
  output$DetailsName <- renderText( {
    filteredDataset()[selectedProducer(),"Name"]
  })
  output$DetailsStreet <- renderText( {
    paste0("Address: ", filteredDataset()[selectedProducer(),"Street"])
  })
  output$DetailsCity <- renderText( {
    paste0("Location: ", filteredDataset()[selectedProducer(),"City"])
  })
  output$DetailsTelephone <- renderText( {
    paste0("Phone: ", filteredDataset()[selectedProducer(),"Telephone"])
  })
  output$DetailsMail <- renderText( {
    paste0("e-mail: ", filteredDataset()[selectedProducer(),"Mail"])
  })
  output$DetailsWeb <- renderText( {
    paste0("Website: ", filteredDataset()[selectedProducer(),"Site.Web"])
  })
  output$DetailsLabel <- renderText( {
    paste0("Label: ", filteredDataset()[selectedProducer(),"Label"])
  })
  output$DetailsProduct <- renderText( {
    paste0("Products: ", filteredDataset()[selectedProducer(),"Products"])
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
        title = "Edit",
        fluidPage(
          fluidRow(
            column(6,
                   textInput("editName",
                             label='Name',
                             value=selectedProducer$Name[1]),
            textInput("editStreet",
                      label="Address",
                      value=selectedProducer$Street[1]),
            textInput("editCity",
                      label="Location",
                      value=selectedProducer$City[1])),
          column(6,textInput("editTelephone",
                             label="Phone",
                             value=selectedProducer$Telephone[1]),
                 textInput("editMail",
                           label="e-mail",
                           value=selectedProducer$Mail[1]),
                 textInput("editWeb",
                           label="website",
                           value=selectedProducer$Web.Site[1]))
        ),
        hr(),
        textInput("editLabel",
                  label="Label",
                  value=selectedProducer$Label[1]),
        textInput("editProduct",
                  label="Products",
                  value=selectedProducer$Products[1])
        
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
    values$dataset[[1]][editIndex,"Street"] <- input$editStreet
    values$dataset[[1]][editIndex,"City"] <- input$editCity
    values$dataset[[1]][editIndex,"Telephone"] <- input$editTelephone
    values$dataset[[1]][editIndex,"Mail"] <- input$editMail
    values$dataset[[1]][editIndex,"Site.Web"] <- input$editWeb
    values$dataset[[1]][editIndex,"Label"] <- input$editLabel
    values$dataset[[1]][editIndex,"Products"] <- input$editProduct
    
    values$dataset[[1]][editIndex,"ProductsTruncated"] <- paste0(strtrim(input$editProduct,50),"...")
    
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
    title = "Add",
    fluidPage(
      fluidRow(
        column(6,
               textInput("addName",
                         label='Name'),
               textInput("addStreet",
                         label="Address"),
               textInput("addCity",
                         label="Location")),
        column(6,textInput("addTelephone",
                           label="Phone"),
               textInput("addMail",
                         label="e-mail"),
               textInput("addWeb",
                         label="website"))
      ),
      hr(),
      textInput("addLabel",
                label="Label"),
      textInput("addProduct",
                label="Products")
      
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
    Longitude = 6.332217,
    Latitude = 46.532801,
    Source="Crowdsourced",
    Name = input$addName,
    Type="-",
    Responsable="-",
    Street = input$addStreet,
    City = input$addCity,
    Telephone = input$addTelephone,
    Mobile="-",
    Mail = input$addMail,
    Site.Web = input$addWeb,
    Label = input$addLabel,
    Products = input$addProduct,
    ProductsTruncated = paste0(strtrim(input$addProduct,50),"..."),
    Horaires="-")
  
  print(colnames(newRow))
  print(colnames(values$dataset[[1]]))
  
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