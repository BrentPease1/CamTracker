## server.R


shinyServer(function(input, output, session) {   
  
  points <- as.data.table(loadData())
  points[, `:=`(deploy_date = anydate(deploy_date),
                retrieval_date = anydate(retrieval_date),
                species_count = Reduce(`+`, lapply(.SD,function(x) !is.na(x)))), .SDcols = c(10:109)]
  
  #for storing clicks
  output$data_viz_map = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = points, 
                 lng = ~Longitude, 
                 lat = ~Latitude,
                 popup = ~paste("<b>Deployment Date:</b>", deploy_date, "<br>",
                                "<b>Retrieval Data:</b>", retrieval_date, "<br>",
                                "<b>Complete Checklist</b>:", complete_checklist, "<br>",
                                "<b>Species Count</b>:", species_count)) %>%
      setView(lng = -97.38, lat = 42.877, zoom = 4)})
  
  
  #leaflet - select camera location
  output$map = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -97.38, lat = 42.877, zoom = 4)
  })
  
  
  # Enable the Submit button when all mandatory fields are filled out
  # vapply is basically same as sapply
  # this says return a true if the mandataory field is not empty or null
  # and if it is empty or null, return False. vapply does this for all the 
  # mandatory fields specified in fieldsMandataory
  # all() returns a single true/false for the question "are all of of values true"?
  # if mandatoryfilled is TRUE, then the submit button is enabled using toggleState
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    #this enables/disables submit button based on whether the mandatory responses
    #have been filled
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  # store the checkboxGroupInput
  output$mammals_detected <- renderTable({
    input$mammals_detected
  }, rownames = T)
  
  
  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data,
              timestamp = epochTime())
    data <- t(data)
    data <- c(data, input$mammals_detected)
    data
  })    
  
  # When the Submit button is clicked, submit the response ####
  observeEvent(input$submit, {
    
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    # Save the data (show an error message in case of error) ####
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  # submit another response ####
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
  
  # render the admin panel
  output$adminPanelContainer <- renderUI({
    if (!isAdmin()) return()
    
    div(
      id = "adminPanel",
      h3("Camera checklists"),
      downloadButton("downloadBtn", "Download all responses"), br(), br(),
      downloadButton('subset_downloadBtn','Download selected responses'), br(), br(),
      DT::dataTableOutput("responsesTable"), br()
    )
  })
  
  # determine if current user is admin
  isAdmin <- reactive({
    is.null(session$user) || session$user %in% adminUsers
  })    
  
  # server - ensure valid email address ####
  observeEvent(input$email, {
    if(input$email != ""){
      feedbackWarning(
        inputId = "email",
        condition = isValidEmail(input$email) == F,
        text = 'Invalid Email Address',
        color = "#F89406",
        icon = shiny::icon("warning-sign", lib =
                             "glyphicon")
      )
    }
  })
  
  
  # server - save click event from map ####
  showpos <- function(x=NULL, y=NULL){#Show marker on click
    content <- paste0("Lon=", round(x, 5),
                      "; Lat=", round(y, 5))
    proxy <- leafletProxy("map")
    #add Popup
    proxy %>% clearMarkers() %>% addMarkers(x, y)
    
  }
  observe({#Observer to show Popups on click
    click <- input$map_click
    if (!is.null(click)) {
      showpos(x=click$lng, y=click$lat)
      
      updateTextInput(session, 'Longitude', value = click$lng)
      updateTextInput(session, 'Latitude', value = click$lat)
    }
  })
  
  
  #filter mammal checklist based on click$lng and click$lat
  observe({
    
    long <- as.numeric(input$Longitude)
    lat <- as.numeric(input$Latitude)
    state_name <- state_filter(long, lat)
    tmp <- mams[,lapply(.SD, function(x) grepl(state_name, x, perl=TRUE)), 
                by = mams$`Common Name`, .SDcols = c(5:54)]
    new_mams <- tmp[rowSums(tmp[,c(2:51)])>0]
    
    updateCheckboxGroupInput(session, inputId = "mammals_detected", choices = new_mams$mams)
  }
  )
  
  
  
  
  # Allow user to download responses ####
  output$downloadBtn <- downloadHandler(
    filename = function() { 
      sprintf("camera_trap_checklist.csv", humanTime())
    },
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE)
    }
  ) 
  
  output$subset_downloadBtn <- downloadHandler(
    filename = function() { 
      sprintf("mimic-google-form_%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(subsetData(), file, row.names = FALSE)
    }
  ) 
  
  
  
  
  
  
  
  # subset the data on various inputs from ui.R
  subsetData <- reactive({
    species <- input$focal_species
    state <- input$focal_state
    if(species == 'All' & state == 'All'){
      points <- as.data.table(loadData())
      points[, `:=`(deploy_date = anydate(deploy_date),
                    retrieval_date = anydate(retrieval_date),
                    species_count = Reduce(`+`, lapply(.SD,function(x) !is.na(x)))), .SDcols = c(10:109)]
      return(points)
    }
    if(state != 'All' & species == 'All'){
      good_state <- states[states$NAME == state,]
      tmp_points <- st_as_sf(x = data.frame(loadData()$Longitude, loadData()$Latitude), coords = c(1:2))
      tmp_points <- st_set_crs(tmp_points, "+init=epsg:4269")
      tmp_points <- st_transform(tmp_points, crs = 4269)
      good_state <- st_transform(good_state, crs = 4269)
      a <- sapply(st_intersects(tmp_points,good_state), function(z) if (length(z)==0) NA_integer_ else z[1])
      
      new_data <- points[c(which(!is.na(a))),]
      return(new_data)
    }
    if(species != 'All' & state == 'All'){
      points <- as.data.table(loadData())
      points[, `:=`(deploy_date = anydate(deploy_date),
                    retrieval_date = anydate(retrieval_date),
                    species_count = Reduce(`+`, lapply(.SD,function(x) !is.na(x)))), .SDcols = c(10:109)]
      
      tmp <- points[,lapply(.SD, function(x) grepl(species, x, perl=TRUE)), #need to fix this!
                    .SDcols = c(10:109)]
      new_data <- points[c(which(rowSums(tmp)>0)),]
      return(new_data)
    }
    if(species != 'All' & state != 'All'){
      points <- as.data.table(loadData())
      points[, `:=`(deploy_date = anydate(deploy_date),
                    retrieval_date = anydate(retrieval_date),
                    species_count = Reduce(`+`, lapply(.SD,function(x) !is.na(x)))), .SDcols = c(10:109)]
      
      tmp <- points[,lapply(.SD, function(x) grepl(species, x, perl=TRUE)), #need to fix this!
                    .SDcols = c(10:109)]
      new_data <- points[c(which(rowSums(tmp)>0)),]
      good_state <- states[states$NAME == state,]
      tmp_points <- st_as_sf(x = data.frame(new_data$Longitude, new_data$Latitude), coords = c(1:2))
      tmp_points <- st_set_crs(tmp_points, "+init=epsg:4269")
      tmp_points <- st_transform(tmp_points, crs = 4269)
      good_state <- st_transform(good_state, crs = 4269)
      a <- sapply(st_intersects(tmp_points,good_state), function(z) if (length(z)==0) NA_integer_ else z[1])
      
      new_data <- points[c(which(!is.na(a))),]
      return(new_data)
    }
    
  })
  
  # display the data in real time to identify if the subsetting
  # is occurring as expected.
  output$viewData <- renderTable({
    subsetData()
  })
  
  # plot the data points
  output$mapPlot <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -97.38, lat = 42.877, zoom = 4) %>%
      addDrawToolbar(
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()
        )
      ) %>%
      addMarkers(data = subsetData(), 
                 lng = ~Longitude, 
                 lat = ~Latitude,
                 popup = ~paste("<b>Deployment Date:</b>", deploy_date, "<br>",
                                "<b>Retrieval Data:</b>", retrieval_date, "<br>",
                                "<b>Complete Checklist</b>:", complete_checklist, "<br>",
                                "<b>Species Count</b>:", species_count))
  })
  
  
} #end server

) #end shiny server

