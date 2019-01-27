## ui.R

library(shiny)
library(leaflet)
library(leaflet.extras)
library(googlesheets)
library(shinyFeedback)
library(htmltools)
library(data.table)
library(anytime)
library(sf)
library(splitstackshape)
library(here)

source(here('helper_functions.R'))

shinyUI(navbarPage('CamTracker',id = 'nav',
        
        tabPanel('Data Submission',
        div(class = 'outer',
        
        fluidPage(tweaks,
        theme = shinythemes::shinytheme('superhero'),
        shinyFeedback::useShinyFeedback(),  #must include for warning messages
        shinyjs::useShinyjs(), #enable/disable submit button
        shinyjs::inlineCSS(appCSS), #formatting
        title = "CamTracker - submit and explore camera trap checklists",
        
        fluidRow(
        column(11.5,
          div(id = 'headers',
              h3('CamTracker - An open database of camera trap checklists'),
              h4("Monitoring the world's mammals, one checklist
                 at a time."),
              hr()), #end div
          
          div(id = "user_info",
              h4('Step 1 of 5: Who was the camera trapper?'),
              # name box ####
              textInput("name", labelMandatory("Name"), ""),
              # email box ####
              textInput("email", labelMandatory("Email"), ""),
              # primary purpose of deployment ####
              selectInput('deployment_purpose',
                          labelMandatory('What was the primary purpose of 
                                         the deployment?'),
                          c('','Hunting',
                            'Research',
                            'Wildlife Viewing','Security','Other')),
              # deployment experience ####
              selectInput('number_deployments',
                          labelMandatory('How many times have you deployed 
                                         a camera trap?'),
                          c('','0','1-5','5-10','10+')),
              hr()), #end div
          
          div(id = "deployment_location",
              
              # deployment map ####
              h4('Step 2 of 5: Where was the camera deployed?'),
              h5('To drop pin, zoom to location and click on map'),
              leafletOutput("map", width = "60%"),
              textInput('Longitude',labelMandatory('Longitude: '),''),
              textInput('Latitude', labelMandatory('Latitude: '),''),
              hr()), #end div
          
          div(id = "deployment_date", 
              # date of deployment ####
              h4('Step 3 of 5: When was the camera operating?'),
              dateInput("deploy_date", 
                        "Depoloyment Date:",
                        format = ),
              dateInput("retrieval_date", 
                        "Retrieval Date:", 
                        format = ),
              hr()), #end div
          
          div(id = "deployment_specifics",               
              # Deployment specifics ####
              h4('Step 4 of 5: Camera deployment details'),
              # Camera bait ####
              selectInput("used_bait",
                          labelMandatory("I used Bait/Lure (e.g., corn, sardines, peanut butter) 
                                         over or near the camera"),
                          c('','Yes','No')),
              #checkboxInput("used_bait", 
              #              labelMandatory("I used bait/lure"), FALSE),
              
              # camera height ####
              selectInput('cam_height',labelMandatory('Approximately how high above the ground
                                                      was the camera mounted?'),
                          c('','1 - 2 ft(0.3 - 0.6 m)',
                            '2-4 ft (0.6 - 1.2 m)',
                            'Above 4 ft (1.2 m)')),
              
              # camera model ####
              selectInput('cam_make',labelMandatory('Please select the camera trap
                                                    manufacturer'),
                          c('',
                            'Browning',
                            'Bushnell',
                            'Covert',
                            'Cuddeback',
                            'HCO',
                            'Moultrie',
                            'Muddy',
                            'Plotwatcher',
                            'Primos',
                            'Reconyx',
                            'Spypoint',
                            'Stealth Cam',
                            'Wildgame Innovations',
                            'Other')),
              hr()), #end div
          div(id = "species_detected",
              # select species detected ####
              
              h4('Step 5 of 5: List all species detected during the deployment'),
              
              column(width = 11.5, mammals),
              
              # double check mammals are correct ####
              h4('Here are the mammals you selected:'),
              tableOutput(outputId = 'mammals_detected'),
              
              # are you submitting complete checklist ####
              selectInput('complete_checklist',
                          labelMandatory('Is this a complete checklist of
                                         all species that you were able to identify from the
                                         deployment?'),c("" ,"Yes","No")),
              
              actionButton("submit", "Submit", class = "btn-primary"),
              
              
              shinyjs::hidden(
                span(id = "submit_msg", "Submitting..."),
                div(id = "error",
                    div(br(), tags$b("Error: "), span(id = "error_msg"))
                )
              )
                          ),
          
          shinyjs::hidden(
            div(
              id = "thankyou_msg",
              h3("Thanks, your response was submitted successfully!"),
              actionLink("submit_another", "Submit another response"))),
          
          hr(),
          p('Shiny App created by Brent Pease')) #end div
          )
          ) #fluid page
        ) #div
        ), #tabPanel
        tabPanel("Data Exploration",
        div(class = 'outer',
        fluidPage(
        titlePanel('Data Exploration'),
        sidebarLayout(
        sidebarPanel(
        selectInput(inputId = 'focal_species',
         label = 'Species',
         choices = c('All',mams_common),
         multiple = F),
        selectInput(inputId = 'focal_state',
         label = 'State',
         choices = c('All',states_name),
         multiple = F),
        uiOutput("DownloadPanel")
        ),
        mainPanel(
        leafletOutput('mapPlot'),  # leaflet output for plotting the points
        tags$hr()
        # tableOutput('viewData')  # table for sanity check
        
        ) # end mainPanel
        ) # end div
        ) # end sidebar layout
        ) # end fluid page
        ) # end tabPanel for data viz#end tabPanel for data viz
        ) # end navbarpage
) # end shinyUI


