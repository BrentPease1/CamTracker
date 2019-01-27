## helper_functions.R

# Preamble specifications and/or helper functions ####
# which fields get saved ####
fieldsAll <- c("name", "email","deployment_purpose", "number_deployments",
               "Longitude","Latitude","cam_height","cam_make","used_bait",
               "deploy_date","retrieval_date","complete_checklist")

# which fields are mandatory ####
fieldsMandatory <- c("name", "email","number_deployments","Longitude","Latitude",
                     "cam_height","cam_make","mammals_detected","complete_checklist")


# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

#make sure email address is valid
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

table <- "1r2M89iBpOCFA99PAxiNgiioq2EfmlV46Mv3w_wL7vzc"

saveData <- function(data) {
  # Grab the Google Sheet
  sheet <- gs_key(table)
  # Add the data as a new row
  gs_add_row(sheet, input = data)
}

loadData <- function() {
  # Grab the Google Sheet
  sheet <- gs_key(table)
  # Read the data
  a <- gs_read_csv(sheet)
  a <- a[,-c(1:4)]
}

# directory where responses get stored
responsesDir <- file.path("responses")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 15px; }
#error { color: red; }
#body { background: #fcfcfc; }
#header { background: #fff;
#border-bottom: 1px solid #ddd;
#margin: -20px -15px 0; 
#padding: 15px 15px 10px; }"

tweaks <- list(tags$head(tags$style(HTML("
                                 .multicol { 
                                 -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                 -moz-column-count: 3;    /* Firefox */ 
                                 column-count: 3; 
                                 -moz-column-fill: auto;
                                 -column-fill: auto;
                                 } 
                                 "))))


# mammal list ####
mams <- fread(here("Data/mammal_species_list.csv"))
mams <- cSplit(mams, "States", ",")
sea_orders <- c('Sirenia')
sea_fams <- c('Otariidae',
              'Phocidae',
              'Delphinidae',
              'Odobenidae',
              'Balaenopteridae',
              'Balaenidae',
              'Phocoenidae',
              'Ziphiidae',
              'Kogiidae')
mams <- mams[!(Order %in% sea_orders)]
mams <- mams[!(Family %in% sea_fams)]
mams_common <- mams$`Common Name` #just get the combined names column

mammals <-
  list(tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'mammals_detected', 
                                   label    = "Select the mammals detected:", 
                                   choices  = mams$Common.Name,
                                   #  selected = all_rows,
                                   inline   = FALSE)))

#also read states in outside of filter function for data exploration
states <- st_read(dsn = here("Data/states_shapefile/cb_2016_us_state_20m.shp"),
                  layer = 'cb_2016_us_state_20m',
                  quiet = T)
states_name <- sort(as.character(states$NAME))
#state filtering function 
state_filter <- function(Long, Lat){
  #make camera location point shapefile
  cam_loc <- st_point(x = c(Long, Lat))
  
  #read in state shapefile
  states <- st_read(dsn = here("Data/states_shapefile/cb_2016_us_state_20m.shp"),
                    layer = 'cb_2016_us_state_20m',
                    quiet = T)
  
  #sp::over equivalent
  a <- sapply(st_intersects(cam_loc,states), function(z) if (length(z)==0) NA_integer_ else z[1])
  state_name <- as.vector(as.character(states$NAME[a]))
  return(state_name)
}
