library(shiny)
library(tidyverse)
library(timevis)
library(DT)
library(aws.s3)
library(magick)

# source("utils.R")

dimTests <- read_csv("dimTests.csv")
dimMeds <- read_csv("dimMeds.csv")
dimVisits <- read_csv("dimVisits.csv")
dimDogs <- read_csv("dimDogs.csv")
dimVets <- read_csv("dimVets.csv")
dimVaccines <- read_csv("dimVaccines.csv")


# all_data <- dimVisits %>% 
#   left_join(dimDogs, by = "dog_name") %>% 
#   left_join(dimVets, by = "facility_name") %>% 
#   left_join(dimVaccines, by = c("dog_name", "visit_date" = "vaccine_date_given", "facility_name")) %>% 
#   left_join(dimMeds, by = c("dog_name", "facility_name", "visit_id")) %>% 
#   left_join(dimTests, by = c("dog_name", "visit_date" = "test_date_given", "facility_name")) %>% 
#   distinct()

all_pets <- sort(unique(dimDogs$dog_name))
# all_visit_categories <- dimVisits %>% 
#   select(visit_category) %>% 
#   filter(!(str_detect(visit_category, "Initial visit|;"))) %>% 
#   distinct() %>% 
#   pull() %>% 
#   sort()

# UI
ui <- fluidPage(
  
  # use custom css
    tags$head(
      tags$link(href = "style.css", rel = "stylesheet")
    ),
    
    titlePanel("Pet Records",
               windowTitle = "Pets"),
    
  sidebarLayout(
    
    # inputs
    sidebarPanel(radioButtons(inputId = "pet",
                              label = "Select pet:",
                              choices = all_pets,
                              selected = "Layla"), 
                 
                 hr(), # horizontal line for visual separation
                 
                 # display relevent pet information
                 imageOutput("pet_image", inline = TRUE),
                 br(), br(),
                 htmlOutput("pet_info"), 
                 
                 hr(), # horizontal line for visual separation
                 
                 # built with Shiny by RStudio
                 br(), br(),    # two line breaks for visual separation
                 h5("Built with",
                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                    "by",
                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                    "."),
                 width = 2),
    
    # Outputs
    mainPanel(width = 10, tabsetPanel(tabPanel("Medical History", 
                                   wellPanel(h4("Medical History and Tests Timeline"),
                                             timevisOutput("med_history_timeline"),
                                             actionButton("medfit", "Reset view")),
                                   fluidRow(column(6, wellPanel(h4("Current Medications"), 
                                                                dataTableOutput(outputId = "current_meds_table")
                                                                )
                                                   ),
                                            column(6, wellPanel(h4("Past Medications"), 
                                                                dataTableOutput(outputId = "past_meds_table")
                                                                )
                                                   )
                                            ),
                                   wellPanel(h4("Vets"), 
                                             dataTableOutput(outputId = "vets_table")
                                             )
                                   ), 
                                   tabPanel("Vaccine History",
                                            wellPanel(h4("Vaccine Timeline"),
                                                      checkboxGroupInput(inputId = "vacc",
                                                                         label = NULL,
                                                                         choices = c("Current Vaccines" = "Y", 
                                                                                     "Past Vaccines" = "N"),
                                                                         selected = "Y"
                                                                         ),
                                                      timevisOutput("vaccine_history_timeline"),
                                                      actionButton("vaccinefit", "Reset view")
                                                      )
                                            )
                          )
              )
    )
  )


# Server
server <- function(input, output) {
  
  # Get pet image to be displayed in sidepanel
  output$pet_image <- renderImage({
    req(input$pet)
    tmpfile <- dimDogs %>% 
      filter(dog_name %in% input$pet) %>% 
      select(picture) %>% 
      str_replace("https://s3.amazonaws.com", "s3:/") %>% 
      get_object() %>% 
      image_read() %>% 
      image_write(tempfile(fileext = 'png'), format = 'png')
    
    # Return a list
    list(src = tmpfile, 
         height = "200px", 
         contentType = "image/png")
   }, deleteFile = TRUE)

  # Create pet info to be displayed in sidepanel
  output$pet_info <- renderText({
    req(input$pet)
    dob <- paste(strong("DOB:"), dimDogs %>% 
      filter(dog_name %in% input$pet) %>% 
      pull(date_of_birth))
    species <- paste(strong("Species:"), dimDogs %>% 
                      filter(dog_name %in% input$pet) %>% 
                      pull(species))
    breed <- paste(strong("Breed:"), dimDogs %>% 
                      filter(dog_name %in% input$pet) %>% 
                      pull(breed))
    sex <- paste(strong("Sex:"), dimDogs %>% 
                    filter(dog_name %in% input$pet) %>% 
                    unite(sex, sex, reproductive_status, sep = " ") %>% 
                    pull(sex))
    color <- paste(strong("Color:"), dimDogs %>% 
                    filter(dog_name %in% input$pet) %>% 
                    pull(color))
    paste(dob, species, breed, sex, color, sep = "<br>")
  })
  
  # Create grouped medical and tests history timeline
  output$med_history_timeline <- renderTimevis({
    req(input$pet)
    
    timeline_visits <- dimVisits %>% 
      inner_join(dimDogs, by = "dog_name") %>% 
      filter(dog_name %in% input$pet, str_detect(visit_category, "medical")) %>% 
      rename(content = med_visit_summary, start = visit_date, title = visit_notes, category = visit_category) %>% 
      select(dog_name, facility_name, start, title, content, category) %>% 
      mutate(group = "med")
    
    timeline_tests <- dimTests %>% 
      inner_join(dimDogs, by = "dog_name") %>%
      filter(dog_name %in% input$pet, str_detect(test_category, "medical")) %>%
      rename(content = test_name, start = test_date_performed, title = test_result, category = test_category) %>%
      select(dog_name, facility_name, start, title, content, category) %>% 
      mutate(group = "test") 
    
    grouped_data <- timeline_visits %>% 
      bind_rows(timeline_tests) %>% 
      mutate(className = case_when(
        category == "medical" ~ "medical",
        category == "medical follow-up" ~ "medical-follow-up",
        category == "medical; routine" ~ "medical",
        category == "medical follow-up; routine" ~ "medical-follow-up"))
    
    groups <- data.frame(
      id = c("med", "test"),
      content = c("Medical History", "Test History")
    )
      
      timevis(grouped_data, groups = groups)
  })
  
  # reset timeline view
  observeEvent(input$medfit, {
    fitWindow("med_history_timeline")
  })
  
  # Create current meds data table
  output$current_meds_table <- renderDataTable({
    req(input$pet)
    dimMeds %>%
      filter(dog_name %in% input$pet, med_current_flag == "Y") %>%
      select(med_name, med_start_date) %>% 
      datatable(options = list(pageLength = 5, dom = 'ltip'),
                rownames = FALSE)
  })
  
  # Create past meds data table
  output$past_meds_table <- renderDataTable({
    req(input$pet)
    dimMeds %>%
      filter(dog_name %in% input$pet, med_current_flag == "N") %>%
      select(med_name, med_start_date) %>% 
      datatable(options = list(pageLength = 5),
                rownames = FALSE)
  })
  
  # Create vest data table
  output$vets_table <- renderDataTable({
    req(input$pet)
    dimVisits %>%
      left_join(dimVets, by = "facility_name") %>% 
      filter(dog_name %in% input$pet) %>%
      select(facility_name, vet_phone, vet_website, vet_email, vet_state) %>% 
      distinct() %>% 
      datatable(options = list(pageLength = 10, dom = 'ltip'),
                rownames = FALSE)
  })
  
  # Create vaccine timeline
  output$vaccine_history_timeline <- renderTimevis({
    req(input$pet)
    
    dimVaccines %>% 
      inner_join(dimDogs, by = "dog_name") %>% 
      filter(dog_name %in% input$pet, vaccine_current_flag %in% input$vacc) %>% 
      rename(content = vaccine_name, start = vaccine_date_given, end = vaccine_date_expires, title = facility_name) %>% 
      mutate(className = case_when(
        vaccine_current_flag == "Y" ~ "current",
        vaccine_current_flag == "N" ~ "past")) %>% 
      timevis()
  })
  
  # reset timeline view
  observeEvent(input$vaccinefit, {
    fitWindow("vaccine_history_timeline")
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)