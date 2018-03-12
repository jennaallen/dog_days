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
    mainPanel(tabsetPanel(tabPanel("Medical History", fluidRow(
      # display timeline
      column(7, wellPanel(h4("Medical History Timeline"),
                          timevisOutput("med_history_timeline"),
                          actionButton("fit", "Reset view")
                          ),
             # display vets table
             fluidRow(column(12, wellPanel(h4("Vets"), 
                                           dataTableOutput(outputId = "vets_table")
                                           )
                             )
                      )
             ), 
      # display medical tests
      column(5, wellPanel(h4("Medical Tests"),
                          dataTableOutput(outputId = "med_tests_table")
                          ),
             # display current medications
             fluidRow(column(12, wellPanel(h4("Current Medications"), 
                                           dataTableOutput(outputId = "current_meds_table")
                                           ),
                             # display past medications
                             fluidRow(column(12, wellPanel(h4("Past Medications"),
                                                           dataTableOutput(outputId = "past_meds_table")
                                                           )
                                             )
                                      )
                             )
                      )
             )
      )
      )
      # tabPanel("Vaccine History", wellPanel(timevisOutput("vaccine_history_timeline")))
      # tabPanel("Vaccine History", fluidRow(
      #   # display timeline
      #   column(7, wellPanel(h4("Medical History Timeline"),
      #                       timevisOutput("med_history_timeline"),
      #                       actionButton("fit", "Reset view")
      #   ),
      #   # display vets table
      #   fluidRow(column(12, wellPanel(h4("Vets"), 
      #                                 dataTableOutput(outputId = "vets_table")
      #   )
      #   )
      #   )
      #   ), 
      #   # display medical tests
      #   column(5, wellPanel(h4("Current Vaccines"),
      #                       dataTableOutput(outputId = "med_tests_table")
      #   ),
      #   # display current medications
      #   fluidRow(column(12, wellPanel(h4("Current Medications"), 
      #                                 dataTableOutput(outputId = "current_meds_table")
      #   ),
      #   # display past medications
      #   fluidRow(column(12, wellPanel(h4("Past Medications"),
      #                                 dataTableOutput(outputId = "past_meds_table")
      #   )
      #   )
      #   )
      #   )
      #   )
      #   )
      # )
      # )
      ), width = 10
            )
  )
)
  
                  # checkboxGroupInput(inputId = "visit_category",
                  #                    label = "Select visit history:",
                  #                    choices = all_visit_categories,
                  #                    selected = "medical")), width = 2),
     # column(wellPanel(timevisOutput("med_history_timeline")
     #                   ), width = 6
     #         ),
     #  column(wellPanel(dataTableOutput(outputId = "med_tests_table")
     #                   ), width = 4)
      # column(wellPanel(DT::dataTableOutput(outputId = "teststable")
      #                  ),
      #        width = 3,
      #        fluidRow(column(wellPanel(DT::dataTableOutput(outputId = "current_meds_table")
      #                                  ), width = 12,
      #                        fluidRow(column(wellPanel(DT::dataTableOutput(outputId = "past_meds_table")
      #                                                  ), width = 12)
      #                                 )
      #                        )
      #                 )
      #        )
  #     ))#),
  #     
  #     # selectInput(inputId = "dog",
  #     #             label = "Select dog:",
  #     #             choices = all_dogs,
  #     #             selected = "Layla",
  #     #             multiple = TRUE)
  #     
  #     )
  # )
    
#     # Output(s)
#     mainPanel(
#       DT::dataTableOutput(outputId = "teststable"),
#     width = 4),
#     
#     wellPanel("something", width = 4)
#   )
# )

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
  
  # Create medical history timeline
  output$med_history_timeline <- renderTimevis({
    req(input$pet)
    config <- list(
      orientation = "top",
      multiselect = TRUE
      )
    
    dimVisits %>% 
      inner_join(dimDogs, by = "dog_name") %>% 
      filter(dog_name %in% input$pet, str_detect(visit_category, "medical")) %>%
      rename(id = visit_id, content = med_visit_summary, start = visit_date) %>% 
        mutate(className = case_when(
         visit_category == "medical" ~ "medical",
         visit_category == "medical follow-up" ~ "medical-follow-up",
         visit_category == "medical; routine" ~ "medical",
         visit_category == "medical follow-up; routine" ~ "medical-follow-up"),
         # possibly unite some fields together to make what displays in the title
         title = visit_notes) %>%
      timevis(options = config)
    })
  
  # reset timeline view
  observeEvent(input$fit, {
    fitWindow("med_history_timeline")
  })
  
  # Create test history timeline
  output$test_history_timeline <- renderTimevis({
    req(input$pet)
    config <- list(
      orientation = "top",
      multiselect = TRUE
    )

    dimTests %>%
      inner_join(dimDogs, by = "dog_name") %>%
      filter(dog_name %in% input$pet, str_detect(test_category, "medical")) %>%
      rename(content = test_name, start = test_date_performed) %>%
      mutate(className = case_when(
        visit_category == "medical" ~ "medical",
        visit_category == "medical follow-up" ~ "medical-follow-up"),
        # possibly unite some fields together to make what displays in the title
        title = test_result) %>%
      timevis(options = config)
  })
  
  
  # Create tests data table
  output$med_tests_table <- renderDataTable({
    req(input$pet)
    dimVisits %>% 
      inner_join(dimDogs, by = "dog_name") %>% 
      inner_join(dimTests, by = c("dog_name", "visit_date" = "test_date_performed", "facility_name")) %>%
      filter(dog_name %in% input$pet, str_detect(test_category, "medical")) %>% 
      select(visit_id, visit_date, test_name, test_result) %>% 
      datatable(options = list(pageLength = 5, dom = 'ltip'),
                               rownames = FALSE)
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
      datatable(options = list(pageLength = 5, dom = 'ltip'),
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
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)