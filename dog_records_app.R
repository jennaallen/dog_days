library(shiny)
library(readr)
library(stringr)
library(dplyr)
library(timevis)
library(DT)

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

all_dogs <- sort(unique(dimDogs$dog_name))
all_visit_categories <- dimVisits %>% 
  select(visit_category) %>% 
  filter(!(str_detect(visit_category, "Initial visit|;"))) %>% 
  distinct() %>% 
  pull() %>% 
  sort()

# UI
ui <- fluidPage(
    tags$head(
      tags$link(href = "style.css", rel = "stylesheet")
    ),
  #sidebarLayout(
  #tags$style(HTML('.vis-item.vis-selected td, table.dataTable td.selected {background-color: green !important;}')),
  #tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
    # Input(s)
    fluidRow(
      column(wellPanel(radioButtons(inputId = "dog",
                                    label = "Select dog:",
                                    choices = all_dogs,
                                    selected = "Layla"),
                  checkboxGroupInput(inputId = "visit_category",
                                     label = "Select visit history:",
                                     choices = all_visit_categories,
                                     selected = "medical")), width = 2),
      column(wellPanel(timevisOutput("timeline")
                       ), width = 6
             ),
      column(wellPanel(DT::dataTableOutput(outputId = "med_tests_table")), width = 4)
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
      )#),
      
      # selectInput(inputId = "dog",
      #             label = "Select dog:",
      #             choices = all_dogs,
      #             selected = "Layla",
      #             multiple = TRUE)
      
)
    
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
  
  # Create timeline
  output$timeline <- renderTimevis({
    req(input$dog, input$visit_category)
    config <- list(
      orientation = "top",
      multiselect = TRUE
      )
    
    dimVisits %>% 
      inner_join(dimDogs, by = "dog_name") %>% 
      filter(dog_name %in% input$dog, str_detect(visit_category, input$visit_category)) %>%
      rename(id = visit_id, content = visit_summary, start = visit_date) %>% 
       mutate(className = case_when(
         visit_category == "medical" ~ "medical",
         visit_category == "medical follow-up" ~ "medical-follow-up",
         visit_category == "routine" ~ "routine"),
         # possibly unite some fields together to make what displays in the title
         title = visit_notes) %>% 
      timevis(options = config)
    })
  
  # Create tests data table
  output$med_tests_table <- DT::renderDataTable({
    req(input$dog)
    dimVisits %>% 
      inner_join(dimDogs, by = "dog_name") %>% 
      inner_join(dimTests, by = c("dog_name", "visit_date" = "test_date_given", "facility_name")) %>%
      filter(dog_name %in% input$dog, str_detect(test_category, input$visit_category)) %>% 
      select(visit_id, visit_date, test_name, test_result, test_category) %>% 
      datatable(options = list(pageLength = 10),
                               rownames = FALSE)
   })

  # # Create current meds data table
  # output$current_meds_table <- DT::renderDataTable({
  #   req(input$dog)
  #   current_meds_from_selected_dog <- dimMeds %>%
  #     filter(dog_name %in% input$dog, current_flag == "Y") %>%
  #     select(med_name, med_start_date)
  #   DT::datatable(data = current_meds_from_selected_dog, 
  #                 options = list(pageLength = 10), 
  #                 rownames = FALSE)
  # })
  # 
  # # Create past meds data table
  # output$past_meds_table <- DT::renderDataTable({
  #   req(input$dog)
  #   past_meds_from_selected_dog <- dimMeds %>%
  #     filter(dog_name %in% input$dog, current_flag == "N") %>%
  #     select(med_name, med_start_date)
  #   DT::datatable(data = past_meds_from_selected_dog, 
  #                 options = list(pageLength = 10), 
  #                 rownames = FALSE)
  # })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)