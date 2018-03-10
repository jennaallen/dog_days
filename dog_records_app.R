library(shiny)
library(readr)
library(dplyr)
library(timevis)
library(DT)

source("utils.R")

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
  filter(visit_category != "Initial visit") %>% 
  distinct() %>% 
  pull() %>% 
  sort()

# UI
ui <- fluidPage(
   # tags$head(
   #   tags$link(href = "style.css", rel = "stylesheet")
   # ),
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
                       ), width = 7
             ),
      column(wellPanel(DT::dataTableOutput(outputId = "med_tests_table")), width = 3)
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
  
  # Create med history timeline
  output$timeline <- renderTimevis({
    req(input$dog, input$visit_category)
    config <- list(
      orientation = "top",
      multiselect = TRUE
    )
    dimVisits %>% 
      inner_join(dimDogs, by = "dog_name") %>% 
      filter(dog_name %in% input$dog, visit_category %in% input$visit_category) %>%
      rename(id = visit_id, content = visit_summary, start = visit_date) %>% 
        # distinct() %>% 
      # mutate(style = "color: red; background-color: pink;",
      #        title = "test") %>% 
        timevis(options = config)
  })
  
 
  output$med_tests_table <- DT::renderDataTable({
    req(input$dog)
    dimVisits %>% 
      inner_join(dimDogs, by = "dog_name") %>% 
      inner_join(dimTests, by = c("dog_name", "visit_date" = "test_date_given", "facility_name")) %>%
      filter(dog_name %in% input$dog, visit_category %in% input$visit_category,!(test_name %in% c("Fecal Test", "Heartworm Test"))) %>% 
      select(visit_id, visit_date, test_name, test_result) %>% 
      datatable(options = list(pageLength = 10),
                               rownames = FALSE)
  #      # mutate(selected = if_else(id %in% input$med_history_timeline_selected, 1, 0)) %>% 
  #      # datatable(options = list(pageLength = 10, 
  #      #                          columnDefs = list(list(targets = 6, visible = FALSE))
  #      #                          )
  #      #           ) %>% 
  #     #  formatStyle("selected", target = "row", backgroundColor = styleEqual(c(0, 1), c('transparent', 'yellow'))
  #     #              ) #%>% 
  #     #select(id, content)
   })

  # # hide V6
  # datatable(df, options = list(
  #   columnDefs = list(list(targets = 6, visible = FALSE))
  # )) %>% formatStyle(
  #   'V1', 'V6',
  #   backgroundColor = styleEqual(c(0, 1), c('gray', 'yellow'))
  # )

  
  # # Create tests data table
  # output$teststable <- renderTable({
  #   req(input$dog)
  #   tests_from_selected_dog <- dimTests %>%
  #     filter(dog_name %in% input$dog) %>%
  #     select(test_name, test_result)
  # })
  # 
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