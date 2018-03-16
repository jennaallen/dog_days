library(shiny)
library(tidyverse)
library(timevis)
library(DT)
library(aws.s3)
library(sparkline)
library(magick)
library(shinythemes)

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
   theme = shinytheme("lumen"),
   tags$style("table.dataTable tr.selected td, table.dataTable td.selected {
              background-color: #bddfec !important;
              }"),
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
                 # htmlOutput("pet_image"),
                 imageOutput("pet_image", inline = TRUE),
                 br(), br(),
                 htmlOutput("pet_info"), 
                 br(),
                 h5(icon("scale", lib = "glyphicon"), "Weight History (lbs.)"),
                 sparklineOutput("pet_weight"),
                 
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
    mainPanel(width = 10, tabsetPanel(tabPanel(div(icon("medkit"), "Medical History"), 
                                   wellPanel(h3("Medical History and Tests Timeline"),
                                             br(),
                                             timevisOutput("med_history_timeline"),
                                             actionButton("medfit", "Reset view")),
                                   conditionalPanel(condition = "output.show_visit_details", fluidRow(column(4, wellPanel(h3("Visit Details"),
                                                                                                                          htmlOutput(outputId = "visit_info")
                                                                                                                          )
                                                                                                             ),
                                                                                                      column(4, wellPanel(h3("Tests Performed"),
                                                                                                                          htmlOutput(outputId = "test_info")
                                                                                                      )),
                                                                                                      column(4, wellPanel(h3("Prescribed Medications"),
                                                                                                                          htmlOutput(outputId = "medication_info")))
                                   #          column(6, wellPanel(h3("Past Medications"), 
                                   #                              dataTableOutput(outputId = "past_meds_table")
                                   #                              )
                                   #                 )
                                   ))
                                   # wellPanel(h3("Vets"), 
                                   #           dataTableOutput(outputId = "vets_table")
                                   #           )
                                   ), 
                                   tabPanel(div(icon("heartbeat"), "Vaccine History"),
                                            wellPanel(h3("Vaccine Timeline"),
                                                      checkboxGroupInput(inputId = "vacc",
                                                                         label = NULL,
                                                                         choices = c("Current Vaccines" = "Y", 
                                                                                     "Past Vaccines" = "N"),
                                                                         selected = "Y"
                                                                         ),
                                                      br(),
                                                      timevisOutput("vaccine_history_timeline"),
                                                      actionButton("vaccinefit", "Reset view")
                                                      ),
                                            # Show vaccine certificate
                                            conditionalPanel(condition = "output.show_vaccine_cert", 
                                                             h3("Vaccine Certificate"),
                                                             br(),
                                                             downloadButton("download_vacc", "Download PDF"),
                                                             br(), br()),
                                            uiOutput("vaccine_cert")
                                            ),
                                   tabPanel(div(icon("hospital-o"), "Medication History"),
                                            wellPanel(h3("Current Medications"),
                                                      br(),
                                                      dataTableOutput(outputId = "current_meds_table")
                                                      ),
                                                     wellPanel(h3("Past Medications"), 
                                                               br(),
                                                               dataTableOutput(outputId = "past_meds_table")
                                                               )
                                            ),
                                   tabPanel(div(icon("user-md"), "Vet History"),
                                            wellPanel(h3("Vets"), 
                                                      br(),
                                                      dataTableOutput(outputId = "vets_table")
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
      image_write(tempfile(fileext = ".png"), format = "png")

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
  
  # Create pet weight history sparkline
  output$pet_weight <- renderSparkline({
    dimVisits %>% 
      select(dog_name, visit_date, visit_weight) %>% 
      filter(dog_name == input$pet, !is.na(visit_weight)) %>% 
      pull(visit_weight) %>% 
      sparkline(width = "98%", 
                height = "100px", 
                spotRadius = 7, 
                highlightSpotColor = "#999", 
                fillColor = FALSE, 
                lineColor = "#20c997", 
                highlightLineColor = "#20c997",
                lineWidth = 3,
                maxSpotColor = "#6f42c1",
                minSpotColor = "#6f42c1")
  })
  
  
  # Create grouped medical and tests history timeline
  output$med_history_timeline <- renderTimevis({
    req(input$pet)
    
    timeline_visits <- dimVisits %>% 
      inner_join(dimDogs, by = "dog_name") %>% 
      filter(dog_name %in% input$pet, str_detect(visit_category, "medical")) %>% 
      mutate(group = "med",
             id = paste(visit_id, group, sep = "_")) %>% 
      rename(content = med_visit_summary, start = visit_date, title = visit_notes, category = visit_category) %>% 
      select(id, dog_name, facility_name, start, title, content, category, group) 
    
    timeline_tests <- dimTests %>% 
      inner_join(dimDogs, by = "dog_name") %>%
      filter(dog_name %in% input$pet, str_detect(test_category, "medical")) %>%
      mutate(group = "test",
             id = paste(test_id, group, sep = "_")) %>% 
      rename(content = test_name, start = test_date_performed, title = test_result, category = test_category) %>%
      select(id, dog_name, facility_name, start, title, content, category, group) 
    
    grouped_data <- timeline_visits %>% 
      bind_rows(timeline_tests) %>% 
      mutate(className = group)
    
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
  
  # create server to ui variable for visit details conditional panel
  output$show_visit_details <- reactive({
    
    if (!is.null(input$med_history_timeline_selected)) {
      get_group <- input$med_history_timeline_data %>% 
        filter(id == input$med_history_timeline_selected) %>% 
        pull(group)
    }
    
    !is.null(input$med_history_timeline_selected) & get_group == "med"
    
  })
  outputOptions(output, "show_visit_details", suspendWhenHidden = FALSE)
  
  # create server to ui variable for visit details conditional panel
  output$show_test_results <- reactive({
    
    if (!is.null(input$med_history_timeline_selected)) {
      get_group <- input$med_history_timeline_data %>% 
        filter(id == input$med_history_timeline_selected) %>% 
        pull(group)
    }
    
    !is.null(input$med_history_timeline_selected) & get_group == "test"
    
  })
  outputOptions(output, "show_visit_details", suspendWhenHidden = FALSE)
  
  
  output$visit_info <- renderText({
    
    if (!is.null(input$med_history_timeline_selected)) {
      id <- input$med_history_timeline_data %>%
        filter(group == "med", id == input$med_history_timeline_selected) %>% 
        select(id) %>% 
        mutate(id = str_extract(id, "\\d+")) %>% 
        pull() %>% 
        as.integer()
        
      # could join visits with vet info to get vet phone and include it here
      date <- paste(strong("Visit Date:"), dimVisits %>% 
                      filter(visit_id == id) %>% 
                      pull(visit_date))
      notes <- paste(strong("Visit Information:"), dimVisits %>% 
                       filter(visit_id == id) %>% 
                       pull(visit_notes))
      vet <- paste(strong("Vet:"), dimVisits %>% 
                     filter(visit_id == id) %>% 
                     pull(facility_name))
      paste(date, notes, vet, sep = "<br>")
    }
  })
  
  output$test_info <- renderText({
    
    if (!is.null(input$med_history_timeline_selected)) {
      id <- input$med_history_timeline_data %>%
        filter(group == "med", id == input$med_history_timeline_selected) %>% 
        select(id) %>% 
        mutate(id = str_extract(id, "\\d+")) %>% 
        pull() %>% 
        as.integer()
      
      # could join visits with vet info to get vet phone and include it here
      tests <- dimVisits %>% 
                       left_join(dimTests, by = c("dog_name", "facility_name", "visit_date")) %>% 
                      filter(visit_id == id) %>% 
                      pull(test_name) %>% 
        paste(collapse  = "<br>")
    }
  })
  
  output$medication_info <- renderText({
    
    if (!is.null(input$med_history_timeline_selected)) {
      id <- input$med_history_timeline_data %>%
        filter(group == "med", id == input$med_history_timeline_selected) %>% 
        select(id) %>% 
        mutate(id = str_extract(id, "\\d+")) %>% 
        pull() %>% 
        as.integer()
      
      # could join visits with vet info to get vet phone and include it here
      tests <- dimVisits %>% 
        left_join(dimMeds, by = c("dog_name", "facility_name", "visit_date")) %>% 
        filter(visit_id == id, !(med_category %in% "flea and tick")) %>% 
        select(med_name) %>% 
        distinct() %>% 
        pull() %>% 
        paste(collapse  = "<br>")
    }
  })
  
  # Create current meds data table
  output$current_meds_table <- renderDataTable({
    req(input$pet)
    dimMeds %>%
      filter(dog_name %in% input$pet, med_current_flag == "Y") %>%
      select(med_name, facility_name, med_start_date, med_dosage, med_dosage_freq, med_category) %>% 
      datatable(options = list(pageLength = 5, dom = 'ltip'),
                rownames = FALSE)
  })
  
  # Create past meds data table
  output$past_meds_table <- renderDataTable({
    req(input$pet)
    dimMeds %>%
      filter(dog_name %in% input$pet, med_current_flag == "N") %>%
      select(med_name, facility_name, med_end_date, med_dosage, med_dosage_freq, med_category) %>% 
      arrange(desc(med_end_date)) %>% 
      datatable(options = list(pageLength = 10),
                rownames = FALSE)
  })
  
  # Create vets data table
  output$vets_table <- renderDataTable({
    req(input$pet)
    dimVisits %>%
      left_join(dimVets, by = "facility_name") %>% 
      filter(dog_name %in% input$pet) %>%
      select(facility_name, vet_phone, vet_website, vet_email, vet_state) %>% 
      distinct() %>% 
      arrange(facility_name) %>% 
      datatable(options = list(pageLength = 10, dom = 'ltip'),
                rownames = FALSE)
  })
  
  # Create vaccine timeline
  output$vaccine_history_timeline <- renderTimevis({
    req(input$pet)
    
    vac <- dimVaccines %>% 
      inner_join(dimDogs, by = "dog_name") %>% 
      select(dog_name, content = vaccine_name, start = vaccine_date_given, end = vaccine_date_expires, title = facility_name, current_flag = vaccine_current_flag, doc = vaccine_certification)
    
    tests <- dimTests %>% 
      inner_join(dimDogs, by = "dog_name") %>%
      filter(!is.na(test_current_flag)) %>% 
      select(dog_name, content = test_name, start = test_date_performed, end = test_date_expires, title = facility_name, current_flag = test_current_flag, doc = test_result_doc) 
    
vac %>% 
    bind_rows(tests) %>% 
  rowid_to_column(var = "id") %>% 
      filter(dog_name %in% input$pet, current_flag %in% input$vacc) %>% 
    mutate(className = case_when(
      current_flag == "Y" ~ "current",
      current_flag == "N" ~ "past"),
    title = paste("Date Given: ", start, "\n", "Date Expires: ", end, "\n" ,"Vet: ", title,"\n",id, sep = "")) %>% 
    timevis()
  })
  
  # reset timeline view
  observeEvent(input$vaccinefit, {
    fitWindow("vaccine_history_timeline")
  })
  
  # create server to ui variable for vaccine certification conditional panel
  output$show_vaccine_cert <- reactive({
    if (!is.null(input$vaccine_history_timeline_selected)) {
    show_cert <- input$vaccine_history_timeline_data %>%
      filter(id == input$vaccine_history_timeline_selected) %>%
      pull(doc)
    } else {
      show_cert <- NA
    }
    
    !is.null(input$vaccine_history_timeline_selected) & !is.na(show_cert)
  })
  outputOptions(output, "show_vaccine_cert", suspendWhenHidden = FALSE)
  
  # show vaccine related file if vaccine is selected in timeline
  output$vaccine_cert <- renderUI({
    
    if (!is.null(input$vaccine_history_timeline_selected)) {
    cert <- input$vaccine_history_timeline_data %>%
        filter(id == input$vaccine_history_timeline_selected) %>% 
        pull(doc)
    
    if (!is.na(cert)) {
      cert %>% 
      str_replace("https://s3.amazonaws.com", "s3:/") %>%
        get_object() %>% 
        writeBin("www/test.pdf") # tempfile(fileext = ".pdf")
      tags$iframe(style = "height:1400px; width:100%", src = "test.pdf")
    } else {
        h3("No Vaccine Certificate available")
    }
  }

  })
  
  # Download vaccine certificate
  output$download_vacc <- downloadHandler(
    filename = function() {
      "vaccine_cert.pdf"
    },
    content = function(file) {
        file.copy("www/test.pdf", file)
    }
  )
  
  # clear selection if different dog is chosen not quite sure how to do this, get warning of argument is length zero
  # eventReactive(input$pet, {
  #   input$vaccine_history_timeline_selected <- NULL
  # })
  
  # Create medication history timeline
  # output$medication_history_timeline <- renderTimevis({
  #   req(input$pet)
  #   
  #   medincines <- dimMeds %>% 
  #     inner_join(dimDogs, by = "dog_name") %>% 
  #     filter(dog_name %in% input$pet) %>% 
  #     rename(content = med_name, start = med_start_date, title = med_category) %>% 
  #     mutate(group = if_else(med_current_flag == "Y", "current", "past"))
  #   
  #   
  #   groups <- data.frame(
  #     id = c("current", "past"),
  #     content = c("Current Medications", "Past Medications")
  #   )
  #   
  #   timevis(medincines, groups = groups)
  # })
    
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)