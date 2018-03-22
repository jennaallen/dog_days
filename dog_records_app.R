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
                                             h5("Select an item to view more details or test results (where available). The information is shown below the timeline."),
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
                                   )),
                                   conditionalPanel(condition = "output.show_test_results", h3("Test Results"),
                                                    br(),
                                                    downloadButton("download_test_results", "Download PDF"),
                                                    br(), br()),
                                   uiOutput("test_results")
                                   # wellPanel(h3("Vets"), 
                                   #           dataTableOutput(outputId = "vets_table")
                                   #           )
                                   ), 
                                   tabPanel(div(icon("heartbeat"), "Vaccine History"),
                                            wellPanel(h3("Vaccine Timeline"),
                                                      h6("Select an item to view vaccine certificate (where available) below timeline"),
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
  
  # get all the variables needed for use in the following render functions
  
  get_group <- reactive({
    if (!is.null(input$med_history_timeline_selected)) {
      input$med_history_timeline_data %>%
        filter(id == input$med_history_timeline_selected) %>%
        pull(group)
    }
  })
  
  show_visit_details_fun <- reactive({
    !is.null(input$med_history_timeline_selected) && get_group() == "med"
  })
  
  show_test_results_fun <- reactive({
    !is.null(input$med_history_timeline_selected) && get_group() == "test"
  })
  
  id <- reactive({
    if (!is.null(input$med_history_timeline_selected)) {
   input$med_history_timeline_selected %>% 
      str_extract("\\d+") %>% 
      as.integer()
    }
  })    
  
  test_result <- reactive({
    if (show_test_results_fun()) {
      dimTests %>%
        filter(test_id == id()) %>%
        pull(test_result_doc)
     }
  })

  # create server to ui variable for visit details conditional panel
  output$show_visit_details <- reactive({
    show_visit_details_fun()
  })
  outputOptions(output, "show_visit_details", suspendWhenHidden = FALSE)
  
  # create server to ui variable for test results conditional panel
  output$show_test_results <- reactive({

    show_test_results_fun() && !is.na(test_result())

  })
  outputOptions(output, "show_test_results", suspendWhenHidden = FALSE)
  
  
  output$visit_info <- renderText({
    
    if (show_visit_details_fun()) {
        
        # could join visits with vet info to get vet phone and include it here
        date <- paste(strong("Visit Date:"), dimVisits %>%
                        filter(visit_id == id()) %>%
                        pull(visit_date))
        vet <- paste(strong("Vet:"), dimVisits %>%
                       filter(visit_id == id()) %>%
                       pull(facility_name))
        doctor <- paste(strong("Doctor:"), dimVisits %>%
                       filter(visit_id == id()) %>%
                       pull(visit_doctor))
        notes <- paste(strong("Visit Information:"), dimVisits %>%
                         filter(visit_id == id()) %>%
                         pull(visit_notes))
        paste(date, vet, doctor, notes, sep = "<br>")
      
    }
  })
  
  output$test_info <- renderText({
    
    if (show_visit_details_fun()) {
      
        
        dimVisits %>% 
          left_join(dimTests, by = c("dog_name", "facility_name", "visit_date")) %>% 
          filter(visit_id == id(), !(test_category %in% "routine")) %>% 
          pull(test_name) %>% 
          paste(collapse  = "<br>")
    }
  })
  
  output$medication_info <- renderText({
    
    if (show_visit_details_fun()) {
        
        dimVisits %>% 
          left_join(dimMeds, by = c("dog_name", "facility_name", "visit_date")) %>% 
          filter(visit_id == id(), !(med_category %in% "flea and tick")) %>% 
          select(med_name) %>% 
          distinct() %>% 
          pull() %>% 
          paste(collapse  = "<br>")
    }
  })
  
 # show test results file if test is selected in timeline
  output$test_results <- renderUI({

    if (show_test_results_fun()) {

      if (!is.na(test_result())) {
        test_result() %>%
          str_replace("https://s3.amazonaws.com", "s3:/") %>%
          get_object() %>%
          writeBin("www/test_result.pdf") # tempfile(fileext = ".pdf")
        tags$iframe(style = "height:1400px; width:100%", src = "test_result.pdf")
      } else {
        h3("No Test Results Available")
      }
    }

  })
  
  # Download test results
  output$download_test_results <- downloadHandler(
    filename = function() {
      "test_result.pdf"
    },
    content = function(file) {
      file.copy("www/test_result.pdf", file)
    }
  )
  
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
    req(input$pet, input$vacc)
    
    vac <- dimVaccines %>% 
      inner_join(dimDogs, by = "dog_name") %>% 
      select(dog_name, content = vaccine_name, start = vaccine_date_given, end = vaccine_date_expires, title = facility_name, current_flag = vaccine_current_flag, doc = vaccine_certification)
    
    tests <- dimTests %>% 
      inner_join(dimDogs, by = "dog_name") %>%
      filter(!is.na(test_current_flag)) %>% 
      select(dog_name, content = test_name, start = test_date_performed, end = test_date_expires, title = facility_name, current_flag = test_current_flag, doc = test_result_doc) 
    
    if (length(input$vacc) == 1 && input$vacc == "Y") {
      vac %>% 
        bind_rows(tests) %>% 
        rowid_to_column(var = "id") %>% 
        filter(dog_name %in% input$pet, current_flag %in% input$vacc) %>% 
        mutate(className = case_when(
          current_flag == "Y" ~ "current",
          current_flag == "N" ~ "past"),
          title = paste("Date Given: ", start, "\n", "Date Expires: ", end, "\n" ,"Vet: ", title,"\n", sep = "")) %>% 
        timevis()
    } else {
     vacc_data <- vac %>% 
        bind_rows(tests) %>% 
        rowid_to_column(var = "id") %>% 
        filter(dog_name %in% input$pet, current_flag %in% input$vacc) %>% 
        mutate(className = case_when(
          current_flag == "Y" ~ "current",
          current_flag == "N" ~ "past"),
          title = paste("Date Given: ", start, "\n", "Date Expires: ", end, "\n" ,"Vet: ", title,"\n", sep = ""),
          group = content) 
       
        groups <- data.frame(
          id = c("Rabies", "Distemper", "Bordetella (drops)", "Bordetella (injection)", "Flu", "Lepto", "Rattlesnake", "Fecal Test", "Heartworm Test"),
           content = c("Rabies", "Distemper", "Bordetella (drops)", "Bordetella (injection)", "Flu", "Lepto", "Rattlesnake", "Fecal Test", "Heartworm Test")
        )
         timevis(vacc_data, groups = groups)
      }
    
  })
  
  # reset timeline view
  observeEvent(input$vaccinefit, {
    fitWindow("vaccine_history_timeline")
  })
  
  cert <- reactive({
    if (!is.null(input$vaccine_history_timeline_selected)) {
    input$vaccine_history_timeline_data %>%
    filter(id == input$vaccine_history_timeline_selected) %>%
    pull(doc)
    }
  })  
  
  # create server to ui variable for vaccine certification conditional panel
  output$show_vaccine_cert <- reactive({
    !is.null(input$vaccine_history_timeline_selected) && !is.na(cert())
  })
  outputOptions(output, "show_vaccine_cert", suspendWhenHidden = FALSE)
  
  # show vaccine related file if vaccine is selected in timeline
  output$vaccine_cert <- renderUI({
    
    if (!is.null(input$vaccine_history_timeline_selected)) {
      
      if (!is.na(cert())) {
        cert() %>% 
          str_replace("https://s3.amazonaws.com", "s3:/") %>%
          get_object() %>% 
          writeBin("www/vaccine.pdf") # tempfile(fileext = ".pdf")
        tags$iframe(style = "height:1400px; width:100%", src = "vaccine.pdf")
      } else {
        h3("No Vaccine Certificate Available")
      }

}
  })
  
  # Download vaccine certificate
  output$download_vacc <- downloadHandler(
    filename = function() {
      "vaccine_cert.pdf"
    },
    content = function(file) {
        file.copy("www/vaccine.pdf", file)
    }
  )
  
  # clear selection if different dog is chosen not quite sure how to do this, get warning of argument is length zero
  # observeEvent(input$pet, 
  #    input$vaccine_history_timeline_selected <- NULL
  #  )
  
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