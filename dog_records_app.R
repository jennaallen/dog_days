library(shiny)
library(tidyverse)
library(timevis)
library(DT)
library(aws.s3)
library(sparkline)
library(magick)
library(shinythemes)
library(RMySQL)

# source("utils.R")

# pool <- dbPool(
#   drv = RMySQL::MySQL(),
#   dbname = "PetRecords",
#   host = Sys.getenv("RDShost"),
#   username = Sys.getenv("RDSpetsuser"),
#   password = Sys.getenv("RDSpetspw")
# )

# dimTests <- read_csv("dimTests.csv")
# dimMeds <- read_csv("dimMeds.csv")
# dimVisits <- read_csv("dimVisits.csv")
# dimDogs <- read_csv("dimDogs.csv")
# dimVets <- read_csv("dimVets.csv")
# dimVaccines <- read_csv("dimVaccines.csv")


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
    sidebarPanel(uiOutput("all_pets"),
                 
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
                                                                                                                          htmlOutput(outputId = "medication_info"))),
                                                                                                      fluidRow(uiOutput("exam"))
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
  
  pet_records <- reactivePoll(86400000, session,
                              checkFunc = function() {
                                con <- dbConnect(MySQL(),
                                       username = Sys.getenv("RDSpetsuser"),
                                  password = Sys.getenv("RDSpetspw"),
                                  host = Sys.getenv("RDShost"),
                                  dbname = 'PetRecords')
                                
                 max_date <- dbGetQuery(con, "SELECT MAX(updated_date) AS max_updated_date 
                            FROM viewMaxUpdatedDates")
                 
                 # disconnect from RDS
                 dbDisconnect(con)
                 
                 return(max_date)
                 
               }, 
               valueFunc = function() {
                 con <- dbConnect(MySQL(),
                                  username = Sys.getenv("RDSpetsuser"),
                                  password = Sys.getenv("RDSpetspw"),
                                  host = Sys.getenv("RDShost"),
                                  dbname = 'PetRecords')
                 
                 tables <- c("dimPets",
                             "dimTests", 
                             "viewVisitsPets", 
                             "viewMedHistTimeline", 
                             "viewVisitsVets", 
                             "viewVisitsTests", 
                             "viewVisitsMeds", 
                             "viewMedsPetsVets", 
                             "viewVisitsPetsVets", 
                             "viewVaccineHistTimeline")
                 
                 df_list <- setNames(map(tables, ~ dbReadTable(con, .)), tables)
                 
                 # disconnect from RDS
                 dbDisconnect(con)
                 
                 return(df_list)
               }
    
  )
  
  # Check boxes
  output$all_pets <- renderUI({
    pets <- pet_records()$dimPets %>% 
      pull(pet_name)
    
    # Create the checkboxes and select them all by default
    radioButtons(inputId = "pet",
                 label = "Select pet:",
                 choices = pets,
                 selected = "Layla")
  })
  
  # Get pet image to be displayed in sidepanel
  output$pet_image <- renderImage({
    req(input$pet)
    tmpfile <- pet_records()$dimPets %>%
      filter(pet_name %in% input$pet) %>%
      select(pet_picture) %>%
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
    dob <- paste(strong("DOB:"), pet_records()$dimPets %>% 
                   filter(pet_name %in% input$pet) %>% 
                   pull(pet_dob))
    species <- paste(strong("Species:"), pet_records()$dimPets %>% 
                       filter(pet_name %in% input$pet) %>% 
                       pull(pet_species))
    breed <- paste(strong("Breed:"), pet_records()$dimPets %>%
                     filter(pet_name %in% input$pet) %>% 
                     pull(pet_breed))
    sex <- paste(strong("Sex:"), pet_records()$dimPets %>% 
                   filter(pet_name %in% input$pet) %>% 
                   pull(pet_sex))
    color <- paste(strong("Color:"), pet_records()$dimPets %>% 
                     filter(pet_name %in% input$pet) %>% 
                     pull(pet_color))
    paste(dob, species, breed, sex, color, sep = "<br>")
  })
  
  # Create pet weight history sparkline
  output$pet_weight <- renderSparkline({
    req(input$pet)
    pet_records()$viewVisitsPets %>% 
      select(pet_name, visit_date, visit_weight) %>% 
      filter(pet_name == input$pet, !is.na(visit_weight)) %>% 
      arrange(visit_date) %>% 
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
    
    config <- list(
      zoomKey = "ctrlKey"
    )
    
    grouped_data <- pet_records()$viewMedHistTimeline %>% 
      filter(pet_name %in% input$pet) %>% 
      mutate(className = group)
      
    groups <- data.frame(
      id = c("med", "test"),
      content = c("Medical History", "Test History")
    )
      
      timevis(grouped_data, groups = groups, options = config)
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
      pet_records()$dimTests %>% 
        filter(test_id == id()) %>%
        pull(test_result_doc)
     }
  })
  
  exam <- reactive({
    if (show_visit_details_fun()) {
      pet_records()$viewVisitsPets %>% 
        filter(visit_id == id()) %>%
        pull(visit_exam_doc)
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
  
  # create server to ui variable for exam conditional panel
  output$show_exam <- reactive({
    show_visit_details_fun() && !is.na(exam())
  })
  outputOptions(output, "show_exam", suspendWhenHidden = FALSE)
  
  output$visit_info <- renderText({
    
    if (show_visit_details_fun()) {
        
        date <- paste(strong("Visit Date:"), pet_records()$viewVisitsVets %>% 
                        filter(visit_id == id()) %>%
                        pull(visit_date))
        vet <- paste(strong("Vet:"), pet_records()$viewVisitsVets %>% 
                       filter(visit_id == id()) %>%
                       pull(vet_name))
        doctor <- paste(strong("Doctor:"), pet_records()$viewVisitsVets %>%
                       filter(visit_id == id()) %>%
                       pull(visit_doctor))
        vet_phone <- paste(strong("Vet Phone:"), pet_records()$viewVisitsVets %>%
                          filter(visit_id == id()) %>%
                          pull(vet_phone))
        notes <- paste(strong("Visit Information:"), pet_records()$viewVisitsVets %>%
                         filter(visit_id == id()) %>%
                         pull(visit_notes))
        paste(date, vet, doctor, vet_phone, notes, sep = "<br>")
      
    }
  })
  
  output$test_info <- renderText({
    
    if (show_visit_details_fun()) {
      
      pet_records()$viewVisitsTests %>%
          filter(visit_id == id(), !(test_category %in% "routine")) %>% 
          pull(test_name) %>% 
          paste(collapse  = "<br>")
    }
  })
  
  output$medication_info <- renderText({
    
    if (show_visit_details_fun()) {
        
      pet_records()$viewVisitsMeds %>%
          filter(visit_id == id(), !(med_category %in% "flea and tick")) %>% 
          select(med_name) %>% 
          distinct() %>% 
          pull() %>% 
          paste(collapse  = "<br>")
    }
  })
  
  # show exam file if visit is selected in timeline
  output$exam <- renderUI({
    
    if (show_visit_details_fun()) {
      
      if (!is.na(exam())) {
        exam() %>%
          str_replace("https://s3.amazonaws.com", "s3:/") %>%
          get_object() %>%
          writeBin("www/exam.pdf") # tempfile(fileext = ".pdf")
        tags$iframe(style = "height:1400px; width:100%", src = "exam.pdf")
      } else {
        h3("No Exam Notes Available")
      }
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
    pet_records()$viewMedsPetsVets %>%
      filter(pet_name %in% input$pet, med_current_flag == "Y") %>%
      select(Medication = med_name, "Prescribing Vet" =  vet_name, "Start Date" = med_start_date, Dosage = med_dosage, Frequency = med_dosage_freq, Category = med_category) %>% 
      datatable(options = list(pageLength = 5, dom = 'ltip'),
                rownames = FALSE)
  })
  
  # Create past meds data table
  output$past_meds_table <- renderDataTable({
    req(input$pet)
    pet_records()$viewMedsPetsVets %>%
      filter(pet_name %in% input$pet, med_current_flag == "N") %>%
      arrange(desc(med_end_date)) %>%
      select(Medication = med_name, "Prescribing Vet" = vet_name, "End Date" = med_end_date, Dosage = med_dosage, Frequency = med_dosage_freq, Category = med_category) %>% 
      datatable(options = list(pageLength = 10),
                rownames = FALSE)
  })
  
  # Create vets data table
  output$vets_table <- renderDataTable({
    req(input$pet)
    pet_records()$viewVisitsPetsVets %>%
      filter(pet_name %in% input$pet, vet_name != "No Vet") %>%
      select(vet_name, vet_phone, vet_website, vet_email, vet_state) %>% 
      distinct() %>% 
      arrange(vet_name) %>%
      datatable(options = list(pageLength = 10, dom = 'ltip'),
                rownames = FALSE)
  })
  
  # Create vaccine timeline
  output$vaccine_history_timeline <- renderTimevis({
    req(input$pet, input$vacc)
    
    config <- list(
      zoomKey = "ctrlKey"
    )
    
    if (length(input$vacc) == 1 && input$vacc == "Y") {
      pet_records()$viewVaccineHistTimeline %>%
       rowid_to_column(var = "id") %>% 
        filter(pet_name %in% input$pet, current_flag %in% input$vacc) %>% 
        mutate(content = paste0("<b>",content, "</b>", "&nbsp;&nbsp;&nbsp;<i>expires:&nbsp;", days_to_expiration,"&nbsp;days</i>"),
          title = paste("Date Given: ", start, "\n", "Date Expires: ", end, "\n" ,"Vet: ", vet_name, sep = "")) %>% 
        timevis(options = config)
    } else {
     vacc_data <- pet_records()$viewVaccineHistTimeline %>% 
        rowid_to_column(var = "id") %>% 
        filter(pet_name %in% input$pet, current_flag %in% input$vacc) %>% 
        mutate(title = paste("Date Given: ", start, "\n", "Date Expires: ", end, "\n" ,"Vet: ", vet_name, sep = ""),
        group = content)
       
        groups <- data.frame(
          id = c("Rabies", "Distemper", "Bordetella (drops)", "Bordetella (injection)", "Flu", "Lepto", "Rattlesnake", "Fecal Test", "Heartworm Test"),
           content = c("Rabies", "Distemper", "Bordetella (drops)", "Bordetella (injection)", "Flu", "Lepto", "Rattlesnake", "Fecal Test", "Heartworm Test")
        )
         timevis(vacc_data, groups = groups, options = config)
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
  #     inner_join(dimDogs, by = "pet_name") %>% 
  #     filter(pet_name %in% input$pet) %>% 
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