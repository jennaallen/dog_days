# Web app to display pet records and keep track of visits, test results, vaccines, etc.

library(shiny)
library(shinythemes)
library(sparkline)
library(timevis)
library(DT)
library(shinycssloaders)

fluidPage(
  # define theme ####
  theme = shinytheme("lumen"),
  # use custom css #### 
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet")
    ),
  
  titlePanel("Pet Records",
             windowTitle = "Pet Records"),
  
  sidebarLayout(
    sidebarPanel(width = 2,
                 # display options for pet selection ####
                 uiOutput("all_pets"),
                 
                 hr(), # horizontal line for visual separation
                 
                 uiOutput("dates"),
                 
                 hr(), # horizontal line for visual separation
                 
                 # display relevent pet information ####
                 imageOutput("pet_image", inline = TRUE),
                 br(), br(),
                 htmlOutput("pet_info"), 
                 br(),
                 h5(icon("scale", lib = "glyphicon"), "Weight History (lbs.)"),
                 sparklineOutput("pet_weight"),
                 br(),
                 tableOutput("pet_weight_table"),
                 
                 hr(), # horizontal line for visual separation
                 # flare #### 
                 br(), br(),
                 h5("Built with",
                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                    "by",
                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                    ".")
                 ),
    
    mainPanel(width = 10, 
              tabsetPanel(id = "tabs", tabPanel(div(icon("medkit"), "Medical History"),
                                   # medical history and test timeline ####
                                   wellPanel(h3("Medical History and Tests Timeline"),
                                             h5("Click an item to view more details or test results (where available). The information is shown below the timeline."),
                                             checkboxInput(inputId = "routine_visits",
                                                          label = "Show routine visits",
                                                          value = FALSE),
                                             withSpinner(timevisOutput("med_history_timeline")
                                                         ),
                                             fluidRow(column(2, actionButton("medfit", "Fit All Items")
                                                             ),
                                                      column(10, h4(htmlOutput(outputId = "helpful_text"))
                                                             )
                                                      )
                                             ),
                                   # visit details ####
                                   conditionalPanel(condition = "output.show_visit_details", 
                                                    fluidRow(column(4, wellPanel(h3("Visit Details"),
                                                                                 htmlOutput(outputId = "visit_info")
                                                                                 )
                                                                    ),
                                                             column(4, wellPanel(h3("Tests Performed"),
                                                                                 tableOutput(outputId = "test_info")
                                                                                 )
                                                                    ),
                                                             column(4, wellPanel(h3("Prescribed Medications"), 
                                                                                 htmlOutput(outputId = "medication_info")
                                                                                 )
                                                                    )
                                                             )
                                                    ),
                                   # exam notes and download button ####
                                   conditionalPanel(condition = "output.show_exam", 
                                                    h3("Exam Notes"),
                                                    br(),
                                                    downloadButton("download_exam", "Download PDF"),
                                                    br(), br()
                                                    ),
                                   uiOutput("exam"),
                                   # test result and download button ####
                                   conditionalPanel(condition = "output.show_test_results", 
                                                    h3("Test Results"),
                                                    br(),
                                                    downloadButton("download_test_results", "Download PDF"),
                                                    br(), br()
                                                    ),
                                   uiOutput("test_results")
                                   ),
                          tabPanel(div(icon("heartbeat"), "Vaccine History"),
                                   # vaccine history timeline ####
                                   wellPanel(h3("Vaccine Timeline"),
                                             h5("Select an item to view vaccine certificate (where available). The information is shown below the timeline."),
                                             checkboxGroupInput(inputId = "vacc",
                                                                label = NULL,
                                                                choices = c("Current Vaccines" = "Y", 
                                                                            "Past Vaccines" = "N"),
                                                                selected = "Y"
                                                                ),
                                             withSpinner(timevisOutput("vaccine_history_timeline")
                                                         ),
                                             fluidRow(column(2, actionButton("vaccinefit", "Fit All Items")
                                                             ), 
                                                      column(10, h4(htmlOutput(outputId = "more_helpful_text"))
                                                             )
                                                      )
                                             ),
                                   # show vaccine certificate ####
                                   conditionalPanel(condition = "output.show_vaccine_cert", 
                                                    h3("Vaccine Certificate"),
                                                    br(),
                                                    downloadButton("download_vacc", "Download PDF"),
                                                    br(), br()
                                                    ),
                                   uiOutput("vaccine_cert")
                                   ),
                          tabPanel(div(icon("hospital-o"), "Medication History"),
                                   # current meds table ####
                                   wellPanel(h3("Current Medications"),
                                             br(),
                                             dataTableOutput(outputId = "current_meds_table")
                                             ),
                                   # past meds table ####
                                   wellPanel(h3("Past Medications"), 
                                             br(),
                                             dataTableOutput(outputId = "past_meds_table")
                                             )
                                   ),
                          tabPanel(div(icon("user-md"), "Vet History"),
                                   # vet history ####
                                   wellPanel(h3("Vets"), 
                                             br(),
                                             dataTableOutput(outputId = "vets_table")
                                             )
                                   )
                          )
              )
    )
  )
