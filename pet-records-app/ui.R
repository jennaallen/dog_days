# Web app to display pet records and keep track of visits, test results, vaccines, etc.

library(shiny)
library(shinythemes)
library(sparkline)
library(timevis)
library(DT)
library(shinycssloaders)
library(fontawesome)

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

                 # display relevent pet information ####
                 imageOutput("pet_image", inline = TRUE),
                 br(), br(),
                 htmlOutput("pet_info"), 
                 br(),
                 h5(fa("weight", fill = "#158cba"), "Weight History (lbs.)"),
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
              tabsetPanel(tabPanel(div(fa("notes-medical", fill = "#158cba"), "Medical History"),
                                   # medical history and test timeline ####
                                   wellPanel(h3("Medical History and Tests Timeline"),
                                             h5("Click an item to view more details or test results (where available). The information is shown below the timeline."),
                                             fluidRow(column(4, uiOutput("med_tl_dates")
                                                             ),
                                                      column(4, checkboxInput(inputId = "routine_visits",
                                                                              label = "Show routine visits",
                                                                              value = FALSE)
                                                             ),
                                                      column(4, h4(htmlOutput(outputId = "helpful_text"))
                                                             )
                                                      ),
                                             withSpinner(timevisOutput("med_history_timeline")
                                                         ),
                                             fluidRow(column(12, div(id = "tl_button", actionButton("med_fit", "Show All Items"),
                                                             actionButton("med_default", "Reset to Default")
                                                                    )
                                                             )
                                                      )
                                             ),
                                   # routine details ####
                                   conditionalPanel(condition = "output.show_routine_details",
                                                    fluidRow(column(4, wellPanel(h3("Visit Details"),
                                                                                 htmlOutput(outputId = "routine_visit_info")
                                                                                 )
                                                                    ),
                                                             column(4, wellPanel(h3("Routine Tests"),
                                                                                 tableOutput(outputId = "routine_test_info")
                                                                                 )
                                                                    ),
                                                             column(4, wellPanel(h3("Prescribed Medications"),
                                                                                 htmlOutput(outputId = "routine_medication_info"))
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
                          tabPanel(div(fa("syringe", fill = "#158cba"), "Vaccine History"),
                                   # vaccine history timeline ####
                                   wellPanel(h3("Vaccine Timeline"),
                                             h5("Click an item to view vaccine certificate (where available). The information is shown below the timeline."),
                                             fluidRow(column(8, checkboxGroupInput(inputId = "vacc",
                                                                label = NULL,
                                                                choices = c("Current Vaccines" = "Y", 
                                                                            "Past Vaccines" = "N"),
                                                                selected = "Y"
                                                                )
                                                            ),
                                                      column(4, h4(htmlOutput(outputId = "more_helpful_text")
                                                                   )
                                                             )
                                                      ),
                                             withSpinner(timevisOutput("vaccine_history_timeline")
                                                         ),
                                             actionButton("vaccinefit", "Show All Items")
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
                          tabPanel(div(fa("capsules", fill = "#158cba"), "Medication History"),
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
                          tabPanel(div(fa("user-md", fill = "#158cba"), "Vet History"),
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
