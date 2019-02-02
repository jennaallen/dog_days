# Web app to display pet records and keep track of visits, test results, vaccines, etc.

library(shiny)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(timevis)
library(DT)
library(aws.s3)
library(sparkline)
library(magick)
library(RSQLite)
library(lubridate)
library(fs)

source("utils.R")

# getting the data outside of server, so data is created once 
# and shared across all user sessions (within the same R process)
# switching to sqlite

tables <- c("dimPets",
            "dimTests",
            "viewVisitsPets",
            "viewRoutineMedHistTimeline",
            "viewMedHistTimeline",
            "viewVisitsVets",
            "viewVisitsTests",
            "viewVisitsMeds",
            "viewMedsPetsVets",
            "viewVisitsPetsVets",
            "viewVaccineHistTimeline")
con <- dbConnect(SQLite(), "PetRecords.sqlite")
pet_records <- setNames(map(tables, ~ dbReadTable(con, .)), tables)
dbDisconnect(con)

function(input, output, session) {
  # create options for pet selection radio button ####
  output$all_pets <- renderUI({
    pets <- pet_records$dimPets %>% 
      pull(pet_name)
    
    radioButtons(inputId = "pet",
                 label = "Select pet:",
                 choices = pets,
                 selected = "Layla")
  })
  
  # get pet image to be displayed in sidepanel ####
  output$pet_image <- renderImage({
    req(input$pet)
    
    tmpfile <- pet_records$dimPets %>%
      filter(pet_name %in% input$pet) %>%
      select(pet_picture) %>%
      str_replace("https://s3.amazonaws.com", "s3:/") %>%
      get_object() %>%
      image_read() %>%
      image_write(tempfile(fileext = ".png"), format = "png")
    
    list(src = tmpfile,
         height = "200px",
         contentType = "image/png")
  }, deleteFile = TRUE)
  
  # create pet info to be displayed in sidepanel ####
  output$pet_info <- renderText({
    req(input$pet)
    
    dob <- paste(strong("DOB:"), pet_records$dimPets %>% 
                   filter(pet_name %in% input$pet) %>% 
                   mutate(pet_dob = format(as.Date(pet_dob), format = "%m-%d-%Y")) %>% 
                   pull(pet_dob))
    species <- paste(strong("Species:"), pet_records$dimPets %>% 
                       filter(pet_name %in% input$pet) %>% 
                       pull(pet_species))
    breed <- paste(strong("Breed:"), pet_records$dimPets %>%
                     filter(pet_name %in% input$pet) %>% 
                     pull(pet_breed))
    sex <- paste(strong("Sex:"), pet_records$dimPets %>% 
                   filter(pet_name %in% input$pet) %>% 
                   pull(pet_sex))
    color <- paste(strong("Color:"), pet_records$dimPets %>% 
                     filter(pet_name %in% input$pet) %>% 
                     pull(pet_color))
    
    paste(dob, species, breed, sex, color, sep = "<br>")
  })
  
  # create pet weight history sparkline ####
  output$pet_weight <- renderSparkline({
    req(input$pet)
    
    pet_records$viewVisitsPets %>% 
      select(pet_name, visit_date, visit_weight) %>% 
      filter(pet_name == input$pet, !is.na(visit_weight)) %>% 
      arrange(visit_date) %>% 
      pull(visit_weight) %>% 
      sparkline(width = "98%", 
                height = "100px", 
                spotRadius = 7,
                spotColor = FALSE,
                highlightSpotColor = "#999", 
                fillColor = FALSE, 
                lineColor = "#999", 
                highlightLineColor = "#333",
                lineWidth = 3,
                maxSpotColor = "#fd7e14",
                minSpotColor = "#fd7e14")
  })
  
  output$pet_weight_table <- renderTable({
    req(input$pet)
    
    pet_records$viewVisitsPets %>%
      filter(pet_name == input$pet, !is.na(visit_weight)) %>% 
      select(visit_date, visit_weight) %>% 
      arrange(desc(visit_date)) %>% 
      mutate_at(vars(visit_date), funs(format(as.Date(.), format = "%m-%d-%Y"))) %>% 
      mutate(Weight = formatC(visit_weight, format = "f", digits = 1)) %>%
      select(Date = visit_date, Weight)
  })
  
  # create date selections for med timeline date range input ####
  output$med_tl_dates <- renderUI({
    min_date <- pet_records$viewRoutineMedHistTimeline %>% 
      select(start) %>%
      summarize(min(start)) %>% 
      pull()
    
    dateRangeInput(inputId = "med_tl_date_range", 
                   label = "Select Date Range:", 
                   start = Sys.Date() %m-% months(18), 
                   end = Sys.Date() %m+% months(1), 
                   min = ymd(min_date) %m-% months(2),
                   format = "mm-dd-yyyy",
                   startview = "years",
                   width = "75%")
  })
  
  # create medical and tests history timeline ####
  output$med_history_timeline <- renderTimevis({
    req(input$pet)
    
    config <- list(
      zoomKey = "ctrlKey",
      start = input$med_tl_date_range[1],
      end = input$med_tl_date_range[2]
    )
    
    if (input$routine_visits) {
      grouped_data <- pet_records$viewRoutineMedHistTimeline %>% 
        filter(pet_name %in% input$pet) %>% 
        mutate(className = group,
               title = paste("Date:", format(as.Date(start), format = "%m-%d-%Y")))
      
      groups <- data.frame(
        id = c("routine", "med", "test"),
        content = c("Routine", "Medical History", "Test History")
      )
    } else {
      grouped_data <- pet_records$viewMedHistTimeline %>% 
        filter(pet_name %in% input$pet) %>% 
        mutate(className = group,
               title = paste("Date:", format(as.Date(start), format = "%m-%d-%Y")))
      
      groups <- data.frame(
        id = c("med", "test"),
        content = c("Medical History", "Test History")
        )
    }
    timevis(grouped_data, groups = groups, options = config)
  })
  
  # update med timeline date range based on user input and button pushes ####

  # fit all items on timeline on button push
  observeEvent(input$med_fit, {
    fitWindow("med_history_timeline")
  })
  
  # change timeline back to default window
  observeEvent(input$med_default, {
    setWindow("med_history_timeline", Sys.Date() %m-% months(18), Sys.Date() %m+% months(1))
    
    # updateCheckboxInput(session, inputId = "routine_visits", value = FALSE) for some reason adding this doesn't change timeline window and 
    # is in conflict with the updateDateRangeInput below
  })
  
  observeEvent(input$med_history_timeline_window, {
    updateDateRangeInput(session, inputId = "med_tl_date_range",
                         start = prettyDate(input$med_history_timeline_window[1]),
                         end = prettyDate(input$med_history_timeline_window[2]))
  })
  
  # define reactiveValues to prevent errors when user has an item selected in a timeline 
  # and then changes the pet filter or routine visits checkbox
  # reactive values ####
  values <- reactiveValues(med_tl_selected = NULL, vacc_tl_selected = NULL )
  
  # pass value of input$med_history_timeline_selected to reactive value
  observe({
    values$med_tl_selected <- input$med_history_timeline_selected
  })
  
  # clear selection if different pet is chosen or routine visits is checked or unchecked
  observeEvent(c(input$pet, input$routine_visits), {
    values$med_tl_selected <- NULL
  })
  
  # display text to help user know to scroll down
  output$helpful_text <- renderText({
    if (!is.null(values$med_tl_selected)) {
      "Scroll down to view details"
    }
  })
  
  # create reactive variables for use in med timeline render functions ####
  get_group <- reactive({
    if (!is.null(values$med_tl_selected)) {
      input$med_history_timeline_data %>%
        filter(id == values$med_tl_selected) %>%
        pull(group)
    }
  })
  
  show_routine_fun <- reactive({
    !is.null(values$med_tl_selected) && get_group() == "routine"
  })
  
  show_visit_details_fun <- reactive({
    !is.null(values$med_tl_selected) && get_group() == "med"
  })
  
  show_test_results_fun <- reactive({
    !is.null(values$med_tl_selected) && get_group() == "test"
  })
  
  id <- reactive({
    if (!is.null(values$med_tl_selected)) {
      values$med_tl_selected %>% 
        str_extract("\\d+") %>% 
        as.integer()
    }
  })    
  
  test_result <- reactive({
    if (show_test_results_fun()) {
      pet_records$dimTests %>% 
        filter(test_id == id()) %>%
        pull(test_result_doc)
    }
  })
  
  exam <- reactive({
    if (show_visit_details_fun() | show_routine_fun()) {
      pet_records$viewVisitsPets %>% 
        filter(visit_id == id()) %>%
        pull(visit_exam_doc)
    }
  })
  
  # create server to ui variables for conditional panels ####
  # create server to ui variable for routine info conditional panel
  output$show_routine_details <- reactive({
    show_routine_fun()
  })
  outputOptions(output, "show_routine_details", suspendWhenHidden = FALSE)
  
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
    (show_visit_details_fun() | show_routine_fun()) && !is.na(exam())
  })
  outputOptions(output, "show_exam", suspendWhenHidden = FALSE)
  
  # get routine details ####
  output$routine_visit_info <- renderText({
    if (show_routine_fun()) {
      date <- paste(strong("Visit Date:"), pet_records$viewVisitsVets %>% 
                      filter(visit_id == id()) %>%
                      mutate(visit_date = format(as.Date(visit_date), format = "%m-%d-%Y")) %>% 
                      pull(visit_date))
      vet <- paste(strong("Vet:"), pet_records$viewVisitsVets %>% 
                     filter(visit_id == id()) %>%
                     pull(vet_name))
      doctor <- paste(strong("Doctor:"), pet_records$viewVisitsVets %>%
                        filter(visit_id == id()) %>%
                        pull(visit_doctor))
      vet_phone <- paste(strong("Vet Phone:"), pet_records$viewVisitsVets %>%
                           filter(visit_id == id()) %>%
                           pull(vet_phone))
      visit_category <- pet_records$viewVisitsVets %>%
        filter(visit_id == id()) %>%
        pull(visit_category)
      # notes for visits tagged as medical and routine are shown when clicking medical item
      if (str_detect(visit_category, "medical")) {
        notes <- NA
      } else {
        notes <- paste(strong("Visit Information:"), pet_records$viewVisitsVets %>%
                         filter(visit_id == id()) %>%
                         pull(visit_notes))
      }
      
      paste(date, vet, doctor, vet_phone, notes, sep = "<br>")
    }
  })
  
  output$routine_test_info <- renderTable({
    if (show_routine_fun()) {
      tests <- pet_records$viewVisitsTests %>%
        filter(visit_id == id(), test_category %in% "routine") %>% 
        mutate_at(vars(test_name, test_result), funs(replace(., is.na(.), "None"))) %>% 
        select(Name = test_name, Result = test_result) 
      if (nrow(tests) == 0) {
        tibble(Name = "None",
               Result = "None")
      } else {
        tests
      }
    }
  })
  
  output$routine_medication_info <- renderText({
    if (show_routine_fun()) {
      meds <- pet_records$viewVisitsMeds %>%
        filter(visit_id == id(), med_category %in% c("flea and tick", "heartworm")) %>% 
        select(med_name) %>% 
        mutate_at(vars(med_name), funs(replace(., is.na(.), "None"))) %>% 
        distinct() %>% 
        pull() %>% 
        paste(collapse  = "<br>")
      
      if (meds == "") {
        "None"
      } else {
        meds
      }
    }
  })
  
  
  # get visit details ####
  output$visit_info <- renderText({
    if (show_visit_details_fun()) {
      date <- paste(strong("Visit Date:"), pet_records$viewVisitsVets %>% 
                      filter(visit_id == id()) %>%
                      mutate(visit_date = format(as.Date(visit_date), format = "%m-%d-%Y")) %>% 
                      pull(visit_date))
      vet <- paste(strong("Vet:"), pet_records$viewVisitsVets %>% 
                     filter(visit_id == id()) %>%
                     pull(vet_name))
      doctor <- paste(strong("Doctor:"), pet_records$viewVisitsVets %>%
                        filter(visit_id == id()) %>%
                        pull(visit_doctor))
      vet_phone <- paste(strong("Vet Phone:"), pet_records$viewVisitsVets %>%
                           filter(visit_id == id()) %>%
                           pull(vet_phone))
      notes <- paste(strong("Visit Information:"), pet_records$viewVisitsVets %>%
                       filter(visit_id == id()) %>%
                       pull(visit_notes))
      
      paste(date, vet, doctor, vet_phone, notes, sep = "<br>")
    }
  })
  
  output$test_info <- renderTable({
    if (show_visit_details_fun()) {
      pet_records$viewVisitsTests %>%
        filter(visit_id == id(), !(test_category %in% "routine")) %>% 
        mutate_at(vars(test_name, test_result), funs(replace(., is.na(.), "None"))) %>% 
        select(Name = test_name, Result = test_result) 
    }
  })
  
  output$medication_info <- renderText({
    if (show_visit_details_fun()) {
      pet_records$viewVisitsMeds %>%
        filter(visit_id == id(), !(med_category %in% c("flea and tick", "heartworm"))) %>% 
        select(med_name) %>% 
        mutate_at(vars(med_name), funs(replace(., is.na(.), "None"))) %>% 
        distinct() %>% 
        pull() %>% 
        paste(collapse  = "<br>")
    }
  })
  
  # show exam file if visit is selected in timeline ####
  exam_path <- file_temp("exam_pdf", tmp_dir = "www", ext = ".pdf")
  
  output$exam <- renderUI({
    if (show_visit_details_fun() | show_routine_fun()) {
      if (!is.na(exam())) {
        exam() %>%
          str_replace("https://s3.amazonaws.com", "s3:/") %>%
          get_object() %>%
          writeBin(exam_path)
        tags$iframe(style = "height:1400px; width:100%", src = str_sub(exam_path, 5))
      } else {
        h3("No Exam Notes Available")
        }
    }
  })
  
  # download exam results ####
  output$download_exam <- downloadHandler(
    filename = function() {
      "exam.pdf"
    },
    content = function(file) {
      file.copy(exam_path, file)
    }
  )
  
  # show test results file if test is selected in timeline ####
  test_result_path <- file_temp("test_result_pdf", tmp_dir = "www", ext = ".pdf")
  
  output$test_results <- renderUI({
    if (show_test_results_fun()) {
      if (!is.na(test_result())) {
        test_result() %>%
          str_replace("https://s3.amazonaws.com", "s3:/") %>%
          get_object() %>%
          writeBin(test_result_path)
        tags$iframe(style = "height:1400px; width:100%", src = str_sub(test_result_path, 5))
      } else {
        h3("No Test Results Available")
        }
    }
  })
  
  # download test results ####
  output$download_test_results <- downloadHandler(
    filename = function() {
      "test_result.pdf"
    },
    content = function(file) {
      file.copy(test_result_path, file)
    }
  )
  
  # create current meds data table ####
  output$current_meds_table <- renderDataTable({
    req(input$pet)
    
    pet_records$viewMedsPetsVets %>%
      filter(pet_name %in% input$pet, med_current_flag == "Y") %>%
      arrange(desc(med_start_date)) %>%
      mutate(med_start_date = format(as.Date(med_start_date), format = "%m-%d-%Y")) %>% 
      select(Medication = med_name, "Prescribing Vet" =  vet_name, "Start Date" = med_start_date, Dosage = med_dosage, Frequency = med_dosage_freq, Category = med_category) %>% 
      datatable(options = list(pageLength = 10, dom = 'ltip'),
                rownames = FALSE)
  })
  
  # create past meds data table ####
  output$past_meds_table <- renderDataTable({
    req(input$pet)
    
    pet_records$viewMedsPetsVets %>%
      filter(pet_name %in% input$pet, med_current_flag == "N") %>%
      arrange(desc(med_end_date)) %>%
      mutate(med_end_date = format(as.Date(med_end_date), format = "%m-%d-%Y"),
             med_start_date = format(as.Date(med_start_date), format = "%m-%d-%Y")) %>% 
      select(Medication = med_name, "Prescribing Vet" = vet_name, "Start Date" = med_start_date, "End Date" = med_end_date, Dosage = med_dosage, Frequency = med_dosage_freq, Category = med_category) %>% 
      datatable(options = list(pageLength = 10),
                rownames = FALSE)
  })
  
  # create vets data table ####
  output$vets_table <- renderDataTable({
    req(input$pet)
    
    pet_records$viewVisitsPetsVets %>%
      filter(pet_name %in% input$pet, vet_name != "No Vet") %>%
      select(Vet = vet_name, Phone = vet_phone, Website = vet_website, Email = vet_email, State = vet_state) %>% 
      distinct() %>% 
      arrange(Vet) %>%
      datatable(options = list(pageLength = 10, dom = 'ltip'),
                rownames = FALSE)
  })
  
  # create vaccine timeline ####
  output$vaccine_history_timeline <- renderTimevis({
    req(input$pet, input$vacc)
    
    config <- list(
      zoomKey = "ctrlKey"
    )
    
    if (length(input$vacc) == 1 && input$vacc == "Y") {
      pet_records$viewVaccineHistTimeline %>%
        rowid_to_column(var = "id") %>% 
        filter(pet_name %in% input$pet, current_flag %in% input$vacc) %>% 
        mutate(content = paste0("<b>",content, "</b>", "&nbsp;&nbsp;&nbsp;<i>expires in ", days_to_expiration," days</i>"),
               title = paste("Date Given: ", format(as.Date(start), format = "%m-%d-%Y"), "\n", "Date Expires: ", format(as.Date(end), format = "%m-%d-%Y"), "\n" ,"Vet: ", vet_name, sep = "")) %>% 
        timevis(options = config)
    } else {
      vacc_data <- pet_records$viewVaccineHistTimeline %>% 
        rowid_to_column(var = "id") %>% 
        filter(pet_name %in% input$pet, current_flag %in% input$vacc) %>% 
        mutate(title = paste("Date Given: ", format(as.Date(start), format = "%m-%d-%Y"), "\n", "Date Expires: ", format(as.Date(end), format = "%m-%d-%Y"), "\n" ,"Vet: ", vet_name, sep = ""),
               group = content)
      
      groups <- data.frame(
        id = c("Rabies", "Distemper", "Bordetella (drops)", "Bordetella (injection)", "Flu", "Lepto", "Rattlesnake", "Fecal Test", "Heartworm Test"),
        content = c("Rabies", "Distemper", "Bordetella (drops)", "Bordetella (injection)", "Flu", "Lepto", "Rattlesnake", "Fecal Test", "Heartworm Test")
      )
      timevis(vacc_data, groups = groups, options = config)
      }
  })
  
  # reset timeline view on button push
  observeEvent(input$vaccinefit, {
    fitWindow("vaccine_history_timeline")
  })
  
  # pass value of input$vaccine_history_timeline_selected to reactive value ####
  observe({
    values$vacc_tl_selected <- input$vaccine_history_timeline_selected
  })
  
  # clear selection if different pet or current vs. past vaccine history is chosen
  observeEvent(c(input$pet, input$vacc), {
    values$vacc_tl_selected <- NULL
  })
  
  # display text to help user know to scroll down
  output$more_helpful_text <- renderText({
    if (!is.null(values$vacc_tl_selected)) {
      "Scroll down to view details"
    }
  })
  
  cert <- reactive({
    # get doc not found error if data is filtered and all values in the column are NA
    # timeline data deletes column if all values in a column are NA
    if (!is.null(values$vacc_tl_selected)) {
      input$vaccine_history_timeline_data %>%
        filter(id == values$vacc_tl_selected) %>%  
        pull(doc)
    }
  })  
  
  # create server to ui variable for vaccine certificate conditional panel ####
  output$show_vaccine_cert <- reactive({
    !is.null(values$vacc_tl_selected) && !is.na(cert())
  })
  outputOptions(output, "show_vaccine_cert", suspendWhenHidden = FALSE)
  
  # show vaccine cert if vaccine is selected in timeline ####
  vaccine_cert_path <- file_temp("vaccine_cert_pdf", tmp_dir = "www", ext = ".pdf")
  
  output$vaccine_cert <- renderUI({
    if (!is.null(values$vacc_tl_selected)) {
      if (!is.na(cert())) {
        cert() %>% 
          str_replace("https://s3.amazonaws.com", "s3:/") %>%
          get_object() %>% 
          writeBin(vaccine_cert_path)
        tags$iframe(style = "height:1400px; width:100%", src = str_sub(vaccine_cert_path, 5))
      } else {
        h3("No Vaccine Certificate Available")
        }
    }
  })
  
  # download vaccine certificate ####
  output$download_vacc <- downloadHandler(
    filename = function() {
      "vaccine_cert.pdf"
    },
    content = function(file) {
      file.copy(vaccine_cert_path, file)
    }
  )
  
  # delete session files when the application exits (when runApp exits) 
  # or after each user session ends
  onStop(function()
    unlink(c(test_result_path, exam_path, vaccine_cert_path)))
  
}