# steps:
# go to pet database and get vaccine expiration date information
# filter for current vaccines
# generate a report for each animal
# send email/slack alert if vaccine due within 1 month from date
# automate so that report is generated weekly
# triggers: if any expiration date of vaccine table is within 1 month of today and if the report is different

library(drake)
library(tidyverse)
library(RMySQL)
library(kableExtra)
library(slackr)
library(formattable)

# connect to AWS RDS
con <- dbConnect(MySQL(),
                 username = Sys.getenv("RDSpetsuser"),
                 password = Sys.getenv("RDSpetspw"),
                 host = Sys.getenv("RDShost"),
                 dbname = 'PetRecords')

days_to_expiration <- as.numeric(dbGetQuery(con, "SELECT MIN(days_to_expiration) as min_date 
                                       FROM viewVaccinesPetsVets
                                       WHERE vaccine_current_flag = 'Y'"))

trigger(condition = days_to_expiration <= 30)

kable_table <- function(dog) {
  dog %>% 
    mutate(vaccine_date_expires = cell_spec(vaccine_date_expires, "html", color = ifelse(days_to_expiration <= 30, "red", "green")),
           days_to_expiration = color_tile("white", "orange")(days_to_expiration)) %>% 
    select(pet_name, vaccine_name, vaccine_date_given, vaccine_date_expires, vet_name, vet_phone, vet_website, vet_email, days_to_expiration) %>% 
    kable(format = "html", escape = F) %>% 
    kable_styling(bootstrap_options = c("striped", "hover"))
}

get_data <- function(dog_name, table, connection) {
  dbReadTable(connection, table) %>% 
    filter(vaccine_current_flag == "Y", pet_name == dog_name) 
}

# max_cu_date <- dbGetQuery(con, "SELECT MAX(cu_date) AS max_cu_date
#                                 FROM viewMaxVaccineCreatedUpdatedDates")
# 
# data_plan <- drake_plan(
#   layla = target(get_data("Layla", "viewVaccinesPetsVets", con), trigger(change = max_cu_date)),
#   lloyd = target(get_data("Lloyd", "viewVaccinesPetsVets", con), trigger(change = max_cu_date)),
#   strings_in_dots = "literals"
# )

data_plan <- drake_plan(
  layla = get_data("Layla", "viewVaccinesPetsVets", con),
  lloyd = get_data("Lloyd", "viewVaccinesPetsVets", con),
  strings_in_dots = "literals"
)

output_types <- drake_plan(
  table = kable_table(dataset__)
)

output_plan <- plan_analyses(
  plan = output_types,
  datasets = data_plan
)

# to get slack to work I had to generate a token here https://api.slack.com/custom-integrations/legacy-tokens
slackrSetup()

report_plan <- drake_plan(
  report = rmarkdown::render(
    knitr_in("alerts.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE),
  notification = slackr("A new vaccine report is ready"),
  strings_in_dots = "literals"
)

whole_plan <- bind_plans(
  data_plan,
  output_plan,
  report_plan
)

# config <- drake_config(whole_plan)
# vis_drake_graph(config)

# if (days_to_expiration <= 30) {
#   make(whole_plan)
# }

make(whole_plan)

# disconnect from RDS
dbDisconnect(con)
        
        