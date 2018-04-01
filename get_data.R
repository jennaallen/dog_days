# get the all the things and put them in the database

library(readr)
library(RMySQL)
library(lubridate)

dimPets <- read_csv("database/modified/dimPets.csv", na = "NULL")
dimVets <- read_csv("database/modified/dimVets.csv", na = "NULL")
dimVisits_orig <- read_csv("database/modified/dimVisits.csv", na = "NULL")
dimTests_orig <- read_csv("database/modified/dimTests.csv", na = "NULL")
dimMeds_orig <- read_csv("database/modified/dimMeds.csv", na = "NULL")
dimVaccines_orig <- read_csv("database/modified/dimVaccines.csv", na = "NULL")



# connect to AWS RDS
con <- dbConnect(MySQL(),
                 username = Sys.getenv("RDSuser"),
                 password = Sys.getenv("RDSpw"),
                 host = Sys.getenv("RDShost"),
                 dbname = 'PetRecords')


dbWriteTable(con, name = "dimPets", value = dimPets , append = TRUE, row.names = FALSE)
dbWriteTable(con, name = "dimVets", value = dimVets , append = TRUE, row.names = FALSE)

pet_ids <- dbGetQuery(con, "SELECT pet_id, pet_name
                                    FROM PetRecords.dimPets")
vet_ids <- dbGetQuery(con, "SELECT vet_id, vet_name
                                    FROM PetRecords.dimVets")
visit_ids <- dbGetQuery(con, "SELECT visit_id, visit_date, pet_id, vet_id
                                    FROM PetRecords.dimVisits")

visit_ids <- visit_ids %>% 
  mutate_at(vars(visit_date), ymd)


dimVisits <- dimVisits_orig %>% 
  filter(visit_category != "Initial visit") %>% 
  left_join(pet_ids, by = "pet_name") %>% 
  left_join(vet_ids, by = "vet_name") %>% 
  select(-visit_id, -pet_name, -vet_name)

dbWriteTable(con, name = "dimVisits", value = dimVisits, append = TRUE, row.names = FALSE)

dimTests <- dimTests_orig %>% 
  left_join(pet_ids, by = "pet_name") %>% 
  left_join(vet_ids, by = "vet_name") %>% 
  left_join(visit_ids, by = c("visit_date", "pet_id", "vet_id")) %>% 
  select(-pet_name, -vet_name, -test_id, -visit_date)
  
dbWriteTable(con, name = "dimTests", value = dimTests, append = TRUE, row.names = FALSE)

dimMeds <- dimMeds_orig %>% 
  left_join(pet_ids, by = "pet_name") %>% 
  left_join(vet_ids, by = "vet_name") %>% 
  left_join(visit_ids, by = c("visit_date", "pet_id", "vet_id")) %>% 
  select(-pet_name, -vet_name, -visit_date)

# the apostrophe in Hill's needs to be escaped
dbWriteTable(con, name = "dimMeds", value = dimMeds, append = TRUE, row.names = FALSE)

dimVaccines <- dimVaccines_orig %>% 
  left_join(pet_ids, by = "pet_name") %>% 
  left_join(vet_ids, by = "vet_name") %>% 
  left_join(visit_ids, by = c("vaccine_date_given" = "visit_date", "pet_id", "vet_id")) %>% 
  select(-pet_name, -vet_name, -vaccine_id)

dbWriteTable(con, name = "dimVaccines", value = dimVaccines, append = TRUE, row.names = FALSE)

# disconnect from RDS
dbDisconnect(con)
