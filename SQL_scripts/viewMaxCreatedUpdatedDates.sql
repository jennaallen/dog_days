USE PetRecords;
CREATE  OR REPLACE VIEW viewMaxCreatedUpdatedDates AS
SELECT MAX(updated_date) AS cu_date
FROM PetRecords.dimMeds
UNION
SELECT MAX(created_date) AS cu_date
FROM PetRecords.dimMeds
UNION
SELECT MAX(updated_date) AS cu_date
FROM PetRecords.dimPets
UNION
SELECT MAX(created_date) AS cu_date
FROM PetRecords.dimPets
UNION
SELECT MAX(updated_date) AS cu_date
FROM PetRecords.dimTests
UNION
SELECT MAX(created_date) AS cu_date
FROM PetRecords.dimTests
UNION
SELECT MAX(updated_date) AS cu_date
FROM PetRecords.dimVaccines
UNION
SELECT MAX(created_date) AS cu_date
FROM PetRecords.dimVaccines
UNION
SELECT MAX(updated_date) AS cu_date
FROM PetRecords.dimVets
UNION
SELECT MAX(created_date) AS cu_date
FROM PetRecords.dimVets
UNION
SELECT MAX(updated_date) AS cu_date
FROM PetRecords.dimVisits
UNION
SELECT MAX(created_date) AS cu_date
FROM PetRecords.dimVisits

SELECT MAX(cu_date) AS max_cu_date
FROM viewMaxCreatedUpdatedDates