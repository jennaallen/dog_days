USE PetRecords;
CREATE  OR REPLACE VIEW viewTestsPets AS
SELECT dimTests.test_id,
    dimTests.test_name,
    dimTests.test_description,
    dimTests.test_result,
    dimTests.test_series_num,
    dimTests.test_date_performed,
    dimTests.test_date_expires,
    dimTests.test_current_flag,
    dimTests.test_category,
    dimTests.test_result_doc,
    dimTests.pet_id,
    dimTests.vet_id,
    dimTests.visit_id,
    dimPets.pet_name,
    dimPets.pet_dob,
    dimPets.pet_species,
    dimPets.pet_breed,
    dimPets.pet_sex,
    dimPets.pet_color
FROM PetRecords.dimTests
INNER JOIN PetRecords.dimPets
ON dimTests.pet_id = dimPets.pet_id
