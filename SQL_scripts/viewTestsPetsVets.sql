USE PetRecords;
CREATE  OR REPLACE VIEW viewTestsPetsVets AS
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
    dimTests.created_date,
    dimTests.updated_date,
    dimTests.pet_id,
    dimTests.vet_id,
    dimTests.visit_id,
    dimPets.pet_name,
    dimPets.pet_dob,
    dimPets.pet_species,
    dimPets.pet_breed,
    dimPets.pet_sex,
    dimPets.pet_color,
    dimVets.vet_name,
    dimVets.vet_address,
    dimVets.vet_city,
    dimVets.vet_state,
    dimVets.vet_zip,
    dimVets.vet_phone,
    dimVets.vet_website,
    dimVets.vet_email,
    dimVets.vet_med_rec_site
FROM PetRecords.dimTests
INNER JOIN PetRecords.dimPets
ON dimTests.pet_id = dimPets.pet_id
INNER JOIN PetRecords.dimVets
ON dimTests.vet_id = dimVets.vet_id;

