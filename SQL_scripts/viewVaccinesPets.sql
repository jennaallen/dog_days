USE PetRecords;
CREATE  OR REPLACE VIEW viewVaccinesPets AS
SELECT dimVaccines.vaccine_id,
    dimVaccines.vaccine_name,
    dimVaccines.vaccine_description,
    dimVaccines.vaccine_series_num,
    dimVaccines.vaccine_date_given,
    dimVaccines.vaccine_date_expires,
    dimVaccines.vaccine_notes,
    dimVaccines.vaccine_current_flag,
    dimVaccines.vaccine_certificate,
    dimVaccines.pet_id,
    dimVaccines.vet_id,
    dimVaccines.visit_id,
    dimPets.pet_name,
    dimPets.pet_dob,
    dimPets.pet_species,
    dimPets.pet_breed,
    dimPets.pet_sex,
    dimPets.pet_color
FROM PetRecords.dimVaccines
INNER JOIN PetRecords.dimPets
ON dimVaccines.pet_id = dimPets.pet_id


