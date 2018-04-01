USE PetRecords;
CREATE  OR REPLACE VIEW viewMedsPetsVets AS
SELECT dimMeds.med_id,
    dimMeds.med_name,
    dimMeds.med_description,
    dimMeds.med_dosage,
    dimMeds.med_dosage_freq,
    dimMeds.med_start_date,
    dimMeds.med_end_date,
    dimMeds.med_category,
    dimMeds.med_current_flag,
    dimMeds.med_prescription,
    dimMeds.pet_id,
    dimMeds.vet_id,
    dimMeds.visit_id,
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
FROM PetRecords.dimMeds
INNER JOIN PetRecords.dimPets
ON dimMeds.pet_id = dimPets.pet_id
LEFT JOIN PetRecords.dimVets
ON dimMeds.vet_id = dimVets.vet_id
