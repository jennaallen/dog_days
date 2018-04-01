USE PetRecords;
CREATE  OR REPLACE VIEW viewVisitsPets AS
SELECT dimVisits.visit_id,
    dimVisits.visit_date,
    dimVisits.visit_weight,
    dimVisits.visit_receipt,
    dimVisits.visit_notes,
    dimVisits.med_visit_summary,
    dimVisits.routine_visit_summary,
    dimVisits.visit_category,
    dimVisits.visit_doctor,
    dimVisits.visit_exam_doc,
    dimVisits.pet_id,
    dimVisits.vet_id,
    dimPets.pet_name,
    dimPets.pet_dob,
    dimPets.pet_species,
    dimPets.pet_breed,
    dimPets.pet_sex,
    dimPets.pet_color
FROM PetRecords.dimVisits
INNER JOIN PetRecords.dimPets
ON dimVisits.pet_id = dimPets.pet_id

