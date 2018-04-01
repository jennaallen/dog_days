USE PetRecords;
CREATE  OR REPLACE VIEW viewVisitsMeds AS
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
    dimMeds.med_id,
    dimMeds.med_name,
    dimMeds.med_description,
    dimMeds.med_dosage,
    dimMeds.med_dosage_freq,
    dimMeds.med_start_date,
    dimMeds.med_end_date,
    dimMeds.med_category,
    dimMeds.med_current_flag,
    dimMeds.med_prescription
FROM PetRecords.dimVisits
LEFT JOIN PetRecords.dimMeds
ON dimVisits.visit_id = dimMeds.visit_id;
