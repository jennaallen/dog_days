USE PetRecords;
CREATE  OR REPLACE VIEW viewVisitsTests AS
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
    dimTests.test_id,
    dimTests.test_name,
    dimTests.test_description,
    dimTests.test_result,
    dimTests.test_series_num,
    dimTests.test_date_performed,
    dimTests.test_date_expires,
    dimTests.test_current_flag,
    dimTests.test_category,
    dimTests.test_result_doc
FROM PetRecords.dimVisits
LEFT JOIN PetRecords.dimTests
ON dimVisits.visit_id = dimTests.visit_id;
